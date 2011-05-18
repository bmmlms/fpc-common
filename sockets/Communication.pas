{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2011 Alexander Nottelmann

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 3
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
    ------------------------------------------------------------------------
}
unit Communication;

interface

uses
  Windows, SysUtils, Classes, Sockets, Protocol, Commands, Generics.Collections,
  ExtendedStream;

type
  TDynStringArray = array of string;

  TCommandEvent = procedure(Socket: TSocketThread; Command: TCommand) of object;

  TFileTransferInfo = class
  public
    Filename: string;
    Stream: TExtendedStream;
    Direction: TTransferDirection;
  end;

  TCommandThreadBase = class(TSocketThread)
  private
    FPacketSender: TPacketManager;
    FPacketReader: TPacketManager;
    FProtocolManager: TProtocolManager;
//    FFileTransfers: TList<TFileTransferInfo>;
    FLastSyncedTransfer: Cardinal;

    FProc: TCommandEvent;

    FOnDebug: TDebugEvent;
    FOnCommandReceived: TCommandEvent;
    FOnBytesTransferred: TTransferProgressEvent;

    FDebugData: string;
    FCommand: TCommand;
    FTransferredCommandID: Cardinal;
    FTransferredSize: UInt64;
    FTransferredTransferred: UInt64;
    FTransferredDirection: TTransferDirection;

    procedure SyncCommand(Proc: TCommandEvent; Command: TCommand);
    procedure SyncCommand2;
    procedure SyncDebug;
    procedure SyncTransferred;

    procedure PacketManagerDebug(Sender: TSocketThread; Data: string);
    procedure PacketManagerBytesTransferred(Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal; Size, Transferred: UInt64);
  protected
    procedure DoStuff; override;

    procedure DoReceivedData(Buf: Pointer; Len: Integer); override;
    procedure DoReceivedCommand(Command: TCommand); virtual;
    procedure DoException(E: Exception); override;

    procedure WriteDebug(Data: string);
  public
    constructor Create(Handle: Cardinal; Stream: TSocketStream); overload; override;
    constructor Create(Host: string; Port: Integer; Stream: TSocketStream); overload; override;
    destructor Destroy; override;

    function SendCommand(Command: TCommand): Cardinal;
//    property PacketSender: TPacketManager read FPacketSender;
    property ProtocolManager: TProtocolManager read FProtocolManager;
    property OnBytesTransferred: TTransferProgressEvent read FOnBytesTransferred write FOnBytesTransferred;
    property OnCommandReceived: TCommandEvent read FOnCommandReceived write FOnCommandReceived;
    property OnDebug: TDebugEvent read FOnDebug write FOnDebug;
  end;

  TCommandSocketBase = class
  protected
    FOnConnected: TSocketEvent;
    FOnBytesTransferred: TTransferProgressEvent;
    FOnCommandReceived: TCommandEvent;
    FOnEnded: TSocketEvent;

    procedure ThreadConnected(Sender: TSocketThread); virtual;
    procedure ThreadEnded(Sender: TSocketThread); virtual;
  public
    property OnConnected: TSocketEvent read FOnConnected write FOnConnected;
    property OnBytesTransferred: TTransferProgressEvent read FOnBytesTransferred write FOnBytesTransferred;
    property OnCommandReceived: TCommandEvent read FOnCommandReceived write FOnCommandReceived;
    property OnEnded: TSocketEvent read FOnEnded write FOnEnded;
  end;

  TCommandStream = class(TSocketStream)
  public
  end;

  TSocketThreadList = TList<TSocketThread>;

  TCommandClient = class(TCommandSocketBase)
  private
    FThread: TCommandThreadBase;
    FHost: string;

    procedure ThreadBytesTransferred(Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal; Size, Transferred: UInt64);
    procedure ThreadCommandReceived(Socket: TSocketThread; Command: TCommand);
  protected
    procedure ThreadConnected(Sender: TSocketThread); override;
    procedure ThreadEnded(Sender: TSocketThread); override;
  public
    procedure Start;
    procedure Stop;

    property Thread: TCommandThreadBase read FThread;
    property Host: string read FHost write FHost;
  end;

  TCommandClientThread = class(TCommandThreadBase)
  protected
    procedure DoConnected; override;
    procedure DoReceivedCommand(Command: TCommand); override;
  end;

  TClientStream = class(TCommandStream)
  public
    procedure Process(Received: Cardinal); override;
  end;

implementation

{ TCommandThreadBase }

constructor TCommandThreadBase.Create(Handle: Cardinal;
  Stream: TSocketStream);
begin
  inherited;

  FPacketSender := TPacketManager.Create;
  FPacketSender.OnDebug := PacketManagerDebug;
  FPacketSender.OnBytesTransferred := PacketManagerBytesTransferred;

  FPacketReader := TPacketManager.Create;
  FPacketReader.OnDebug := PacketManagerDebug;
  FPacketReader.OnBytesTransferred := PacketManagerBytesTransferred;

  FProtocolManager := TProtocolManager.Create;
end;

constructor TCommandThreadBase.Create(Host: string; Port: Integer;
  Stream: TSocketStream);
begin
  inherited;

  FPacketSender := TPacketManager.Create;
  FPacketSender.OnDebug := PacketManagerDebug;

  FPacketReader := TPacketManager.Create;
  FPacketReader.OnDebug := PacketManagerDebug;

  FProtocolManager := TProtocolManager.Create;
end;

destructor TCommandThreadBase.Destroy;
begin
  FPacketSender.Free;
  FPacketReader.Free;

  FProtocolManager.Free;

  inherited;
end;

procedure TCommandThreadBase.DoException(E: Exception);
begin
  inherited;
  WriteDebug(E.Message);
end;

procedure TCommandThreadBase.DoReceivedCommand(Command: TCommand);
begin
  // Fertiges Command Bytes rübergeben
  if Assigned(FOnBytesTransferred) then
    FOnBytesTransferred(Self, tdReceive, Command.ID, Command.CmdLength, Command.CmdLength);

  FProtocolManager.Handle(Command, tdReceive);
end;

procedure TCommandThreadBase.DoReceivedData(Buf: Pointer; Len: Integer);
var
  i: Integer;
  Cmd: TCommand;
begin
  inherited;

  FPacketReader.Read(FRecvStream);

  for i := FPacketReader.ReceivedCommands.Count - 1 downto 0 do
  begin
    Cmd := FPacketReader.ReceivedCommands[i];
    DoReceivedCommand(Cmd);

    SyncCommand(FOnCommandReceived, Cmd);

    FPacketReader.ReceivedCommands.Remove(Cmd);
    Cmd.Free;
  end;
end;

procedure TCommandThreadBase.DoStuff;
var
  i: Integer;
  P: TPacket;
begin
  inherited;

  FPacketSender.Process;

  FSendLock.Enter;
  try
    while FSendStream.Size < 1000000 do
    begin
      P := FPacketSender.BuildPacket;
      if P <> nil then
      begin
        P.Write(FSendStream);
        P.Free;
      end else
        Break;
    end;
  finally
    FSendLock.Leave;
  end;

  if FLastSyncedTransfer < GetTickCount - 500 then
  begin
    if FPacketSender.SendCache.Count > 0 then
      for i := 0 to FPacketSender.SendCache.Count - 1 do
      begin
        PacketManagerBytesTransferred(Self, tdSend, FPacketSender.SendCache[i].ID,
          FPacketSender.SendCache[i].Size, FPacketSender.SendCache[i].Transferred);
      end;

    if FPacketReader.RecvCache.Count > 0 then
      for i := 0 to FPacketReader.RecvCache.Count - 1 do
      begin
        PacketManagerBytesTransferred(Self, tdReceive, FPacketReader.RecvCache[i].ID,
          FPacketReader.RecvCache[i].Size, FPacketReader.RecvCache[i].Transferred);
      end;

    FLastSyncedTransfer := GetTickCount;
  end;
end;

procedure TCommandThreadBase.PacketManagerBytesTransferred(
  Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal;
  Size, Transferred: UInt64);
begin
  FTransferredCommandID := CommandID;
  FTransferredSize := Size;
  FTransferredTransferred := Transferred;
  FTransferredDirection := Direction;

  if Assigned(FOnBytesTransferred) then
    if UseSynchronize then
      Synchronize(SyncTransferred)
    else
      SyncTransferred;
end;

procedure TCommandThreadBase.PacketManagerDebug(Sender: TSocketThread;
  Data: string);
begin
  WriteDebug(Data);
end;

function TCommandThreadBase.SendCommand(Command: TCommand): Cardinal;
begin
  Result := FPacketSender.Send(Command);

  // Muss unter .Send(), weil es erst hier eine ID hat.
  FProtocolManager.Handle(Command, tdSend);
end;

procedure TCommandThreadBase.SyncCommand(Proc: TCommandEvent;
  Command: TCommand);
begin
  if Assigned(Proc) then
  begin
    FProc := Proc;
    FCommand := Command;
    if UseSynchronize then
      Synchronize(SyncCommand2)
    else
      SyncCommand2;
  end;
end;

procedure TCommandThreadBase.SyncCommand2;
begin
  FProc(Self, FCommand);
end;

procedure TCommandThreadBase.SyncDebug;
begin
  if Assigned(FOnDebug) then
    FOnDebug(Self, FDebugData);
end;

procedure TCommandThreadBase.SyncTransferred;
begin
  if Assigned(FOnBytesTransferred) then
    FOnBytesTransferred(Self, FTransferredDirection, FTransferredCommandID, FTransferredSize, FTransferredTransferred);
end;

procedure TCommandThreadBase.WriteDebug(Data: string);
begin
  FDebugData := Data;
  if UseSynchronize then
    Synchronize(SyncDebug)
  else
    SyncDebug;
end;

{ TCommandBase }

procedure TCommandSocketBase.ThreadConnected(Sender: TSocketThread);
begin
  if Assigned(FOnConnected) then
    FOnConnected(Sender);
end;

procedure TCommandSocketBase.ThreadEnded(Sender: TSocketThread);
begin
  if Assigned(FOnEnded) then
    FOnEnded(Sender);
end;

{ TCommandClient }

procedure TCommandClient.Start;
begin
  FThread := TCommandClientThread.Create(FHost, 6000, TClientStream.Create);
  FThread.OnConnected := ThreadConnected;
  FThread.OnBytesTransferred := ThreadBytesTransferred;
  FThread.OnCommandReceived := ThreadCommandReceived;
  FThread.OnEnded := ThreadEnded;
  FThread.UseSynchronize := True;
  FThread.Resume;
end;

procedure TCommandClient.Stop;
begin
  if FThread <> nil then
    FThread.Terminate;
end;

procedure TCommandClient.ThreadBytesTransferred(Sender: TObject;
  Direction: TTransferDirection; CommandID: Cardinal; Size,
  Transferred: UInt64);
begin
  if Assigned(FOnBytesTransferred) then
    FOnBytesTransferred(Sender, Direction, CommandID, Size, Transferred);
end;

procedure TCommandClient.ThreadCommandReceived(Socket: TSocketThread;
  Command: TCommand);
begin
  if Assigned(FOnCommandReceived) then
    FOnCommandReceived(Socket, Command);
end;

procedure TCommandClient.ThreadConnected(Sender: TSocketThread);
begin

  inherited;
end;

procedure TCommandClient.ThreadEnded(Sender: TSocketThread);
begin
  FThread := nil;

  inherited;
end;

{ TClientStream }

procedure TClientStream.Process(Received: Cardinal);
begin
  inherited;

end;

{ TCommandClientThread }

procedure TCommandClientThread.DoConnected;
var
  Cmd: TCommand;
begin
  inherited;

  Cmd := TCommandGetVersion.Create;
  SendCommand(Cmd);
end;

procedure TCommandClientThread.DoReceivedCommand(Command: TCommand);
begin
  inherited;

  case Command.CommandType of
    ctGetVersion: ;
    ctGetVersionResponse: ;
    ctGetArchiveResponse: ;
    ctError:
      begin

      end;
  end;
end;

end.
