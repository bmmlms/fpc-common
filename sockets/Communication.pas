{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2015 Alexander Nottelmann

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

    FLastSyncedTransfer: Cardinal;

    FProc: TCommandEvent;

    FOnLog: TLogEvent;
    FOnCommandReceived: TCommandEvent;
    FOnBytesTransferred: TTransferProgressEvent;

    FDebugData: string;

    // Für CommandReceived-Event
    FID: Cardinal;
    FCommandHeader: TCommandHeader;
    FCommand: TCommand;

    FTransferredCommandID: Cardinal;
    FTransferredCommandHeader: TCommandHeader;
    FTransferredTransferred: UInt64;
    FTransferredDirection: TTransferDirection;

    procedure Initialize;

    procedure SyncCommand(Proc: TCommandEvent; ID: Cardinal; CommandHeader: TCommandHeader; Command: TCommand);
    procedure SyncCommand2;
    procedure SyncDebug;
    procedure SyncTransferred;

    procedure PacketManagerLog(Sender: TSocketThread; Data: string);
    procedure PacketManagerBytesTransferred(Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal;
      CommandHeader: TCommandHeader; Transferred: UInt64);
  protected
    procedure DoStuff; override;

    procedure DoReceivedData(Buf: Pointer; Len: Integer); override;
    procedure DoReceivedCommand(ID: Cardinal; CommandHeader: TCommandHeader; Command: TCommand); virtual;
    procedure DoException(E: Exception); override;

    procedure WriteLog(Data: string);
  public
    constructor Create(Handle: Cardinal; Stream: TSocketStream); overload; override;
    constructor Create(Host: string; Port: Integer; Stream: TSocketStream; Secure: Boolean); overload; override;
    destructor Destroy; override;

    function SendCommand(Command: TCommand): Cardinal;

    property OnBytesTransferred: TTransferProgressEvent read FOnBytesTransferred write FOnBytesTransferred;
    property OnCommandReceived: TCommandEvent read FOnCommandReceived write FOnCommandReceived;
    property OnLog: TLogEvent read FOnLog write FOnLog;
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

    procedure ThreadBytesTransferred(Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal;
      CommandHeader: TCommandHeader; Transferred: UInt64);
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
    procedure DoReceivedCommand(ID: Cardinal; CommandHeader: TCommandHeader; Command: TCommand); override;
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

  Initialize;
end;

constructor TCommandThreadBase.Create(Host: string; Port: Integer;
  Stream: TSocketStream; Secure: Boolean);
begin
  inherited;

  Initialize;
end;

destructor TCommandThreadBase.Destroy;
begin
  FPacketSender.Free;
  FPacketReader.Free;

  inherited;
end;

procedure TCommandThreadBase.DoException(E: Exception);
begin
  inherited;

end;

procedure TCommandThreadBase.DoReceivedCommand(ID: Cardinal; CommandHeader: TCommandHeader; Command: TCommand);
begin
  // Fertiges Command Bytes rübergeben
  if Assigned(FOnBytesTransferred) then
    FOnBytesTransferred(Self, tdReceive, ID, CommandHeader, CommandHeader.CommandLength);
end;

procedure TCommandThreadBase.DoReceivedData(Buf: Pointer; Len: Integer);
var
  i: Integer;
  Cmd: TReceivedCommand;
begin
  inherited;

  FPacketReader.Read(FRecvStream);

  for i := 0 to FPacketReader.ReceivedCommands.Count - 1 do
  begin
    Cmd := FPacketReader.ReceivedCommands[i];
    DoReceivedCommand(Cmd.ID, Cmd.CommandHeader, Cmd.Command);

    SyncCommand(FOnCommandReceived, Cmd.ID, Cmd.CommandHeader, Cmd.Command);

    FPacketReader.ReceivedCommands[i].Free;
  end;
  FPacketReader.ReceivedCommands.Clear;
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
          FPacketSender.SendCache[i].CommandHeader, FPacketSender.SendCache[i].Transferred);
      end;

    if FPacketReader.RecvCache.Count > 0 then
      for i := 0 to FPacketReader.RecvCache.Count - 1 do
      begin
        PacketManagerBytesTransferred(Self, tdReceive, FPacketReader.RecvCache[i].ID,
          FPacketReader.RecvCache[i].CommandHeader, FPacketReader.RecvCache[i].Transferred);
      end;

    FLastSyncedTransfer := GetTickCount;
  end;
end;

procedure TCommandThreadBase.Initialize;
begin
  FPacketSender := TPacketManager.Create;
  FPacketSender.OnLog := PacketManagerLog;
  FPacketSender.OnBytesTransferred := PacketManagerBytesTransferred;

  FPacketReader := TPacketManager.Create;
  FPacketReader.OnLog := PacketManagerLog;
  FPacketReader.OnBytesTransferred := PacketManagerBytesTransferred;
end;

procedure TCommandThreadBase.PacketManagerBytesTransferred(
  Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal;
  CommandHeader: TCommandHeader; Transferred: UInt64);
begin
  FTransferredCommandID := CommandID;
  FTransferredCommandHeader := CommandHeader;
  FTransferredTransferred := Transferred;
  FTransferredDirection := Direction;

  if Assigned(FOnBytesTransferred) then
    if UseSynchronize then
      Synchronize(SyncTransferred)
    else
      SyncTransferred;
end;

procedure TCommandThreadBase.PacketManagerLog(Sender: TSocketThread;
  Data: string);
begin
  WriteLog(Data);
end;

function TCommandThreadBase.SendCommand(Command: TCommand): Cardinal;
begin
  Result := FPacketSender.Send(Command);
end;

procedure TCommandThreadBase.SyncCommand(Proc: TCommandEvent;
  ID: Cardinal; CommandHeader: TCommandHeader; Command: TCommand);
begin
  if Assigned(Proc) then
  begin
    FProc := Proc;
    FID := ID;
    FCommandHeader := CommandHeader;
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
  if Assigned(FOnLog) then
    FOnLog(Self, FDebugData);
end;

procedure TCommandThreadBase.SyncTransferred;
begin
  if Assigned(FOnBytesTransferred) then
    FOnBytesTransferred(Self, FTransferredDirection, FTransferredCommandID, FTransferredCommandHeader,
      FTransferredTransferred);
end;

procedure TCommandThreadBase.WriteLog(Data: string);
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
  FThread := TCommandClientThread.Create(FHost, 6000, TClientStream.Create, False);
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
  Direction: TTransferDirection; CommandID: Cardinal; CommandHeader: TCommandHeader;
  Transferred: UInt64);
begin
  if Assigned(FOnBytesTransferred) then
    FOnBytesTransferred(Sender, Direction, CommandID, CommandHeader, Transferred);
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
begin
  inherited;

end;

procedure TCommandClientThread.DoReceivedCommand(ID: Cardinal; CommandHeader: TCommandHeader; Command: TCommand);
begin
  inherited;

end;

end.



