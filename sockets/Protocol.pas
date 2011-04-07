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
unit Protocol;

interface

uses
  Windows, SysUtils, Classes, Sockets, Generics.Collections, Commands,
  ExtendedStream;

type
  TTransferDirection = (tdReceive, tdSend);
  TPacketReadRes = (rrOk, rrBadPacket, rrMoreBytesNeeded);

  TPacketManager = class;

  TPacket = class
  private
    FID: Cardinal;
    FCommandLen: Cardinal;
    //FPacketLen: Cardinal;
    FData: TBytes;
  public
    constructor Create; overload;
    constructor Create(ID, CommandLen: Cardinal; Data: Pointer; Len: Cardinal); overload;

    class function Read(Stream: TSocketStream; var Packet: TPacket): TPacketReadRes;
    procedure Write(Stream: TExtendedStream);

    property ID: Cardinal read FID write FID;
    property CommandLen: Cardinal read FCommandLen write FCommandLen;
    //property PacketLen: Cardinal read FPacketLen;
  end;

  TCommandStream = class
  private
    FID: Cardinal;
    FCommandLen: Cardinal;
    FCommandBytesTransferred: Cardinal;
    FCommand: TCommand;
    FCommandStream: TExtendedStream;
    FInputStream: TExtendedStream;
  public
    constructor Create(ID, CommandLen: Cardinal);
    destructor Destroy; override;

    property ID: Cardinal read FID;
    property Size: Cardinal read FCommandLen;
    property Transferred: Cardinal read FCommandBytesTransferred;
  end;

  TTransferProgressEvent = procedure(Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal; Size, Transferred: UInt64) of object;

  TCommandStreamList = TList<TCommandStream>;
  TCommandList = TList<TCommand>;

  TPacketManager = class
  private
    FLock: RTL_CRITICAL_SECTION;

    FLastID: Cardinal;
    FSender: Cardinal;
    FSendCache: TCommandStreamList;
    FRecvCache: TCommandStreamList;

    FOnDebug: TDebugEvent;
    FOnBytesTransferred: TTransferProgressEvent;
    FReceivedCommands: TCommandList;

    procedure WriteDebug(Data: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Process;
    function BuildPacket: TPacket;

    function Send(Command: TCommand): Cardinal;
    procedure Read(Stream: TSocketStream);
    procedure CancelTransfer(CommandID: Cardinal);

    property SendCache: TCommandStreamList read FSendCache;
    property RecvCache: TCommandStreamList read FRecvCache;
    property ReceivedCommands: TCommandList read FReceivedCommands;

    property OnDebug: TDebugEvent read FOnDebug write FOnDebug;
    property OnBytesTransferred: TTransferProgressEvent read FOnBytesTransferred write FOnBytesTransferred;
  end;

  TProtocolEntry = class
  private
    FSentCommand: TCommand;
    FReceivedCommand: TCommand;
  public
    destructor Destroy; override;

    property SentCommand: TCommand read FSentCommand write FSentCommand;
    property ReceivedCommand: TCommand read FReceivedCommand write FReceivedCommand;
  end;

  TProtocolEntryList = TList<TProtocolEntry>;

  TProtocolManager = class
  private
    FEntries: TProtocolEntryList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Handle(Command: TCommand; Direction: TTransferDirection);
    function FindCounterpart(Command: TCommand): TCommand;
  end;

const
  PACKET_HEADER_LEN = 12;

implementation

{ TPacketManager }

function TPacketManager.BuildPacket: TPacket;
var
  P: TPacket;
  L: Cardinal;
begin
  Result := nil;

  EnterCriticalSection(FLock);
  try
    if FSendCache.Count = 0 then
      Exit;

    if FSender > FSendCache.Count - 1 then
      FSender := 0;

    if FSendCache[FSender].FCommandStream.Size = 0 then
      Exit;

    L := 1000000;
    if FSendCache[FSender].FCommandStream.Size < L then
      L := FSendCache[FSender].FCommandStream.Size;

    P := TPacket.Create(FSendCache[FSender].FID, FSendCache[FSender].FCommandLen, FSendCache[FSender].FCommandStream.Memory, L);
    FSendCache[FSender].FCommandStream.RemoveRange(0, L);

    FSendCache[FSender].FCommandBytesTransferred := FSendCache[FSender].FCommandBytesTransferred + L;

    if FSendCache[FSender].FCommandBytesTransferred = FSendCache[FSender].FCommandLen then
    begin
      FSendCache[FSender].Free;
      FSendCache.Delete(FSender);
    end;

    Inc(FSender);
    Result := P;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TPacketManager.CancelTransfer(CommandID: Cardinal);
var
  i: Integer;
begin
  for i := 0 to FSendCache.Count - 1 do
    if FSendCache[i].FID = CommandID then
    begin
      FSendCache[i].Free;
      FSendCache.Delete(i);
      Break;
    end;
  for i := 0 to FRecvCache.Count - 1 do
    if FRecvCache[i].FID = CommandID then
    begin
      FRecvCache[i].Free;
      FRecvCache.Delete(i);
      Break;
    end;
end;

constructor TPacketManager.Create;
begin
  inherited Create;

  InitializeCriticalSection(FLock);

  FLastID := 0;
  FSendCache := TCommandStreamList.Create;
  FRecvCache := TCommandStreamList.Create;
  FReceivedCommands := TCommandList.Create;
end;

destructor TPacketManager.Destroy;
var
  i: Integer;
begin
  DeleteCriticalSection(FLock);

  for i := 0 to FSendCache.Count - 1 do
    FSendCache[i].Free;
  FSendCache.Free;

  for i := 0 to FRecvCache.Count - 1 do
    FRecvCache[i].Free;
  FRecvCache.Free;

  for i := 0 to FReceivedCommands.Count - 1 do
    FReceivedCommands[i].Free;
  FReceivedCommands.Free;

  inherited;
end;

procedure TPacketManager.Process;
var
  i: Integer;
begin
  for i := 0 to FSendCache.Count - 1 do
    FSendCache[i].FCommand.Process(FSendCache[i].FCommandStream);
end;

function TPacketManager.Send(Command: TCommand): Cardinal;
var
  B: TBytes;
  CS: TCommandStream;
  CmdID: Cardinal;
begin
  EnterCriticalSection(FLock);
  try
    if Command.ID = 0 then
    begin
      CmdID := FLastID;
      Command.ID := CmdID;
      Inc(FLastID);

      if FLastID = High(Cardinal) then
        FLastID := 0;
    end else
      CmdID := Command.ID;

    Result := CmdID;

    B := Command.Get;

    CS := TCommandStream.Create(CmdID, Command.CmdLength);
    CS.FCommandStream.Write(B[0], Length(B));

    FSendCache.Add(CS);

    CS.FCommand := Command;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TPacketManager.WriteDebug(Data: string);
begin
  if Assigned(FOnDebug) then
    FOnDebug(nil, Data);
end;

procedure TPacketManager.Read(Stream: TSocketStream);
var
  i: Integer;
  CS: TCommandStream;
  P: TPacket;
  Res: TPacketReadRes;
  Cmd: TCommand;
  PacketRead: Boolean;
  BytesParsed: UInt64;
begin
  CS := nil;
  PacketRead := False;

  Stream.Seek(0, soFromBeginning);
  BytesParsed := 0;
  while Stream.Size > 0 + PACKET_HEADER_LEN do
  begin
    Stream.Seek(0, soFromBeginning);

    P := nil;
    Res := TPacket.Read(Stream, P);
    case Res of
      rrOk: ;
      rrBadPacket:
        raise Exception.Create('rrBadPacket');
      rrMoreBytesNeeded:
        begin
          Break;
        end;
    end;

    PacketRead := True;

    CS := nil;
    for i := 0 to FRecvCache.Count - 1 do
      if FRecvCache[i].FID = P.FID then
      begin
        CS := FRecvCache[i];
        Break;
      end;

    if CS = nil then
    begin
      CS := TCommandStream.Create(P.FID, P.FCommandLen);
      FRecvCache.Add(CS);
    end;

    BytesParsed := BytesParsed + Stream.Position;

    CS.FCommandStream.Write(P.FData[0], Length(P.FData));
    Stream.RemoveRange(0, Stream.Position);

    if P <> nil then
      P.Free;
  end;

  if (CS <> nil) and PacketRead then
  begin
    CS.FCommandBytesTransferred := CS.FCommandBytesTransferred + BytesParsed;
  end;

  for i := FRecvCache.Count - 1 downto 0 do
    if FRecvCache[i].FCommandLen = FRecvCache[i].FCommandStream.Size then
    begin
      try
        Cmd := TCommand.Read(FRecvCache[i].FCommandStream);
        if Cmd <> nil then
        begin
          Cmd.ID := FRecvCache[i].FID;
          FReceivedCommands.Add(Cmd);
          FRecvCache[i].Free;
          FRecvCache.Delete(i);
        end else
        begin
          raise Exception.Create('Cmd = nil');
        end;
      finally

      end;
    end;
end;

{ TPacket }

constructor TPacket.Create(ID, CommandLen: Cardinal; Data: Pointer; Len: Cardinal);
begin
  inherited Create;
  FID := ID;
  FCommandLen := CommandLen;

  SetLength(FData, Len);
  CopyMemory(@FData[0], Data, Len);
end;

class function TPacket.Read(Stream: TSocketStream;
  var Packet: TPacket): TPacketReadRes;
var
  IDx, PacketLen, CommandLenx: Cardinal;
begin
  try
    Result := rrMoreBytesNeeded;
    if Stream.Size < PACKET_HEADER_LEN then
      Exit;

    Stream.Read(PacketLen);
    Stream.Read(IDx);
    Stream.Read(CommandLenx);

    if Stream.Size >= PacketLen then
    begin
      Packet := TPacket.Create;
      Packet.FID := IDx;
      Packet.FCommandLen := CommandLenx;
      SetLength(Packet.FData, PacketLen - PACKET_HEADER_LEN);
      Stream.Read(Packet.FData[0], PacketLen - PACKET_HEADER_LEN);
      Result := rrOk;
    end else
    begin
      // Zurückspulen sonst epic fail
      Stream.Seek((SizeOf(IDx) + SizeOf(PacketLen) + SizeOf(CommandLenx)) * -1, soFromCurrent);
    end;
  except
    Result := rrBadPacket;
  end;
end;

procedure TPacket.Write(Stream: TExtendedStream);
begin
  Stream.Write(Cardinal(Length(FData) + PACKET_HEADER_LEN));
  Stream.Write(FID);
  Stream.Write(FCommandLen);
  Stream.Write(FData[0], Length(FData));
end;

constructor TPacket.Create;
begin
  inherited;
end;

{ TCommandStream }

constructor TCommandStream.Create(ID, CommandLen: Cardinal);
begin
  inherited Create;

  FID := ID;
  FCommandLen := CommandLen;
  FCommandStream := TExtendedStream.Create;
  FInputStream := TExtendedStream.Create;
end;

destructor TCommandStream.Destroy;
begin
  FCommandStream.Free;
  FInputStream.Free;
  if FCommand <> nil then // Gibt es nur bei "streamed-transfer"
    FCommand.Free;

  inherited;
end;

{ TProtocolManager }

constructor TProtocolManager.Create;
begin
  FEntries := TProtocolEntryList.Create;
end;

destructor TProtocolManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to FEntries.Count - 1 do
    FEntries[i].Free;

  FEntries.Free;

  inherited;
end;

function TProtocolManager.FindCounterpart(Command: TCommand): TCommand;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FEntries.Count - 1 do
    if FEntries[i].FSentCommand <> nil then
      if FEntries[i].FSentCommand.ID = Command.ID then
      begin
        Result := FEntries[i].FSentCommand;
        Break;
      end
    else if FEntries[i].FReceivedCommand <> nil then
      if FEntries[i].FReceivedCommand.ID = Command.ID then
      begin
        Result := FEntries[i].FReceivedCommand;
        Break;
      end;
end;

procedure TProtocolManager.Handle(Command: TCommand; Direction: TTransferDirection);
var
  Handeled: Boolean;
  i: Integer;
  E: TProtocolEntry;
begin
  // Alle löschen, die schon benutzt worden müssen sind.
  for i := FEntries.Count - 1 downto 0 do
    if (FEntries[i].FSentCommand <> nil) and (FEntries[i].FReceivedCommand <> nil) then
    begin
      FEntries[i].Free;
      FEntries.Delete(i);
    end;

  Handeled := False;
  case Direction of
    tdReceive:
      for i := 0 to FEntries.Count - 1 do
        if FEntries[i].FSentCommand <> nil then
          if FEntries[i].FSentCommand.ID = Command.ID then
          begin
            FEntries[i].FReceivedCommand := Command.Copy;
            Handeled := True;
          end;
    tdSend:
      for i := 0 to FEntries.Count - 1 do
        if FEntries[i].FReceivedCommand <> nil then
          if FEntries[i].FReceivedCommand.ID = Command.ID then
          begin
            FEntries[i].FSentCommand := Command.Copy;
            Handeled := True;
          end;
  end;

  if not Handeled then
  begin
    E := TProtocolEntry.Create;
    case Direction of
      tdReceive:
        E.FReceivedCommand := Command.Copy;
      tdSend:
        E.FSentCommand := Command.Copy;
    end;
    FEntries.Add(E);
  end;
end;

{ TProtocolEntry }

destructor TProtocolEntry.Destroy;
begin
  if FSentCommand <> nil then
    FSentCommand.Free;
  if FReceivedCommand <> nil then
    FReceivedCommand.Free;

  inherited;
end;

end.
