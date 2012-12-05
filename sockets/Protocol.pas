{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2012 Alexander Nottelmann

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

  TPacketManager = class;

  TPacket = class
  private
    FID: Cardinal;
    FPacketLen: Cardinal;
    FStream: TExtendedStream;
  public
    constructor Create; overload;
    constructor Create(ID: Cardinal; Stream: TExtendedStream; DataLen: Cardinal); overload;
    destructor Destroy; override;

    class function Read(Stream: TSocketStream; var Packet: TPacket): TReadRes;
    procedure Write(Stream: TExtendedStream);

    property ID: Cardinal read FID write FID;
    property PacketLen: Cardinal read FPacketLen;
  end;

  TCommandStream = class
  private
    FID: Cardinal;
    FCommandHeader: TCommandHeader;
    FTransferred: Cardinal;
    FCommand: TCommand;
    FCommandStream: TExtendedStream;
    FInputStream: TExtendedStream;
  public
    constructor Create(ID: Cardinal; CommandHeader: TCommandHeader);
    destructor Destroy; override;

    property ID: Cardinal read FID;
    property CommandHeader: TCommandHeader read FCommandHeader;
    property Transferred: Cardinal read FTransferred write FTransferred;
  end;

  TTransferProgressEvent = procedure(Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal;
    CommandHeader: TCommandHeader; Transferred: UInt64) of object;

  TCommandStreamList = class(TList<TCommandStream>)
  public
    function GetID(ID: Cardinal): TCommandStream;
  end;

  TReceivedCommand = class
  private
    FID: Cardinal;
    FCommandHeader: TCommandHeader;
    FCommand: TCommand;
  public
    constructor Create(ID: Cardinal; CommandHeader: TCommandHeader; Command: TCommand);
    destructor Destroy; override;

    property ID: Cardinal read FID;
    property CommandHeader: TCommandHeader read FCommandHeader;
    property Command: TCommand read FCommand;
  end;

  TReceivedCommandList = TList<TReceivedCommand>;

  TPacketManager = class
  private
    FLock: RTL_CRITICAL_SECTION;

    FLastPacketStreamID: Cardinal;
    FSender: Cardinal;       // TODO: was ist das? was tut es?
    FSendCache: TCommandStreamList;
    FRecvCache: TCommandStreamList;

    FOnDebug: TDebugEvent;
    FOnBytesTransferred: TTransferProgressEvent;
    FReceivedCommands: TReceivedCommandList;

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
    property ReceivedCommands: TReceivedCommandList read FReceivedCommands;

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
    //function FindCounterpart(Command: TCommand): TCommand;
  end;

const
  PACKET_HEADER_LEN = 8;

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

    if Integer(FSender) > FSendCache.Count - 1 then
      FSender := 0;

    if FSendCache[FSender].FCommandStream.Size = 0 then
      raise Exception.Create('FSendCache[FSender].FCommandStream.Size = 0');

    L := 16384;
    if FSendCache[FSender].FCommandStream.Size < L then
      L := FSendCache[FSender].FCommandStream.Size;

    FSendCache[FSender].FCommandStream.Seek(0, soFromBeginning);
    P := TPacket.Create(FSendCache[FSender].ID, FSendCache[FSender].FCommandStream, L);
    FSendCache[FSender].FCommandStream.RemoveRange(0, L);

    FSendCache[FSender].Transferred := FSendCache[FSender].Transferred + L;

    if FSendCache[FSender].Transferred = FSendCache[FSender].CommandHeader.CommandLength + COMMAND_HEADER_LEN then
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

  FSendCache := TCommandStreamList.Create;
  FRecvCache := TCommandStreamList.Create;
  FReceivedCommands := TReceivedCommandList.Create;
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

procedure TPacketManager.WriteDebug(Data: string);
begin
  if Assigned(FOnDebug) then
    FOnDebug(nil, Data);
end;

function TPacketManager.Send(Command: TCommand): Cardinal;
var
  B: TBytes;
  CS: TCommandStream;
  PacketStreamID: Cardinal;
  CommandHeader: TCommandHeader;
begin
  EnterCriticalSection(FLock);
  try
    PacketStreamID := FLastPacketStreamID;
    Inc(FLastPacketStreamID);

    Result := PacketStreamID;

    B := Command.Get;


    CommandHeader := TCommandHeader.Create(1, Length(B), Command.CommandType);
    CS := TCommandStream.Create(PacketStreamID, CommandHeader);

    CommandHeader.Write(CS.FCommandStream);
    CS.FCommandStream.Write(B[0], Length(B));
    CS.FCommand := Command;

    FSendCache.Add(CS);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TPacketManager.Read(Stream: TSocketStream);
var
  i: Integer;
  CS: TCommandStream;
  P: TPacket;
  PRes, CHRes: TReadRes;
  CommandHeader: TCommandHeader;
  Cmd: TCommand;
  PacketRead: Boolean;
begin
  CS := nil;
  PacketRead := False;

  CommandHeader := nil;

  Stream.Seek(0, soFromBeginning);
  while Stream.Size > PACKET_HEADER_LEN do
  begin
    Stream.Seek(0, soFromBeginning);

    P := nil;
    PRes := TPacket.Read(Stream, P);

    case PRes of
      rrOk:
        begin
          P.FStream.Seek(0, soFromBeginning);
          if FRecvCache.GetID(P.ID) = nil then
          begin
            CHRes := TCommandHeader.Read(P.FStream, CommandHeader);
            case CHRes of
              rrOk:;
              rrBadPacket:
                raise Exception.Create('rrBadPacket');
              rrMoreBytesNeeded:
                raise Exception.Create('rrMoreBytesNeeded');
            end;
          end;
        end;
      rrBadPacket:
        raise Exception.Create('rrBadPacket');
      rrMoreBytesNeeded:
        begin
          Break;
        end;
    end;

    PacketRead := True;

    CS := FRecvCache.GetID(P.ID);

    if CS = nil then
    begin
      CS := TCommandStream.Create(P.FID, CommandHeader);
      FRecvCache.Add(CS);
    end;

    if P.FStream.Size - P.FStream.Position > 0 then
      CS.FCommandStream.CopyFrom(P.FStream, P.FStream.Size - P.FStream.Position);
    Stream.RemoveRange(0, Stream.Position);

    if PacketRead then
    begin
      CS.Transferred := CS.Transferred + CS.FCommandStream.Size;
      if Assigned(FOnBytesTransferred) then
        FOnBytesTransferred(Self, tdReceive, CS.ID, CS.CommandHeader, CS.Transferred);
    end;

    if P <> nil then
      P.Free;
  end;

  for i := FRecvCache.Count - 1 downto 0 do
    if FRecvCache[i].CommandHeader.CommandLength = FRecvCache[i].FCommandStream.Size then
    begin
      try
        FRecvCache[i].FCommandStream.Seek(0, soFromBeginning);
        Cmd := TCommand.Read(FRecvCache[i].CommandHeader, FRecvCache[i].FCommandStream);
        if Cmd <> nil then
        begin
          FReceivedCommands.Add(TReceivedCommand.Create(CS.ID, FRecvCache[i].CommandHeader.Copy, Cmd));
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

constructor TPacket.Create(ID: Cardinal; Stream: TExtendedStream; DataLen: Cardinal);
begin
  inherited Create;
  FID := ID;

  FStream := TExtendedStream.Create;
  FStream.CopyFrom(Stream, DataLen)
end;

destructor TPacket.Destroy;
begin
  FStream.Free;

  inherited;
end;

class function TPacket.Read(Stream: TSocketStream;
  var Packet: TPacket): TReadRes;
var
  IDx, PacketLen: Cardinal;
begin
  try
    Result := rrMoreBytesNeeded;
    if Stream.Size < PACKET_HEADER_LEN then
      Exit;

    Stream.Read(PacketLen);
    Stream.Read(IDx);

    if Stream.Size >= PacketLen then
    begin
      Packet := TPacket.Create;
      Packet.FID := IDx;
      Packet.FPacketLen := PacketLen;

      Packet.FStream.CopyFrom(Stream, PacketLen - PACKET_HEADER_LEN);

      Result := rrOk;
    end else
    begin
      // Zurückspulen sonst epic fail
      Stream.Seek((SizeOf(IDx) + SizeOf(PacketLen)) * -1, soFromCurrent);
    end;
  except
    Result := rrBadPacket;
  end;
end;

procedure TPacket.Write(Stream: TExtendedStream);
begin
  FStream.Seek(0, soFromBeginning);
  Stream.Write(Cardinal(FStream.Size + PACKET_HEADER_LEN));
  Stream.Write(FID);
  Stream.CopyFrom(FStream, FStream.Size);
end;

constructor TPacket.Create;
begin
  inherited;

  FStream := TExtendedStream.Create;
end;

{ TCommandStream }

constructor TCommandStream.Create(ID: Cardinal; CommandHeader: TCommandHeader);
begin
  inherited Create;

  FID := ID;
  FCommandStream := TExtendedStream.Create;
  FInputStream := TExtendedStream.Create;
  FCommandHeader := CommandHeader;
end;

destructor TCommandStream.Destroy;
begin
  FCommandStream.Free;
  FInputStream.Free;

  FCommand.Free; // Gibt es nur bei "streamed-transfer"

  FCommandHeader.Free;

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

{
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
}

procedure TProtocolManager.Handle(Command: TCommand; Direction: TTransferDirection);
var
  Handeled: Boolean;
  i: Integer;
  E: TProtocolEntry;
begin
  // Alle löschen, die schon benutzt worden müssen sind.
  {
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
  }
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

{ TCommandStreamList }

function TCommandStreamList.GetID(ID: Cardinal): TCommandStream;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].ID = ID then
      Exit(Items[i]);
end;

{ TReceivedCommand }

constructor TReceivedCommand.Create(ID: Cardinal; CommandHeader: TCommandHeader;
  Command: TCommand);
begin
  inherited Create;

  FID := ID;
  FCommandHeader := CommandHeader;
  FCommand := Command;
end;

destructor TReceivedCommand.Destroy;
begin
  FCommandHeader.Free;
  FCommand.Free;

  inherited;
end;

end.
