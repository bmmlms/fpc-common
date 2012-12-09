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
unit Commands;

interface

uses
  Windows, SysUtils, Classes, Sockets, Generics.Collections, ExtendedStream,
  zlib;

type
  TCommandTypes = (ctHandshake, ctHandshakeResponse,
    ctLogIn, ctLogInResponse,
    ctLogOut, ctLogOutResponse,
    ctGetServerData, ctGetServerDataResponse,
    ctTitleChanged,
    ctNetworkTitleChangedResponse,
    ctServerInfoResponse,
    ctMessageResponse,
    ctUpdateStats,
    ctSetSettings,
    ctClientStats,
    ctSubmitStream,
    ctSetStreamData);

  TReadRes = (rrOk, rrBadPacket, rrMoreBytesNeeded);
  TBytes = array of Byte;

  TCommand = class;

  TCommandHeader = class
  private
    FVersion: Cardinal;
    FCommandType: TCommandTypes;
    FCommandLength: Cardinal;
  public
    constructor Create(Version, CommandLen: Cardinal; CommandType: TCommandTypes); overload;

    class function Read(Stream: TExtendedStream; var CommandHeader: TCommandHeader): TReadRes;
    procedure Write(Stream: TExtendedStream);

    function Copy: TCommandHeader;

    property Version: Cardinal read FVersion;
    property CommandType: TCommandTypes read FCommandType;
    property CommandLength: Cardinal read FCommandLength;
  end;

  TCommandRegistration = record
    CommandType: TCommandTypes;
    CommandClass: TClass;
  end;

  TCommand = class
  private
    class var FCommands: TList<TCommandRegistration>;
  protected
    FCommandType: TCommandTypes; // Wird nicht von hier versendet im Stream oder so!
                                 // analog zum commandtype hier drüber sollte hier auch noch die version gemirrort werden.
    FStream: TStream;

    procedure DoGet(S: TExtendedStream); virtual;
    function FGetCmdLength: Cardinal; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    class procedure RegisterCommand(CommandType: TCommandTypes; CommandClass: TClass);
    class function Read(CommandHeader: TCommandHeader; Stream: TExtendedStream): TCommand;
    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); virtual;

    function Get: TBytes;
    function Process(ToStream: TExtendedStream): Boolean; virtual;
    procedure LoadStream(Stream: TExtendedStream);

    property CommandType: TCommandTypes read FCommandType;
    property CmdLength: Cardinal read FGetCmdLength;
    property Stream: TStream read FStream;
  end;

const
  COMMAND_HEADER_LEN = 12;

implementation

{ TCommandHeader }

function TCommandHeader.Copy: TCommandHeader;
begin
  Result := TCommandHeader.Create(FVersion, FCommandLength, FCommandType);
end;

constructor TCommandHeader.Create(Version, CommandLen: Cardinal;
  CommandType: TCommandTypes);
begin
  inherited Create;

  FVersion := Version;
  FCommandLength := CommandLen;
  FCommandType := CommandType;
end;

class function TCommandHeader.Read(Stream: TExtendedStream;
  var CommandHeader: TCommandHeader): TReadRes;
var
  T: Cardinal;
begin
  try
    Result := rrMoreBytesNeeded;
    if Stream.Size - Stream.Position < COMMAND_HEADER_LEN then
      Exit;

    CommandHeader := TCommandHeader.Create;
    Stream.Read(CommandHeader.FVersion);
    Stream.Read(T);
    CommandHeader.FCommandType := TCommandTypes(T);
    Stream.Read(CommandHeader.FCommandLength);

    Result := rrOk;
  except
    Result := rrBadPacket;
  end;
end;

procedure TCommandHeader.Write(Stream: TExtendedStream);
begin
  Stream.Write(FVersion);
  Stream.Write(Cardinal(FCommandType));
  Stream.Write(FCommandLength);
end;

{ TCommand }

constructor TCommand.Create;
begin
  inherited;

end;

destructor TCommand.Destroy;
begin
  FStream.Free;

  inherited;
end;

procedure TCommand.DoGet(S: TExtendedStream);
begin

end;

function TCommand.FGetCmdLength: Cardinal;
begin
  Result := Length(Get);
end;

function TCommand.Get: TBytes;
var
  S: TExtendedStream;
begin
  S := TExtendedStream.Create;
  try
    DoGet(S);

    SetLength(Result, S.Size);
    if S.Size > 0 then
      CopyMemory(@Result[0], S.Memory, S.Size);
  finally
    S.Free;
  end;
end;

procedure TCommand.Load(CommandHeader: TCommandHeader;
  Stream: TExtendedStream);
begin

end;

procedure TCommand.LoadStream(Stream: TExtendedStream);
var
  DecompressStream: TExtendedStream;
begin
  if FStream <> nil then
    FStream.Free;

  if Stream.Position < Stream.Size then
  begin
    DecompressStream := TExtendedStream.Create;

    zlib.ZDecompressStream(Stream, DecompressStream);

    FStream := DecompressStream;

    FStream.Seek(0, soFromBeginning);
  end;
end;

function TCommand.Process(ToStream: TExtendedStream): Boolean;
begin
  Result := False;
end;

class function TCommand.Read(CommandHeader: TCommandHeader; Stream: TExtendedStream): TCommand;
var
  i: Integer;
  Cmd: TCommand;
begin
  for i := 0 to FCommands.Count - 1 do
    if FCommands[i].CommandType = CommandHeader.CommandType then
    begin
      Cmd := TCommand(FCommands[i].CommandClass.Create);
      Cmd.Load(CommandHeader, Stream);
      Exit(Cmd);
    end;
  raise Exception.Create('Unbekannter CommandType');
end;

class procedure TCommand.RegisterCommand(CommandType: TCommandTypes;
  CommandClass: TClass);
var
  R: TCommandRegistration;
begin
  if not Assigned(FCommands) then
    FCommands := TList<TCommandRegistration>.Create;
  R.CommandType := CommandType;
  R.CommandClass := CommandClass;
  FCommands.Add(R);
end;

end.


