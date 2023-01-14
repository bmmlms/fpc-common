{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2023 Alexander Nottelmann

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
  Classes,
  StreamHelper,
  Functions,
  Generics.Collections,
  Sockets,
  SysUtils,
  Windows;

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
    ctSetStreamData,
    ctGetMonitorStreamsResponse, ctGetMonitorStreams,
    ctSyncWishlist,
    ctSearchCharts, ctSearchChartsResponse,
    ctHereIsADummyForARemovedCommand1, ctHereIsADummyForARemovedCommand2, // Das muss so bleiben, damit Client und Server das selbe Enum kennen!
    ctStreamAnalyzationData,
    ctGenerateAuthToken, ctGenerateAuthTokenResponse,
    ctPing, ctPingResponse,
    ctConvertManualToAutomatic, ctConvertManualToAutomaticResponse,
    ctGetStreamData, ctGetStreamDataResponse);

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

    class function Read(Stream: TMemoryStream; var CommandHeader: TCommandHeader): TReadRes;
    procedure Write(Stream: TMemoryStream);

    function Copy: TCommandHeader;

    property Version: Cardinal read FVersion;
    property CommandType: TCommandTypes read FCommandType;
    property CommandLength: Cardinal read FCommandLength;
  end;

  TCommandRegistration = record
    CommandType: TCommandTypes;
    CommandClass: TClass;
  end;

  TCommandClass = class of TCommand;

  TCommand = class
  private
  class var FCommands: TList<TCommandRegistration>;
  protected
    FVersion: Cardinal;
    FCommandType: TCommandTypes; // Wird nicht von hier versendet im Stream oder so!
    // analog zum commandtype hier drüber sollte hier auch noch die version gemirrort werden.
    FStream: TStream;

    procedure DoGet(S: TMemoryStream); virtual;
    function FGetCmdLength: Cardinal; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    class procedure RegisterCommand(CommandType: TCommandTypes; CommandClass: TCommandClass);
    class procedure UnregisterCommands;
    class function Read(CommandHeader: TCommandHeader; Stream: TMemoryStream): TCommand;
    procedure Load(CommandHeader: TCommandHeader; Stream: TMemoryStream); virtual;

    function Get: TBytes;
    function Process(ToStream: TMemoryStream): Boolean; virtual;
    procedure LoadStream(Stream: TMemoryStream);

    property Version: Cardinal read FVersion;
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

constructor TCommandHeader.Create(Version, CommandLen: Cardinal; CommandType: TCommandTypes);
begin
  inherited Create;

  FVersion := Version;
  FCommandLength := CommandLen;
  FCommandType := CommandType;
end;

class function TCommandHeader.Read(Stream: TMemoryStream; var CommandHeader: TCommandHeader): TReadRes;
var
  T: Cardinal;
begin
  try
    Result := rrMoreBytesNeeded;
    if Stream.Size - Stream.Position < COMMAND_HEADER_LEN then
      Exit;

    CommandHeader := TCommandHeader.Create;
    Stream.Read(CommandHeader.FVersion, False);
    Stream.Read(T, False);
    CommandHeader.FCommandType := TCommandTypes(T);
    Stream.Read(CommandHeader.FCommandLength, False);

    Result := rrOk;
  except
    Result := rrBadPacket;
  end;
end;

procedure TCommandHeader.Write(Stream: TMemoryStream);
begin
  Stream.Write(FVersion, False);
  Stream.Write(Cardinal(FCommandType), False);
  Stream.Write(FCommandLength, False);
end;

{ TCommand }

constructor TCommand.Create;
begin
  inherited;

  FVersion := 1;
end;

destructor TCommand.Destroy;
begin
  FStream.Free;

  inherited;
end;

procedure TCommand.DoGet(S: TMemoryStream);
begin

end;

function TCommand.FGetCmdLength: Cardinal;
begin
  Result := Length(Get);
end;

function TCommand.Get: TBytes;
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    DoGet(S);

    SetLength(Result, S.Size);
    if S.Size > 0 then
      CopyMemory(@Result[0], S.Memory, S.Size);
  finally
    S.Free;
  end;
end;

procedure TCommand.Load(CommandHeader: TCommandHeader; Stream: TMemoryStream);
begin

end;

procedure TCommand.LoadStream(Stream: TMemoryStream);
var
  DecompressedStream: TMemoryStream;
begin
  if FStream <> nil then
    FStream.Free;

  if Stream.Position < Stream.Size then
  begin
    DecompressedStream := TMemoryStream.Create;
    TFunctions.DecompressStream(Stream, DecompressedStream);
    FStream := DecompressedStream;

    FStream.Seek(0, soFromBeginning);
  end;
end;

function TCommand.Process(ToStream: TMemoryStream): Boolean;
begin
  Result := False;
end;

class function TCommand.Read(CommandHeader: TCommandHeader; Stream: TMemoryStream): TCommand;
var
  i: Integer;
  Cmd: TCommand;
begin
  for i := 0 to FCommands.Count - 1 do
    if FCommands[i].CommandType = CommandHeader.CommandType then
    begin
      {
      Context := TRttiContext.Create;
      try
        ClassType := Context.GetType(FCommands[i].CommandClass);

        Cmd := TCommand(ClassType.GetMethod('Create').Invoke(ClassType.AsInstance.MetaclassType, []).AsObject);
        Cmd.Load(CommandHeader, Stream);
      finally
        Context.Free;
      end;
      }
      Cmd := TCommand(FCommands[i].CommandClass.NewInstance);
      Cmd.Create;
      Cmd.Load(CommandHeader, Stream);

      Exit(Cmd);
    end;
  raise Exception.Create('Unknown CommandType');
end;

class procedure TCommand.RegisterCommand(CommandType: TCommandTypes; CommandClass: TCommandClass);
var
  R: TCommandRegistration;
begin
  if not Assigned(FCommands) then
    FCommands := TList<TCommandRegistration>.Create;
  R.CommandType := CommandType;
  R.CommandClass := CommandClass;
  FCommands.Add(R);
end;

class procedure TCommand.UnregisterCommands;
begin
  FCommands.Free;
end;

end.
