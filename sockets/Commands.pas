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
  Windows, SysUtils, Classes, Sockets, Generics.Collections, ExtendedStream;

type
  TCommandTypes = (ctGetVersion, ctGetVersionResponse, ctGetArchive,
    ctGetArchiveResponse, ctError);

  TBytes = array of Byte;

  TCommand = class
  protected
    // Die ID des Paket-Streams.. gehört hier nur so halb hin.
    FID: Cardinal;

    FVersion: Cardinal;
    FCommandType: TCommandTypes;
    FStream: TStream;

    function DoGet: TBytes; virtual;
    function FGetCmdLength: Cardinal; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    class function Read(Stream: TExtendedStream): TCommand;
    function Copy: TCommand;

    function Get: TBytes;
    function Process(ToStream: TExtendedStream): Boolean; virtual;

    property ID: Cardinal read FID write FID;
    property Version: Cardinal read FVersion;
    property CommandType: TCommandTypes read FCommandType;
    property CmdLength: Cardinal read FGetCmdLength;
    property Stream: TStream read FStream;
  end;

  TCommandGetVersion = class(TCommand)
  private
  protected
  public
    constructor Create;

    class function Load(Len: Cardinal; Stream: TExtendedStream): TCommandGetVersion;
  end;

  TCommandGetVersionResponse = class(TCommand)
  private
    FMajor, FMinor, FRevision, FBuild: Cardinal;
    FDate: TDateTime;
  protected
    function DoGet: TBytes; override;
  public
    constructor Create;

    class function Load(Len: Cardinal; Stream: TExtendedStream): TCommandGetVersionResponse;

    property Major: Cardinal read FMajor write FMajor;
    property Minor: Cardinal read FMinor write FMinor;
    property Revision: Cardinal read FRevision write FRevision;
    property Build: Cardinal read FBuild write FBuild;
    property Date: TDateTime read FDate write FDate;
  end;

  TCommandGetArchive = class(TCommand)
  private
  protected
  public
    constructor Create;

    class function Load(Len: Cardinal; Stream: TExtendedStream): TCommandGetArchive;
  end;

  TCommandGetArchiveResponse = class(TCommand)
  private
    FFilename: string;
    FFilesize: UInt64;
    FExecutable: string;
    procedure SetFilename(const Value: string);
  protected
    function DoGet: TBytes; override;
    function FGetCmdLength: Cardinal; override;
  public
    constructor Create;
    destructor Destroy; override;

    class function Load(Len: Cardinal; Stream: TExtendedStream): TCommandGetArchiveResponse;

    function Process(ToStream: TExtendedStream): Boolean; override;

    property Filename: string read FFilename write SetFilename;
    property Filesize: UInt64 read FFileSize;
    property Executable: string read FExecutable write FExecutable;
  end;

  TCommandError = class(TCommand)
  private
    FForCommandType: TCommandTypes;
    FData: Cardinal;
  protected
    function DoGet: TBytes; override;
  public
    constructor Create;

    class function Load(Len: Cardinal; Stream: TExtendedStream): TCommandError;

    property ForCommandType: TCommandTypes read FForCommandType write FForCommandType;
    property Data: Cardinal read FData write FData;
  end;

const
  COMMAND_HEADER_LEN = 8;

implementation

{ TCommand }

function TCommand.Copy: TCommand;
var
  B: TBytes;
  S: TExtendedStream;
begin
  B := Get;
  S := TExtendedStream.Create;
  try
    S.Write(B[0], Length(B));
    Result := Read(S);
    Result.FID := FID;
  finally
    S.Free;
  end;
end;

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

function TCommand.DoGet: TBytes;
begin
  SetLength(Result, 0);
end;

function TCommand.FGetCmdLength: Cardinal;
begin
  Result := Length(Get);
end;

function TCommand.Get: TBytes;
var
  S: TExtendedStream;
  B: TBytes;
  L: Cardinal;
begin
  S := TExtendedStream.Create;
  try
    B := DoGet;

    S.Write(FVersion);
    L := Length(B) + 8;
    S.Write(L, SizeOf(L));
    S.Write(Cardinal(FCommandType));
    if Length(B) > 0 then
      S.Write(B[0], Length(B));

    SetLength(Result, S.Size);
    CopyMemory(@Result[0], S.Memory, S.Size);
  finally
    S.Free;
  end;
end;

function TCommand.Process(ToStream: TExtendedStream): Boolean;
begin
  Result := False;
end;

class function TCommand.Read(Stream: TExtendedStream): TCommand;
var
  CommandVersion, CommandLen, CommandType: Cardinal;
  CmdType: TCommandTypes;
begin
  Stream.Seek(0, soFromBeginning);
  Stream.Read(CommandVersion, SizeOf(CommandVersion));
  Stream.Read(CommandLen, SizeOf(CommandLen));
  Stream.Read(CommandType, SizeOf(CommandType));

  CmdType := TCommandTypes(CommandType);
  case CmdType of
    ctGetVersion:
      Result := TCommandGetVersion.Load(CommandLen - COMMAND_HEADER_LEN, Stream);
    ctGetVersionResponse:
      Result := TCommandGetVersionResponse.Load(CommandLen - COMMAND_HEADER_LEN, Stream);
    ctGetArchive:
      Result := TCommandGetArchive.Load(CommandLen - COMMAND_HEADER_LEN, Stream);
    ctGetArchiveResponse:
      Result := TCommandGetArchiveResponse.Load(CommandLen - COMMAND_HEADER_LEN, Stream);
    ctError:
      Result := TCommandError.Load(CommandLen - COMMAND_HEADER_LEN, Stream);
    else
      raise Exception.Create('Unbekannter CommandType');
    Result.FVersion := CommandVersion;
  end;
end;

{ TCommandGetVersion }

class function TCommandGetVersion.Load(Len: Cardinal; Stream: TExtendedStream): TCommandGetVersion;
begin
  Result := TCommandGetVersion.Create;
  Result.FCommandType := ctGetVersion;
end;

constructor TCommandGetVersion.Create;
begin
  inherited Create;

  FCommandType := ctGetVersion;
end;

{ TCommandGetVersionResponse }

class function TCommandGetVersionResponse.Load(Len: Cardinal; Stream: TExtendedStream): TCommandGetVersionResponse;
begin
  Result := TCommandGetVersionResponse.Create;
  Result.FCommandType := ctGetVersionResponse;
  Stream.Read(Result.FMajor);
  Stream.Read(Result.FMinor);
  Stream.Read(Result.FRevision);
  Stream.Read(Result.FBuild);
  Stream.Read(Result.FDate);
end;

constructor TCommandGetVersionResponse.Create;
begin
  inherited Create;

  FCommandType := ctGetVersionResponse;
end;

function TCommandGetVersionResponse.DoGet: TBytes;
var
  S: TExtendedStream;
begin
  S := TExtendedStream.Create;
  try
    S.Write(FMajor);
    S.Write(FMinor);
    S.Write(FRevision);
    S.Write(FBuild);
    S.Write(FDate);

    SetLength(Result, S.Size);
    CopyMemory(@Result[0], S.Memory, S.Size);
  finally
    S.Free;
  end;
end;

{ TCommandGetArchive }

constructor TCommandGetArchive.Create;
begin
  inherited;

  FCommandType := ctGetArchive;
end;

class function TCommandGetArchive.Load(Len: Cardinal;
  Stream: TExtendedStream): TCommandGetArchive;
begin
  Result := TCommandGetArchive.Create;
  Result.FCommandType := ctGetArchive;
end;

{ TCommandGetArchiveResponse }

constructor TCommandGetArchiveResponse.Create;
begin
  inherited;

  FCommandType := ctGetArchiveResponse;
end;

destructor TCommandGetArchiveResponse.Destroy;
begin

  inherited;
end;

function TCommandGetArchiveResponse.DoGet: TBytes;
var
  S: TExtendedStream;
begin
  S := TExtendedStream.Create;
  try
    S.Write(FFilename);
    S.Write(FFilesize);
    S.Write(FExecutable);

    SetLength(Result, S.Size);
    CopyMemory(@Result[0], S.Memory, S.Size);
  finally
    S.Free;
  end;
end;

function TCommandGetArchiveResponse.FGetCmdLength: Cardinal;
begin
  Result := Length(Get) + TFileStream(FStream).Size;
end;

class function TCommandGetArchiveResponse.Load(Len: Cardinal;
  Stream: TExtendedStream): TCommandGetArchiveResponse;
begin
  Result := TCommandGetArchiveResponse.Create;

  Stream.Read(Result.FFilename);
  Stream.Read(Result.FFilesize);
  Stream.Read(Result.FExecutable);

  if Result.FStream <> nil then
    Result.FStream.Free;

  // Falls es über Copy() kommt, gibt es keinen Stream
  if Stream.Position < Stream.Size then
  begin
    Result.FStream := TMemoryStream.Create;
    Result.FStream.CopyFrom(Stream, Result.FFilesize);
  end;
end;

function TCommandGetArchiveResponse.Process(
  ToStream: TExtendedStream): Boolean;
var
  L: Cardinal;
begin
  Result := True;

  L := 200000;
  if TFileStream(FStream).Size - TFileStream(FStream).Position < 200000 then
    L := TFileStream(FStream).Size - TFileStream(FStream).Position;

  if (L > 0) and (FStream.Position < TFileStream(FStream).Size) then
  begin
    ToStream.Seek(0, soFromEnd);
    ToStream.CopyFrom(FStream, L)
  end else
    Result := False;
end;

procedure TCommandGetArchiveResponse.SetFilename(const Value: string);
begin
  FFilename := Value;

  FStream := TFileStream.Create(FFilename, fmOpenRead);
  FFilesize := TFileStream(FStream).Size;
end;

{ TCommandError }

constructor TCommandError.Create;
begin
  inherited;

  FCommandType := ctError;
end;

function TCommandError.DoGet: TBytes;
var
  S: TExtendedStream;
begin
  S := TExtendedStream.Create;
  try
    S.Write(Cardinal(FForCommandType));
    S.Write(FData);

    SetLength(Result, S.Size);
    CopyMemory(@Result[0], S.Memory, S.Size);
  finally
    S.Free;
  end;
end;

class function TCommandError.Load(Len: Cardinal;
  Stream: TExtendedStream): TCommandError;
var
  C: Cardinal;
begin
  Result := TCommandError.Create;

  Stream.Read(C);
  Stream.Read(Result.FData);
  Result.FForCommandType := TCommandTypes(C);
end;

end.


