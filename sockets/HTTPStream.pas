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

unit HTTPStream;

interface

uses
  Classes,
  Generics.Collections,
  Sockets,
  StreamHelper,
  StrUtils,
  SysUtils,
  Windows;

type
  TTransferEncoding = (teNone, teChunked);

  { THTTPStream }

  THTTPStream = class(TSocketStream)
  private
    FResponseCode: Integer;
    FContentType: string;
    FContentEncoding: string;
    FTransferEncoding: TTransferEncoding;
    FContentLength: Integer;
    FRedirURL: string;

    FDeChunkedStream: TMemoryStream;

    FHeaderRemoved: Boolean;
    FOnHeaderRemoved: TNotifyEvent;

    procedure GetResponseType(const StatusLine: string);
    procedure GetResponseStatus(const StatusLine: string);
    procedure ParseHeader;
  protected
    FHeader: TDictionary<string, string>;
    FHeaderType: string;

    function FGetRecvDataStream: TMemoryStream; override;

    procedure DoHeaderRemoved; virtual;
    function ParseHeaderField(const Name: string; const Value: PByte; const Len: Integer): string; virtual;

    function GetHeaderValue(Name: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Process(Received: Cardinal); override;

    property HeaderRemoved: Boolean read FHeaderRemoved;
    property HeaderType: string read FHeaderType;
    property ResponseCode: Integer read FResponseCode;
    property ContentType: string read FContentType;
    property ContentEncoding: string read FContentEncoding;
    property TransferEncoding: TTransferEncoding read FTransferEncoding;
    property ContentLength: Integer read FContentLength;
    property RedirURL: string read FRedirURL;

    property OnHeaderRemoved: TNotifyEvent read FOnHeaderRemoved write FOnHeaderRemoved;
  end;

implementation

type
  THeaderParseState = (hpsPreName, hpsName, hpsPreValue, hpsValue);

procedure THTTPStream.GetResponseType(const StatusLine: string);
var
  Header: string;
begin
  Header := AnsiLowerCase(StatusLine);
  if Copy(Header, 1, 4) = 'http' then
    FHeaderType := 'http'
  else if Copy(Header, 1, 3) = 'icy' then
    FHeaderType := 'icy';
end;

procedure THTTPStream.GetResponseStatus(const StatusLine: string);
var
  P: Integer;
  Code: string;
begin
  P := Pos(' ', StatusLine);
  if P > 0 then
  begin
    Code := Copy(StatusLine, P + 1, 3);
    FResponseCode := StrToInt(Code);
  end else
    raise Exception.Create('Status code could not be determined');
end;

procedure THTTPStream.Process(Received: Cardinal);
var
  P, Len, RemoveTo: Integer;
  B: PByte;
begin
  inherited;

  if not FHeaderRemoved then
    ParseHeader;

  if FHeaderRemoved then
    if (FTransferEncoding = teChunked) and (Size > 0) then
    begin
      FDeChunkedStream.Seek(0, soFromEnd);
      RemoveTo := 0;
      Seek(0, soFromBeginning);
      repeat
        B := PByte(Int64(Memory) + Position);
        while ((B^ = 13) or (B^ = 10)) and (Int64(B) < Int64(Memory) + Size) do
          B := PByte(Int64(B) + 1);
        Seek(Int64(B) - Int64(Memory), soFromBeginning);

        P := PosInStream([$0A], Position);
        if (P > -1) and (P - Position > 0) then
        begin
          Len := StrToIntDef('$' + Trim(string(AsString(Position, P - Position))), -1);
          if Len = -1 then
            raise Exception.Create('Error dechunking response');
          if Len = 0 then
          begin
            Clear;
            Break;
          end;
          if Size >= Len + P + 1 then
          begin
            Seek(P + 1, soFromBeginning);
            FDeChunkedStream.CopyFrom(Self, Len);
            RemoveTo := Position;
          end else
            Len := -1;
        end else
          Len := -1;
      until (Len = 0) or (Len = -1);
      RemoveRange(0, RemoveTo);
    end;

  if (not HeaderRemoved) and (Size > 5192) then
    raise Exception.Create('Header could not be found');
end;

constructor THTTPStream.Create;
begin
  inherited;

  FHeaderRemoved := False;
  FHeaderType := '';
  FTransferEncoding := teNone;
  FContentLength := -1;
  FResponseCode := -1;
  FDeChunkedStream := TMemoryStream.Create;
  FHeader := TDictionary<string, string>.Create;
end;

destructor THTTPStream.Destroy;
begin
  FDeChunkedStream.Free;
  FHeader.Free;

  inherited;
end;

procedure THTTPStream.DoHeaderRemoved;
begin

end;

function THTTPStream.ParseHeaderField(const Name: string; const Value: PByte; const Len: Integer): string;
begin
  SetString(Result, PChar(Value), Len);
end;

function THTTPStream.FGetRecvDataStream: TMemoryStream;
begin
  if FTransferEncoding = teChunked then
    Result := FDechunkedStream
  else
    Result := Self;
end;

function THTTPStream.GetHeaderValue(Name: string): string;
begin
  Result := '';
  if FHeader.ContainsKey(Name.ToLower) then
    Exit(FHeader[Name.ToLower]);
end;

procedure THTTPStream.ParseHeader;
const
  NL: TBytes = [$0D, $0A];
  NLC: set of TByte = [$0D, $0A];
var
  HeaderEnd, StatusLineEnd: Integer;
  StatusLine, Name: string;
  State: THeaderParseState = hpsPreName;
  Current, TokenStart: PByte;
begin
  HeaderEnd := PosInStream(NL + NL, 0);
  if HeaderEnd = -1 then
    Exit;

  StatusLineEnd := PosInStream(NL, 0);
  if StatusLineEnd = -1 then
    Exit;

  StatusLine := AsString(0, StatusLineEnd);

  GetResponseStatus(StatusLine);
  GetResponseType(StatusLine);

  TokenStart := Memory + StatusLineEnd;
  Current := TokenStart;
  while Current < Memory + HeaderEnd + 1 do
  begin
    case State of
      hpsPreName:
        if not (Current^ in NLC) then
        begin
          TokenStart := Current;
          State := hpsName;
        end;
      hpsName:
        if Current^ = $3A { ':' } then
        begin
          SetString(Name, PChar(TokenStart), SizeInt(Current - TokenStart));
          Name := Name.Trim.ToLower;
          if Name = '' then
            raise Exception.Create('Invalid header');

          TokenStart := Current + 1;
          State := hpsPreValue;
        end;
      hpsPreValue:
        if Current^ <> $20 { ' ' } then
        begin
          TokenStart := Current;
          State := hpsValue;
        end;
      hpsValue:
        if Current^ in [$0D, $0A] then
        begin
          FHeader.Remove(Name);
          FHeader.Add(Name, ParseHeaderField(Name, TokenStart, SizeInt(Current - TokenStart)).Trim);

          TokenStart := Current + 1;
          State := hpsPreName;
        end;
    end;

    Inc(Current);
  end;

  FContentType := GetHeaderValue('content-type').ToLower;

  if (Pos(';', FContentType) > 0) then
  begin
    FContentEncoding := Trim(Copy(FContentType, Pos(';', FContentType) + 1, Length(FContentType)));
    FContentType := Trim(Copy(FContentType, 1, Pos(';', FContentType) - 1));
  end;

  FContentLength := StrToIntDef(GetHeaderValue('content-length'), -1);
  FRedirURL := GetHeaderValue('location');

  if LowerCase(GetHeaderValue('transfer-encoding')) = 'chunked' then
    FTransferEncoding := teChunked;

  RemoveRange(0, HeaderEnd + 4);
  FHeaderRemoved := True;
  DoHeaderRemoved;
  if Assigned(FOnHeaderRemoved) then
    FOnHeaderRemoved(Self);
end;

end.
