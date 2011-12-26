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
unit HTTPStream;

interface

uses
  SysUtils, Windows, Classes, StrUtils, ExtendedStream, Sockets;

type
  TTransferEncoding = (teNone, teChunked);

  THTTPStream = class(TSocketStream)
  private
    FResponseCode: Integer;
    FContentType: string;
    FTransferEncoding: TTransferEncoding;
    FContentLength: Integer;
    FRedirURL: string;

    FDeChunkedStream: TExtendedStream;

    FHeaderRemoved: Boolean;
    FOnHeaderRemoved: TNotifyEvent;

    procedure GetHeaderType;
    procedure GetResponseCode;
    procedure ProcessHeader;
  protected
    FHeader: string;
    FHeaderType: string;

    function FGetRecvDataStream: TExtendedStream; override;

    procedure DoHeaderRemoved; virtual;

    function GetHeaderData(Name: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Process(Received: Cardinal); override;

    property HeaderRemoved: Boolean read FHeaderRemoved;
    property HeaderType: string read FHeaderType;
    property ResponseCode: Integer read FResponseCode;
    property ContentType: string read FContentType;
    property TransferEncoding: TTransferEncoding read FTransferEncoding;
    property ContentLength: Integer read FContentLength;
    property RedirURL: string read FRedirURL;

    property OnHeaderRemoved: TNotifyEvent read FOnHeaderRemoved write FOnHeaderRemoved;
  end;

implementation

procedure THTTPStream.GetResponseCode;
var
  P: Integer;
  Code: string;
begin
  P := Pos(' ', FHeader);
  if P > 0 then
  begin
    Code := Copy(FHeader, P + 1, 3);
    FResponseCode := StrToInt(Code);
  end else
    raise Exception.Create('Response code could not be determined');
end;

procedure THTTPStream.Process(Received: Cardinal);
var
  P, Len, RemoveTo: Integer;
  B: PByte;
begin
  inherited;
  if not FHeaderRemoved then
    ProcessHeader;

  if FHeaderRemoved then
  begin
    if (FTransferEncoding = teChunked) and (Size > 0) then
    begin
      RemoveTo := 0;
      Seek(0, soFromBeginning);
      repeat
        B := PByte(Int64(Memory) + Position);
        while ((B^ = 13) or (B^ = 10)) and (Int64(B) < Int64(Memory) + Size) do
          B := PByte(Int64(B) + 1);
        Seek(Int64(B) - Int64(Memory), soFromBeginning);

        P := PosInStream(#10, Position);
        if (P > -1) and (P - Position > 0) then
        begin
          Len := StrToIntDef('$' + Trim(string(ToString(Position, P - Position))), -1);
          if Len = -1 then
            raise Exception.Create('Error dechunking content');
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
  end;

  if (not HeaderRemoved) and (Size > 5192) then
    raise Exception.Create('Header could not be found');
end;

procedure THTTPStream.GetHeaderType;
var
  Header: string;
begin
  Header := AnsiLowerCase(FHeader);
  if Copy(Header, 1, 4) = 'http' then
    FHeaderType := 'http'
  else if Copy(Header, 1, 3) = 'icy' then
    FHeaderType := 'icy';
end;

constructor THTTPStream.Create;
begin
  inherited;
  FHeaderRemoved := False;
  FHeader := '';
  FHeaderType := '';
  FTransferEncoding := teNone;
  FContentLength := -1;
  FResponseCode := -1;
  FDeChunkedStream := TExtendedStream.Create;
end;

destructor THTTPStream.Destroy;
begin
  FDeChunkedStream.Free;
  inherited;
end;

procedure THTTPStream.DoHeaderRemoved;
begin

end;

function THTTPStream.FGetRecvDataStream: TExtendedStream;
begin
  if FTransferEncoding = teChunked then
    Result := FDechunkedStream
  else
    Result := Self;
end;

function THTTPStream.GetHeaderData(Name: string): string;
var
  n, n2: Integer;
  Header2: string;
begin
  Header2 := AnsiLowerCase(FHeader);
  Name := AnsiLowerCase(Name) + ':';
  Result := '';
  n := Pos(Name, Header2);
  if n > 0 then
  begin
    n2 := PosEx(#10, FHeader, n);
    if n2 > 0 then
      Result := Copy(FHeader, n + Length(Name), n2 - n - Length(Name))
    else
      Result := Copy(FHeader, n + Length(Name), Length(FHeader));
  end;
  Result := Trim(Result);
end;

procedure THTTPStream.ProcessHeader;
var
  i: Integer;
begin
  i := PosInStream(#13#10#13#10, 0);
  if i > -1 then
  begin
    FHeader := string(ToString(0, i));
    WriteDebug('Header received', FHeader, 0, 1);
    GetResponseCode;
    GetHeaderType;
    FContentType := LowerCase(GetHeaderData('content-type'));
    FContentLength := StrToIntDef(GetHeaderData('content-length'), -1);
    FRedirURL := GetHeaderData('location');
    if LowerCase(GetHeaderData('transfer-encoding')) = 'chunked' then
      FTransferEncoding := teChunked;

    RemoveRange(0, i + 4);
    FHeaderRemoved := True;
    DoHeaderRemoved;
    if Assigned(FOnHeaderRemoved) then
      FOnHeaderRemoved(Self);
  end;
end;

end.
