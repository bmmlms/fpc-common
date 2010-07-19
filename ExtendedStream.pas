{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010 Alexander Nottelmann

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
unit ExtendedStream;

interface

uses
  SysUtils, Windows, Classes;

type
  TExtendedStream = class(TMemoryStream)
  private
  protected
  public
    {$WARN HIDDEN_VIRTUAL OFF}
    procedure Write(Value: Integer); overload;
    procedure Write(Value: Cardinal); overload;
    procedure Write(Value: Boolean); overload;
    // procedure Write(Value: AnsiString); overload;
    procedure Write(Value: string); overload;
    procedure Write(Value: TDateTime); overload;
    procedure Read(var Value: Integer); overload;
    procedure Read(var Value: Cardinal); overload;
    procedure Read(var Value: Boolean); overload;
    procedure Read(var Value: AnsiString); overload;
    procedure Read(var Value: UnicodeString); overload;
    procedure Read(var Value: TDateTime); overload;
    procedure SetData(Value: AnsiString);
    procedure Add(Value: AnsiString);
    procedure RemoveRange(FromOffset, Count: Integer);
    function PosInStream(Search: AnsiString; FromOffset: Int64): Integer;
    function ToString: AnsiString; reintroduce; overload;
    function ToString(FromOffset, Count: Int64): AnsiString; reintroduce; overload;
    constructor Create;
    {$WARN HIDDEN_VIRTUAL ON}
  end;

implementation

{ TExtendedStream }

constructor TExtendedStream.Create;
begin
  inherited Create;
end;

function TExtendedStream.PosInStream(Search: AnsiString; FromOffset: Int64): Integer;
var
  i: Integer;
  p: Pointer;
begin
  Result := -1;
  p := Pointer(Int64(Memory) + FromOffset);
  for i := 0 to Size - FromOffset - Length(Search) do
  begin
    if CompareMem(p, @Search[1], Length(Search)) then
    begin
      Result := i;
      Break;
    end;
    Inc(Cardinal(p));
  end;
  if Result > -1 then
    Result := Result + FromOffset;
end;

{
procedure TExtendedStream.Write(Value: AnsiString);
var
  Len: Integer;
begin
  Len := Length(Value);
  WriteBuffer(Len, SizeOf(Integer));
  if Len > 0 then
    WriteBuffer(Value[1], Length(Value));
end;
}

procedure TExtendedStream.Write(Value: string);
var
  Len: Integer;
  {$IF CompilerVersion < 18.5}
  P: Pointer;
  {$IFEND}
begin
  Len := Length(Value) * 2;
  WriteBuffer(Len, SizeOf(Len));
  {$IF CompilerVersion >= 18.5}
  WriteBuffer(Value[1], Len);
  {$ELSE}
  P := GetMemory(Len);
  StringToWideChar(s, P, Len);
  Stream.WriteBuffer(P^, Len);
  FreeMemory(P);
  {$IFEND}
end;

procedure TExtendedStream.Write(Value: Cardinal);
begin
  WriteBuffer(Value, SizeOf(Cardinal));
end;

procedure TExtendedStream.Write(Value: Boolean);
begin
  WriteBuffer(Value, SizeOf(Boolean));
end;

procedure TExtendedStream.Write(Value: Integer);
begin
  WriteBuffer(Value, SizeOf(Integer));
end;

procedure TExtendedStream.Write(Value: TDateTime);
begin
  WriteBuffer(Value, SizeOf(TDateTime));
end;

procedure TExtendedStream.Read(var Value: AnsiString);
var
  Len: Integer;
begin
  // REMARK: Diese Funktion wird rausgeworfen irgendwann.
  // Dann wird nur noch die andere benutzt zum String lesen.
  // Sind ATM noch zwei um alte streamWriter-Versionen zu unterstützen.
  Read(Len);
  if Len > 0 then
  begin
    SetLength(Value, Len);
    ReadBuffer(Value[1], Len);
  end;
end;

procedure TExtendedStream.Read(var Value: UnicodeString);
var
  Len: Integer;
  P: PWideChar;
begin
  Read(Len);
  P := GetMemory(Len + 2);
  ZeroMemory(P, Len + 2);
  ReadBuffer(P^, Len);
  Value := WideCharToString(P);
  FreeMemory(P);
end;

procedure TExtendedStream.Read(var Value: Cardinal);
begin
  ReadBuffer(Value, SizeOf(Cardinal));
end;

procedure TExtendedStream.Read(var Value: Boolean);
begin
  ReadBuffer(Value, SizeOf(Boolean));
end;

procedure TExtendedStream.Read(var Value: Integer);
begin
  ReadBuffer(Value, SizeOf(Integer));
end;

procedure TExtendedStream.Read(var Value: TDateTime);
begin
  ReadBuffer(Value, SizeOf(TDateTime));
end;

procedure TExtendedStream.RemoveRange(FromOffset, Count: Integer);
var
  OutStream: TMemoryStream;
begin
  if Count <= 0 then
    Exit;
  OutStream := TMemoryStream.Create;
  try
    Position := 0;

    if FromOffset > 0 then
      OutStream.CopyFrom(Self, FromOffset);

    Seek(Count, soFromCurrent);

    if Size - Position > 0 then
      OutStream.CopyFrom(Self, Size - Position);

    Self.Clear;

    if OutStream.Size > 0 then
      inherited CopyFrom(OutStream, 0);
  finally
    OutStream.Free;
  end;
end;

procedure TExtendedStream.SetData(Value: AnsiString);
begin
  Clear;
  Add(Value);
end;

procedure TExtendedStream.Add(Value: AnsiString);
begin
  if Length(Value) > 0 then
    WriteBuffer(Value[1], Length(Value));
end;

function TExtendedStream.ToString: AnsiString;
begin
  Result := ToString(0, Size);
end;

function TExtendedStream.ToString(FromOffset, Count: Int64): AnsiString;
var
  i: Cardinal;
  Res: Pointer;
begin
  Result := '';
  if Count = 0 then
    Exit;
  if Size < Count - FromOffset then
    raise Exception.Create('Size < Count - FromOffset');
  if (FromOffset < 0) or (Count < 1) then
    raise Exception.Create('(FromOffset < 0) or (Count < 1)');

  GetMem(Res, Count);
  CopyMemory(Res, Pointer(Integer(Memory) + FromOffset), Count);
  for i := 0 to Count - 1 do
    if PByte(Cardinal(Res) + i)^ = 0 then
      PByte(Cardinal(Res) + i)^ := Byte(' ');
  SetLength(Result, Count);
  CopyMemory(@Result[1], Res, Count);
  FreeMem(Res);
end;

end.
