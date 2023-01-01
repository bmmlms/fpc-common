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

unit StreamHelper;

interface

uses
  Classes,
  SysUtils;

{ TStreamHelper }

type
  TStreamHelper = class helper for TStream
  public
    procedure Read(out Value: Integer); overload;
    procedure Read(out Value: Boolean); overload;
    procedure Read(out Value: UnicodeString); overload;
    procedure Read(out Value: AnsiString); overload;
    procedure Read(out Value: TDateTime); overload;
    procedure Read(out Value: UInt64); overload;
    procedure Read(out Value: Byte); overload;
    procedure Read(out Value: LongWord); overload;

    procedure Write(Value: Integer); overload;
    procedure Write(Value: Boolean); overload;
    procedure Write(Value: string); overload;
    procedure Write(Value: TDateTime); overload;
    procedure Write(Value: UInt64); overload;
    procedure Write(Value: Byte); overload;
    procedure Write(Value: UInt16); overload;
    procedure Write(Value: LongWord); overload;
  end;

  { TCustomMemoryStreamHelper }

  TCustomMemoryStreamHelper = class helper for TCustomMemoryStream
  public
    function PosInStream(const Search: AnsiString; const FromOffset: Int64): Integer;
    function ToString: AnsiString; overload;
    function ToString(const FromOffset, Count: Int64): AnsiString; overload;
  end;

  { TMemoryStreamHelper }

  TMemoryStreamHelper = class helper for TMemoryStream
    procedure RemoveRange(const FromOffset, Count: Integer);
    procedure SetData(const Value: AnsiString);
  end;

implementation

{ TStreamHelper }

procedure TStreamHelper.Read(out Value: UnicodeString);
var
  Len: Integer;
  P: PWideChar;
begin
  Value := '';
  Read(Len);
  if Len > Size then
    raise Exception.Create('Len > Size');
  if Len > 0 then
  begin
    P := GetMemory(Len + 2);
    FillChar(P^, Len + 2, #0);
    ReadBuffer(P^, Len);
    Value := WideCharToString(P);
    FreeMemory(P);
  end;
end;

procedure TStreamHelper.Read(out Value: AnsiString);
var
  Len: Integer;
  P: PWideChar;
begin
  Value := '';
  Read(Len);
  if Len > Size then
    raise Exception.Create('Len > Size');
  if Len > 0 then
  begin
    P := GetMemory(Len + 2);
    FillChar(P^, Len + 2, #0);
    ReadBuffer(P^, Len);
    Value := WideCharToString(P);
    FreeMemory(P);
  end;
end;

procedure TStreamHelper.Read(out Value: Boolean);
begin
  ReadBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Read(out Value: Integer);
begin
  ReadBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Read(out Value: TDateTime);
begin
  ReadBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Read(out Value: UInt64);
begin
  ReadBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Read(out Value: Byte);
begin
  ReadBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Read(out Value: LongWord);
begin
  ReadBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Write(Value: Integer);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Write(Value: Boolean);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Write(Value: string);
var
  Len: Integer;
  P: Pointer;
begin
  Len := Length(Value) * 2;
  WriteBuffer(Len, SizeOf(Len));
  if Len > 0 then
  begin
    P := GetMem(Len + 1);
    StringToWideChar(Value, P, Len + 1);
    WriteBuffer(P^, Len);
    FreeMem(P);
  end;
end;

procedure TStreamHelper.Write(Value: TDateTime);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Write(Value: UInt64);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Write(Value: Byte);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Write(Value: UInt16);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Write(Value: LongWord);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

{ TCustomMemoryStreamHelper }

function TCustomMemoryStreamHelper.PosInStream(const Search: AnsiString; const FromOffset: Int64): Integer;
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
    p += 1;
  end;
  if Result > -1 then
    Result := Result + FromOffset;
end;

function TCustomMemoryStreamHelper.ToString: AnsiString;
begin
  Result := ToString(0, Size);
end;

function TCustomMemoryStreamHelper.ToString(const FromOffset, Count: Int64): AnsiString;
begin
  Result := '';
  if Count = 0 then
    Exit;
  if Size < Count - FromOffset then
    raise Exception.Create('Size < Count - FromOffset');
  if (FromOffset < 0) or (Count < 1) then
    raise Exception.Create('(FromOffset < 0) or (Count < 1)');

  SetLength(Result, Count);

  Move(Pointer(Memory + FromOffset)^, Result[1], Count);
end;

{ TMemoryStreamHelper }

procedure TMemoryStreamHelper.RemoveRange(const FromOffset, Count: Integer);
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

    Clear;

    if OutStream.Size > 0 then
      inherited CopyFrom(OutStream, 0);
  finally
    OutStream.Free;
  end;
end;

procedure TMemoryStreamHelper.SetData(const Value: AnsiString);
begin
  Clear;
  if Length(Value) > 0 then
    WriteBuffer(Value[1], Length(Value));
end;

end.
