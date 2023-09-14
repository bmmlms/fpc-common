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
  LazUTF8,
  SysUtils;

  { TStreamHelper }

type
  TStreamHelper = class helper for TStream
  private
    function ReadLEB32S: Int32;
    function ReadLEB32U: Uint32;
    function ReadLEB64S: Int64;
    function ReadLEB64U: UInt64;

    function WriteLEB32U(Value: UInt32): Byte;
    function WriteLEB32S(Value: Integer): Byte;
    function WriteLEB64U(Value: UInt64): Byte;
    function WriteLEB64S(Value: Int64): Byte;
  public
    procedure Read(out Value: Integer; const LEB: Boolean); overload;
    procedure Read(out Value: Boolean); overload;
    procedure Read(out Value: string; const ReadUTF8: Boolean); overload;
    procedure Read(out Value: TDateTime); overload;
    procedure Read(out Value: UInt64; const LEB: Boolean); overload;
    procedure Read(out Value: Int64; const LEB: Boolean); overload;
    procedure Read(out Value: Byte); overload;
    procedure Read(out Value: LongWord; const LEB: Boolean); overload;

    procedure Write(const Value: Integer; const LEB: Boolean); overload;
    procedure Write(const Value: Boolean); overload;
    procedure Write(const Value: string; const WriteUTF8: Boolean); overload;
    procedure Write(const Value: TDateTime); overload;
    procedure Write(const Value: UInt64; const LEB: Boolean); overload;
    procedure Write(const Value: Int64; const LEB: Boolean); overload;
    procedure Write(const Value: Byte); overload;
    procedure Write(const Value: LongWord; const LEB: Boolean); overload;
  end;

  { TCustomMemoryStreamHelper }

  TCustomMemoryStreamHelper = class helper for TCustomMemoryStream
  public
    function PosInStream(const Search: TBytes; const FromOffset: Int64): Int64;
    function AsString: AnsiString; overload;
    function AsString(const FromOffset, Count: Int64): AnsiString; overload;
  end;

  { TMemoryStreamHelper }

  TMemoryStreamHelper = class helper for TMemoryStream
    procedure RemoveRange(const FromOffset, Count: Integer);
    procedure SetData(const Value: AnsiString);
  end;

implementation

{ TStreamHelper }

function Ash32(const Value, Shift: UInt32): Int32;
begin
  Result := (Value and Int32.MaxValue) shr Shift;
  Dec(Result, (Value and (not Int32.MaxValue)) shr Shift);
end;

function Ash64(const Value: Int64; const Shift: UInt32): Int64;
begin
  Result := (Value and Int64.MaxValue) shr Shift - (Value and (not Int64.MaxValue)) shr Shift;
end;

function TStreamHelper.ReadLEB32S: Int32;
var
  Shift: Integer = 0;
  R: UInt32 = 0;
  B, Expected: Byte;
begin
  Shift := 0;
  R := 0;
  while Shift < 32 do
  begin
    B := ReadByte;
    R := R or (UInt32(B and $7f) shl Shift);
    if B and $80 = 0 then
    begin
      if Shift + 7 < 32 then
      begin
        if B and $40 <> 0 then
          R := R or (UInt32.MaxValue shl (Shift + 7));
      end else
      begin
        Expected := Ash32(R, Shift);
        if Expected and $7F <> B then
          raise Exception.Create('Invalid data');
      end;
      Exit(R);
    end;
    Inc(Shift, 7);
  end;
  raise Exception.Create('Too many bytes');
end;

function TStreamHelper.ReadLEB32U: Uint32;
var
  Shift: Integer = 0;
  R: UInt32 = 0;
  B: Byte;
begin
  R := 0;
  Shift := 0;
  while Shift < 32 do
  begin
    B := ReadByte;
    R := R or (UInt32(B and $7F) shl Shift);
    if B and $80 = 0 then
    begin
      if R shr Shift <> B then
        raise Exception.Create('Invalid data');
      Exit(R);
    end;
    Inc(Shift, 7);
  end;
  raise Exception.Create('Too many bytes');
end;

function TStreamHelper.ReadLEB64S: Int64;
var
  Shift: Integer = 0;
  R: UInt64 = 0;
  B, Expected: Byte;
begin
  while Shift < 64 do
  begin
    B := ReadByte;
    R := R or (UInt64(B and $7f) shl Shift);
    if B and $80 = 0 then
    begin
      if Shift + 7 < 64 then
      begin
        if B and $40 <> 0 then
          R := R or (UInt64.MaxValue shl (Shift + 7));
      end else
      begin
        Expected := Ash64(R, Shift);
        if Expected and $7F <> B then
          raise Exception.Create('Invalid data');
      end;
      Exit(R);
    end;
    Inc(Shift, 7);
  end;
  raise Exception.Create('Too many bytes');
end;

function TStreamHelper.ReadLEB64U: UInt64;
var
  Shift: Integer = 0;
  R: UInt64 = 0;
  B: Byte;
begin
  R := 0;
  Shift := 0;
  while Shift < 64 do
  begin
    B := ReadByte;
    R := R or (UInt64(B and $7F) shl Shift);
    if B and $80 = 0 then
    begin
      if R shr Shift <> B then
        raise Exception.Create('Invalid data');
      Exit(R);
    end;
    Inc(Shift, 7);
  end;
  raise Exception.Create('Too many bytes');
end;

function TStreamHelper.WriteLEB32U(Value: UInt32): Byte;
var
  B: Byte;
begin
  Result := 0;
  repeat
    B := Value and $7f;
    Value := Value shr 7;

    if Value <> 0 then
      B := B or $80;

    WriteByte(B);
    Inc(Result);

    if Value = 0 then
      Break;
  until False;
end;

function TStreamHelper.WriteLEB32S(Value: Integer): Byte;
var
  B, Size: Byte;
  Neg, More: Boolean;
begin
  More := True;
  Neg := Value < 0;
  Size := SizeOf(Value) * 8;
  Result := 0;
  repeat
    B := Value and $7f;
    Value := Value shr 7;

    if Neg then
      Value := Value or (Integer(-1) shl (Size - 7));

    if (((Value = 0) and (B and $40 = 0)) or ((Value = -1) and (B and $40 <> 0))) then
      More := False
    else
      B := B or $80;

    WriteByte(B);
    Inc(Result);
  until not More;
end;

function TStreamHelper.WriteLEB64U(Value: UInt64): Byte;
var
  B: Byte;
begin
  Result := 0;
  repeat
    B := Value and $7f;
    Value := Value shr 7;

    if Value <> 0 then
      B := B or $80;

    WriteByte(B);
    Inc(Result);

    if Value = 0 then
      Break;
  until False;
end;


function TStreamHelper.WriteLEB64S(Value: Int64): Byte;
var
  B, Size: Byte;
  Neg, More: boolean;
begin
  More := True;
  Neg := Value < 0;
  Size := sizeof(Value) * 8;
  Result := 0;
  repeat
    B := Value and $7f;
    Value := Value shr 7;

    if Neg then
      Value := Value or (Int64(-1) shl (Size - 7));

    if (((Value = 0) and (B and $40 = 0)) or ((Value = -1) and (B and $40 <> 0))) then
      More := False
    else
      B := B or $80;

    WriteByte(B);
    Inc(Result);
  until not More;
end;

procedure TStreamHelper.Read(out Value: string; const ReadUTF8: Boolean);
var
  Len: Integer;
  Len2: UInt32;
  P: PWideChar;
begin
  Value := '';

  if ReadUTF8 then
  begin
    Len2 := ReadLEB32U;
    if Len2 = 0 then
      Exit;

    SetLength(Value, Len2);
    ReadBuffer(Value[1], Len2);
  end else
  begin
    Read(Len, False);
    if Len = 0 then
      Exit;

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

procedure TStreamHelper.Read(out Value: Integer; const LEB: Boolean);
begin
  if LEB then
    Value := ReadLEB32S
  else
    ReadBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Read(out Value: TDateTime);
begin
  ReadBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Read(out Value: UInt64; const LEB: Boolean);
begin
  if LEB then
    Value := ReadLEB64U
  else
    ReadBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Read(out Value: Int64; const LEB: Boolean);
begin
  if LEB then
    Value := ReadLEB64S
  else
    ReadBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Read(out Value: Byte);
begin
  ReadBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Read(out Value: LongWord; const LEB: Boolean);
begin
  if LEB then
    Value := ReadLEB32U
  else
    ReadBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Write(const Value: Integer; const LEB: Boolean);
begin
  if LEB then
    WriteLEB32S(Value)
  else
    WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Write(const Value: Boolean);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Write(const Value: string; const WriteUTF8: Boolean);
var
  Len: Integer;
  Len2: UInt32;
  P: Pointer;
begin
  if WriteUTF8 then
  begin
    Len2 := Length(Value);
    WriteLEB32U(Len2);
    if Len2 > 0 then
      WriteBuffer(Value[1], Len2);
  end else
  begin
    Len := UTF8Length(Value) * 2;
    WriteBuffer(Len, SizeOf(Len));
    if Len > 0 then
    begin
      P := GetMem(Len + 1);
      StringToWideChar(Value, P, Len + 1);
      WriteBuffer(P^, Len);
      FreeMem(P);
    end;
  end;
end;

procedure TStreamHelper.Write(const Value: TDateTime);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Write(const Value: UInt64; const LEB: Boolean);
begin
  if LEB then
    WriteLEB64U(Value)
  else
    WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Write(const Value: Int64; const LEB: Boolean);
begin
  if LEB then
    WriteLEB64S(Value)
  else
    WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Write(const Value: Byte);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TStreamHelper.Write(const Value: LongWord; const LEB: Boolean);
begin
  if LEB then
    WriteLEB32U(Value)
  else
    WriteBuffer(Value, SizeOf(Value));
end;

{ TCustomMemoryStreamHelper }

function TCustomMemoryStreamHelper.PosInStream(const Search: TBytes; const FromOffset: Int64): Int64;
var
  P: Pointer;
begin
  Result := -1;

  P := Pointer(PtrUInt(Memory) + FromOffset);
  if P > Memory + Size then
    raise Exception.Create('P > Memory + Size');

  while P <= Memory + Size - Length(Search) do
  begin
    if CompareMem(P, @Search[0], Length(Search)) then
      Exit(P - Memory);
    Inc(P);
  end;
end;

function TCustomMemoryStreamHelper.AsString: AnsiString;
begin
  Result := AsString(0, Size);
end;

function TCustomMemoryStreamHelper.AsString(const FromOffset, Count: Int64): AnsiString;
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
