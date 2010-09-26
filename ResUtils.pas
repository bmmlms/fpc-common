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
unit ResUtils;

interface

uses
  SysUtils, Windows, Classes, Contnrs;

type
  TResData = class
  private
    FResTypeIsInt: Boolean;
    FResTypeInt: WORD;
    FResType: WideString;
    FNameIsInt: Boolean;
    FNameInt: WORD;
    FName: WideString;

    FDataVersion: DWORD;
    FFlags: WORD;
    FLanguage: WORD;
    FVersion: DWORD;
    FCharacteristics: DWORD;

    FResData: TMemoryStream;

    FEmpty: Boolean;
  public
    constructor Create; overload;
    constructor Create(Stream: TMemoryStream); overload;
    destructor Destroy; override;
    procedure Save(Stream: TMemoryStream);

    property Empty: Boolean read FEmpty;

    property ResTypeIsInt: Boolean read FResTypeIsInt write FResTypeIsInt;
    property ResTypeInt: WORD read FResTypeInt write FResTypeInt;
    property ResType: WideString read FResType write FResType;
    property NameIsInt: Boolean read FNameIsInt write FNameIsInt;
    property NameInt: WORD read FNameInt write FNameInt;
    property Name: WideString read FName write FName;
    property Language: WORD read FLanguage write FLanguage;
    property ResData: TMemoryStream read FResData;
  end;

  TResDataList = class(TObjectList)
  private
    function Get2(Index: Integer): TResData;
    procedure Put2(Index: Integer; Item: TResData);
  public
    constructor Create;
    destructor Destroy; override;

    property Items[Index: Integer]: TResData read Get2 write Put2; default;
  end;

  TResFile = class
  private
    FResDataList: TResDataList;
  public
    constructor Create; overload;
    constructor Create(Filename: string); overload;
    destructor Destroy; override;

    procedure Open(Filename: string);
    procedure Save(Filename: string);

    property ResDataList: TResDataList read FResDataList;
  end;

implementation

{ TResData }

constructor TResData.Create;
begin
  inherited Create;

  FResTypeIsInt := True;
  FResTypeInt := 0;
  FResType := '';
  FNameIsInt := True;
  FNameInt := 0;
  FName := '';
  FEmpty := False;
  FDataVersion := 0;
  FFlags := 0;
  FLanguage := 0;
  FVersion := 0;
  FCharacteristics := 0;
  FResData := TMemoryStream.Create;
end;

constructor TResData.Create(Stream: TMemoryStream);
  function ReadString(Stream: TMemoryStream): WideString;
  begin
    Result := WideString(PWideChar(Pointer(Integer(Stream.Memory) + Stream.Position)));
    Stream.Seek(Length(Result) * SizeOf(WideChar) + 1, soFromCurrent);
  end;
var
  DataSize, HeaderSize: DWORD;
  DataType: WORD;
begin
  Create;

  Stream.ReadBuffer(DataSize, SizeOf(DataSize));
  Stream.ReadBuffer(HeaderSize, SizeOf(HeaderSize));

  Stream.ReadBuffer(DataType, SizeOf(DataType));
  if DataType = MAXWORD then
  begin
    Stream.ReadBuffer(FResTypeInt, SizeOf(FResTypeInt));
  end else
  begin
    FResTypeIsInt := False;
    Stream.Seek(-SizeOf(DataType), soFromCurrent);
    FResType := ReadString(Stream);

    if FResType = '' then
      raise Exception.Create('Error reading resource type string');
  end;

  if (Stream.Position mod 4) <> 0 then
    Stream.Seek(4 - Stream.Position mod 4, soFromCurrent);

  Stream.ReadBuffer(DataType, SizeOf(DataType));
  if DataType = MAXWORD then
  begin
    Stream.ReadBuffer(FNameInt, SizeOf(FNameInt));
  end else
  begin
    FNameIsInt := False;
    Stream.Seek(-SizeOf(DataType), soFromCurrent);
    FName := ReadString(Stream);

    if FName = '' then
      raise Exception.Create('Error reading resource name string');
  end;

  if (Stream.Position mod 4) <> 0 then
    Stream.Seek(4 - Stream.Position mod 4, soFromCurrent);

  Stream.ReadBuffer(FDataVersion, SizeOf(FDataVersion));
  Stream.ReadBuffer(FFlags, SizeOf(FFlags));
  Stream.ReadBuffer(FLanguage, SizeOf(FLanguage));
  Stream.ReadBuffer(FVersion, SizeOf(FVersion));
  Stream.ReadBuffer(FCharacteristics, SizeOf(FCharacteristics));

  if DataSize > 0 then
  begin
    FResData.CopyFrom(Stream, DataSize);
  end;

  FEmpty := (ResData.Size = 0) and ((Name = '') or (NameInt = 0));

  if (Stream.Position mod 4) <> 0 then
    Stream.Seek(4 - Stream.Position mod 4, soFromCurrent);
end;

destructor TResData.Destroy;
begin
  FResData.Free;
  inherited;
end;

procedure TResData.Save(Stream: TMemoryStream);
var
  DataSize, HeaderSize: DWORD;
  Z: Byte;
  MW, StringTerm: WORD;
  Start: Integer;
begin
  MW := MAXWORD;
  Z := 0;
  StringTerm := 0;
  Start := Stream.Position;

  DataSize := FResData.Size;
  Stream.WriteBuffer(DataSize, SizeOf(DataSize));

  HeaderSize := 0;
  Stream.WriteBuffer(HeaderSize, SizeOf(HeaderSize));

  if FResTypeIsInt then
  begin
    Stream.WriteBuffer(MW, SizeOf(MW));
    Stream.WriteBuffer(FResTypeInt, SizeOf(FResTypeInt));
  end else
  begin
    Stream.WriteBuffer(FResType[1], Length(FResType) * SizeOf(WChar));
    Stream.WriteBuffer(StringTerm, SizeOf(StringTerm))
  end;

  while Stream.Position mod 4 <> 0 do
    Stream.WriteBuffer(Z, SizeOf(Z));

  if FNameIsInt then
  begin
    Stream.WriteBuffer(MW, SizeOf(MW));
    Stream.WriteBuffer(FNameInt, SizeOf(FNameInt));
  end else
  begin
    Stream.WriteBuffer(FName[1], Length(FName) * SizeOf(WChar));
    Stream.WriteBuffer(StringTerm, SizeOf(StringTerm))
  end;

  while Stream.Position mod 4 <> 0 do
    Stream.WriteBuffer(Z, SizeOf(Z));

  Stream.WriteBuffer(FDataVersion, SizeOf(FDataVersion));
  Stream.WriteBuffer(FFlags, SizeOf(FFlags));
  Stream.WriteBuffer(FLanguage, SizeOf(FLanguage));
  Stream.WriteBuffer(FVersion, SizeOf(FVersion));
  Stream.WriteBuffer(FCharacteristics, SizeOf(FCharacteristics));

  HeaderSize := Stream.Position - Start;
  CopyMemory(Pointer(Integer(Stream.Memory) + Start + SizeOf(DataSize)), @HeaderSize, SizeOf(HeaderSize));

  if FResData.Size > 0 then
  begin
    FResData.Seek(0, soFromBeginning);
    Stream.CopyFrom(FResData, FResData.Size);
  end;

  while Stream.Position mod 4 <> 0 do
    Stream.WriteBuffer(Z, SizeOf(Z));
end;

{ TResFile }

constructor TResFile.Create;
begin
  inherited;
  FResDataList := TResDataList.Create;
end;

constructor TResFile.Create(Filename: string);
begin
  Create;
  Open(Filename);
end;

destructor TResFile.Destroy;
begin
  FResDataList.Free;
  inherited;
end;

procedure TResFile.Open(Filename: string);
var
  i: Integer;
  Stream: TMemoryStream;
  ResData: TResData;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(Filename);

    i := 0;
    while Stream.Position + 2 * SizeOf(DWORD) < Stream.Size do
    begin
      ResData := TResData.Create(Stream);
      if (not ResData.Empty) and (i = 0) then
        raise Exception.Create('16 bit resource files are not supported')
      else if ResData.Empty then
        ResData.Free
      else
        FResDataList.Add(ResData);
      Inc(i);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TResFile.Save(Filename: string);
var
  i: Integer;
  E: TResData;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    E := TResData.Create;
    try
      E.Save(Stream);
    finally
      E.Free;
    end;
    for i := 0 to FResDataList.Count - 1 do
      FResDataList[i].Save(Stream);
    Stream.SaveToFile(Filename);
  finally
    Stream.Free;
  end;
end;

{ TResDataList }

constructor TResDataList.Create;
begin
  inherited Create;
  OwnsObjects := True;
end;

destructor TResDataList.Destroy;
begin
  inherited;
end;

function TResDataList.Get2(Index: Integer): TResData;
begin
  Result := TResData(inherited Get(Index));
end;

procedure TResDataList.Put2(Index: Integer; Item: TResData);
begin
  inherited Put(Index, Item);
end;

end.
