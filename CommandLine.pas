{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2025 Alexander Nottelmann

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

unit CommandLine;

interface

uses
  Classes,
  Generics.Collections,
  SysUtils;

type
  TCommandLineRecord = class
  private
    FParam: string;
    FValues: TStringList;
  public
    constructor Create(Param: string);
    destructor Destroy; override;

    property Param: string read FParam;
    property Values: TStringList read FValues;
  end;

  TCommandLine = class
  private
    FRecords: TList<TCommandLineRecord>;
  public
    constructor Create(Data: string);
    destructor Destroy; override;

    function GetParam(Name: string): TCommandLineRecord;

    property Records: TList<TCommandLineRecord> read FRecords;
  end;

implementation

{ TCommandLineRecord }

constructor TCommandLineRecord.Create(Param: string);
begin
  inherited Create;

  FParam := Param;
  FValues := TStringList.Create;
end;

destructor TCommandLineRecord.Destroy;
begin
  FValues.Free;

  inherited;
end;

{ TCommandLine }

constructor TCommandLine.Create(Data: string);
var
  i, ParsedCount: Integer;
  InDingens: Boolean;
  Arg: string;
  SL: TStringList;
  FoundRecord: TCommandLineRecord;
  LastParam: string;
begin
  ParsedCount := 0;
  Arg := '';

  FRecords := TList<TCommandLineRecord>.Create;

  SL := TStringList.Create;
  try
    InDingens := False;
    for i := 1 to Length(Data) do
      if Data[i] = '"' then
      begin
        if InDingens then
        begin
          if ParsedCount > 0 then
            SL.Add(Arg);
          Inc(ParsedCount);
          Arg := '';
        end;
        InDingens := not InDingens;
      end else if Data[i] = ' ' then
      begin
        if (not InDingens) and (Arg <> '') then
        begin
          if ParsedCount > 0 then
            SL.Add(Arg);
          Inc(ParsedCount);
          Arg := '';
        end else if Arg <> '' then
          Arg := Arg + Data[i];
      end else
        Arg := Arg + Data[i];

    if Arg <> '' then
      SL.Add(Arg);



    LastParam := '';

    for i := 0 to SL.Count - 1 do
      if Length(SL[i]) >= 1 then
        if SL[i][1] = '-' then
        begin
          LastParam := LowerCase(SL[i]);

          FoundRecord := GetParam(SL[i]);

          if FoundRecord = nil then
            FRecords.Add(TCommandLineRecord.Create(SL[i]));

        end else
        begin
          FoundRecord := GetParam(LastParam);

          if FoundRecord <> nil then
            FoundRecord.Values.Add(SL[i]);
        end;

  finally
    SL.Free;
  end;
end;

destructor TCommandLine.Destroy;
var
  i: Integer;
begin
  for i := 0 to FRecords.Count - 1 do
    FRecords[i].Free;
  FRecords.Free;

  inherited;
end;

function TCommandLine.GetParam(Name: string): TCommandLineRecord;
var
  i: Integer;
begin
  for i := 0 to FRecords.Count - 1 do
    if LowerCase(FRecords[i].Param) = LowerCase(Name) then
      Exit(FRecords[i]);
  Exit(nil);
end;

end.
