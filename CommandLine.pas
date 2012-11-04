unit CommandLine;

interface

uses
  Windows, Messages, SysUtils, Classes, Generics.Collections,
  Functions;

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
    begin
      if Data[i] = '"' then
      begin
        if InDingens then
        begin
          if ParsedCount > 0 then
          begin
            SL.Add(Arg);
          end;
          Inc(ParsedCount);
          Arg := '';
        end;
        InDingens := not InDingens;
      end else if Data[i] = ' ' then
      begin
        if (not InDingens) and (Arg <> '') then
        begin
          if ParsedCount > 0 then
          begin
            SL.Add(Arg);
          end;
          Inc(ParsedCount);
          Arg := '';
        end else if Arg <> '' then
          Arg := Arg + Data[i];
      end else
      begin
        Arg := Arg + Data[i];
      end;
    end;

    if Arg <> '' then
    begin
      SL.Add(Arg);
    end;



    LastParam := '';

    for i := 0 to SL.Count - 1 do
    begin
      if Length(SL[i]) >= 1 then
      begin
        if SL[i][1] = '-' then
        begin
          LastParam := LowerCase(SL[i]);

          FoundRecord := GetParam(SL[i]);

          if FoundRecord = nil then
          begin
            FRecords.Add(TCommandLineRecord.Create(SL[i]));
          end;

        end else
        begin
          FoundRecord := GetParam(LastParam);

          if FoundRecord <> nil then
          begin
            FoundRecord.Values.Add(SL[i]);
          end;
        end;
      end;
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
