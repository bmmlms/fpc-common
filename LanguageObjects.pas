{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2014 Alexander Nottelmann

    Fixed/Enhanced by:
    Thomas Benz
    Steffen Wagner

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
    IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
    OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
    IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
    NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
    THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    ------------------------------------------------------------------------
}

unit LanguageObjects;

interface

uses
  Windows, SysUtils, Classes, StrUtils, TypInfo;

type
  TOccurence = class;
  TEntry = class;
  TProject = class;

  TLanguage = class
  private
    FID: string;
    FName: string;
    FLCID: Cardinal;
    FAvailable: Boolean;
  public
    constructor Create(LCID: Cardinal); overload;
    property ID: string read FID;
    property Name: string read FName;
    property LCID: Cardinal read FLCID;
    property Available: Boolean read FAvailable write FAvailable;
  end;

  TTranslation = class
  private
    FLanguage: TLanguage;
    FTranslation: string;
  public
    constructor Create(Language: TLanguage; Translation: string);
    property Language: TLanguage read FLanguage;
    property Translation: string read FTranslation write FTranslation;
  end;

  TOccurenceList = class(TList)
  private
    function Get2(Index: Integer): TOccurence;
    procedure Put2(Index: Integer; Item: TOccurence);
  public
    function GetText: string;
    procedure Clear; override;
    function ContainsFile(Filename: string): Boolean;
    function ContainsFileFrom(Files: TStringList): Boolean;
    property Items[Index: Integer]: TOccurence read Get2 write Put2; default;
  end;

  TEntryList = class(TList)
  private
    function Get2(Index: Integer): TEntry;
    procedure Put2(Index: Integer; Item: TEntry);
  public
    property Items[Index: Integer]: TEntry read Get2 write Put2; default;
  end;

  TLanguageList = class(TList)
  private
    FPopulated: Boolean;
    function Get2(Index: Integer): TLanguage;
    procedure Put2(Index: Integer; Item: TLanguage);
  public
    constructor Create(Populate: Boolean = False);
    destructor Destroy; override;
    function FindLanguage(Language: TLanguage): TLanguage; overload;
    function FindLanguage(Language: string): TLanguage; overload;
    property Items[Index: Integer]: TLanguage read Get2 write Put2; default;
  end;

  TTranslationList = class(TList)
  private
    function Get2(Index: Integer): TTranslation;
    procedure Put2(Index: Integer; Item: TTranslation);
  public
    property Items[Index: Integer]: TTranslation read Get2 write Put2; default;
  end;

  TClassList = class(TList)
  private
    function Get2(Index: Integer): TClass;
    procedure Put2(Index: Integer; Item: TClass);
  public
    property Items[Index: Integer]: TClass read Get2 write Put2; default;
  end;

  TFileTypes = (ftProject, ftExe, ftRes, ftUnknown);

  TProject = class
  private
    FName: string;
    FFileType: TFileTypes;
    FLanguages: TLanguageList;
    FPrimaryLanguage: TLanguage;
    FEntries: TEntryList;
    FFilename: string;
    FChanged: Boolean;

    FIgnoreStrings: TStringList;
    FIgnoreFiles: TStringList;
    FImportDir: string;
    FImportOpts: Integer;

    FOnChanged: TNotifyEvent;

    procedure FSetChanged(Value: Boolean);
  public
    constructor Create(Name: string; Languages: TLanguageList); overload;
    constructor Create(Stream: TCustomMemoryStream); overload;
    destructor Destroy; override;

    function Clone: TProject;

    procedure AddLanguage(Lang: TLanguage);
    procedure RemoveLanguage(Lang: TLanguage);
    procedure Save(Stream: TMemoryStream; SaveMeta: Boolean);
    property Name: string read FName write FName;
    property FileType: TFileTypes read FFileType write FFileType;
    property Entries: TEntryList read FEntries;
    property Languages: TLanguageList read FLanguages;
    property PrimaryLanguage: TLanguage read FPrimaryLanguage write FPrimaryLanguage;
    property Filename: string read FFilename write FFilename;
    property Changed: Boolean read FChanged write FSetChanged;

    property IgnoreStrings: TStringList read FIgnoreStrings;
    property IgnoreFiles: TStringList read FIgnoreFiles;
    property ImportDir: string read FImportDir write FImportDir;
    property ImportOpts: Integer read FImportOpts write FImportOpts;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TOccurence = class
  private
    FFilename: string;
    FLine: Cardinal;
    FComponent: string;
  public
    constructor Create(Filename: string; Line: Cardinal; Component: string); overload;
    constructor Create(Line: string); overload;
    property Filename: string read FFilename;
    property Line: Cardinal read FLine;
    property Component: string read FComponent;
    function GetString: string;
    function Clone: TOccurence;
  end;

  TEntry = class
  private
    FProject: TProject;
    FOccurences: TOccurenceList;
    FHash: Cardinal;
    FName: string;
    FTranslations: TTranslationList;
    function FGetIsFullyTranslated: Boolean;
  protected
  public
    constructor Create(Name: string; Project: TProject); overload;
    constructor Create(FromEntry: TEntry; Project: TProject); overload;
    constructor Create(FromEntry: TEntry; Languages: TLanguageList; Project: TProject); overload;
    constructor Create(Name, OccurenceLine: string; Project: TProject); overload;
    destructor Destroy; override;
    procedure UpdateFrom(Entry: TEntry);
    procedure SetOccurences(FromEntry: TEntry);
    property Occurences: TOccurenceList read FOccurences;
    property Hash: Cardinal read FHash;
    property Name: string read FName write FName;
    property Translations: TTranslationList read FTranslations;
    property IsFullyTranslated: Boolean read FGetIsFullyTranslated;
  end;

  TTextMarker = class
  private
    FProp: string;
    FOriginalText: string;
    FTranslatedText: string;
  public
    constructor Create(Prop, OriginalText, TranslatedText: string);
    property Prop: string read FProp;
    property OriginalText: string read FOriginalText;
    property TranslatedText: string read FTranslatedText;
  end;

  TTranslationMarker = class(TComponent)
  private
    FProps: TList;
    FObj: TObject;
  public
    constructor Create(AOwner: TComponent; Obj: TObject); reintroduce;
    destructor Destroy; override;
    function FindText(Prop, TranslatedText: string): string;
    procedure AddText(Prop, OriginalText, TranslatedText: string);
    property Obj: TObject read FObj write FObj;
  end;

  TTranslateProc = procedure() of object;

  TLanguageManager = class
  private
    FProjects: TList;
    FCurrentLanguage: TLanguage;
    FCurrentUserLanguage: TLanguage;
    FIgnoreClassList: TClassList;
    FSync: TRTLCriticalSection;
    procedure Setup;
    function TranslateString(C: TObject; Owner: TComponent; PropName, S: string): string;
    procedure TranslateProperty(C: TObject; Owner: TComponent; Prop: TPropInfo);
    procedure TranslateRecursive(C: TObject; Owner: TComponent; Translated: TList);
  public
    constructor Create;
    destructor Destroy; override;
    function Get(s: string): string;
    procedure Translate(C: TComponent); overload;
    procedure Translate(C: TComponent; PreTranslate, PostTranslate: TTranslateProc); overload;
    procedure SetLanguage(Language: string);
    procedure LoadFromFile(LanguageFile: string);
    property CurrentLanguage: TLanguage read FCurrentLanguage write FCurrentLanguage;
    property CurrentUserLanguage: TLanguage read FCurrentUserLanguage;
    property IgnoreClassList: TClassList read FIgnoreClassList;
  end;

var
  Language: TLanguageManager;
  LanguageList: TLanguageList;

function _(s: string): string;

implementation

function HashString(Value: string): Cardinal;
var
  i: Integer;
  x: Cardinal;
begin
  Result := 0;
  for i := 1 To Length(Value) do begin
    Result := (Result Shl 4) + Ord(Value[i]);
    x := Result and $F0000000;
    if (x <> 0) then
      Result := Result xor (x shr 24);
    Result := Result and (not x);
  end;
end;

function PosInStream(Stream: TCustomMemoryStream; Search: AnsiString; FromOffset: Int64): Integer;
var
  i: Integer;
  p: Pointer;
begin
  Result := -1;
  p := Pointer(Int64(Stream.Memory) + FromOffset);
  for i := 0 to Stream.Size - FromOffset - Length(Search) do
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

function _(s: string): string;
begin
  if Language <> nil then
    Result := Language.Get(s)
  else
    Result := s;
end;

{ TEntry }

constructor TEntry.Create(FromEntry: TEntry; Project: TProject);
var
  i: Integer;
begin
  Create(FromEntry.Name, Project);
  if FromEntry.Occurences <> nil then
    for i := 0 to FromEntry.Occurences.Count - 1 do
      FOccurences.Add(FromEntry.Occurences[i].Clone);
  for i := 0 to FromEntry.Translations.Count - 1 do
    FTranslations.Add(TTranslation.Create(FromEntry.FTranslations[i].Language, FromEntry.FTranslations[i].Translation));
end;

constructor TEntry.Create(FromEntry: TEntry; Languages: TLanguageList; Project: TProject);
var
  i: Integer;
begin
  Create(FromEntry.Name, Project);
  if FromEntry.Occurences <> nil then
    for i := 0 to FromEntry.Occurences.Count - 1 do
      FOccurences.Add(FromEntry.Occurences[i].Clone);
  for i := 0 to Languages.Count - 1 do
    FTranslations.Add(TTranslation.Create(Languages[i], ''));
end;

constructor TEntry.Create(Name, OccurenceLine: string; Project: TProject);
begin
  Create(Name, Project);
  Occurences.Add(TOccurence.Create(OccurenceLine));
end;

constructor TEntry.Create(Name: string; Project: TProject);
begin
  FProject := Project;
  FName := Name;
  FHash := HashString(Name);
  FOccurences := TOccurenceList.Create;
  FTranslations := TTranslationList.Create;
end;

destructor TEntry.Destroy;
var
  i: Integer;
begin
  for i := 0 to FTranslations.Count - 1 do
    FTranslations[i].Free;
  FTranslations.Free;
  FOccurences.Free;
  inherited;
end;

function TEntry.FGetIsFullyTranslated: Boolean;
var
  i: Integer;
begin
  if FProject = nil then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  for i := 0 to Translations.Count - 1 do
    if ((Trim(Translations[i].Translation) = '') or
        (Translations[i].Translation = Name)) and
       (Translations[i].Language <> FProject.PrimaryLanguage) then
    begin
      Result := False;
      Break;
    end;
end;

procedure TEntry.SetOccurences(FromEntry: TEntry);
var
  i: Integer;
begin
  FOccurences.Clear;
  for i := 0 to FromEntry.Occurences.Count - 1 do
    FOccurences.Add(FromEntry.Occurences[i].Clone);
end;

procedure TEntry.UpdateFrom(Entry: TEntry);
begin
  FName := Entry.Name;
  SetOccurences(Entry);
end;

{ TProject }

constructor TProject.Create(Name: string; Languages: TLanguageList);
var
  i: Integer;
begin
  FName := Name;
  FFileType := ftUnknown;
  FChanged := False;
  FIgnoreStrings := TStringList.Create;
  FIgnoreStrings.CaseSensitive := True;
  FIgnoreFiles := TStringList.Create;
  FImportDir := '';
  FImportOpts := 0;
  FLanguages := TLanguageList.Create;
  if Languages <> nil then
    for i := 0 to Languages.Count - 1 do
      FLanguages.Add(Languages[i]);
  FPrimaryLanguage := nil;
  FEntries := TEntryList.Create;
end;

function TProject.Clone: TProject;
var
  MS: TMemoryStream;
begin
  Result := nil;
  MS := TMemoryStream.Create;
  try
    Save(MS, True);
    Result := TProject.Create(MS);
    Result.Filename := Filename;
  finally
    MS.Free;
  end;
end;

constructor TProject.Create(Stream: TCustomMemoryStream);
  function LoadFromStream(Stream: TStream; Offset, Len: Integer): string;
  var
    P: PWideChar;
  begin
    Stream.Seek(Offset, soFromBeginning);

    P := GetMemory(Len + 2);
    ZeroMemory(P, Len + 2);
    Stream.ReadBuffer(P^, Len);
    Result := WideCharToString(P);
    FreeMemory(P);
  end;
  function GetSafe(const s: string): string;
  begin
    Result := s;
    Result := StringReplace(Result, '\r', #13, [rfReplaceAll]);
    Result := StringReplace(Result, '\n', #10, [rfReplaceAll]);
  end;
type
  TFileSections = (fsNone, fsSettings, fsOptions, fsIgnoreStrings,
    fsIgnoreFiles, fsLanguages, fsEntries);
var
  Section: TFileSections;
  i, LastNL, NL: Integer;
  PriLang, LangID, Line: string;
  n: Integer;
  j: Integer;
  LangFound, AlreadyExists: Boolean;
  Entry: TEntry;
  Occurence: TOccurence;
  xx: Integer;
begin
  FChanged := False;
  FImportDir := '';
  FImportOpts := 0;

  LastNL := 0;
  LangID := '';
  PriLang := '';
  FPrimaryLanguage := nil;
  Section := fsNone;
  FIgnoreStrings := TStringList.Create;
  FIgnoreStrings.CaseSensitive := True;
  FIgnoreFiles := TStringList.Create;
  FEntries := TEntryList.Create;
  FLanguages := TLanguageList.Create;
  Entry := nil;
  try
    NL := PosInStream(Stream, #13#00#10#00, LastNL);
    while NL > -1 do
    begin
      while NL = LastNL do
      begin
        LastNL := NL + 4;
        NL := PosInStream(Stream, #13#00#10#00, LastNL);
      end;

      if NL = -1 then
        Break;

      Line := LoadFromStream(Stream, LastNL, NL - LastNL);
      LastNL := NL + 4;
      NL := PosInStream(Stream, #13#00#10#00, LastNL);

      if LowerCase(Line) = '[settings]' then
      begin
        Section := fsSettings;
        Continue;
      end;
      if LowerCase(Line) = '[options]' then
      begin
        Section := fsOptions;
        Continue;
      end;
      if LowerCase(Line) = '[ignorestrings]' then
      begin
        Section := fsIgnoreStrings;
        Continue;
      end;
      if LowerCase(Line) = '[ignorefiles]' then
      begin
        Section := fsIgnoreFiles;
        Continue;
      end;
      if LowerCase(Line) = '[languages]' then
      begin
        Section := fsLanguages;
        Continue;
      end;
      if LowerCase(Line) = '[entries]' then
      begin
        Section := fsEntries;
        Continue;
      end;

      case Section of
        fsNone: ;
        fsSettings:
          begin
            if LowerCase(Copy(Line, 1, 5)) = 'name=' then
              FName := Copy(Line, 6, Length(Line) - 5);
            if LowerCase(Copy(Line, 1, 16)) = 'primarylanguage=' then
              PriLang := Copy(Line, 17, 2);
          end;
        fsOptions:
          begin
            if LowerCase(Copy(Line, 1, 10)) = 'importdir=' then
              FImportDir := Copy(Line, 11, Length(Line) - 9);
            if LowerCase(Copy(Line, 1, 11)) = 'importopts=' then
              FImportOpts := StrToIntDef(Copy(Line, 12, 1), 0);
          end;
        fsIgnoreStrings:
          begin
            if LowerCase(Copy(Line, 1, 5)) = 'name=' then
              if Trim(Copy(Line, 6, Length(Line) - 5)) <> '' then
                FIgnoreStrings.Add(GetSafe(Copy(Line, 6, Length(Line) - 5)));
          end;
        fsIgnoreFiles:
          begin
            if LowerCase(Copy(Line, 1, 5)) = 'name=' then
              if Trim(Copy(Line, 6, Length(Line) - 5)) <> '' then
                FIgnoreFiles.Add(Copy(Line, 6, Length(Line) - 5));
          end;
        fsLanguages:
          begin
            if LowerCase(Copy(Line, 1, 3)) = 'id=' then
              LangID := Copy(Line, 4, Length(Line) - 3)
            else LangID := '';

            if Length(LangID) = 2 then
            begin
              for i := 0 to LanguageList.Count - 1 do
              begin
                if LanguageList[i].FID = LangID then
                begin
                  Languages.Add(LanguageList[i]);
                  LangID := '';
                  Break;
                end;
              end;
            end;
          end;
        fsEntries:
          begin
            if LowerCase(Copy(Line, 1, 8)) = 'text_id=' then
            begin
              if (Entry <> nil) and (Entry.Translations.Count = 0) then
              begin
                FEntries.Remove(Entry);
                FreeAndNil(Entry);
              end;
              Entry := TEntry.Create(GetSafe(Copy(Line, 9, Length(Line) - 8)), Self);
              FEntries.Add(Entry);
            end else if (LowerCase(Copy(Line, 1, 10)) = 'occurence=') or
                        (LowerCase(Copy(Line, 1, 10)) = 'occurance=') then // Hatte früher einen kleinen Rechtschreibfehler...
            begin
              if Entry <> nil then
              begin
                try
                  Occurence := TOccurence.Create(Copy(Line, 11, Length(Line) - 10));
                  Entry.Occurences.Add(Occurence);
                except
                end;
              end;
            end else if LowerCase(Copy(Line, 1, 5)) = 'text_' then
            begin
              LangID := Copy(Line, 6, 2);
              LangFound := False;
              for i := 0 to Languages.Count - 1 do
                if Languages[i].ID = LangID then
                begin
                  Entry.Translations.Add(TTranslation.Create(Languages[i], GetSafe(Copy(Line, 9, Length(Line) - 8))));
                  LangFound := True;
                  Break;
                end;
              if not LangFound then
              begin
                for i := 0 to LanguageList.Count - 1 do
                begin
                  if LanguageList[i].ID = LangID then
                  begin
                    Languages.Add(LanguageList[i]);
                    Entry.Translations.Add(TTranslation.Create(LanguageList[i], GetSafe(Copy(Line, 9, Length(Line) - 8))));
                    LangID := '';
                    Break;
                  end;
                end;
              end;
            end;
          end;
      end;
    end;
    if Length(PriLang) = 2 then
      for i := 0 to Languages.Count - 1 do
        if Languages[i].ID = PriLang then
        begin
          FPrimaryLanguage :=  Languages[i];
          Break;
        end;
    if FPrimaryLanguage = nil then
      raise Exception.Create('No primary language could be found');

    for i := 0 to Languages.Count - 1 do
      for n := 0 to Entries.Count - 1 do
      begin
        LangFound := False;
        for j := 0 to Entries[n].Translations.Count - 1 do
          if Entries[n].Translations[j].Language = Languages[i] then
          begin
            LangFound := True;
            Break;
          end;
        if not LangFound then
          Entries[n].Translations.Add(TTranslation.Create(Languages[i], ''))
      end;
  except
    FreeAndNil(FLanguages);
    for i := 0 to FEntries.Count - 1 do
      FEntries[i].Free;
    FreeAndNil(FEntries);
    raise;
  end;
end;

destructor TProject.Destroy;
var
  i: Integer;
begin
  if FLanguages <> nil then
    FLanguages.Free;
  if FEntries <> nil then
  begin
    for i := 0 to FEntries.Count - 1 do
      TEntry(FEntries[i]).Free;
    FEntries.Free;
  end;
  FIgnoreStrings.Free;
  FIgnoreFiles.Free;
  inherited;
end;

procedure TProject.FSetChanged(Value: Boolean);
begin
  FChanged := True;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TProject.AddLanguage(Lang: TLanguage);
var
  i: Integer;
begin
  if FLanguages.IndexOf(Lang) = -1 then
  begin
    FLanguages.Add(Lang);
    for i := 0 to FEntries.Count - 1 do
      FEntries[i].FTranslations.Add(TTranslation.Create(Lang, ''));
  end;
  FChanged := True;
end;

procedure TProject.RemoveLanguage(Lang: TLanguage);
var
  i: Integer;
  n: Integer;
begin
  for i := 0 to FEntries.Count - 1 do
    for n := FEntries[i].FTranslations.Count - 1 downto 0 do
      if FEntries[i].FTranslations[n].FLanguage = Lang then
      begin
        FEntries[i].FTranslations[n].Free;
        FEntries[i].FTranslations.Delete(n);
      end;
  FLanguages.Remove(Lang);
  FChanged := True;
end;

procedure TProject.Save(Stream: TMemoryStream; SaveMeta: Boolean);
  procedure WriteToStream(s: string; Stream: TStream);
  var
    Len: Integer;
    {$IF CompilerVersion < 18.5}
    P: Pointer;
    {$IFEND}
  begin
    s := s + #13#10;
    Len := Length(s) * 2;
    {$IF CompilerVersion >= 18.5}
    Stream.WriteBuffer(s[1], Len);
    {$ELSE}
    P := GetMemory(Len);
    StringToWideChar(s, P, Len);
    Stream.WriteBuffer(P^, Len);
    FreeMemory(P);
    {$IFEND}
  end;
  function MakeSafe(const s: string): string;
  begin
    Result := s;
    Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  end;
var
  i, n: Integer;
begin
  WriteToStream('[settings]', Stream);
  WriteToStream('version=1', Stream);
  WriteToStream('name=' + FName, Stream);
  WriteToStream('primarylanguage=' + PrimaryLanguage.FID, Stream);
  WriteToStream('', Stream);

  if SaveMeta then
  begin
    WriteToStream('[options]', Stream);
    if FImportDir <> '' then
      WriteToStream('importdir=' + FImportDir, Stream);
    WriteToStream('importopts=' + IntToStr(FImportOpts), Stream);
    WriteToStream('', Stream);

    if FIgnoreStrings.Count > 0 then
    begin
      WriteToStream('[ignorestrings]', Stream);
      for i := 0 to FIgnoreStrings.Count - 1 do
        WriteToStream('name=' + MakeSafe(FIgnoreStrings[i]), Stream);
      WriteToStream('', Stream);
    end;

    if FIgnoreFiles.Count > 0 then
    begin
      WriteToStream('[ignorefiles]', Stream);
      for i := 0 to FIgnoreFiles.Count - 1 do
        WriteToStream('name=' + FIgnoreFiles[i], Stream);
      WriteToStream('', Stream);
    end;
  end;

  WriteToStream('[languages]', Stream);

  for i := 0 to FLanguages.Count - 1 do
  begin
    WriteToStream('id' + '=' + FLanguages[i].ID, Stream);
  end;

  WriteToStream('', Stream);

  WriteToStream('[entries]', Stream);

  for i := 0 to FEntries.Count - 1 do
  begin
    WriteToStream('text_id=' + MakeSafe(FEntries[i].Name), Stream);
    if SaveMeta then
      for n := 0 to FEntries[i].Occurences.Count - 1 do
        WriteToStream('occurence=' + FEntries[i].Occurences[n].GetString, Stream);

    for n := 0 to FEntries[i].Translations.Count - 1 do
    begin
      WriteToStream('text_' + FEntries[i].Translations[n].FLanguage.ID + '=' + MakeSafe(FEntries[i].Translations[n].FTranslation), Stream);
    end;
    WriteToStream('', Stream);
  end;

  FChanged := False;
end;

{ TOccurenceList }

procedure TOccurenceList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited;
end;

function TOccurenceList.ContainsFile(Filename: string): Boolean;
var
  n: Integer;
begin
  Result := False;
  for n := 0 to Count - 1 do
    if LowerCase(Items[n].FFilename) = LowerCase(Filename) then
    begin
      Result := True;
      Exit;
    end;
end;

function TOccurenceList.ContainsFileFrom(Files: TStringList): Boolean;
var
  i, n: Integer;
begin
  Result := False;
  for i := 0 to Files.Count - 1 do
    for n := 0 to Count - 1 do
      if LowerCase(Items[n].FFilename) = LowerCase(Files[i]) then
      begin
        Result := True;
        Exit;
      end;
end;

function TOccurenceList.Get2(Index: Integer): TOccurence;
begin
  Result := TOccurence(inherited Get(Index));
end;

function TOccurenceList.GetText: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    Result := Result + Items[i].Filename + ':' + IntToStr(Items[i].Line);
    if Items[i].Component <> '' then
      Result := Result + ':' + Items[i].Component;
    Result := Result + #13#10;
  end;
  Result := Trim(Result);
end;

procedure TOccurenceList.Put2(Index: Integer; Item: TOccurence);
begin
  inherited Put(Index, Item);
end;

{ TEntryList }

function TEntryList.Get2(Index: Integer): TEntry;
begin
  Result := TEntry(inherited Get(Index));
end;

procedure TEntryList.Put2(Index: Integer; Item: TEntry);
begin
  inherited Put(Index, Item);
end;

{ TLanguageList }

var
  //Locales: TList;
  // T.Benz 10.02.2012
  Locales: TStrings;

function LocalesEnumProc(szLoc: LPSTR): Integer; stdcall;
{
var
  P: Pointer;
}
begin
  // T.Benz 10.02.2012
  // Die Verwaltung des Speichers überlasse ich der Stringliste
{  GetMem(P, StrLen(szLoc) + 1);
  ZeroMemory(P, StrLen(szLoc) + 1);
  StrCopy(P, szLoc);
  Locales.Add(P);}
  Locales.Add(szLoc);
  Result := 1;
end;

constructor TLanguageList.Create(Populate: Boolean);
var
  i, n, P: Integer;
  s: string;
  L: TLanguage;
begin
  inherited Create;
  FPopulated := Populate;
  if Populate then
  begin
    // T.Benz 10.02.2012
    //Locales := TList.Create;
    Locales := TStringList.Create;
    try
      try
        EnumSystemLocalesA(@LocalesEnumProc, LCID_INSTALLED);
        for i := 0 to Locales.Count - 1 do
        begin
          // T.Benz 10.02.2012
          // ich überlasse die Verwaltung des Speichers der Stringliste
          // das FreeMem verursachte in einigen anderen Programmteilen
          // Schutzverletzungen
          {
          s := string(StrPas(PAnsiChar(Locales[i])));
          L := TLanguage.Create(StrToInt('$' + Copy(s, 5, 4)));
          Add(L);
          FreeMem(Locales[i]);
          }
          s := Locales[i];
          L := TLanguage.Create(StrToInt('$' + Copy(s, 5, 4)));
          Add(L);
        end;
        for i := 0 to Count - 1 do
        begin
          P := Pos('(', Items[i].FName);
          if P > 0 then
          begin
            Items[i].FName := Copy(Items[i].FName, 1, P - 1);
          end;
        end;
        for i := Count - 2 downto 0 do
        begin
          for n := Count - 1 downto i + 1 do
          begin
            if Items[i].ID = Items[n].ID then
            begin
              Items[i].Free;
              Delete(i);
              Break;
            end;
          end;
        end;
        for i := Count - 1 downto 0 do
          if Items[i].FID = '' then
          begin
            Items[i].Free;
            Delete(i);
          end;
      finally
        Locales.Free;
      end;
    except
    end;
  end;
end;

destructor TLanguageList.Destroy;
var
  i: Integer;
begin
  if FPopulated then
  begin
    for i := 0 to Count - 1 do
      Items[i].Free;
  end;
  inherited;
end;

function TLanguageList.FindLanguage(Language: string): TLanguage;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if LowerCase(Items[i].ID) = LowerCase(Language) then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TLanguageList.FindLanguage(Language: TLanguage): TLanguage;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].ID = Language.ID then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TLanguageList.Get2(Index: Integer): TLanguage;
begin
  Result := TLanguage(inherited Get(Index));
end;

procedure TLanguageList.Put2(Index: Integer; Item: TLanguage);
begin
  inherited Put(Index, Item);
end;

{ TTranslation }

constructor TTranslation.Create(Language: TLanguage; Translation: string);
begin
  FLanguage := Language;
  FTranslation := Translation;
end;

{ TLanguage }

constructor TLanguage.Create(LCID: Cardinal);
var
  szLangCode: array[0..2] of AnsiChar;
  szLangName: array[0..254] of AnsiChar;
  LangID, LangName: AnsiString;
begin
  LangID := '';
  LangName := '';
  FLCID := LCID;
  FAvailable := False;
  ZeroMemory(@szLangCode[0], 3);
  if GetLocaleInfoA(LCID, LOCALE_SISO639LANGNAME, @szLangCode[0], 3) > 0 then
  begin
    CharLowerBuffA(@szLangCode[1], Length(szLangCode));
    LangID := szLangCode;
  end;
  ZeroMemory(@szLangName[0], 255);
  if GetLocaleInfoA(LCID, LOCALE_SENGLANGUAGE, @szLangName[0], 255) > 0 then
  begin
    LangName := szLangName;
  end;
  FID := string(LangID);
  FName := string(LangName);
end;

{ TTranslationList }

function TTranslationList.Get2(Index: Integer): TTranslation;
begin
  Result := TTranslation(inherited Get(Index));
end;

procedure TTranslationList.Put2(Index: Integer; Item: TTranslation);
begin
  inherited Put(Index, Item);
end;

{ TClassList }

function TClassList.Get2(Index: Integer): TClass;
begin
  Result := TClass(inherited Get(Index));
end;

procedure TClassList.Put2(Index: Integer; Item: TClass);
begin
  inherited Put(Index, Item);
end;

{ TLanguageManager }

var
  Resources: TStringList;

function EnumResNameProc(hModule: HINST; lpszType: PChar; lpszName: PChar; lParam: LPARAM): BOOL; stdcall;
  function IsIntResource(lpszType: PChar): Boolean;
  begin
    Result := ((DWORD(lpszType) shr 16) = 0);
  end;
begin
  if not IsIntResource(lpszName) then
  begin
    if (Copy(lpszName, 1, 10) = 'LINGUSLANG') and (lpszType = RT_RCDATA) then
      Resources.Add(lpszName);
  end;
  Result := True;
end;

procedure TLanguageManager.Setup;
type
  TGetUserDefaultUILanguage = function: LANGID; stdcall;
var
  i, n: Integer;
  LangID: Cardinal;
  szLangCode: array[0..2] of AnsiChar;
  GetUserDefaultUILanguage: TGetUserDefaultUILanguage;
  Project: TProject;
  Lang: TLanguage;
begin
  if FProjects.Count = 0 then
    raise Exception.Create('No translations could be found');

  for n := 0 to FProjects.Count - 1 do
  begin
    Project := TProject(FProjects[n]);
    for i := 0 to Project.Languages.Count - 1 do
    begin
      if LanguageList.FindLanguage(Project.Languages[i].ID) <> nil then
        LanguageList.FindLanguage(Project.Languages[i].ID).Available := True;
    end;
  end;

  // Irgendeine Sprache suchen
  for n := 0 to FProjects.Count - 1 do
  begin
    Project := TProject(FProjects[n]);
    if Project.Languages.Count > 0 then
    begin
      FCurrentLanguage := LanguageList.FindLanguage(Project.Languages[0]);
      Break;
    end;
  end;

  // Englisch suchen
  for n := 0 to FProjects.Count - 1 do
  begin
    Project := TProject(FProjects[n]);
    for i := 0 to Project.Languages.Count - 1 do
      if Project.Languages[i].ID = 'en' then
      begin
        FCurrentLanguage := LanguageList.FindLanguage(Project.Languages[i]);
        Break;
      end;
  end;

  // Benutzersprache suchen
  @GetUserDefaultUILanguage := GetProcAddress(GetModuleHandle('kernel32'), 'GetUserDefaultUILanguage');
  if Assigned(@GetUserDefaultUILanguage) then
  begin
    LangID := GetUserDefaultUILanguage;
    ZeroMemory(@szLangCode[0], 3);
    if GetLocaleInfoA(LangID, LOCALE_SISO639LANGNAME, @szLangCode[0], 3) > 0 then
    begin
      Lang := LanguageList.FindLanguage(LowerCase(string(szLangCode)));
      if Lang <> nil then
      begin
        if Lang.Available then
          FCurrentLanguage := Lang;
        FCurrentUserLanguage := Lang;
      end;
    end;
  end;

  if FCurrentLanguage = nil then
    raise Exception.Create('No Language could be determined');
end;

constructor TLanguageManager.Create;
type
  TGetUserDefaultUILanguage = function: LANGID; stdcall;
var
  i: Integer;
  Res: TResourceStream;
  Project: TProject;
begin
  inherited;

  InitializeCriticalSection(FSync);
  FCurrentLanguage := nil;
  FProjects := TList.Create;
  FIgnoreClassList := TClassList.Create;

  try
    Resources := TStringList.Create;
    try
      EnumResourceNames(HInstance, RT_RCDATA, @EnumResNameProc, 0);

      for i := 0 to Resources.Count - 1 do
      begin
        Res := TResourceStream.Create(HInstance, PChar(Resources[i]), MakeIntResource(10));
        try
          Project := TProject.Create(Res);
          FProjects.Add(Project);
        finally
          Res.Free;
        end;
      end;
      Setup;
    finally
      Resources.Free;
    end;
  except
    for i := 0 to FProjects.Count - 1 do
      TProject(FProjects[i]).Free;
    FProjects.Clear;
  end;
end;

procedure TLanguageManager.LoadFromFile(LanguageFile: string);
var
  i: Integer;
  MS: TMemoryStream;
  Project: TProject;
begin
  if FProjects.Count > 0 then
  begin
    for i := 0 to FProjects.Count - 1 do
      TProject(FProjects[i]).Free;
    FProjects.Clear;
  end;

  try
    MS := TMemoryStream.Create;
    try
      MS.LoadFromFile(LanguageFile);
      Project := TProject.Create(MS);
      FProjects.Add(Project);
    finally
      MS.Free;
    end;
    Setup;
  except
    for i := 0 to FProjects.Count - 1 do
      TProject(FProjects[i]).Free;
    FProjects.Clear;
    raise Exception.Create('Error loading languagefile');
  end;
end;

destructor TLanguageManager.Destroy;
var
  i: Integer;
begin
  DeleteCriticalSection(FSync);
  for i := 0 to FProjects.Count - 1 do
    TProject(FProjects[i]).Free;
  FProjects.Free;
  FIgnoreClassList.Free;
  inherited;
end;

function TLanguageManager.Get(s: string): string;
var
  j, i, n: Integer;
  Hash: Cardinal;
  Project: TProject;
begin
  Hash := HashString(s);
  EnterCriticalSection(FSync);
  Result := '';
  if FCurrentLanguage <> nil then
    for j := 0 to FProjects.Count - 1 do
    begin
      Project := TProject(FProjects[j]);
      for i := 0 to Project.Entries.Count - 1 do
        if Project.Entries[i].Hash = Hash then
        begin
          for n := 0 to Project.Entries[i].Translations.Count - 1 do
            if Project.Entries[i].Translations[n].Language.ID = FCurrentLanguage.ID then
            begin
              Result := Project.Entries[i].Translations[n].Translation;
              Break;
            end;
          Break;
        end;
    end;
  if Result = '' then
    Result := s;
  LeaveCriticalSection(FSync);
end;

procedure TLanguageManager.TranslateProperty(C: TObject; Owner: TComponent; Prop: TPropInfo);
  function ReadString(C: TObject; Prop: TPropInfo): string;
  var
    Name: string;
  begin
    Result := '';
    Name := string(Prop.Name);
    case Prop.PropType^.Kind of
      tkString, tkLString:
        Result := GetStrProp(C, Name);
      tkWString:
        Result := GetWideStrProp(C, Name);
      {$IF CompilerVersion > 18.5}
      tkUString:
        Result := GetUnicodeStrProp(C, Name);
      {$IFEND}
    end;
  end;
var
  OldValue, NewValue: string;
begin
  if Prop.SetProc <> nil then
  begin
    case Prop.PropType^.Kind of
      {$IF CompilerVersion > 18.5}
      tkString, tkLString, tkWString, tkUString:
      {$ELSE}
      tkString, tkLString, tkWString:
      {$IFEND}
        begin
          OldValue := ReadString(C, Prop);
          NewValue := TranslateString(C, Owner, string(Prop.Name), OldValue);
          if (NewValue <> '') then
          begin
            SetWideStrProp(C, @Prop, NewValue);
          end;
        end;
    end;
  end;
end;

function TLanguageManager.TranslateString(C: TObject; Owner: TComponent;
  PropName, S: string): string;
var
  OldValue: string;
  TM: TTranslationMarker;
  i: Integer;
begin
  Result := '';
  TM := nil;

  for i := 0 to Owner.ComponentCount - 1 do
  begin
    if Owner.Components[i] is TTranslationMarker then
      if TTranslationMarker(Owner.Components[i]).Obj = C then
      begin
        TM := TTranslationMarker(Owner.Components[i]);
      end;
  end;

  if TM <> nil then
  begin
    OldValue := TM.FindText(string(PropName), S);
    if OldValue = '' then
    begin
      OldValue := S;
      if OldValue <> '' then
      begin
        Result := Language.Get(OldValue);
        TM.AddText(string(PropName), OldValue, Result);
      end;
    end else
    begin
      Result := Language.Get(OldValue);

      // Ich bin mir nicht sicher ob das hier hin muss, aber es behebt
      // http://www.delphipraxis.net/146618-lingus-ubersetzungswerkzeug-3.html#post1036234
      // von drschubi.
      TM.AddText(string(PropName), OldValue, Result);
    end;
  end else
  begin
    OldValue := S;
    if OldValue <> '' then
    begin
      TM := TTranslationMarker.Create(Owner, C);
      Result := Language.Get(OldValue);
      TM.AddText(string(PropName), OldValue, Result);
    end;
  end;
end;

procedure TLanguageManager.TranslateRecursive(C: TObject; Owner: TComponent; Translated: TList);
var
  i, n, Count: Integer;
  PropInfo: PPropInfo;
  PropList: TPropList;
  Cl: TObject;
  NewValue: string;
begin
  if C = nil then
    Exit;

  // Ignore-List überprüfen
  for i := 0 to FIgnoreClassList.Count - 1 do
    if (FIgnoreClassList[i] = C.ClassType) or (C.InheritsFrom(FIgnoreClassList[i])) then
      Exit;

  // Prüfen, ob das Objekt schon in der Liste ist.
  // Das sollte StackOverflow-Exceptions verhindern.
  for i := 0 to Translated.Count - 1 do
    if Translated[i] = C then
      Exit;
  Translated.Add(C);

  if C is TComponent then
    Owner := TComponent(C);

  {$IF CompilerVersion > 18.5}
  Count := GetPropList(PTypeInfo(C.ClassInfo), [tkClass, tkString, tkLString, tkWString, tkUString], @PropList);
  {$ELSE}
  Count := GetPropList(PTypeInfo(C.ClassInfo), [tkClass, tkString, tkLString, tkWString], @PropList);
  {$IFEND}

  for i := 0 to Count - 1 do
  begin
    PropInfo := PropList[i];

    case PropInfo^.PropType^.Kind of
      {$IF CompilerVersion > 18.5}
      tkString, tkLString, tkWString, tkUString:
      {$ELSE}
      tkString, tkLString, tkWString:
      {$IFEND}
        begin
          if (PropInfo^.Name = 'Caption') or (PropInfo^.Name = 'Hint')
             or (PropInfo^.Name = 'Title')  or (PropInfo^.Name = 'Description') // Steffen: ToolTip Komponente
             or (PropInfo^.Name = 'DisplayLabel') then // Steffen: Database property
          begin
            TranslateProperty(C, Owner, PropInfo^);
          end;
        end;
      tkClass:
        begin
          begin
            if C.ClassName <> 'TMenuItem' then
            begin
              Cl := GetObjectProp(C, string(PropInfo^.Name));
              if (Cl <> nil) and (Cl.ClassName <> 'TAction') then
              begin
                if Cl.InheritsFrom(TStrings) then
                begin
                  // Spezialbehandlung für TStrings, siehe
                  // http://www.delphipraxis.net/152626-rtti-memo-lines-tstrings.html
                  NewValue := TranslateString(Cl, Owner, 'Text', TStrings(Cl).Text);
                  // Ohne <> Vergleich werden sonst bei z.B. TComboBox Index auf -1 gesetzt
                  if (NewValue <> '') and (TStrings(Cl).Text <> NewValue) then
                    TStrings(Cl).Text := NewValue;
                end else if Cl.InheritsFrom(TCollection) then
                begin
                  for n := 0 to TCollection(Cl).Count - 1 do
                    TranslateRecursive(TCollection(Cl).Items[n], Owner, Translated);
                end else
                  TranslateRecursive(Cl, Owner, Translated);
              end;
            end;
          end;
        end;
    end;
  end;

  if C is TComponent then
  begin
    for i := 0 to TComponent(C).ComponentCount - 1 do
      TranslateRecursive(TComponent(C).Components[i], TComponent(C).Components[i], Translated);
  end;
end;

procedure TLanguageManager.Translate(C: TComponent);
var
  Translated: TList;
begin
  if C = nil then
    Exit;

  Translated := TList.Create;
  try
    TranslateRecursive(C, C, Translated);
  finally
    Translated.Free;
  end;
end;

procedure TLanguageManager.Translate(C: TComponent;
  PreTranslate, PostTranslate: TTranslateProc);
begin
  try
    PreTranslate;
    Translate(C);
  finally
    PostTranslate;
  end;
end;

procedure TLanguageManager.SetLanguage(Language: string);
var
  Lang: TLanguage;
begin
  Lang := LanguageList.FindLanguage(Language);
  if Lang <> nil then
    FCurrentLanguage := Lang;
end;

{ TTranslationMarker }

procedure TTranslationMarker.AddText(Prop, OriginalText, TranslatedText: string);
begin
  FProps.Add(TTextMarker.Create(Prop, OriginalText, TranslatedText));
end;

constructor TTranslationMarker.Create(AOwner: TComponent; Obj: TObject);
begin
  inherited Create(AOwner);
  FProps := TList.Create;
  FObj := Obj;
end;

destructor TTranslationMarker.Destroy;
var
  i: Integer;
begin
  for i := 0 to FProps.Count - 1 do
    TTextMarker(FProps[i]).Free;
  FProps.Free;
  inherited;
end;

function TTranslationMarker.FindText(Prop, TranslatedText: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FProps.Count - 1 do
    if (TTextMarker(FProps[i]).Prop = Prop) and (TTextMarker(FProps[i]).TranslatedText = TranslatedText) then
    begin
      Result := TTextMarker(FProps[i]).OriginalText;
      Break;
    end;
end;

{ TTextMarker }

constructor TTextMarker.Create(Prop, OriginalText, TranslatedText: string);
begin
  inherited Create;
  FProp := Prop;
  FOriginalText := OriginalText;
  FTranslatedText := TranslatedText;
end;

{ TOccurence }

constructor TOccurence.Create(Filename: string; Line: Cardinal;
  Component: string);
begin
  inherited Create;

  FFilename := Filename;
  FLine := Line;
  FComponent := Component;
end;

function TOccurence.Clone: TOccurence;
begin
  Result := TOccurence.Create(FFilename, FLine, FComponent);
end;

constructor TOccurence.Create(Line: string);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Delimiter := ':';
    S.DelimitedText := Line;

    if S.Count = 2 then
      Create(S[0], StrToInt(S[1]), '')
    else if S.Count = 3 then
      Create(S[0], StrToInt(S[1]), S[2]);
  finally
    S.Free;
  end;
end;

function TOccurence.GetString: string;
begin
  Result := FFilename + ':' + IntToStr(FLine);
  if FComponent <> '' then
    Result := Result + ':' + FComponent;
end;


initialization
  LanguageList := TLanguageList.Create(True);
  Language := TLanguageManager.Create;

finalization
  Language.Free;
  LanguageList.Free;

end.
