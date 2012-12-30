{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2013 Alexander Nottelmann

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

unit SettingsStorage;

interface

uses
  Windows, SysUtils, Classes, Registry, IniFiles, GUIFunctions, ShlObj,
  Functions, Generics.Collections, ExtendedStream, CommandLine;

const
  SETTINGS = 'Settings';

type
  TDataType = (dtUnknown, dtString, dtInteger, dtBoolean);

  TDataEntry = class
  private
    FName, FSection: string;
  public
    procedure Save(Stream: TExtendedStream); virtual;
    class function Load(Stream: TExtendedStream): TDataEntry;

    property Name: string read FName;
    property Section: string read FSection;
  end;

  TDataString = class(TDataEntry)
  private
    FValue: string;
  public
    constructor Create(Name, Section: string; D: string);

    procedure Save(Stream: TExtendedStream); override;
    procedure Load(Stream: TExtendedStream);
    property Value: string read FValue;
  end;

  TDataInteger = class(TDataEntry)
  private
    FValue: Integer;
  public
    constructor Create(Name, Section: string; D: Integer);

    procedure Save(Stream: TExtendedStream); override;
    procedure Load(Stream: TExtendedStream);

    property Value: Integer read FValue;
  end;

  TDataBoolean = class(TDataEntry)
  private
    FValue: Boolean;
  public
    constructor Create(Name, Section: string; D: Boolean);

    procedure Save(Stream: TExtendedStream); override;
    procedure Load(Stream: TExtendedStream);

    property Value: Boolean read FValue;
  end;

  TSettingsList = class(TList<TDataEntry>)
  public
    destructor Destroy; override;

    procedure Save(Stream: TExtendedStream);
    class function Load(Stream: TExtendedStream): TSettingsList;
  end;

  TSettingsStorage = class
  private
  protected
    FAppName: string;
    FAppPath: string;

    FDataDir: string;

    FDataDirOverridden: Boolean;

    FCommandLine: TCommandLine;
  public
    constructor Create(AppName, AppPath: string; CommandLine: TCommandLine); virtual;

    procedure Assign(AssignFrom: TSettingsStorage); overload;
    procedure Assign(AssignFrom: TSettingsList); overload;
    procedure GetData(Lst: TSettingsList); virtual;
    function DeleteProfile: Boolean; virtual;

    procedure Write(Name: string; Value: string; Section: string = SETTINGS); overload; virtual; abstract;
    procedure Write(Name: string; Value: Integer; Section: string = SETTINGS); overload; virtual; abstract;
    procedure Write(Name: string; Value: Boolean; Section: string = SETTINGS); overload; virtual; abstract;
    procedure Read(Name: string; var Value: string; Default: string; Section: string = SETTINGS); overload; virtual; abstract;
    procedure Read(Name: string; var Value: Integer; Default: Integer; Section: string = SETTINGS); overload; virtual; abstract;

    // REMARK: Das hier ist dumm. Weil es immer an den Integer-Aufruf weitergeleitet wird.
    //         Ändern ist aber nicht so einfach, weil Werte, die das hier benutzen,
    //         auch in TStreamSettings vorhanden sind, was wiederum in der Datei gespeichert wird.
    procedure Read(Name: string; var Value: Cardinal; Default: Cardinal; Section: string = SETTINGS); overload; virtual; abstract;

    procedure Read(Name: string; var Value: Boolean; Default: Boolean; Section: string = SETTINGS); overload; virtual; abstract;
    function Delete(Name: string; Section: string = SETTINGS): Boolean; virtual; abstract;
    function DeleteKey(Section: string): Boolean; virtual; abstract;
    procedure GetValues(Section: string; var List: TStringList); virtual; abstract;

    function CreatePath: Boolean;
    function GetFilePath(Filename: string): string; virtual;
    function PrepareSave: Boolean; virtual; abstract;

    property DataDir: string read FDataDir;
    property DataDirOverridden: Boolean read FDataDirOverridden;
  end;

  TSettingsInstalled = class(TSettingsStorage)
  private
    FRegPath: string;
    procedure GetDataInternal(Path: string; Lst: TSettingsList; TruncLen: Integer);
  public
    constructor Create(AppName, AppPath: string; CommandLine: TCommandLine); override;

    class function Active(AppName: string): Boolean;
    class function GetRegPath(AppName: string): string;
    class function GetDataDir(AppName: string): string;

    procedure GetData(Lst: TSettingsList); overload; override;
    function DeleteProfile: Boolean; override;

    procedure Write(Name: string; Value: string; Section: string = SETTINGS); overload; override;
    procedure Write(Name: string; Value: Integer; Section: string = SETTINGS); overload; override;
    procedure Write(Name: string; Value: Boolean; Section: string = SETTINGS); overload; override;
    procedure Read(Name: string; var Value: string; Default: string; Section: string = SETTINGS); overload; override;
    procedure Read(Name: string; var Value: Integer; Default: Integer; Section: string = SETTINGS); overload; override;
    procedure Read(Name: string; var Value: Cardinal; Default: Cardinal; Section: string = SETTINGS); overload; override;
    procedure Read(Name: string; var Value: Boolean; Default: Boolean; Section: string = SETTINGS); overload; override;
    function Delete(Name: string; Section: string = SETTINGS): Boolean; override;
    function DeleteKey(Section: string): Boolean; override;
    procedure GetValues(Section: string; var List: TStringList); override;

    function PrepareSave: Boolean; override;
  end;

  TSettingsPortable = class(TSettingsStorage)
  private
    FIniFile: string;
  public
    constructor Create(AppName, AppPath: string; CommandLine: TCommandLine); override;

    class function Active(AppName: string): Boolean;
    class function GetDataDir: string;

    procedure GetData(Lst: TSettingsList); override;
    function DeleteProfile: Boolean; override;

    procedure Write(Name: string; Value: string; Section: string = SETTINGS); overload; override;
    procedure Write(Name: string; Value: Integer; Section: string = SETTINGS); overload; override;
    procedure Write(Name: string; Value: Boolean; Section: string = SETTINGS); overload; override;
    procedure Read(Name: string; var Value: string; Default: string; Section: string = SETTINGS); overload; override;
    procedure Read(Name: string; var Value: Integer; Default: Integer; Section: string = SETTINGS); overload; override;
    procedure Read(Name: string; var Value: Cardinal; Default: Cardinal; Section: string = SETTINGS); overload; override;
    procedure Read(Name: string; var Value: Boolean; Default: Boolean; Section: string = SETTINGS); overload; override;
    function Delete(Name: string; Section: string = SETTINGS): Boolean; override;
    function DeleteKey(Section: string): Boolean; override;
    procedure GetValues(Section: string; var List: TStringList); override;

    function PrepareSave: Boolean; override;
  end;

  TSettingsDummy = class(TSettingsStorage)
  public
    constructor Create(AppName, AppPath: string; CommandLine: TCommandLine); override;

    procedure Write(Name: string; Value: string; Section: string = SETTINGS); overload; override;
    procedure Write(Name: string; Value: Integer; Section: string = SETTINGS); overload; override;
    procedure Write(Name: string; Value: Boolean; Section: string = SETTINGS); overload; override;
    procedure Read(Name: string; var Value: string; Default: string; Section: string = SETTINGS); overload; override;
    procedure Read(Name: string; var Value: Integer; Default: Integer; Section: string = SETTINGS); overload; override;
    procedure Read(Name: string; var Value: Cardinal; Default: Cardinal; Section: string = SETTINGS); overload; override;
    procedure Read(Name: string; var Value: Boolean; Default: Boolean; Section: string = SETTINGS); overload; override;
    function Delete(Name: string; Section: string = SETTINGS): Boolean; override;
    function DeleteKey(Section: string): Boolean; override;
    procedure GetValues(Section: string; var List: TStringList); override;

    function GetFilePath(Filename: string): string; override;
    function PrepareSave: Boolean; override;
  end;

implementation

{ TSettingsInstalled }

class function TSettingsInstalled.Active(AppName: string): Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(GetRegPath(AppName), False) then
    begin
      Result := True;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

constructor TSettingsInstalled.Create(AppName, AppPath: string; CommandLine: TCommandLine);
var
  Param: TCommandLineRecord;
begin
  inherited;

  FRegPath := GetRegPath(AppName);

  Param := CommandLine.GetParam('-datadir');

  if (Param <> nil) and (Param.Values.Count = 1) and (DirectoryExists(Param.Values[0])) then
  begin
    FDataDirOverridden := True;
    FDataDir := IncludeTrailingBackslash(Param.Values[0])
  end else
    FDataDir := GetDataDir(AppName);
end;

function TSettingsInstalled.Delete(Name, Section: string): Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(FRegPath + Section, False) then
      Result := Reg.DeleteValue(Name);
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

function TSettingsInstalled.DeleteKey(Section: string): Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey(FRegPath, False) then
      Result := Reg.DeleteKey(Section);
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

function TSettingsInstalled.DeleteProfile: Boolean;
var
  Reg: TRegistry;
  Files: TStringList;
  RegPath: string;
  i, P: Integer;
begin
  inherited;
  Result := True;
  Reg := TRegistry.Create;
  try
    RegPath := Copy(FRegPath, 1, Length(FRegPath) - 1);
    P := RPos('\', RegPath);
    if P > -1 then
    begin
      RegPath := Copy(RegPath, 1, P);
      if not Reg.OpenKey(RegPath, False) then
        Result := False;
      Reg.DeleteKey(FAppName);
    end;

    Files := TStringList.Create;
    try
      FindFiles(FDataDir + FAppName + '_*', Files);
      for i := 0 to Files.Count - 1 do
      begin
        if not DeleteFile(FDataDir + Files[i]) then
          Result := False;
      end;
    finally
      Files.Free;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TSettingsInstalled.GetDataInternal(Path: string; Lst: TSettingsList; TruncLen: Integer);
var
  Sections: TStringList;
  Values: TStringList;
  Reg: TRegistry;
  SV: string;
  IV: Integer;
  BV: Boolean;
  i, n: Integer;
  DT: TDataType;
begin
  SV := '';
  IV := 0;
  BV := False;
  Reg := TRegistry.Create;
  Sections := TStringList.Create;
  Values := TStringList.Create;
  try
    if not Reg.OpenKey(Path, False) then
      raise Exception.Create('');

    Reg.GetKeyNames(Sections);
    for i := 0 to Sections.Count - 1 do
      GetDataInternal(Path + Sections[i] + '\', Lst, TruncLen);

    Reg.GetValueNames(Values);

    for n := 0 to Values.Count - 1 do
    begin
      DT := dtString;
      SV := Reg.GetDataAsString(Values[n], False);
      if SV <> '' then
      begin
        try
          IV := StrToInt(SV);
          DT := dtInteger;
        except

        end;

        if DT = dtUnknown then
        begin
          try
            BV := StrToBool(SV);
            DT := dtBoolean;
          except

          end;
        end;

        case DT of
          dtUnknown:
            raise Exception.Create('Unknown data type');
          dtString:
            Lst.Add(TDataString.Create(Values[n], Copy(Path, TruncLen + 1, Length(Path) - TruncLen - 1), SV));
          dtInteger:
            Lst.Add(TDataInteger.Create(Values[n], Copy(Path, TruncLen + 1, Length(Path) - TruncLen - 1), IV));
          dtBoolean:
            Lst.Add(TDataBoolean.Create(Values[n], Copy(Path, TruncLen + 1, Length(Path) - TruncLen - 1), BV));
        end;
      end;
    end;
  finally
    Reg.Free;
    Sections.Free;
    Values.Free;
  end;
end;

procedure TSettingsInstalled.GetData(Lst: TSettingsList);
begin
  Lst.Clear;
  GetDataInternal(FRegPath, Lst, Length(FRegPath));
end;

class function TSettingsInstalled.GetDataDir(AppName: string): string;
begin
  Result := GetShellFolder(CSIDL_APPDATA);
  if (Trim(Result) <> '') then
  begin
    Result := IncludeTrailingPathDelimiter(Result) + AppName + '\';
  end;
end;

class function TSettingsInstalled.GetRegPath(AppName: string): string;
begin
  Result := '\Software\mistake.ws\' + AppName + '\';
end;

procedure TSettingsInstalled.GetValues(Section: string;
  var List: TStringList);
var
  Reg: TRegistry;
begin
  List.Clear;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(FRegPath + Section, False) then
    begin
      try
        Reg.GetValueNames(List);
      except end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TSettingsInstalled.PrepareSave: Boolean;
begin
  Result := CreatePath;
end;

procedure TSettingsInstalled.Read(Name: string; var Value: Boolean;
  Default: Boolean; Section: string);
var
  Reg: TRegistry;
begin
  Value := Default;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(FRegPath + Section, False) then
    begin
      try
        Value := Reg.ReadBool(Name);
      except end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TSettingsInstalled.Read(Name: string; var Value: Cardinal;
  Default: Cardinal; Section: string);
var
  I: Integer;
begin
  Read(Name, I, Default, Section);
  Value := I;
end;

procedure TSettingsInstalled.Read(Name: string; var Value: Integer;
  Default: Integer; Section: string);
var
  Reg: TRegistry;
begin
  Value := Default;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(FRegPath + Section, False) then
    begin
      try
        Value := Reg.ReadInteger(Name);
      except end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TSettingsInstalled.Read(Name: string; var Value: string; Default,
  Section: string);
var
  Reg: TRegistry;
begin
  Value := Default;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(FRegPath + Section, False) then
    begin
      try
        if Reg.ValueExists(Name) then
          Value := Reg.ReadString(Name);
      except end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TSettingsInstalled.Write(Name, Value, Section: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(FRegPath + Section, True) then
    begin
      Reg.WriteString(Name, Value);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TSettingsInstalled.Write(Name: string; Value: Integer;
  Section: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(FRegPath + Section, True) then
    begin
      Reg.WriteInteger(Name, Value);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TSettingsInstalled.Write(Name: string; Value: Boolean;
  Section: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(FRegPath + Section, True) then
    begin
      Reg.WriteBool(Name, Value);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

{ TSettingsPortable }

class function TSettingsPortable.Active(AppName: string): Boolean;
begin
  Result := FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + LowerCase(AppName) + '_settings.ini');
end;

constructor TSettingsPortable.Create(AppName, AppPath: string; CommandLine: TCommandLine);
var
  Param: TCommandLineRecord;
begin
  inherited;

  Param := CommandLine.GetParam('-datadir');

  if (Param <> nil) and (Param.Values.Count = 1) and (DirectoryExists(Param.Values[0])) then
  begin
    FDataDirOverridden := True;
    FDataDir := IncludeTrailingBackslash(Param.Values[0])
  end else
    FDataDir := GetDataDir;

  FIniFile := FDataDir + LowerCase(AppName) + '_settings.ini';
end;

function TSettingsPortable.Delete(Name, Section: string): Boolean;
var
  Ini: TIniFile;
begin
  Result := False;
  Ini := TIniFile.Create(FIniFile);
  try
    try
      Ini.DeleteKey(Section, Name);
      Ini.UpdateFile;
      Result := True;
    except
    end;
  finally
    Ini.Free;
  end;
end;

function TSettingsPortable.DeleteKey(Section: string): Boolean;
var
  Ini: TIniFile;
begin
  Result := False;
  Ini := TIniFile.Create(FIniFile);
  try
    try
      Ini.EraseSection(Section);
      Ini.UpdateFile;
      Result := True;
    except
    end;
  finally
    Ini.Free;
  end;
end;

function TSettingsPortable.DeleteProfile: Boolean;
var
  Files: TStringList;
  i: Integer;
begin
  inherited;
  Result := True;

  Files := TStringList.Create;
  try
    FindFiles(FDataDir + FAppName + '_*', Files);
    for i := 0 to Files.Count - 1 do
    begin
      if not DeleteFile(FDataDir + Files[i]) then
        Result := False;
    end;
  finally
    Files.Free;
  end;
end;

procedure TSettingsPortable.GetData(Lst: TSettingsList);
var
  Sections: TStringList;
  Values: TStringList;
  Ini: TIniFile;
  SV: string;
  IV: Integer;
  BV: Boolean;
  i, n, P: Integer;
  DT: TDataType;
begin
  SV := '';
  IV := 0;
  BV := False;
  Lst.Clear;
  try
    Ini := TIniFile.Create(FIniFile);
  except
    Exit;
  end;

  Sections := TStringList.Create;
  Values := TStringList.Create;
  try
    Ini.ReadSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      Ini.ReadSectionValues(Sections[i], Values);

      for n := 0 to Values.Count - 1 do
      begin
        P := Pos('=', Values[n]);
        if P > 0 then
        begin
          Values[n] := Copy(Values[n], 1, P - 1);

          SV := Ini.ReadString(Sections[i], Values[n], '');
          DT := dtString;
          if SV <> '' then
          begin
            try
              IV := StrToInt(SV);
              DT := dtInteger;
            except

            end;

            if DT = dtUnknown then
            begin
              try
                BV := StrToBool(SV);
                DT := dtBoolean;
              except

              end;
            end;

            case DT of
              dtUnknown:
                raise Exception.Create('Unknown data type');
              dtString:
                Lst.Add(TDataString.Create(Values[n], Sections[i], SV));
              dtInteger:
                Lst.Add(TDataInteger.Create(Values[n], Sections[i], IV));
              dtBoolean:
                Lst.Add(TDataBoolean.Create(Values[n], Sections[i], BV));
            end;
          end;
        end;
      end;
    end;
  finally
    Ini.Free;
    Sections.Free;
    Values.Free;
  end;
end;

class function TSettingsPortable.GetDataDir: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

procedure TSettingsPortable.GetValues(Section: string;
  var List: TStringList);
var
  Ini: TIniFile;
begin
  List.Clear;
  try
    Ini := TIniFile.Create(FIniFile);
  except
    Exit;
  end;
  try
    try
      Ini.ReadSection(Section, List);
    except end;
  finally
    Ini.Free;
  end;
end;

function TSettingsPortable.PrepareSave: Boolean;
begin
  Result := CreatePath;
end;

procedure TSettingsPortable.Read(Name: string; var Value: Boolean;
  Default: Boolean; Section: string);
var
  Ini: TIniFile;
begin
  Value := Default;
  try
    Ini := TIniFile.Create(FIniFile);
  except
    Exit;
  end;
  try
    try
      Value := Ini.ReadBool(Section, Name, Default);
    except end;
  finally
    Ini.Free;
  end;
end;

procedure TSettingsPortable.Read(Name: string; var Value: Cardinal;
  Default: Cardinal; Section: string);
var
  I: Integer;
begin
  Read(Name, I, Default, Section);
  Value := I;
end;

procedure TSettingsPortable.Read(Name: string; var Value: Integer;
  Default: Integer; Section: string);
var
  Ini: TIniFile;
begin
  Value := Default;
  try
    Ini := TIniFile.Create(FIniFile);
  except
    Exit;
  end;
  try
    try
      Value := Ini.ReadInteger(Section, Name, Default);
    except end;
  finally
    Ini.Free;
  end;
end;

procedure TSettingsPortable.Read(Name: string; var Value: string; Default,
  Section: string);
var
  Ini: TIniFile;
begin
  Value := Default;
  try
    Ini := TIniFile.Create(FIniFile);
  except
    Exit;
  end;
  try
    try
      if Ini.ValueExists(Section, Name) then
        Value := Ini.ReadString(Section, Name, Default);
    except end;
  finally
    Ini.Free;
  end;
end;

procedure TSettingsPortable.Write(Name, Value, Section: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FIniFile);
  try
    Ini.WriteString(Section, Name, Value);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TSettingsPortable.Write(Name: string; Value: Integer;
  Section: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FIniFile);
  try
    Ini.WriteInteger(Section, Name, Value);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TSettingsPortable.Write(Name: string; Value: Boolean;
  Section: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FIniFile);
  try
    Ini.WriteBool(Section, Name, Value);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

{ TSettingsStorage }

procedure TSettingsStorage.Assign(AssignFrom: TSettingsStorage);
var
  Lst: TSettingsList;
  Files: TStringList;
  i: Integer;
begin
  Lst := TSettingsList.Create;
  try
    AssignFrom.GetData(Lst);
    Assign(Lst);

    if AssignFrom.FDataDir <> '' then
    begin
      Files := TStringList.Create;
      try
        FindFiles(AssignFrom.FDataDir + AssignFrom.FAppName + '_*', Files);
        for i := 0 to Files.Count - 1 do
        begin
          if AssignFrom is TSettingsPortable then
            if LowerCase(Files[i]) = LowerCase(ExtractFileName(TSettingsPortable(AssignFrom).FIniFile)) then
              Continue;

          if not CopyFile(PChar(AssignFrom.FDataDir + Files[i]), PChar(FDataDir + Files[i]), False) then
            raise Exception.Create('');
        end;
      finally
        Files.Free;
      end;
    end;
  finally
    Lst.Free;
  end;
end;

procedure TSettingsStorage.Assign(AssignFrom: TSettingsList);
var
  i: Integer;
begin
  for i := 0 to AssignFrom.Count - 1 do
  begin
    if AssignFrom[i] is TDataString then
    begin
      Write(TDataString(AssignFrom[i]).Name, TDataString(AssignFrom[i]).Value, TDataString(AssignFrom[i]).Section);
    end;
    if AssignFrom[i] is TDataInteger then
    begin
      Write(TDataInteger(AssignFrom[i]).Name, TDataInteger(AssignFrom[i]).Value, TDataInteger(AssignFrom[i]).Section);
    end;
    if AssignFrom[i] is TDataBoolean then
    begin
      Write(TDataBoolean(AssignFrom[i]).Name, TDataBoolean(AssignFrom[i]).Value, TDataBoolean(AssignFrom[i]).Section);
    end;
  end;
end;

constructor TSettingsStorage.Create(AppName, AppPath: string; CommandLine: TCommandLine);
begin
  inherited Create;
  FAppName := AppName;
  FAppPath := AppPath;
  FCommandLine := CommandLine;
end;

function TSettingsStorage.CreatePath: Boolean;
begin
  Result := ForceDirectories(FDataDir);
end;

function TSettingsStorage.DeleteProfile: Boolean;
begin
  Result := False;
end;

procedure TSettingsStorage.GetData(Lst: TSettingsList);
begin

end;

function TSettingsStorage.GetFilePath(Filename: string): string;
begin
  if FDataDir = '' then
  begin
    Result := '';
    Exit;
  end;

  Result := FDataDir + LowerCase(FAppName) + '_' + Filename
end;

{ TSettingsDummy }

constructor TSettingsDummy.Create(AppName, AppPath: string; CommandLine: TCommandLine);
begin
  inherited;
  FDataDir := '';
end;

function TSettingsDummy.Delete(Name, Section: string): Boolean;
begin
  Result := True;
end;

function TSettingsDummy.DeleteKey(Section: string): Boolean;
begin
  Result := True;
end;

function TSettingsDummy.GetFilePath(Filename: string): string;
begin
  Result := '';
end;

procedure TSettingsDummy.GetValues(Section: string; var List: TStringList);
begin

end;

function TSettingsDummy.PrepareSave: Boolean;
begin
  Result := True;
end;

procedure TSettingsDummy.Read(Name: string; var Value: Cardinal;
  Default: Cardinal; Section: string);
begin
  Value := Default;
end;

procedure TSettingsDummy.Read(Name: string; var Value: Integer;
  Default: Integer; Section: string);
begin
  Value := Default;
end;

procedure TSettingsDummy.Read(Name: string; var Value: string; Default,
  Section: string);
begin
  Value := Default;
end;

procedure TSettingsDummy.Read(Name: string; var Value: Boolean;
  Default: Boolean; Section: string);
begin
  Value := Default;
end;

procedure TSettingsDummy.Write(Name, Value, Section: string);
begin

end;

procedure TSettingsDummy.Write(Name: string; Value: Integer; Section: string);
begin

end;

procedure TSettingsDummy.Write(Name: string; Value: Boolean; Section: string);
begin

end;

{ TDataEntry }

class function TDataEntry.Load(Stream: TExtendedStream): TDataEntry;
var
  T: Byte;
  Name, Section: string;
begin
  Stream.Read(T);
  Stream.Read(Name);
  Stream.Read(Section);

  case T of
    0:
      begin
        Result := TDataString.Create(Name, Section, '');
        TDataString(Result).Load(Stream);
      end;
    1:
      begin
        Result := TDataInteger.Create(Name, Section, 0);
        TDataInteger(Result).Load(Stream);
      end;
    2:
      begin
        Result := TDataBoolean.Create(Name, Section, False);
        TDataBoolean(Result).Load(Stream);
      end;
    else
      raise Exception.Create('Unknown TDataEntry Type');
  end;
end;

procedure TDataEntry.Save(Stream: TExtendedStream);
begin
  if Self is TDataString then
    Stream.Write(Byte(0))
  else if Self is TDataInteger then
    Stream.Write(Byte(1))
  else if Self is TDataBoolean then
    Stream.Write(Byte(2));

  Stream.Write(FName);
  Stream.Write(FSection);
end;

{ TDataString }

constructor TDataString.Create(Name, Section: string; D: string);
begin
  Self.FName := Name;
  Self.FSection := Section;
  Self.FValue := D;
end;

procedure TDataString.Load(Stream: TExtendedStream);
begin
  Stream.Read(FValue);
end;

procedure TDataString.Save(Stream: TExtendedStream);
begin
  inherited;
  Stream.Write(FValue);
end;

{ TDataInteger }

constructor TDataInteger.Create(Name, Section: string; D: Integer);
begin
  Self.FName := Name;
  Self.FSection := Section;
  Self.FValue := D;
end;

procedure TDataInteger.Load(Stream: TExtendedStream);
begin
  Stream.Read(FValue);
end;

procedure TDataInteger.Save(Stream: TExtendedStream);
begin
  inherited;
  Stream.Write(FValue);
end;

{ TDataBoolean }

constructor TDataBoolean.Create(Name, Section: string; D: Boolean);
begin
  Self.FName := Name;
  Self.FSection := Section;
  Self.FValue := D;
end;

procedure TDataBoolean.Load(Stream: TExtendedStream);
begin
  Stream.Read(FValue);
end;

procedure TDataBoolean.Save(Stream: TExtendedStream);
begin
  inherited;
  Stream.Write(FValue);
end;

{ TSettingsList }

destructor TSettingsList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited;
end;

class function TSettingsList.Load(Stream: TExtendedStream): TSettingsList;
var
  i, C: Integer;
begin
  Result := TSettingsList.Create;
  Stream.Read(C);
  for i := 0 to C - 1 do
    Result.Add(TDataEntry.Load(Stream));
end;

procedure TSettingsList.Save(Stream: TExtendedStream);
var
  i: Integer;
begin
  Stream.Write(Count);
  for i := 0 to Count - 1 do
    Items[i].Save(Stream);
end;

end.
