{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2011 Alexander Nottelmann

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
  Functions;

const
  SETTINGS = 'Settings';

type
  TDataType = (dtUnknown, dtString, dtInteger, dtBoolean);

  TDataString = class
  public
    Name, Section: string;
    D: string;
    constructor Create(Name, Section: string; D: string);
  end;

  TDataInteger = class
  public
    Name, Section: string;
    D: Integer;
    constructor Create(Name, Section: string; D: Integer);
  end;

  TDataBoolean = class
  public
    Name, Section: string;
    D: Boolean;
    constructor Create(Name, Section: string; D: Boolean);
  end;
  
  TSettingsStorage = class
  private
  protected
    FAppName: string;
    FAppPath: string;

    FDataDir: string;
  public
    constructor Create(AppName, AppPath: string); virtual;

    procedure Assign(AssignFrom: TSettingsStorage);
    procedure GetData(Lst: TList); virtual;
    function DeleteProfile: Boolean; virtual;
    
    procedure Write(Name: string; Value: string; Section: string = SETTINGS); overload; virtual; abstract;
    procedure Write(Name: string; Value: Integer; Section: string = SETTINGS); overload; virtual; abstract;
    procedure Write(Name: string; Value: Boolean; Section: string = SETTINGS); overload; virtual; abstract;
    procedure Read(Name: string; var Value: string; Default: string; Section: string = SETTINGS); overload; virtual; abstract;
    procedure Read(Name: string; var Value: Integer; Default: Integer; Section: string = SETTINGS); overload; virtual; abstract;
    procedure Read(Name: string; var Value: Cardinal; Default: Cardinal; Section: string = SETTINGS); overload; virtual; abstract;
    procedure Read(Name: string; var Value: Boolean; Default: Boolean; Section: string = SETTINGS); overload; virtual; abstract;
    function Delete(Name: string; Section: string = SETTINGS): Boolean; virtual; abstract;
    function DeleteKey(Section: string): Boolean; virtual; abstract;
    procedure GetValues(Section: string; var List: TStringList); virtual; abstract;

    function CreatePath: Boolean;
    function GetFilePath(Filename: string): string; virtual;
    function PrepareSave: Boolean; virtual; abstract;
  end;

  TSettingsInstalled = class(TSettingsStorage)
  private
    FRegPath: string;
    procedure GetDataInternal(Path: string; Lst: TList; TruncLen: Integer);
  public
    constructor Create(AppName, AppPath: string); override;

    class function Active(AppName: string): Boolean;
    class function GetRegPath(AppName: string): string;
    class function GetDataDir(AppName: string): string;

    procedure GetData(Lst: TList); overload; override;
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
    constructor Create(AppName, AppPath: string); override;

    class function Active(AppName: string): Boolean;
    class function GetDataDir: string;

    procedure GetData(Lst: TList); override;
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
    constructor Create(AppName, AppPath: string); override;

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

constructor TSettingsInstalled.Create(AppName, AppPath: string);
begin
  inherited;

  FRegPath := GetRegPath(AppName);
  FDataDir := GetDataDir(AppName);

  {
  Lst := TStringList.Create;
  try
    // ISSUE: Irgendwann entfernen. Wenn Files von alter Version sind, wird diesen
    // ein AppName+_+Filename angehängt, um mit neuen Versionen kompatibel zu sein.
    if FDataDir <> '' then
    begin
      FindFiles(FDataDir + '*', Lst);
      for i := 0 to Lst.Count - 1 do
      begin
        if LowerCase(Copy(Lst[i], 1, Length(AppName) + 1)) <> LowerCase(AppName + '_') then
          CopyFile(PChar(FDataDir + Lst[i]), PChar(FDataDir + LowerCase(AppName) + '_' + Lst[i]), True);
      end;
    end;
  finally
    Lst.Free;
  end;
  }
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

procedure TSettingsInstalled.GetDataInternal(Path: string; Lst: TList; TruncLen: Integer);
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

procedure TSettingsInstalled.GetData(Lst: TList);
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

constructor TSettingsPortable.Create(AppName, AppPath: string);
begin
  inherited;

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

procedure TSettingsPortable.GetData(Lst: TList);
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
  Lst: TList;
  Files: TStringList;
  i: Integer;
begin
  Lst := TList.Create;
  try
    AssignFrom.GetData(Lst);
    for i := 0 to Lst.Count - 1 do
    begin
      if TObject(Lst[i]) is TDataString then
      begin
        Write(TDataString(Lst[i]).Name, TDataString(Lst[i]).D, TDataString(Lst[i]).Section);
      end;
      if TObject(Lst[i]) is TDataInteger then
      begin
        Write(TDataInteger(Lst[i]).Name, TDataInteger(Lst[i]).D, TDataInteger(Lst[i]).Section);
      end;
      if TObject(Lst[i]) is TDataBoolean then
      begin
        Write(TDataBoolean(Lst[i]).Name, TDataBoolean(Lst[i]).D, TDataBoolean(Lst[i]).Section);
      end;
    end;

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
    for i := 0 to Lst.Count - 1 do
      TObject(Lst[i]).Free;
    Lst.Free;
  end;
end;

constructor TSettingsStorage.Create(AppName, AppPath: string);
begin
  inherited Create;
  FAppName := AppName;
  FAppPath := AppPath;
end;

function TSettingsStorage.CreatePath: Boolean;
begin
  Result := ForceDirectories(FDataDir);
end;

function TSettingsStorage.DeleteProfile: Boolean;
begin
  Result := False;
end;

procedure TSettingsStorage.GetData(Lst: TList);
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

constructor TSettingsDummy.Create(AppName, AppPath: string);
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

{ TDataString }

constructor TDataString.Create(Name, Section: string; D: string);
begin
  Self.Name := Name;
  Self.Section := Section;
  Self.D := D;
end;

{ TDataInteger }

constructor TDataInteger.Create(Name, Section: string; D: Integer);
begin
  Self.Name := Name;
  Self.Section := Section;
  Self.D := D;
end;

{ TDataBoolean }

constructor TDataBoolean.Create(Name, Section: string; D: Boolean);
begin
  Self.Name := Name;
  Self.Section := Section;
  Self.D := D;
end;

end.
