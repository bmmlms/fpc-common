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
unit AppDataBase;

interface

uses
  Windows, Messages, SysUtils, Classes, Registry, SyncObjs, ShlObj, ActiveX,
  LanguageObjects, SettingsStorage, Functions, GUIFunctions;

type
  TAppVersion = record
    Major, Minor, Revision, Build: Byte;
    AsString: AnsiString;
  end;

  TPortable = (poYes, poNo, poUndefined);

  TAppDataBase = class(TObject)
  private
    FCS: TCriticalSection;
    FOnlyOne: Boolean;
    FWasSetup: Boolean;
    FAutoUpdate: Boolean;
    FLastUpdateChecked: Integer;
    FInstallUpdateOnStart: Boolean;
    FLanguage: string;
    FFirstStartShown: Boolean;

    FMainWidthDefault: Integer;
    FMainHeightDefault: Integer;

    FMainMaximized: Boolean;
    FMainLeft: Integer;
    FMainTop: Integer;
    FMainWidth: Integer;
    FMainHeight: Integer;

    FProxyEnabled: Boolean;
    FProxyHost: string;
    FProxyPort: Integer;

    FWindowHandle: Cardinal;
    FFileMapping: Cardinal;
    FMutexHandle: Cardinal;

    FAppPath: string;
    FAppName: string;
    FAppVersion: TAppVersion;
    FTempDir: string;
    FProjectLink: string;
    FProjectHelpLink: string;
    FPortableAllowed: Boolean;
    FPortable: TPortable;
    FRunningFromInstalledLocation: Boolean;

    procedure GetVersionInfo;
    procedure GetTempDir;
    procedure GetPortableAllowed;
    procedure GetPortable;
    procedure GetRunningFromInstalledLocation;

    procedure InitOnlyOne;
    function ReadHandle: Cardinal;
    procedure WriteHandle(Handle: Cardinal);
    procedure FSetWindowHandle(Value: Cardinal);
    procedure FSetPortable(Value: TPortable);

    procedure FSetInfoShown(Idx: Integer; Val: Boolean);
    function FGetInfoShown(Idx: Integer): Boolean;
  protected
    FStorage: TSettingsStorage;
    procedure DoSave; virtual;
  public
    constructor Create(AppName: String; OnlyOne: Boolean; DefWidth, DefHeight: Integer); reintroduce;
    destructor Destroy; override;
    procedure Load; virtual;
    procedure Save(Handle: Cardinal = 0);
    procedure Lock;
    procedure Unlock;

    property MainMaximized: Boolean read FMainMaximized write FMainMaximized;
    property MainLeft: Integer read FMainLeft write FMainLeft;
    property MainTop: Integer read FMainTop write FMainTop;
    property MainWidth: Integer read FMainWidth write FMainWidth;
    property MainHeight: Integer read FMainHeight write FMainHeight;

    property ProxyEnabled: Boolean read FProxyEnabled write FProxyEnabled;
    property ProxyHost: string read FProxyHost write FProxyHost;
    property ProxyPort: Integer read FProxyPort write FProxyPort;

    property AppPath: string read FAppPath;
    property AppName: string read FAppName;
    property AppVersion: TAppVersion read FAppVersion;
    property TempDir: string read FTempDir;
    property ProjectLink: string read FProjectLink;
    property ProjectHelpLink: string read FProjectHelpLink;
    property PortableAllowed: Boolean read FPortableAllowed;
    property Portable: TPortable read FPortable write FSetPortable;
    property RunningFromInstalledLocation: Boolean read FRunningFromInstalledLocation;

    property WasSetup: Boolean read FWasSetup write FWasSetup;
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate;
    property LastUpdateChecked: Integer read FLastUpdateChecked write FLastUpdateChecked;
    property InstallUpdateOnStart: Boolean read FInstallUpdateOnStart write FInstallUpdateOnStart;
    property Language: string read FLanguage write FLanguage;
    property FirstStartShown: Boolean read FFirstStartShown write FFirstStartShown;
    property WindowHandle: Cardinal read FWindowHandle write FSetWindowHandle;
    property InfoShown[Idx: Integer]: Boolean read FGetInfoShown write FSetInfoShown;

    property Storage: TSettingsStorage read FStorage;
  end;

implementation

constructor TAppDataBase.Create(AppName: string; OnlyOne: Boolean; DefWidth, DefHeight: Integer);
begin
  FMainWidthDefault := DefWidth;
  FMainHeightDefault := DefHeight;

  FCS := TCriticalSection.Create;
  FAppPath := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)));
  FAppName := AppName;
  FOnlyOne := OnlyOne;
  FStorage := nil;

  FProjectLink := 'http://mistake.ws/projekte/' + LowerCase(FAppName) + '/';
  FProjectHelpLink := 'http://mistake.ws/projekte/' + LowerCase(FAppName) + '/help/';

  InitOnlyOne;

  FWindowHandle := 0;

  GetVersionInfo;
  GetTempDir;
  GetPortableAllowed;
  GetRunningFromInstalledLocation;

  GetPortable;
end;

destructor TAppDataBase.Destroy;
begin
  FCS.Free;
  if FFileMapping > 0 then
    CloseHandle(FFileMapping);
  if FMutexHandle > 0 then
    CloseHandle(FMutexHandle);
  if FStorage <> nil then
    FStorage.Free;
  inherited Destroy;
end;

procedure TAppDataBase.FSetWindowHandle(Value: Cardinal);
begin
  FWindowHandle := Value;
  WriteHandle(Value);
end;

procedure TAppDataBase.FSetInfoShown(Idx: Integer; Val: Boolean);
begin
  if Val then
    FStorage.Write('InfoShown' + IntToStr(Idx), Val, 'Infos')
  else
    try
      FStorage.Delete('InfoShown' + IntToStr(Idx), 'Infos');
    except end;
end;

procedure TAppDataBase.GetPortable;
var
  EI, EP: Boolean;
begin
  EI := TSettingsInstalled.Active(FAppName);
  EP := TSettingsPortable.Active(FAppName);

  FPortable := poUndefined;

  try
    if EP and PortableAllowed then
    begin
      FSetPortable(poYes);
    end else if EI then
    begin
      FSetPortable(poNo);
    end;
  except

  end;

  if FPortable = poUndefined then
  begin
    FSetPortable(poUndefined);
  end;
end;

procedure TAppDataBase.FSetPortable(Value: TPortable);
begin
  FPortable := Value;
  case Value of
    poYes:
      begin
        if FStorage <> nil then
          FStorage.Free;
        FStorage := TSettingsPortable.Create(FAppName, FAppPath);
        Load;
      end;
    poNo:
      begin
        if FStorage <> nil then
          FStorage.Free;
        FStorage := TSettingsInstalled.Create(FAppName, FAppPath);
        Load;
      end;
    poUndefined:
      begin
        if FStorage <> nil then
          FStorage.Free;
        FStorage := TSettingsDummy.Create(FAppName, FAppPath);
        Load;
        FWasSetup := False;
      end;
  end;
end;

function TAppDataBase.FGetInfoShown(Idx: Integer): Boolean;
begin
  FStorage.Read('InfoShown' + IntToStr(Idx), Result, False, 'Infos');
end;

procedure TAppDataBase.Load;
begin
  FStorage.Read('MainMaximized', FMainMaximized, False);
  FStorage.Read('MainWidth', FMainWidth, FMainWidthDefault);
  FStorage.Read('MainHeight', FMainHeight, FMainHeightDefault);
  FStorage.Read('MainLeft', FMainLeft, -1);
  FStorage.Read('MainTop', FMainTop, -1);

  FStorage.Read('ProxyEnabled', FProxyEnabled, False);
  FStorage.Read('ProxyHost', FProxyHost, '');
  FStorage.Read('ProxyPort', FProxyPort, 8080);

  FStorage.Read('WasSetup', FWasSetup, False);

  FStorage.Read('AutoUpdate', FAutoUpdate, True);
  FStorage.Read('LastUpdateChecked', FLastUpdateChecked, 0);
  FStorage.Read('InstallUpdateOnStart', FInstallUpdateOnStart, False);
  FStorage.Read('Language', FLanguage, '');
  FStorage.Read('FirstStartShown', FFirstStartShown, False);
end;

procedure TAppDataBase.DoSave;
begin
  FStorage.PrepareSave;

  FStorage.Write('MainMaximized', FMainMaximized);
  FStorage.Write('MainWidth', FMainWidth);
  FStorage.Write('MainHeight', FMainHeight);
  FStorage.Write('MainLeft', FMainLeft);
  FStorage.Write('MainTop', FMainTop);

  FStorage.Write('ProxyEnabled', FProxyEnabled);
  FStorage.Write('ProxyHost', FProxyHost);
  FStorage.Write('ProxyPort', FProxyPort);

  FStorage.Write('WasSetup', FWasSetup);
  FStorage.Write('AutoUpdate', FAutoUpdate);
  FStorage.Write('LastUpdateChecked', FLastUpdateChecked);
  FStorage.Write('InstallUpdateOnStart', FInstallUpdateOnStart);
  FStorage.Write('Language', FLanguage);
  FStorage.Write('FirstStartShown', FFirstStartShown);
end;

procedure TAppDataBase.Lock;
begin
  FCS.Enter;
end;

procedure TAppDataBase.Unlock;
begin
  FCS.Leave;
end;

procedure TAppDataBase.InitOnlyOne;
var
  Handle: Cardinal;
begin
  FFileMapping := 0;
  if FOnlyOne then
  begin
    FMutexHandle := CreateMutex(nil, True, PChar(FAppName + 'Mutex'));
    if GetLastError = ERROR_ALREADY_EXISTS then
    begin
      Handle := ReadHandle;
      PostMessage(Handle, WM_USER + 123, 0, 0);
      Halt;
    end else
    begin
      WriteHandle(0);
    end;
  end;
end;

function TAppDataBase.ReadHandle: Cardinal;
var
  hFileMapping: THandle;
  SA: TSecurityAttributes;
  pSD: TSecurityDescriptor;
  Mem: PCardinal;
begin
  if not InitializeSecurityDescriptor(@pSD, SECURITY_DESCRIPTOR_REVISION) then
    Exit;
  if not SetSecurityDescriptorDacl(@pSD, true, nil, false) then
    Exit;
  SA.nLength := SizeOf(SA);
  SA.lpSecurityDescriptor := @pSD;
  SA.bInheritHandle := True;
  hFileMapping := CreateFileMapping(INVALID_HANDLE_VALUE, @SA,
    PAGE_READONLY, 0, SizeOf(Result), PChar(FAppName + 'WndHandle'));
  if hFileMapping <> 0 then
  begin
    Mem := MapViewOfFile(hFileMapping, FILE_MAP_READ, 0, 0, SizeOf(Result));
    if Assigned(Mem) then
    begin
      Move(Mem^, Result, SizeOf(Result));
      UnmapViewOfFile(Mem);
    end;
    CloseHandle(hFileMapping);
  end;
end;

procedure TAppDataBase.Save(Handle: Cardinal);
var
  Res: Boolean;
  Res2: Integer;
begin
  Res := False;
  while not Res do
    try
      DoSave;
      Res := True;
    except
      if Handle = 0 then
        raise
      else
      begin
        Res2 := MsgBox(Handle, _('An error occured while saving application settings. Please make sure you can write to ' +
                                 'the registry if the application was installed or to the application path if it is used ' +
                                 'in portable mode. Click "Yes" to try again, "No" to exit without saving settings.'),
                                 _('Info'), MB_ICONEXCLAMATION or MB_YESNO or MB_DEFBUTTON1);
        if Res2 = IDNO then
          Res := True
      end;
    end;
end;

procedure TAppDataBase.WriteHandle(Handle: Cardinal);
var
  SA: TSecurityAttributes;
  pSD: TSecurityDescriptor;
  Mem: PCardinal;
begin
  if FMutexHandle = 0 then
    Exit;
  if not InitializeSecurityDescriptor(@pSD, SECURITY_DESCRIPTOR_REVISION) then
    Exit;
  if not SetSecurityDescriptorDacl(@pSD, true, nil, false) then
    Exit;
  SA.nLength := SizeOf(SA);
  SA.lpSecurityDescriptor := @pSD;
  SA.bInheritHandle := True;
  FFileMapping := CreateFileMapping(INVALID_HANDLE_VALUE, @SA,
    PAGE_READWRITE, 0, SizeOf(Handle), PChar(FAppName + 'WndHandle'));
  if FFileMapping <> 0 then
  begin
    Mem := MapViewOfFile(FFileMapping, FILE_MAP_WRITE, 0, 0, SizeOf(Handle));
    if Assigned(Mem) then
    begin
      Move(Handle, Mem^, SizeOf(Handle));
      UnmapViewOfFile(Mem);
    end;
  end;
end;

procedure TAppDataBase.GetPortableAllowed;
  function GetRandomFile(Dir: string): string;
  var
    E: Boolean;
  begin
    E := True;
    while E do
    begin
      Result := IncludeTrailingBackslash(Dir) + IntToStr(Random(100000));
      E := FileExists(Result);
    end;
  end;
var
  Dir, Filename: string;
  S: TMemoryStream;
  Old: LongBool;
  Ret: Boolean;
  //RmDirectory: Boolean;
begin
  //RmDirectory := False;
  FPortableAllowed := True;

  if FRunningFromInstalledLocation then
  begin
    FPortableAllowed := False;
    Exit;
  end;

  S := TMemoryStream.Create;
  try
    try
      Dir := IncludeTrailingBackslash(ExtractFilePath(AppPath));
      {
      if not DirectoryExists(Dir) then
      begin
        if ForceDirectories(Dir) then
          RmDirectory := True;
      end;
      }
      Filename := GetRandomFile(Dir);
      S.SaveToFile(Filename);
      DeleteFile(Filename);
    except
      FPortableAllowed := False;
    end;
  finally
    //if RmDirectory then
    //  RmDir(Dir);
    S.Free;
  end;
end;

procedure TAppDataBase.GetRunningFromInstalledLocation;
const
  UninstallPath = 'Software\Microsoft\Windows\CurrentVersion\Uninstall';
var
  i: Integer;
  Reg: TRegistry;
  TmpKeyNames: TStringList;
begin
  FRunningFromInstalledLocation := False;
  TmpKeyNames := TStringList.Create;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.KeyExists(UninstallPath) then
      if Reg.OpenKeyReadOnly(UninstallPath) then
        Reg.GetKeyNames(TmpKeyNames);
    Reg.CloseKey;
    for i := 0 to TmpKeyNames.Count - 1 do
      if Reg.OpenKeyReadOnly(UninstallPath + '\' + TmpKeyNames.Strings[i]) then
      begin
        if Reg.ValueExists('DisplayName') then
          if LowerCase(Reg.ReadString('DisplayName')) = LowerCase(AppName) then
          begin
            if LowerCase(IncludeTrailingPathDelimiter(Reg.ReadString('InstallLocation'))) =
               LowerCase(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))) then
              FRunningFromInstalledLocation := True;
            Exit;
          end;
        Reg.CloseKey;
      end;
  finally
    Reg.Free;
    TmpKeyNames.Free;
  end;
end;

procedure TAppDataBase.GetTempDir;
begin
  FTempDir := Functions.GetTempDir;
  if FTempDir <> '' then
  begin
    if ForceDirectories(FTempDir + FAppName) then
      FTempDir := FTempDir + FAppName + '\';
  end else
    raise Exception.Create(_('The folder for temporary files could not be determined.'#13#10 +
                             'Please ask for support at http://mistake.ws/forum/.'#13#10 +
                             'The application will be terminated.'));
end;

procedure TAppDataBase.GetVersionInfo;
var
  VerInfoSize: Integer;
  VerValueSize: DWord;
  Dummy: DWord;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  FAppVersion.Major := 0;
  FAppVersion.Minor := 0;
  FAppVersion.Revision := 0;
  FAppVersion.Build := 0;
  FAppVersion.AsString := '';
  if VerInfoSize <> 0 then
  begin
    GetMem(VerInfo, VerInfoSize);
    try
      if GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo) then
      begin
        if VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then
        begin
          with VerValue^ do
          begin
            FAppVersion.Major := dwFileVersionMS shr 16;
            FAppVersion.Minor := dwFileVersionMS and $FFFF;
            FAppVersion.Revision := dwFileVersionLS shr 16;
            FAppVersion.Build := dwFileVersionLS and $FFFF;
          end;
          FAppVersion.AsString := AnsiString(Format('%d.%d.%d.%d', [FAppVersion.Major,
            FAppVersion.Minor, FAppVersion.Revision, FAppVersion.Build]));
        end;
      end;
    finally
      FreeMem(VerInfo,VerInfoSize);
    end;
  end;
  if (FAppVersion.Major = 0) and (FAppVersion.Minor = 0) and
     (FAppVersion.Revision = 0) and (FAppVersion.Build = 0) then
  begin
    raise Exception.Create(_('The version of the application could not be determined.'#13#10 +
                             'Please ask for support at http://mistake.ws/forum/.'#13#10 +
                             'The application will be terminated.'));
  end;
end;

end.
