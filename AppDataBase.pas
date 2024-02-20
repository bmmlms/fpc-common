{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2024 Alexander Nottelmann

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
  Classes,
  CommandLine,
  Functions,
  LanguageObjects,
  Registry,
  SettingsStorage,
  SyncObjs,
  SysUtils,
  Types,
  Windows;

type
  TArrayElement = string;
  TArray = array of TArrayElement;

  TLicense = (alGPL, alProprietary);

  TPortable = (poYes, poNo, poUndefined);

  EAlreadyRunning = class(Exception);

  { TAppDataBase }

  TAppDataBase = class(TObject)
  private
    FCS: SyncObjs.TCriticalSection;
    FOnlyOne: Boolean;
    FWasSetup: Boolean;
    FAutoUpdate: Boolean;
    FLastUpdateChecked: Integer;
    FInstallUpdateOnStart: Boolean;
    FLanguage: string;
    FFirstStartShown: Boolean;
    FLastUsedVersion: TAppVersion;
    FSuppressUpdatedInfo: Boolean;

    FMainMaximized: Boolean;
    FMainLeft: Integer;
    FMainTop: Integer;
    FMainWidth: Integer;
    FMainHeight: Integer;

    FCheckCertificate: Boolean;
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
    FPortableAllowed: Boolean;
    FPortable: TPortable;
    FRunningFromInstalledLocation: Boolean;
    FCommandLine: TCommandLine;
    FWebLanguages: TStringList;

    FLicense: TLicense;
    FArchitecture: string;

    FSkipSave: Boolean;

    procedure GetVersionInfo;
    procedure GetTempDir;
    procedure GetPortableAllowed;
    procedure GetPortable;
    procedure GetRunningFromInstalledLocation;

    function ReadHandle: Cardinal;
    procedure WriteHandle(Handle: Cardinal);
    procedure FSetWindowHandle(Value: Cardinal);
    procedure FSetPortable(Value: TPortable);

    procedure FSetInfoShown(Idx: Integer; Val: Boolean);
    function FGetInfoShown(Idx: Integer): Boolean;
  protected
    FStorage: TSettingsStorage;

    FProjectUpdateLinks: TStringDynArray;
    FProjectHomepageLink: string;
    FProjectLink: string;
    FProjectHelpLink: string;
    FProjectForumLink: string;
    FProjectDonateLink: string;
    FProjectThanksText: string;

    FGitSHA: string;
    FCodename: string;

    procedure InitOnlyOne; virtual;
    procedure DoSave; virtual;
    procedure NotifyRunningInstance(Handle: Cardinal); virtual;
  public
    constructor Create(AppName: String; OnlyOne: Boolean; License: TLicense);
    destructor Destroy; override;
    procedure Load; virtual;
    procedure Save(Handle: Cardinal = 0);
    procedure BuildThanksText; virtual;
    procedure Lock;
    procedure Unlock;

    property MutexHandle: Cardinal read FMutexHandle;

    property MainMaximized: Boolean read FMainMaximized write FMainMaximized;
    property MainLeft: Integer read FMainLeft write FMainLeft;
    property MainTop: Integer read FMainTop write FMainTop;
    property MainWidth: Integer read FMainWidth write FMainWidth;
    property MainHeight: Integer read FMainHeight write FMainHeight;

    property CheckCertificate: Boolean read FCheckCertificate write FCheckCertificate;
    property ProxyEnabled: Boolean read FProxyEnabled write FProxyEnabled;
    property ProxyHost: string read FProxyHost write FProxyHost;
    property ProxyPort: Integer read FProxyPort write FProxyPort;

    property AppPath: string read FAppPath;
    property AppName: string read FAppName;
    property AppVersion: TAppVersion read FAppVersion;
    property GitSHA: string read FGitSHA;
    property Codename: string read FCodename;
    property TempDir: string read FTempDir;
    property ProjectUpdateLinks: TStringDynArray read FProjectUpdateLinks;
    property ProjectHomepageLink: string read FProjectHomepageLink;
    property ProjectLink: string read FProjectLink;
    property ProjectHelpLink: string read FProjectHelpLink;
    property ProjectForumLink: string read FProjectForumLink;
    property ProjectDonateLink: string read FProjectDonateLink;
    property ProjectThanksText: string read FProjectThanksText;
    property PortableAllowed: Boolean read FPortableAllowed;
    property Portable: TPortable read FPortable write FSetPortable;
    property RunningFromInstalledLocation: Boolean read FRunningFromInstalledLocation;
    property CommandLine: TCommandLine read FCommandLine;
    property WebLanguages: TStringList read FWebLanguages;

    property WasSetup: Boolean read FWasSetup write FWasSetup;
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate;
    property LastUpdateChecked: Integer read FLastUpdateChecked write FLastUpdateChecked;
    property InstallUpdateOnStart: Boolean read FInstallUpdateOnStart write FInstallUpdateOnStart;
    property Language: string read FLanguage write FLanguage;
    property FirstStartShown: Boolean read FFirstStartShown write FFirstStartShown;
    property LastUsedVersion: TAppVersion read FLastUsedVersion;
    property SuppressUpdatedInfo: Boolean read FSuppressUpdatedInfo write FSuppressUpdatedInfo;
    property WindowHandle: Cardinal read FWindowHandle write FSetWindowHandle;
    property InfoShown[Idx: Integer]: Boolean read FGetInfoShown write FSetInfoShown;

    property Storage: TSettingsStorage read FStorage;

    property SkipSave: Boolean read FSkipSave write FSkipSave;

    property License: TLicense read FLicense write FLicense;
    property Architecture: string read FArchitecture;
  end;

implementation

{ TAppDataBase }

constructor TAppDataBase.Create(AppName: string; OnlyOne: Boolean; License: TLicense);
begin
  inherited Create;

  FSkipSave := False;

  FLicense := License;
  {$IF defined(CPU64)}
  FArchitecture := 'x86_64';
  {$ELSEIF defined(CPU32)}
  FArchitecture := 'i386';
  {$ELSE}
  Unknown Architecture
  {$ENDIF}

  FCS := SyncObjs.TCriticalSection.Create;
  FAppPath := ExtractFilePath(ParamStr(0));
  FAppName := AppName;
  FOnlyOne := OnlyOne;
  FStorage := nil;

  if Length(FProjectUpdateLinks) = 0 then
  begin
    SetLength(FProjectUpdateLinks, 1);
    {$IFDEF DEBUG}
    FProjectUpdateLinks[0] := 'http://mistake.gaia';
    {$ELSE}
    FProjectUpdateLinks[0] := 'http://mistake.ws';
    {$ENDIF}
  end;

  if FProjectHomepageLink = '' then
    FProjectHomepageLink := 'http://mistake.ws';
  if FProjectLink = '' then
    FProjectLink := 'http://mistake.ws/projekte/' + LowerCase(FAppName);
  if FProjectHelpLink = '' then
    FProjectHelpLink := 'http://mistake.ws/projekte/' + LowerCase(FAppName) + '/help';
  if FProjectForumLink = '' then
    FProjectForumLink := 'http://mistake.ws/forum';
  if FProjectDonateLink = '' then
    FProjectDonateLink := '';
  if FProjectThanksText = '' then
    FProjectThanksText := '';

  FCommandLine := TCommandLine.Create(GetCommandLine);

  FWebLanguages := TStringList.Create;
  FWebLanguages.Add('de');
  FWebLanguages.Add('en');

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

  // Don't free it, let windows clean it up when we really exited
  //if FMutexHandle > 0 then
  //  ReleaseMutex(FMutexHandle);

  if FStorage <> nil then
    FStorage.Free;
  FCommandLine.Free;
  RemoveDir(FTempDir);
  FWebLanguages.Free;
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
    except
    end;
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
      FSetPortable(poYes)
    else if EI then
      FSetPortable(poNo);
  except

  end;

  if FPortable = poUndefined then
    FSetPortable(poUndefined);
end;

procedure TAppDataBase.FSetPortable(Value: TPortable);
begin
  FPortable := Value;
  case Value of
    poYes:
    begin
      if FStorage <> nil then
        FStorage.Free;
      FStorage := TSettingsPortable.Create(FAppName, FAppPath, FCommandLine);
      Load;
    end;
    poNo:
    begin
      if FStorage <> nil then
        FStorage.Free;
      FStorage := TSettingsInstalled.Create(FAppName, FAppPath, FCommandLine);
      Load;
    end;
    poUndefined:
    begin
      if FStorage <> nil then
        FStorage.Free;
      FStorage := TSettingsDummy.Create(FAppName, FAppPath, FCommandLine);
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
var
  LastUsedVersion: string;
begin
  FStorage.Read('MainMaximized', FMainMaximized, False);
  FStorage.Read('MainWidth', FMainWidth, -1);
  FStorage.Read('MainHeight', FMainHeight, -1);
  FStorage.Read('MainLeft', FMainLeft, -1);
  FStorage.Read('MainTop', FMainTop, -1);

  FStorage.Read('CheckCertificate', FCheckCertificate, True);
  FStorage.Read('ProxyEnabled', FProxyEnabled, False);
  FStorage.Read('ProxyHost', FProxyHost, '');
  FStorage.Read('ProxyPort', FProxyPort, 8080);

  FStorage.Read('WasSetup', FWasSetup, False);

  FStorage.Read('AutoUpdate', FAutoUpdate, True);
  FStorage.Read('LastUpdateChecked', FLastUpdateChecked, 0);
  FStorage.Read('InstallUpdateOnStart', FInstallUpdateOnStart, False);
  FStorage.Read('Language', FLanguage, '');

  FStorage.Read('FirstStartShown', FFirstStartShown, False);

  FStorage.Read('LastUsedVersion', LastUsedVersion, AppVersion.AsString);
  FStorage.Read('SuppressUpdatedInfo', FSuppressUpdatedInfo, False);
  try
    FLastUsedVersion := TFunctions.ParseVersion(LastUsedVersion);
  except
    FLastUsedVersion := AppVersion;
  end;
end;

procedure TAppDataBase.DoSave;
begin
  FStorage.PrepareSave;

  FStorage.Write('MainMaximized', FMainMaximized);
  FStorage.Write('MainWidth', FMainWidth);
  FStorage.Write('MainHeight', FMainHeight);
  FStorage.Write('MainLeft', FMainLeft);
  FStorage.Write('MainTop', FMainTop);

  FStorage.Write('CheckCertificate', FCheckCertificate);
  FStorage.Write('ProxyEnabled', FProxyEnabled);
  FStorage.Write('ProxyHost', FProxyHost);
  FStorage.Write('ProxyPort', FProxyPort);

  FStorage.Write('WasSetup', FWasSetup);
  FStorage.Write('AutoUpdate', FAutoUpdate);
  FStorage.Write('LastUpdateChecked', FLastUpdateChecked);
  FStorage.Write('InstallUpdateOnStart', FInstallUpdateOnStart);
  FStorage.Write('Language', FLanguage);
  FStorage.Write('FirstStartShown', FFirstStartShown);
  FStorage.Write('LastUsedVersion', AppVersion.AsString);
  FStorage.Write('SuppressUpdatedInfo', FSuppressUpdatedInfo);
end;

procedure TAppDataBase.Lock;
begin
  FCS.Enter;
end;

procedure TAppDataBase.NotifyRunningInstance(Handle: Cardinal);
begin
  PostMessage(Handle, WM_USER + 1234, 0, 0);
end;

procedure TAppDataBase.Unlock;
begin
  FCS.Leave;
end;

procedure TAppDataBase.InitOnlyOne;
var
  Handle: Cardinal;
  MutexName: string;
begin
  FFileMapping := 0;
  if FOnlyOne then
  begin
    MutexName := FAppName + 'Mutex';
    FMutexHandle := CreateMutex(nil, True, PChar(MutexName));
    while (GetLastError = ERROR_ALREADY_EXISTS) and (ReadHandle > 0) and (ParamStr(1) = '/profileupdate') do
    begin
      Sleep(500);
      FMutexHandle := CreateMutex(nil, True, PChar(MutexName));
    end;

    if GetLastError = ERROR_ALREADY_EXISTS then
    begin
      Handle := ReadHandle;

      if Handle = 0 then
        TFunctions.MsgBox(Format(_('You have tried to start %s but a previous instance is closing at the moment. Please try again in some seconds.'), [AppName]), _('Info'), MB_ICONINFORMATION)
      else
      begin
        NotifyRunningInstance(Handle);
      end;

      raise EAlreadyRunning.Create('');
    end else
      WriteHandle(0);
  end;
end;

function TAppDataBase.ReadHandle: Cardinal;
var
  hFileMapping: THandle;
  SA: TSecurityAttributes;
  pSD: TSecurityDescriptor;
  Mem: PCardinal;
  MappingName: string;
begin
  Result := 0;
  if not InitializeSecurityDescriptor(@pSD, SECURITY_DESCRIPTOR_REVISION) then
    Exit;
  if not SetSecurityDescriptorDacl(@pSD, True, nil, False) then
    Exit;
  SA.nLength := SizeOf(SA);
  SA.lpSecurityDescriptor := @pSD;
  SA.bInheritHandle := True;
  MappingName := FAppName + 'WndHandle';
  hFileMapping := CreateFileMapping(INVALID_HANDLE_VALUE, @SA, PAGE_READONLY, 0, SizeOf(Result), PChar(MappingName));
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
  if FSkipSave then
    Exit;
  while not Res do
    try
      DoSave;
      Res := True;
    except
      if Handle = 0 then
        raise
      else
      begin
        Res2 := TFunctions.MsgBox(_('An error occured while saving application settings. Please make sure you can write to ' + 'the registry if the application was installed or to the application path if it is used ' +
          'in portable mode. Click "Yes" to try again, "No" to exit without saving settings.'), _('Info'), MB_ICONEXCLAMATION or MB_YESNO or MB_DEFBUTTON1);
        if Res2 = IDNO then
          Res := True;
      end;
    end;
end;

procedure TAppDataBase.BuildThanksText;
begin
  FProjectThanksText := '';
end;

procedure TAppDataBase.WriteHandle(Handle: Cardinal);
var
  SA: TSecurityAttributes;
  pSD: TSecurityDescriptor;
  Mem: PCardinal;
  MappingName: string;
begin
  if FMutexHandle = 0 then
    Exit;
  if not InitializeSecurityDescriptor(@pSD, SECURITY_DESCRIPTOR_REVISION) then
    Exit;
  if not SetSecurityDescriptorDacl(@pSD, True, nil, False) then
    Exit;
  SA.nLength := SizeOf(SA);
  SA.lpSecurityDescriptor := @pSD;
  SA.bInheritHandle := True;
  MappingName := FAppName + 'WndHandle';
  FFileMapping := CreateFileMapping(INVALID_HANDLE_VALUE, @SA, PAGE_READWRITE, 0, SizeOf(Handle), PChar(MappingName));
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
      Result := ConcatPaths([Dir, IntToStr(Random(100000))]);
      E := FileExists(Result);
    end;
  end;

var
  Dir, Filename: string;
  S: TMemoryStream;
begin
  FPortableAllowed := True;

  if FRunningFromInstalledLocation then
  begin
    FPortableAllowed := False;
    Exit;
  end;

  S := TMemoryStream.Create;
  try
    try
      Dir := ExtractFilePath(AppPath);
      Filename := GetRandomFile(Dir);
      S.SaveToFile(Filename);
      SysUtils.DeleteFile(Filename);
    except
      FPortableAllowed := False;
    end;
  finally
    S.Free;
  end;
end;

procedure TAppDataBase.GetRunningFromInstalledLocation;

  function IsInstalled(const RootKey: HKEY): Boolean;
  const
    UninstallPath = 'Software\Microsoft\Windows\CurrentVersion\Uninstall';
  var
    i: Integer;
    Reg: TRegistry;
    KeyName: UnicodeString;
    TmpKeyNames: TUnicodeStringArray = [];
  begin
    Result := False;
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      if Reg.KeyExists(UninstallPath) then
        if Reg.OpenKeyReadOnly(UninstallPath) then
        begin
          TmpKeyNames += Reg.GetKeyNames;
          Reg.CloseKey;
        end;

      for KeyName in TmpKeyNames do
        if Reg.OpenKeyReadOnly(UninstallPath + '\' + KeyName) then
        begin
          if Reg.ValueExists('DisplayName') and (LowerCase(Reg.ReadString('DisplayName')) = LowerCase(AppName)) then
          begin
            if LowerCase(IncludeTrailingPathDelimiter(Reg.ReadString('InstallLocation'))) = LowerCase(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))) then
              FRunningFromInstalledLocation := True;
            Exit(True);
          end;
          Reg.CloseKey;
        end;
    finally
      Reg.Free;
    end;
  end;

begin
  FRunningFromInstalledLocation := IsInstalled(HKEY_CURRENT_USER) or IsInstalled(HKEY_LOCAL_MACHINE);
end;

procedure TAppDataBase.GetTempDir;
var
  Rec: TCommandLineRecord;
begin
  Rec := FCommandLine.GetParam('-tempdir');

  if (Rec <> nil) and (Rec.Values.Count > 0) then
    FTempDir := Rec.Values[0]
  else
    FTempDir := ConcatPaths([TFunctions.GetTempDir, FAppName]);

  if FTempDir <> '' then
    ForceDirectories(FTempDir);

  if not DirectoryExists(FTempDir) then
    raise Exception.Create(Format(_('The folder for temporary files could not be determined.'#13#10 + 'Please ask for support at %s.'#13#10 + 'The application will be terminated.'), [FProjectForumLink]));
end;

procedure TAppDataBase.GetVersionInfo;
begin
  try
    FAppVersion := TFunctions.GetFileVersion(ParamStr(0));
  except
    FAppVersion.Major := 0;
    FAppVersion.Minor := 0;
    FAppVersion.Revision := 0;
    FAppVersion.Build := 0;
  end;

  if (FAppVersion.Major = 0) and (FAppVersion.Minor = 0) and (FAppVersion.Revision = 0) and (FAppVersion.Build = 0) then
    raise Exception.Create(Format(_('The version of the application could not be determined.'#13#10 + 'Please ask for support at %s.'#13#10 + 'The application will be terminated.'), [FProjectForumLink]));
end;

end.
