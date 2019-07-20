{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2019 Alexander Nottelmann

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

unit AppStartup;

interface

uses
  Windows, SysUtils, WinSock, Forms, AppData, UpdateClient, Update, WizardBase,
  ProfileSettings, Functions, SettingsStorage, LanguageObjects, Dialogs,
  AppDataBase, About, Menus, UpdatedInfo, Logging, MsgDlg;

type
  TPatch = packed record
    Call: Byte;
    Proc: Pointer;
    Ret: Byte;
  end;

type
  TAppEvents = class
  public
    class procedure OnActivate(Sender: TObject);
  end;

  TWizardClass = class of TfrmWizardBase;

function InitAppStageOne: Boolean;
function InitAppStageTwo(WizardClass: TWizardClass): Boolean;
function InitWinsock: Boolean;

implementation

{ TAppEvents }

class procedure TAppEvents.OnActivate(Sender: TObject);
begin
  ShowWindow(Application.Handle, SW_HIDE);
end;

function PatchedIsAltGRPressed: Boolean;
begin
  Result := False;
end;

function InitAppStageOne: Boolean;
var
  Ver: TOSVersionInfo;
  VerRec: TAppVersion;
  OldProtect: Cardinal;
  Patch: TPatch;
  ProfileSettings: TfrmProfileSettings;
begin
  Randomize;
  Result := True;

  SetErrorMode(SEM_FAILCRITICALERRORS);

  Application.OnActivate := TAppEvents.OnActivate;

  Ver.dwOSVersionInfoSize := SizeOf(Ver);
  if GetVersionEx(Ver) then
  begin
    VerRec := ParseVersion(Ver.dwMajorVersion, Ver.dwMinorVersion, 0, 0);
    if not IsVersionNewer(ParseVersion('5.0.0.0'), VerRec) then
    begin
      TfrmMsgDlg.ShowMsg(nil, Format(_('%s requires at least Windows Vista, earlier versions of windows are not supported.'#13#10 +
                                       'If you continue running %s using a not supported operating system I am not responsible for any problems that might occur.'), [AppGlobals.AppName, AppGlobals.AppName]),
                         mtWarning, [mbOK], mbOK, 12);
    end;
  end;

  // Gibt manchmal bei ALT-GR eine Exception. Die möchten wir nicht.
  Patch.Call := $E8;
  Patch.Proc := Pointer(Integer(Pointer(@PatchedIsAltGRPressed)) - Integer(Pointer(@IsAltGRPressed)) - 5);
  Patch.Ret := $C3;
  if VirtualProtect(@IsAltGRPressed, SizeOf(TPatch), PAGE_EXECUTE_READWRITE, OldProtect) then
    try
      CopyMemory(@IsAltGRPressed, @Patch, SizeOf(TPatch));
    finally
      VirtualProtect(@IsAltGRPressed, SizeOf(TPatch), OldProtect, OldProtect);
    end;

  if TSettingsInstalled.Active(AppGlobals.AppName) and
     TSettingsPortable.Active(AppGlobals.AppName) then
  begin
    ProfileSettings := TfrmProfileSettings.Create(nil);
    try
      ProfileSettings.ShowModal;
      if AppGlobals.Portable = poUndefined then
      begin
        Result := False;
        Exit;
      end;
    finally
      ProfileSettings.Free;
    end;
  end;

  Language.SetLanguage(AppGlobals.Language);
end;

function InitAppStageTwo(WizardClass: TWizardClass): Boolean;
var
  Res: Integer;
  Updater: TUpdateClient;
  About: TfrmAbout;
  Wizard: TfrmWizardBase;
  UpdatedInfo: TfrmUpdatedInfo;
begin
  Result := True;

  if AppGlobals.InstallUpdateOnStart then
  begin
    Res := MsgBox(0, _('The update downloaded last time can now be installed.'#13#10'Do you want to install the new version now?'), _('Question'), MB_ICONQUESTION or MB_YESNO);
    try
      if Res = IDYES then
      begin
        Updater := TUpdateClient.Create;
        try
          Result := False;
          Updater.RunUpdate;
          Exit;
        finally
          Updater.Free;
        end;
      end;
    finally
      AppGlobals.InstallUpdateOnStart := False;
      try
        AppGlobals.Save;
      except end;
    end;
  end;

  if IsVersionNewer(AppGlobals.LastUsedVersion, AppGlobals.AppVersion) and (not AppGlobals.SuppressUpdatedInfo) and
    (AppGlobals.ProjectDonateLink <> '') then
  begin
    UpdatedInfo := TfrmUpdatedInfo.Create(nil);
    try
      UpdatedInfo.ShowModal;
    finally
      UpdatedInfo.Free;
    end;
  end;

  if not AppGlobals.FirstStartShown then
  begin
    About := TfrmAbout.Create(nil, _('Application information'), True);
    try
      About.ShowModal;
    finally
      About.Free;
    end;
  end;

  if not AppGlobals.WasSetup then
  begin
    Wizard := WizardClass.Create(nil);
    try
      Wizard.ShowModal;
    finally
      Wizard.Free;
    end;
  end;

  Application.OnActivate := nil;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
end;

function InitWinsock: Boolean;
var
  Data: TWSAData;
begin
  Result := True;
  if WSAStartup($0101, Data) <> 0 then
  begin
    MessageBox(0, 'The Application could not be started because Winsock could not be initialized.', 'Error', MB_ICONERROR);
    Result := False;
  end;
end;

initialization

finalization
  WSACleanup;

end.
