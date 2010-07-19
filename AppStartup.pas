unit AppStartup;

interface

uses
  Windows, WinSock, Forms, AppData, UpdateClient, Update, Wizard,
  ProfileSettings, Functions, SettingsStorage, LanguageObjects,
  AppDataBase, About;

function InitApp: Boolean;

implementation

function InitApp: Boolean;
var
  Res: Integer;
  Updater: TUpdateClient;
  About: TfrmAbout;
  Wizard: TfrmWizard;
  ProfileSettings: TfrmProfileSettings;
begin
  Result := True;

  SetErrorMode(SEM_FAILCRITICALERRORS);

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
      AppGlobals.Save;
    end;
  end;

  if not AppGlobals.FirstStartShown then
  begin
    About := TfrmAbout.Create(nil, _('Application information'));
    try
      About.ShowModal;
    finally
      About.Free;
    end;
    AppGlobals.FirstStartShown := True;
  end;

  if not AppGlobals.WasSetup then
  begin
    Wizard := TfrmWizard.Create(nil);
    try
      Wizard.ShowModal;
    finally
      Wizard.Free;
    end;
  end;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
end;

var
  Data: TWSAData;
initialization
  if WSAStartup($0101, Data) <> 0 then
  begin
    MessageBox(0, 'The Application could not be started because Winsock could not be initialized.', 'Error', MB_ICONEXCLAMATION);
    Halt;
  end;

finalization
  WSACleanup;

end.
