unit ProfileSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, AppDataBase, AppData, Functions,
  LanguageObjects, SettingsStorage;

type
  TfrmProfileSettings = class(TForm)
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOk: TBitBtn;
    pnlHeader: TPanel;
    Shape1: TShape;
    lblTop: TLabel;
    lblProfiles: TLabel;
    rbInstalled: TRadioButton;
    rbPortable: TRadioButton;
    btnDelete: TButton;
    btnDelete2: TButton;
    btnCopy: TButton;
    btnCopy2: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
  private

  public

  end;

implementation

{$R *.dfm}

procedure TfrmProfileSettings.btnCopyClick(Sender: TObject);
var
  S: TSettingsInstalled;
  S2: TSettingsPortable;
begin
  if (((Sender = btnCopy) and (TSettingsPortable.Active(AppGlobals.AppName))) or
      ((Sender = btnCopy2) and (TSettingsInstalled.Active(AppGlobals.AppName)))) then
    if MsgBox(Handle, _('All settings of the other profile will be replaced by the current profile''s settings.'#13#10 +
                        'Please be aware that existing settings and files containing data will be overwritten.'#13#10 +
                        'Proceed?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON2) = IDNO then
    begin
      Exit;
    end;

  try
    S := TSettingsInstalled.Create(AppGlobals.AppName, AppGlobals.AppPath, AppGlobals.CommandLine);
    S2 := TSettingsPortable.Create(AppGlobals.AppName, AppGlobals.AppPath, AppGlobals.CommandLine);
    try
      if Sender = btnCopy then
        S2.Assign(S)
      else
        S.Assign(S2);
    finally
      S.Free;
      S2.Free;
    end;

    if Sender = btnCopy2 then
    begin
      btnCopy.Enabled := True;
      btnDelete.Enabled := True;
    end else
    begin
      btnCopy2.Enabled := True;
      btnDelete2.Enabled := True;
    end;
  except
    MsgBox(Handle, _('An error occured while copying the settings.'#13#10 +
                     'It is possible that some settings were copied before the error occured.'), _('Error'), MB_ICONERROR);
    Exit;
  end;
end;

procedure TfrmProfileSettings.btnOkClick(Sender: TObject);
begin
  if rbInstalled.Checked then
    AppGlobals.Portable := poNo
  else
    AppGlobals.Portable := poYes;
  Close;
end;

procedure TfrmProfileSettings.btnDeleteClick(Sender: TObject);
var
  S: TSettingsStorage;
begin
  if MsgBox(Handle, _('All data saved in this profile will be deleted.'#13#10 +
                      'Are you sure you want to delete this profile?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON2) = IDYES then
  begin
    if Sender = btnDelete then
      S := TSettingsInstalled.Create(AppGlobals.AppName, AppGlobals.AppPath, AppGlobals.CommandLine)
    else
      S := TSettingsPortable.Create(AppGlobals.AppName, AppGlobals.AppPath, AppGlobals.CommandLine);
    try
      if not S.DeleteProfile then
      begin
        MsgBox(Handle, _('An error occured deleting the profile, but parts of it may have been removed.'), _('Info'), MB_ICONEXCLAMATION)
      end;

      if Sender = btnDelete then
      begin
        btnCopy.Enabled := False;
        btnDelete.Enabled := False;
      end else
      begin
        btnCopy2.Enabled := False;
        btnDelete2.Enabled := False;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TfrmProfileSettings.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmProfileSettings.FormCreate(Sender: TObject);
begin
  Language.Translate(Self);

  AppGlobals.Portable := poUndefined;
  lblProfiles.Caption := _('It seems there are settings saved for this application in the registry/application ' +
                           'data folder and in the application''s folder.'#13#10 +
                           'If you do not want to see this dialog again, please delete one profile.'#13#10 +
                           'Which profile do you want to use?');
  rbPortable.Enabled := AppGlobals.PortableAllowed;

  btnDelete.Enabled := not AppGlobals.Storage.DataDirOverridden;
  btnDelete2.Enabled := not AppGlobals.Storage.DataDirOverridden;
  btnCopy.Enabled := not AppGlobals.Storage.DataDirOverridden;
  btnCopy2.Enabled := not AppGlobals.Storage.DataDirOverridden;
end;

end.
