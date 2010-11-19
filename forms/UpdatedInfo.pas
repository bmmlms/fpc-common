unit UpdatedInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, AppData, pngimage, LanguageObjects,
  ShellAPI;

type
  TfrmUpdatedInfo = class(TForm)
    txtInfo: TMemo;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnClose: TBitBtn;
    btnDonateEn: TImage;
    btnDonateDe: TImage;
    chkNotShowAgain: TCheckBox;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnDonateClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TfrmUpdatedInfo.btnCloseClick(Sender: TObject);
begin
  AppGlobals.SuppressUpdatedInfo := chkNotShowAgain.Checked;
  Close;
end;

procedure TfrmUpdatedInfo.btnDonateClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(AppGlobals.ProjectDonateLink), '', '', 1);
  AppGlobals.SuppressUpdatedInfo := chkNotShowAgain.Checked;
  Close;
end;

procedure TfrmUpdatedInfo.FormCreate(Sender: TObject);
begin
  Language.Translate(Self);

  txtInfo.Text := _('You just upgraded to %s version %s!'#13#10#13#10 +
                    'I hope you enjoy using %s. If you like this software, please consider ' +
                    'donating to support further development.');
  txtInfo.Text := Format(txtInfo.Text, [AppGlobals.AppName, AppGlobals.AppVersion.AsString, AppGlobals.AppName]);

  if AppGlobals.ProjectDonateLink <> '' then
  begin
    if Language.CurrentLanguage.ID = 'de' then
      btnDonateDe.Visible := True
    else
      btnDonateEn.Visible := True;
  end;
end;

procedure TfrmUpdatedInfo.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmUpdatedInfo.FormShow(Sender: TObject);
begin
  btnClose.SetFocus;
end;

end.
