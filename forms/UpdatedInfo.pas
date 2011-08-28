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
    procedure FormResize(Sender: TObject);
  private
  public
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

  txtInfo.Text := _('You just upgraded %s to version %s!'#13#10#13#10 +
                    'I hope you enjoy using %s. If you like this software, please consider ' +
                    'a donation to help paying the website''s server and to support further development.'#13#10 +
                    'For more information, please click the "Donate" button.');
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

procedure TfrmUpdatedInfo.FormResize(Sender: TObject);
begin
  btnDonateDe.Left := ClientWidth div 2 - btnDonateDe.Width div 2;
  btnDonateEn.Left := ClientWidth div 2 - btnDonateEn.Width div 2;
end;

procedure TfrmUpdatedInfo.FormShow(Sender: TObject);
begin
  btnClose.SetFocus;
end;

end.
