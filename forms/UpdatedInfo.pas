{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2021 Alexander Nottelmann

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

unit UpdatedInfo;

interface

uses
  AppData,
  Buttons,
  Classes,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  LanguageObjects,
  MControls,
  StdCtrls,
  SysUtils,
  Variants;

type

  { TfrmUpdatedInfo }

  TfrmUpdatedInfo = class(TForm)
    btnDonateDe: TImage;
    btnDonateEn: TImage;
    Panel1: TPanel;
    txtInfo: TMemo;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnClose: TBitBtn;
    chkNotShowAgain: TCheckBox;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnDonateClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  end;

implementation

{$R *.lfm}

procedure TfrmUpdatedInfo.btnCloseClick(Sender: TObject);
begin
  AppGlobals.SuppressUpdatedInfo := chkNotShowAgain.Checked;
  Close;
end;

procedure TfrmUpdatedInfo.btnDonateClick(Sender: TObject);
begin
  TFunctions.ShellExecute(0, 'open', AppGlobals.ProjectDonateLink);
  AppGlobals.SuppressUpdatedInfo := chkNotShowAgain.Checked;
  Close;
end;

procedure TfrmUpdatedInfo.FormCreate(Sender: TObject);
begin
  Language.Translate(Self);

  txtInfo.Text := _('You just upgraded %s to version %s!'#13#10#13#10 + 'I hope you enjoy using %s. If you like this software please consider ' +
    'a donation to help paying the website''s server and to support further development.'#13#10 + 'For more information please click the "Donate" button below. If you want to donate later just ' +
    'open the "About..." window from the main menu which contains the donation links.');
  txtInfo.Text := Format(txtInfo.Text, [AppGlobals.AppName, AppGlobals.AppVersion.AsString, AppGlobals.AppName]);

  if AppGlobals.ProjectDonateLink <> '' then
    if Language.CurrentLanguage.ID = 'de' then
      btnDonateDe.Visible := True
    else
      btnDonateEn.Visible := True;
end;

procedure TfrmUpdatedInfo.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
  btnClose.ApplyFocus;
end;

end.
