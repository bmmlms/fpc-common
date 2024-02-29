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
  MControlFocuser,
  MControls,
  StdCtrls,
  SysUtils,
  Variants,
  Windows;

type

  { TfrmUpdatedInfo }

  TfrmUpdatedInfo = class(TForm)
    btnDonate: TImage;
    txtInfo: TMemo;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnClose: TBitBtn;
    chkNotShowAgain: TCheckBox;
    procedure btnDonateMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnDonateMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnDonateClick(Sender: TObject);
  private
    function PointInImagePicture(const X: Integer): Boolean;
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
var
  P, P2: TPicture;
  P3: Graphics.TBitmap;
begin
  Language.Translate(Self);

  txtInfo.Text := _('You just upgraded %s to version %s!'#13#10#13#10 + 'I hope you enjoy using %s. If you like this software please consider ' +
    'a donation to help paying the website''s server and to support further development.'#13#10 + 'For more information please click the "Donate" button below. If you want to donate later just ' +
    'open the "About..." window from the main menu which contains the donation links.');
  txtInfo.Text := Format(txtInfo.Text, [AppGlobals.AppName, AppGlobals.AppVersion.AsString, AppGlobals.AppName]);

  if AppGlobals.ProjectDonateLink <> '' then
  begin
    P := TPicture.Create;
    P2 := TPicture.Create;
    P3 := Graphics.TBitmap.Create;
    try
      if Language.CurrentLanguage.ID = 'de' then
        P.LoadFromResourceName(HINSTANCE, 'DONATE_DE')
      else
        P.LoadFromResourceName(HINSTANCE, 'DONATE_EN');

      P2.LoadFromResourceName(HINSTANCE, 'DONATE');

      P3.SetSize(P.Width + 12 + P2.Width, P2.Height);
      P3.PixelFormat := pf32bit;
      P3.Canvas.Draw(0, P3.Height div 2 - P.Height div 2, P.Pixmap);
      P3.Canvas.Draw(P.Width + 12, 0, P2.Pixmap);

      btnDonate.Picture.Graphic := P3;
    finally
      P.Free;
      P2.Free;
      P3.Free;
    end;

    btnDonate.Visible := True;
  end;

  Constraints.MinHeight := Scale96ToFont(Constraints.MinHeight);
  Constraints.MinWidth := Scale96ToFont(Constraints.MinWidth);
end;

procedure TfrmUpdatedInfo.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmUpdatedInfo.btnDonateMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if PointInImagePicture(X) then
    btnDonate.Cursor := crHandPoint
  else
    btnDonate.Cursor := crArrow;
end;

procedure TfrmUpdatedInfo.btnDonateMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if PointInImagePicture(X) then
    TFunctions.ShellExecute(0, 'open', AppGlobals.ProjectDonateLink);
end;

procedure TfrmUpdatedInfo.FormShow(Sender: TObject);
begin
  btnClose.ApplyFocus;
end;

function TfrmUpdatedInfo.PointInImagePicture(const X: Integer): Boolean;
begin
  Result := (X + 5 > btnDonate.ClientWidth / 2 - btnDonate.Picture.Width / 2) and (X - 5 < btnDonate.ClientWidth / 2 + btnDonate.Picture.Width / 2);
end;

end.
