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
unit About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, LanguageObjects, StdCtrls, AppData, ExtCtrls, ShellAPI, ComCtrls,
  Buttons, pngimage, AppDataBase;

type
  TScrollText = class(TGraphicControl)
  private
    FOffset, FTextHeight: Integer;
    FText: TStringList;
    FTimer: TTimer;
    FBMP: TBitmap;

    procedure TimerOnTimer(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start;
  end;

  TfrmAbout = class(TForm)
    pagAbout: TPageControl;
    tabAbout: TTabSheet;
    lblAbout: TLabel;
    imgLogo: TImage;
    lblForumLink: TLabel;
    lblProjectLink: TLabel;
    lblHelpLink: TLabel;
    tabLicense: TTabSheet;
    txtAbout: TMemo;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnClose: TBitBtn;
    lblGPL: TLabel;
    lblVersion: TLabel;
    lblCopyright: TLabel;
    lblHomepage: TLabel;
    btnDonateDe: TImage;
    btnDonateEn: TImage;
    tabThanks: TTabSheet;
    procedure lblProjectLinkClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lblGPLClick(Sender: TObject);
    procedure lblHelpLinkClick(Sender: TObject);
    procedure lblForumLinkClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure lblHomepageClick(Sender: TObject);
    procedure btnDonateClick(Sender: TObject);
    procedure pagAboutChange(Sender: TObject);
  private
    FScrollText: TScrollText;
  public
    constructor Create(AOwner: TComponent; Caption: string); reintroduce;
  end;

implementation

{$R *.dfm}

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.btnDonateClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(AppGlobals.ProjectDonateLink), '', '', 1);
end;

constructor TfrmAbout.Create(AOwner: TComponent; Caption: string);
var
  Icon: TIcon;
begin
  inherited Create(AOwner);

  Language.Translate(Self);

  Self.Caption := Caption;
  lblAbout.Caption := AppGlobals.AppName;
  lblVersion.Caption := _('Version') + ' ' + AppGlobals.AppVersion.AsString;
  lblGPL.Caption := _('Distributed under the terms of the GNU General Public License');

  txtAbout.Text := Format(_('%s'#13#10 +
                            'Copyright (c) 2010 Alexander Nottelmann'#13#10#13#10 +
                            'This program is free software: you can redistribute it and/or modify'#13#10 +
                            'it under the terms of the GNU General Public License as published by'#13#10 +
                            'the Free Software Foundation, either version 3 of the License, or'#13#10 +
                            '(at your option) any later version.'#13#10#13#10 +
                            'This program is distributed in the hope that it will be useful,'#13#10 +
                            'but WITHOUT ANY WARRANTY; without even the implied warranty of'#13#10 +
                            'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the'#13#10 +
                            'GNU General Public License for more details.'#13#10#13#10 +
                            'You should have received a copy of the GNU General Public License'#13#10 +
                            'along with this program.  If not, see <http://www.gnu.org/licenses/>.'), [AppGlobals.AppName]);;

  lblHomepage.Caption := AppGlobals.ProjectHomepageLink;
  lblHelpLink.Caption := _('Help');
  lblProjectLink.Caption := _('Information, changelog and updates');
  lblForumLink.Caption := _('Report problems or request new features');

  if AppGlobals.ProjectLink = '' then
  begin
    lblHelpLink.Top := lblProjectLink.Top;
    lblProjectLink.Visible := False;
  end;

  Icon := TIcon.Create;
  try
    Icon.LoadFromResourceName(HInstance, 'A');
    imgLogo.Picture.Assign(Icon);
    imgLogo.Left := tabAbout.ClientWidth - imgLogo.Width - lblVersion.Left;
  finally
    Icon.Free;
  end;

  if AppGlobals.ProjectDonateLink <> '' then
  begin
    if Language.CurrentLanguage.ID = 'de' then
      btnDonateDe.Visible := True
    else
      btnDonateEn.Visible := True;
  end;

  if AppGlobals.ProjectThanksText <> '' then
  begin
    FScrollText := TScrollText.Create(Self);
    FScrollText.Parent := tabThanks;
    FScrollText.Align := alClient;
    FScrollText.Text := AppGlobals.ProjectThanksText;
  end else
    tabThanks.PageControl := nil;

  pagAbout.ActivePageIndex := 0;
end;

procedure TfrmAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmAbout.lblHomepageClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(AppGlobals.ProjectHomepageLink), '', '', 1);
end;

procedure TfrmAbout.lblGPLClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://www.gnu.org/licenses/gpl-3.0.html', '', '', 1);
end;

procedure TfrmAbout.lblProjectLinkClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(AppGlobals.ProjectLink), '', '', 1);
end;

procedure TfrmAbout.pagAboutChange(Sender: TObject);
begin
  if pagAbout.ActivePage = tabThanks then
  begin
    FScrollText.Start;
  end;
end;

procedure TfrmAbout.lblHelpLinkClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(AppGlobals.ProjectHelpLink), '', '', 1);
end;

procedure TfrmAbout.lblForumLinkClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(AppGlobals.ProjectForumLink), '', '', 1);
end;

{ TScrollText }

constructor TScrollText.Create(AOwner: TComponent);
begin
  inherited;

  FOffset := MaxInt;
  FTextHeight := 0;

  FText := TStringList.Create;
  FBMP := TBitmap.Create;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerOnTimer;
end;

destructor TScrollText.Destroy;
begin
  FText.Free;
  FBMP.Free;
  inherited;
end;

procedure TScrollText.Paint;
begin
  inherited;

  if not FTimer.Enabled then
  begin
    AppGlobals.BuildThanksText;
    FText.Text := AppGlobals.ProjectThanksText;

    TimerOnTimer(FTimer);
    FTimer.Interval := 45;
    FTimer.Enabled := True;
  end;
end;

procedure TScrollText.Start;
begin

end;

procedure TScrollText.TimerOnTimer(Sender: TObject);
var
  R: TRect;
  L, H: Integer;
  i: Integer;
  Line: string;
begin
  if (FOffset = MaxInt) or (FOffset <= 0 - FTextHeight - 60) then
    FOffset := ClientHeight;

  FBMP.Height := ClientHeight;
  FBMP.Width := ClientWidth;
  SetBkMode(FBMP.Canvas.Handle, TRANSPARENT);

  FBMP.Canvas.Brush.Color := clBlack;
  R.Left := 0;
  R.Top := 0;
  R.Bottom := FBMP.Height;
  R.Right := FBMP.Width;
  FBMP.Canvas.FillRect(R);

  FBMP.Canvas.Font.Color := clWhite;
  H := FBMP.Canvas.TextHeight('A');
  FTextHeight := 0;
  for i := 0 to FText.Count - 1 do
  begin
    Line := FText[i];
    FBMP.Canvas.Font.Style := [];
    FBMP.Canvas.Font.Size := 8;
    if Copy(Line, 1, 2) = '&U' then
    begin
      FBMP.Canvas.Font.Style := FBMP.Canvas.Font.Style + [fsBold];
      Line := Copy(Line, 3, Length(Line));
    end;
    if (Copy(Line, 1, 1) = '&') and (StrToIntDef(Copy(Line, 2, 2), -1) <> -1) then
    begin
      FBMP.Canvas.Font.Size := StrToInt(Copy(Line, 2, 2));
      Line := Copy(Line, 4, Length(Line));
    end;

    L := FBMP.Width div 2 - FBMP.Canvas.TextWidth(Line) div 2;
    FBMP.Canvas.TextOut(L, FOffset + i * H + (i * 3), Line);
    FTextHeight := FTextHeight + H + 3;
  end;
  FOffset := FOffset - 1;

  Canvas.Draw(0, 0, FBMP);
end;

end.
