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

unit About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, LanguageObjects, StdCtrls, AppData, ExtCtrls, ShellAPI, ComCtrls,
  Buttons, AppDataBase, Functions, GUIFunctions, Math,
  UITypes;

type
  TScrollText = class(TGraphicControl)
  private
    FOffset: Integer;
    FTextHeight: Integer;
    FText: TStringList;
    FTimer: TTimer;
    FBmp: TBitmap;
    FBmps: array of TBitmap;

    procedure TimerOnTimer(Sender: TObject);
    procedure BuildBitmap;
    function GetTextHeight: Integer;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start;
  end;

  { TfrmAbout }

  TfrmAbout = class(TForm)
    pagAbout: TPageControl;
    pbLogo: TPaintBox;
    tabAbout: TTabSheet;
    lblAbout: TLabel;
    tabLicense: TTabSheet;
    txtAbout: TMemo;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnClose: TBitBtn;
    lblGPL: TLabel;
    lblVersion: TLabel;
    lblCopyright: TLabel;
    lblHomepage: TLabel;
    tabThanks: TTabSheet;
    btnDonateDe: TImage;
    btnDonateEn: TImage;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lblGPLClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure lblHomepageClick(Sender: TObject);
    procedure btnDonateClick(Sender: TObject);
    procedure pagAboutChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure pbLogoPaint(Sender: TObject);
    procedure pnlNavClick(Sender: TObject);
  private
    FScrollText: TScrollText;
    FIsMainWindow: Boolean;
  protected
   // procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent; Caption: string; IsMainWindow: Boolean); reintroduce;
  end;

implementation

{$R *.lfm}

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.btnDonateClick(Sender: TObject);
begin
  ShellExecuteW(0, 'open', PWideChar(UnicodeString(AppGlobals.ProjectDonateLink)), '', '', 1);
end;

constructor TfrmAbout.Create(AOwner: TComponent; Caption: string; IsMainWindow: Boolean);
begin
  FIsMainWindow := IsMainWindow;

  inherited Create(AOwner);

  if IsMainWindow then
    Self.Icon.Handle := Application.Icon.Handle;

  Language.Translate(Self);

  Self.Caption := Caption;
  lblAbout.Caption := AppGlobals.AppName;

  lblVersion.Caption := _('Version') + ' ' + AppGlobals.AppVersion.AsString;
  if (AppGlobals.GitSHA.Length > 0) and (AppGlobals.Codename <> '') then
    lblVersion.Caption := lblVersion.Caption + ' ''%s'' %s'.Format([AppGlobals.Codename, AppGlobals.GitSHA])
  else if AppGlobals.GitSHA.Length > 0 then
    lblVersion.Caption := lblVersion.Caption + ' %s'.Format([AppGlobals.GitSHA]);

  lblGPL.Caption := _('Distributed under the terms of the GNU General Public License');

  case AppGlobals.License of
    alGPL:
      txtAbout.Text := Format(_('%s'#13#10 +
                                'Copyright © 2010-2021 Alexander Nottelmann et al.'#13#10#13#10 +
                                'This program is free software: you can redistribute it and/or modify ' +
                                'it under the terms of the GNU General Public License as published by ' +
                                'the Free Software Foundation, either version 3 of the License, or ' +
                                '(at your option) any later version.'#13#10#13#10 +
                                'This program is distributed in the hope that it will be useful, ' +
                                'but WITHOUT ANY WARRANTY; without even the implied warranty of ' +
                                'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ' +
                                'GNU General Public License for more details.'#13#10#13#10 +
                                'You should have received a copy of the GNU General Public License ' +
                                'along with this program. If not, see <http://www.gnu.org/licenses/>.'), [AppGlobals.AppName]);
    alProprietary:
      begin
        txtAbout.Text := Format(_('%s'#13#10 +
                                  'Copyright © 2010-2021 Alexander Nottelmann'), [AppGlobals.AppName]);
        lblGPL.Visible := False;
        txtAbout.Height := txtAbout.Height + lblGPL.Height + (txtAbout.Top - lblGPL.Top - lblGPL.Height);
        txtAbout.Top := lblGPL.Top;
      end;
  end;

  lblHomepage.Caption := AppGlobals.ProjectHomepageLink;

  {
  lblHelpLink.Caption := _('Help');
  lblProjectLink.Caption := _('Information, changelog and updates');
  lblForumLink.Caption := _('Report problems or request new features');

  if AppGlobals.ProjectLink = '' then
  begin
    lblHelpLink.Top := lblProjectLink.Top;
    lblProjectLink.Visible := False;
  end;
  }

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

{
procedure TfrmAbout.CreateParams(var Params: TCreateParams);
begin
  inherited;

  if FIsMainWindow then
  begin
    // We are WS_EX_APPWINDOW - we do this to get rid of the regular taskbar-entry.
    // The application window is hidden at this point.
    Params.ExStyle := Params.ExStyle and WS_EX_APPWINDOW;
    Params.WndParent := 0;
  end;
end;
}

procedure TfrmAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmAbout.FormResize(Sender: TObject);
begin
  btnDonateDe.Left := tabAbout.ClientWidth div 2 - btnDonateDe.Width div 2;
  btnDonateEn.Left := tabAbout.ClientWidth div 2 - btnDonateEn.Width div 2;
end;

procedure TfrmAbout.pbLogoPaint(Sender: TObject);
var
  TransparentRight, TransparentTop: Integer;
  Icon: TIcon;
begin
  Icon := TIcon.Create;
  try
    Icon.SetSize(96, 96);
    Icon.LoadFromResourceName(HINSTANCE, 'MAINICON');

    GetMaxTransparent(Icon, TransparentTop, TransparentRight);

    DrawIconEx(pbLogo.Canvas.Handle, pbLogo.ClientWidth - TransparentRight, -TransparentTop, Icon.Handle, 96, 96, 0, 0, DI_NORMAL);
  finally
    Icon.Free;
  end;
end;

procedure TfrmAbout.lblHomepageClick(Sender: TObject);
begin
  ShellExecuteW(0, 'open', PWideChar(UnicodeString(AppGlobals.ProjectHomepageLink)), '', '', 1);
end;

procedure TfrmAbout.lblGPLClick(Sender: TObject);
begin
  ShellExecuteW(0, 'open', 'http://www.gnu.org/licenses/gpl-3.0.html', '', '', 1);
end;

procedure TfrmAbout.pagAboutChange(Sender: TObject);
begin
  if pagAbout.ActivePage = tabThanks then
  begin
    FScrollText.Start;
  end;
end;

procedure TfrmAbout.pnlNavClick(Sender: TObject);
begin

end;

{ TScrollText }

constructor TScrollText.Create(AOwner: TComponent);
var
  i: Integer;
  ResStream: TResourceStream;
  Image: TJPEGImage;
  Bmp: TBitmap;
begin
  inherited;

  FOffset := MaxInt;

  FText := TStringList.Create;
  SetLength(FBmps, 0);

  i := 0;
  while True do
  begin
    try
      ResStream := TResourceStream.Create(HInstance, 'THANKSIMAGE' + IntToStr(i), RT_RCDATA);
      try
        Image := TJPEGImage.Create;
        try
          Image.LoadFromStream(ResStream);

          Bmp := TBitmap.Create;
          Bmp.Width := Image.Width;
          Bmp.Height := Image.Height;
          Bmp.Canvas.Draw(0, 0, Image);

          SetLength(FBmps, Length(FBmps) + 1);
          FBmps[Length(FBmps) - 1] := Bmp;
        finally
          Image.Free;
        end;
      finally
        ResStream.Free;
      end;
    except
      Break;
    end;
    Inc(i);
  end;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerOnTimer;
end;

destructor TScrollText.Destroy;
var
  i: Integer;
begin
  FText.Free;
  for i := 0 to Length(FBmps) - 1 do
    FBmps[i].Free;
  FBmp.Free;
  inherited;
end;

function TScrollText.GetTextHeight: Integer;
var
  i, H, Idx, OS: Integer;
  Line: string;
begin
  Result := 0;
  H := FBMP.Canvas.TextHeight('A');
  for i := 0 to FText.Count - 1 do
  begin
    Line := FText[i];
    if Copy(Line, 1, 4) = '&IMG' then
    begin
      Idx := StrToInt(Copy(Line, 5, 1));
      Result := Result + FBmps[Idx].Height + 20;
    end else
    begin
      if Copy(Line, 1, 2) = '&U' then
      begin
        FBMP.Canvas.Font.Style := FBMP.Canvas.Font.Style + [fsBold];
        H := FBMP.Canvas.TextHeight('A');
        Line := Copy(Line, 3, Length(Line));
        FBMP.Canvas.Font.Style := FBMP.Canvas.Font.Style - [fsBold];
      end;
      if (Copy(Line, 1, 1) = '&') and (StrToIntDef(Copy(Line, 2, 2), -1) <> -1) then
      begin
        OS := FBMP.Canvas.Font.Size;
        FBMP.Canvas.Font.Size := StrToInt(Copy(Line, 2, 2));
        H := FBMP.Canvas.TextHeight('A');
        Line := Copy(Line, 4, Length(Line));
        FBMP.Canvas.Font.Size := OS;
      end;
      Result := Result + H + 3;
    end;
  end;
end;

procedure TScrollText.BuildBitmap;
  procedure DistortArea(Percent, YFrom, YTo: Integer);
  type
    TRGBLine = array[word] of TRGBTriple;
     pRGBLine = ^TRGBLine;
  var
    Y, X, Rnd: Integer;
    Pixels: PRGBLine;
  begin
    for Y := YFrom to YTo - 1 do
    begin
      Pixels := FBmp.ScanLine[Y];
      for X := 0 to FBmp.Width - 1 do
      begin
        //Rnd := Random(100);
        //if Percent > Rnd then
        begin
          Pixels^[X].rgbtBlue := 100;
          Pixels^[X].rgbtGreen := 100;
          Pixels^[X].rgbtRed := 200;
        end;
      end;
    end;
  end;
var
  R: TRect;
  i, L, Y, H, Idx, ImageHeight: Integer;
  Line: string;
begin
  if FBMP = nil then
  begin
    FBmp := TBitmap.Create;
    FBmp.Height := ClientHeight;
    FBmp.Width := ClientWidth;
    FBmp.PixelFormat := pf24bit;
    SetBkMode(FBmp.Canvas.Handle, TRANSPARENT);

    FBmp.Canvas.Pen.Color := clYellow;

    FBmp.Canvas.Brush.Color := clBlack;
    FBmp.Canvas.Font.Color := clWhite;
  end;

  R.Left := 0;
  R.Top := 0;
  R.Bottom := FBmp.Height;
  R.Right := FBmp.Width;
  FBmp.Canvas.FillRect(R);
  ImageHeight := 0;

  if FTextHeight = 0 then
    FTextHeight := GetTextHeight;


  H := FBmp.Canvas.TextHeight('Ay');
  for i := 0 to FText.Count - 1 do
  begin
    Line := FText[i];

    Y := i * H + (i * 3) + ImageHeight + FOffset;

    FBmp.Canvas.Font.Style := [];
    FBmp.Canvas.Font.Size := 8;
    if Copy(Line, 1, 4) = '&IMG' then
    begin
      Idx := StrToInt(Copy(Line, 5, 1));
      FBMP.Canvas.Draw(FBmp.Width div 2 - FBmps[Idx].Width div 2, Y, FBmps[Idx]);
      ImageHeight := ImageHeight + FBmps[Idx].Height + 20;
    end else
    begin
      if Copy(Line, 1, 2) = '&U' then
      begin
        FBmp.Canvas.Font.Style := FBMP.Canvas.Font.Style + [fsBold];
        Line := Copy(Line, 3, Length(Line));
      end;
      if (Copy(Line, 1, 1) = '&') and (StrToIntDef(Copy(Line, 2, 2), -1) <> -1) then
      begin
        FBmp.Canvas.Font.Size := StrToInt(Copy(Line, 2, 2));
        Line := Copy(Line, 4, Length(Line));
      end;

      L := FBmp.Width div 2 - FBmp.Canvas.TextWidth(Line) div 2;

      FBmp.Canvas.TextOut(L, Y, Line);
    end;
  end;

  i := 20;
  Y := 4;
  while i < 100 do
  begin
    DistortArea(100 - i, Y - 4, Y);
    Inc(i, 20);
    Inc(Y, 4);
  end;

  i := 20;
  Y := 4;
  while i < 100 do
  begin
    DistortArea(100 - i, FBmp.Height - Y, FBmp.Height - Y + 4);
    Inc(i, 20);
    Inc(Y, 4);
  end;
end;

procedure TScrollText.Paint;
begin
  inherited;

  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(Canvas.ClipRect);

  if not FTimer.Enabled then
  begin
    AppGlobals.BuildThanksText;
    FText.Text := AppGlobals.ProjectThanksText;

    TimerOnTimer(FTimer);
    FTimer.Interval := 35;
    FTimer.Enabled := True;
  end;
end;

procedure TScrollText.Start;
begin

end;

procedure TScrollText.TimerOnTimer(Sender: TObject);
begin
  if (FOffset = MaxInt) or (FOffset <= 0 - FTextHeight) then
    FOffset := ClientHeight;

  BuildBitmap;

  FOffset := FOffset - 1;

  Canvas.Draw(0, 0, FBmp);
end;

end.
