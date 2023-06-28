{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2023 Alexander Nottelmann

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
  AppData,
  AppDataBase,
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  FPCanvas,
  Functions,
  Graphics,
  InterfaceBase,
  LanguageObjects,
  LCLType,
  StdCtrls,
  SysUtils,
  Variants,
  Windows;

type

  { TScrollText }

  TScrollText = class(TGraphicControl)
  private
  const
    IMAGE_SPACING = 20;
  private
    FOffset: Integer;
    FTextHeight: Integer;
    FText: TStringList;
    FTimer: TTimer;
    FBmp: Graphics.TBitmap;
    FBmps: array of Graphics.TBitmap;

    procedure TimerOnTimer(Sender: TObject);
    procedure BuildBitmap;
    function GetTextHeight: Integer;
    function GetFontSpacing(Canvas: TCanvas): Integer;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start;
  end;

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnDonate: TImage;
    lblAbout: TLabel;
    lblCopyright: TLabel;
    lblHomepage: TLabel;
    lblVersion: TLabel;
    pagAbout: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pbLogo: TPaintBox;
    tabAbout: TTabSheet;
    tabLicense: TTabSheet;
    txtAbout: TMemo;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnClose: TBitBtn;
    lblGPL: TLabel;
    tabThanks: TTabSheet;
    procedure btnDonateMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure btnDonateMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lblGPLClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure lblHomepageClick(Sender: TObject);
    procedure pagAboutChange(Sender: TObject);
    procedure pbLogoPaint(Sender: TObject);
  private
    FScrollText: TScrollText;
    function PointInImagePicture(const X: Integer): Boolean;
  public
    constructor Create(AOwner: TComponent; Caption: string); reintroduce;
  end;

implementation

{$R *.lfm}

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

constructor TfrmAbout.Create(AOwner: TComponent; Caption: string);
var
  P, P2: TPicture;
  P3: Graphics.TBitmap;
begin
  inherited Create(AOwner);

  if not Assigned(AOwner) then
  begin
    Self.Icon.Handle := Application.Icon.Handle;
    Position := poScreenCenter;
    ShowInTaskBar := stAlways;
  end;

  Language.Translate(Self);

  Self.Caption := Caption;
  lblAbout.Caption := AppGlobals.AppName;

  lblVersion.Caption := _('Version') + ' ' + AppGlobals.AppVersion.AsString;
  if AppGlobals.GitSHA <> '' then
    lblVersion.Caption := lblVersion.Caption + '-%s'.Format([AppGlobals.GitSHA]);
  if AppGlobals.Codename <> '' then
    lblVersion.Caption := lblVersion.Caption + ' ''%s'''.Format([AppGlobals.Codename]);

  lblGPL.Caption := _('Distributed under the terms of the GNU General Public License');

  case AppGlobals.License of
    alGPL:
      txtAbout.Text := Format(_('%s'#13#10 + 'Copyright © 2010-2023 Alexander Nottelmann et al.'#13#10#13#10 +
        'This program is free software: you can redistribute it and/or modify ' + 'it under the terms of the GNU General Public License as published by ' +
        'the Free Software Foundation, either version 3 of the License, or ' + '(at your option) any later version.'#13#10#13#10 +
        'This program is distributed in the hope that it will be useful, ' + 'but WITHOUT ANY WARRANTY; without even the implied warranty of ' +
        'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ' + 'GNU General Public License for more details.'#13#10#13#10 +
        'You should have received a copy of the GNU General Public License ' + 'along with this program. If not, see <http://www.gnu.org/licenses/>.'), [AppGlobals.AppName]);
    alProprietary:
    begin
      txtAbout.Text := Format(_('%s'#13#10 + 'Copyright © 2010-2023 Alexander Nottelmann'), [AppGlobals.AppName]);
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

  if AppGlobals.ProjectThanksText <> '' then
  begin
    FScrollText := TScrollText.Create(Self);
    FScrollText.Parent := tabThanks;
    FScrollText.Align := alClient;
    FScrollText.Text := AppGlobals.ProjectThanksText;
  end else
    tabThanks.PageControl := nil;

  pagAbout.ActivePageIndex := 0;

  Constraints.MinWidth := Scale96ToFont(Constraints.MinWidth);
  Constraints.MinHeight := Scale96ToFont(Constraints.MinHeight);
end;

procedure TfrmAbout.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmAbout.btnDonateMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if PointInImagePicture(X) then
    btnDonate.Cursor := crHandPoint
  else
    btnDonate.Cursor := crArrow;
end;

procedure TfrmAbout.btnDonateMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Img: TImage absolute Sender;
begin
  if PointInImagePicture(X) then
    TFunctions.ShellExecute(0, 'open', AppGlobals.ProjectDonateLink);
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

    TFunctions.GetMaxTransparent(Icon, TransparentTop, TransparentRight);

    DrawIconEx(pbLogo.Canvas.Handle, pbLogo.ClientWidth - TransparentRight, -TransparentTop, Icon.Handle, 96, 96, 0, 0, DI_NORMAL);
  finally
    Icon.Free;
  end;
end;

procedure TfrmAbout.lblHomepageClick(Sender: TObject);
begin
  TFunctions.ShellExecute(0, 'open', AppGlobals.ProjectHomepageLink);
end;

procedure TfrmAbout.lblGPLClick(Sender: TObject);
begin
  TFunctions.ShellExecute(0, 'open', 'http://www.gnu.org/licenses/gpl-3.0.html');
end;

procedure TfrmAbout.pagAboutChange(Sender: TObject);
begin
  if pagAbout.ActivePage = tabThanks then
    FScrollText.Start;
end;

function TfrmAbout.PointInImagePicture(const X: Integer): Boolean;
begin
  Result := (X + 5 > btnDonate.ClientWidth / 2 - btnDonate.Picture.Width / 2) and (X - 5 < btnDonate.ClientWidth / 2 + btnDonate.Picture.Width / 2);
end;

{ TScrollText }

constructor TScrollText.Create(AOwner: TComponent);
var
  i: Integer = 0;
  ResStream: TResourceStream;
  Image: TJPEGImage;
  Bmp: Graphics.TBitmap;
begin
  inherited;

  FOffset := MaxInt;

  FText := TStringList.Create;
  FBmps := [];

  while FindResource(HINSTANCE, PChar('THANKSIMAGE' + IntToStr(i)), Windows.RT_RCDATA) <> 0 do
  begin
    ResStream := TResourceStream.Create(HInstance, 'THANKSIMAGE' + IntToStr(i), Windows.RT_RCDATA);
    try
      Image := TJPEGImage.Create;
      try
        Image.LoadFromStream(ResStream);

        Bmp := Graphics.TBitmap.Create;
        Bmp.Width := Image.Width;
        Bmp.Height := Image.Height;
        Bmp.Canvas.Draw(0, 0, Image);

        FBmps += [Bmp];
      finally
        Image.Free;
      end;
    finally
      ResStream.Free;
    end;
    Inc(i);
  end;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerOnTimer;
end;

destructor TScrollText.Destroy;
var
  Bmp: Graphics.TBitmap;
begin
  FText.Free;

  for Bmp in FBmps do
    Bmp.Free;

  FBmp.Free;

  inherited;
end;

function TScrollText.GetTextHeight: Integer;
var
  i, Idx: Integer;
  Line: string;
begin
  Result := 0;

  for i := 0 to FText.Count - 1 do
  begin
    FBmp.Canvas.Font.Style := [];
    FBmp.Canvas.Font.Size := 0;

    Line := FText[i];
    if Copy(Line, 1, 4) = '&IMG' then
    begin
      Idx := StrToInt(Copy(Line, 5, 1));
      Result := Result + FBmps[Idx].Height + Scale96ToFont(IMAGE_SPACING);
    end else
    begin
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

      Result := Result + FBMP.Canvas.TextHeight(Line) + GetFontSpacing(FBMP.Canvas);
    end;
  end;
end;

function TScrollText.GetFontSpacing(Canvas: TCanvas): Integer;
var
  TextMetrics: LCLType.TTextMetric;
begin
  WidgetSet.GetTextMetrics(Canvas.Handle, TextMetrics);
  Result := TextMetrics.tmExternalLeading + TextMetrics.tmInternalLeading + Scale96ToFont(2);
end;

procedure TScrollText.BuildBitmap;

  procedure DistortArea(Percent, YFrom, YTo: Integer);
  type
    TRGBLine = array[Word] of TRGBTriple;
    PRGBLine = ^TRGBLine;
  var
    Y, X, Rnd: Integer;
    Pixels: PRGBLine;
  begin
    FBmp.BeginUpdate;
    try
      for Y := YFrom to YTo - 1 do
      begin
        Pixels := FBmp.ScanLine[Y];
        for X := 0 to FBmp.Width - 1 do
        begin
          if (Pixels^[X].rgbtBlue = 0) and (Pixels^[X].rgbtGreen = 0) and (Pixels^[X].rgbtRed = 0) then
            Continue;

          Rnd := Random(100);
          if Percent > Rnd then
          begin
            Pixels^[X].rgbtBlue := Rnd;
            Pixels^[X].rgbtGreen := Rnd;
            Pixels^[X].rgbtRed := Rnd;
          end;
        end;
      end;
    finally
      FBmp.EndUpdate;
    end;
  end;

var
  R: TRect;
  i, Y, Idx: Integer;
  Line: string;
begin
  if FBMP = nil then
  begin
    FBmp := Graphics.TBitmap.Create;
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

  if FTextHeight = 0 then
    FTextHeight := GetTextHeight;

  Y := FOffset;
  for i := 0 to FText.Count - 1 do
  begin
    Line := FText[i];

    FBmp.Canvas.Font.Style := [];
    FBmp.Canvas.Font.Size := 0;

    if Copy(Line, 1, 4) = '&IMG' then
    begin
      Idx := StrToInt(Copy(Line, 5, 1));
      FBMP.Canvas.Draw(FBmp.Width div 2 - FBmps[Idx].Width div 2, Y, FBmps[Idx]);
      Y += FBmps[Idx].Height + Scale96ToFont(IMAGE_SPACING);
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

      FBmp.Canvas.TextOut(FBmp.Width div 2 - FBmp.Canvas.TextWidth(Line) div 2, Y, Line);

      Y += FBMP.Canvas.TextHeight(Line) + GetFontSpacing(FBMP.Canvas);
    end;
  end;

  i := Scale96ToFont(20);
  Y := Scale96ToFont(4);
  while i < 100 do
  begin
    DistortArea(100 - i, Y - Scale96ToFont(4), Y);
    Inc(i, Scale96ToFont(20));
    Inc(Y, Scale96ToFont(4));
  end;

  i := Scale96ToFont(20);
  Y := Scale96ToFont(4);
  while i < 100 do
  begin
    DistortArea(100 - i, FBmp.Height - Y, FBmp.Height - Y + Scale96ToFont(4));
    Inc(i, Scale96ToFont(20));
    Inc(Y, Scale96ToFont(4));
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
