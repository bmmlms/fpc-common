{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2014 Alexander Nottelmann

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
  Buttons, pngimage, AppDataBase, jpeg, Functions, PngFunctions;

type
  TScrollText = class(TGraphicControl)
  private
    FOffset, FTextHeight: Integer;
    FText: TStringList;
    FTimer: TTimer;
    FBmp: TBitmap;
    FBmps: array of TBitmap;

    procedure TimerOnTimer(Sender: TObject);
    procedure BuildBitmap;
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
    procedure btnDonateEnClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure pnlNavClick(Sender: TObject);
  private
    FScrollText: TScrollText;

    function CropPNG(Source: TPNGObject; Left, Top, Width, Height: Integer): TPngImage;
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

procedure TfrmAbout.btnDonateEnClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(AppGlobals.ProjectDonateLink), '', '', 1);
end;

constructor TfrmAbout.Create(AOwner: TComponent; Caption: string);
var
  i, n: Integer;
  TransparentRight, TransparentTop: Integer;
  C: TColor;
  Icon: TIcon;
  PNG, PNGCropped: TPngImage;
begin
  inherited Create(AOwner);

  Language.Translate(Self);

  Self.Caption := Caption;
  lblAbout.Caption := AppGlobals.AppName;
  lblVersion.Caption := _('Version') + ' ' + AppGlobals.AppVersion.AsString;
  if AppGlobals.BuildNumber > 0 then
    lblVersion.Caption := lblVersion.Caption + ' ' + Format(_('build %d'), [AppGlobals.BuildNumber]);
  lblGPL.Caption := _('Distributed under the terms of the GNU General Public License');

  txtAbout.Text := Format(_('%s'#13#10 +
                            'Copyright (c) 2010-2014 Alexander Nottelmann'#13#10#13#10 +
                            'This program is free software: you can redistribute it and/or modify ' +
                            'it under the terms of the GNU General Public License as published by ' +
                            'the Free Software Foundation, either version 3 of the License, or ' +
                            '(at your option) any later version.'#13#10#13#10 +
                            'This program is distributed in the hope that it will be useful, ' +
                            'but WITHOUT ANY WARRANTY; without even the implied warranty of ' +
                            'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ' +
                            'GNU General Public License for more details.'#13#10#13#10 +
                            'You should have received a copy of the GNU General Public License ' +
                            'along with this program. If not, see <http://www.gnu.org/licenses/>.'), [AppGlobals.AppName]);;

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

  Icon := TIcon.Create;
  try
    if ExistsIconSize('A', 96) then
      Icon.Handle := LoadImage(HInstance, 'A', IMAGE_ICON, 96, 96, LR_LOADTRANSPARENT)
    else
      Icon.LoadFromResourceName(HInstance, 'A');

    ConvertToPNG(Icon, PNG);
    try
      TransparentRight := 0;
      TransparentTop := PNG.Height;

      C := PNG.Canvas.Pixels[0, 0];
      for i := 0 to PNG.Height - 1 do
        for n := 0 to PNG.Width - 1 do
          if PNG.Canvas.Pixels[n, i] <> C then
            if n > TransparentRight then
              TransparentRight := n;

      for i := PNG.Height - 1 downto 0 do
        for n := 0 to PNG.Width - 1 do
          if PNG.Canvas.Pixels[n, i] <> C then
            if i < TransparentTop then
              TransparentTop := i;

      PNGCropped := CropPNG(PNG, 0, TransparentTop, Icon.Width - (Icon.Width - TransparentRight), Icon.Height - TransparentTop);
      try
        imgLogo.Picture.Assign(PNGCropped);
      finally
        PNGCropped.Free;
      end;
    finally
      PNG.Free;
    end;

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

function TfrmAbout.CropPNG(Source: TPngImage; Left, Top, Width, Height: Integer): TPngImage;
  function ColorToTriple(Color: TColor): TRGBTriple;
  begin
    Color := ColorToRGB(Color);
    Result.rgbtBlue := Color shr 16 and $FF;
    Result.rgbtGreen := Color shr 8 and $FF;
    Result.rgbtRed := Color and $FF;
  end;
var
  X, Y: Integer;
  Bitmap: TBitmap;
  BitmapLine: PRGBLine;
  AlphaLineA, AlphaLineB: pngimage.PByteArray;
begin
  if (Source.Width < (Left + Width)) or (Source.Height < (Top + Height)) then
    raise Exception.Create('Invalid position/size');

  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := Width;
    Bitmap.Height := Height;
    Bitmap.PixelFormat := pf24bit;

    for Y := 0 to Bitmap.Height - 1 do begin
      BitmapLine := Bitmap.Scanline[Y];
      for X := 0 to Bitmap.Width - 1 do
        BitmapLine^[X] := ColorToTriple(Source.Pixels[Left + X, Top + Y]);
    end;

    Result := TPNGObject.Create;
    Result.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;

  if Source.Header.ColorType in [COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA] then begin
    Result.CreateAlpha;
    for Y := 0 to Result.Height - 1 do begin
      AlphaLineA := Source.AlphaScanline[Top + Y];
      AlphaLineB := Result.AlphaScanline[Y];
      for X := 0 to Result.Width - 1 do
        AlphaLineB^[X] := AlphaLineA^[X + Left];
    end;
  end;
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

procedure TfrmAbout.FormResize(Sender: TObject);
begin
  btnDonateDe.Left := tabAbout.ClientWidth div 2 - btnDonateDe.Width div 2;
  btnDonateEn.Left := tabAbout.ClientWidth div 2 - btnDonateEn.Width div 2;
end;

procedure TfrmAbout.lblHomepageClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(AppGlobals.ProjectHomepageLink), '', '', 1);
end;

procedure TfrmAbout.lblGPLClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://www.gnu.org/licenses/gpl-3.0.html', '', '', 1);
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
  FTextHeight := 0;

  FBmp := TBitmap.Create;

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

procedure TScrollText.BuildBitmap;
  function GetTextHeight: Integer;
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
var
  R: TRect;
  i, L, H, Idx, ImageHeight: Integer;
  Line: string;
begin
  FBMP.Height := GetTextHeight + ClientHeight;
  FBMP.Width := ClientWidth;
  SetBkMode(FBMP.Canvas.Handle, TRANSPARENT);

  FBMP.Canvas.Brush.Color := clBlack;
  R.Left := 0;
  R.Top := 0;
  R.Bottom := FBMP.Height;
  R.Right := FBMP.Width;
  FBMP.Canvas.FillRect(R);
  ImageHeight := 0;

  FBMP.Canvas.Font.Color := clWhite;
  H := FBMP.Canvas.TextHeight('A');
  FTextHeight := 0;
  for i := 0 to FText.Count - 1 do
  begin
    Line := FText[i];
    FBMP.Canvas.Font.Style := [];
    FBMP.Canvas.Font.Size := 8;
    if Copy(Line, 1, 4) = '&IMG' then
    begin
      Idx := StrToInt(Copy(Line, 5, 1));
      FBMP.Canvas.Draw(FBmp.Width div 2 - FBmps[Idx].Width div 2, i * H + (i * 3) + ImageHeight, FBmps[Idx]);
      ImageHeight := ImageHeight + FBmps[Idx].Height + 20;
      FTextHeight := FTextHeight + FBmps[Idx].Height + 20;
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

      L := FBMP.Width div 2 - FBMP.Canvas.TextWidth(Line) div 2;
      FBMP.Canvas.TextOut(L, i * H + (i * 3) + ImageHeight, Line);
      FTextHeight := FTextHeight + H + 3;
    end;
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

    BuildBitmap;

    TimerOnTimer(FTimer);
    FTimer.Interval := 45;
    FTimer.Enabled := True;
  end;
end;

procedure TScrollText.Start;
begin

end;

procedure TScrollText.TimerOnTimer(Sender: TObject);
begin
  if (FOffset = MaxInt) or (FOffset <= 0 - FTextHeight - 100) then
    FOffset := ClientHeight;

  FOffset := FOffset - 1;

  Canvas.Draw(0, FOffset, FBMP);
end;

end.
