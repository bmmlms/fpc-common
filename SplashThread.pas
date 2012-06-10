{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2012 Alexander Nottelmann

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
unit SplashThread;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, PngImage, DWMAPI;

type
  TStates = (stFadeIn, stWaiting, stFadeOut);

  TSplashThread = class(TThread)
  private
    FResourceName: string;

    procedure PremultiplyBitmap(Bitmap: TBitmap);
  protected
    procedure Execute; override;
  public
    constructor Create(WindowClass: string; ResourceName: string);
  end;

var
  WndHandle: LongWord;
  BitmapSize: TSize;
  Bitmap: TBitmap;
  BitmapPos: TPoint;
  BlendFunction: TBlendFunction;
  State: TStates;
  WatchWindowClass: string;
  WaitingStarted: Cardinal;

implementation

// TODO: Was bei multi-monitor? der splash muss auf dem bildschirm sein, wo mittelpunkt der mainform drauf ist.
// TODO: screen darf nicht immer angezeigt werden. das prüfen und drüber nachdenken.

function WindowProc(hwn, msg, wpr, lpr: Longint): Longint; stdcall;
var
  H: HWND;
begin
  case msg of
    WM_TIMER:
      begin
        H := FindWindow(PChar(WatchWindowClass), nil);
        case State of
          stFadeIn:
            begin
              if BlendFunction.SourceConstantAlpha < 255 then
              begin
                if BlendFunction.SourceConstantAlpha + 7 > 255 then
                  BlendFunction.SourceConstantAlpha := 255
                else
                  BlendFunction.SourceConstantAlpha := BlendFunction.SourceConstantAlpha + 7;
                UpdateLayeredWindow(WndHandle, 0, nil, @BitmapSize, Bitmap.Canvas.Handle, @BitmapPos, 0, @BlendFunction, ULW_ALPHA);
              end else
              begin
                WaitingStarted := GetTickCount;
                State := stWaiting;
              end;
            end;
          stWaiting:
            begin
              if ((H > 0) and IsWindowVisible(H)) and (GetTickCount > WaitingStarted + 1500) then
                State := stFadeOut;
            end;
          stFadeOut:
            begin
              if BlendFunction.SourceConstantAlpha > 0 then
              begin
                if BlendFunction.SourceConstantAlpha - 7 < 0 then
                  BlendFunction.SourceConstantAlpha := 0
                else
                  BlendFunction.SourceConstantAlpha := BlendFunction.SourceConstantAlpha - 7;
                UpdateLayeredWindow(WndHandle, 0, nil, @BitmapSize, Bitmap.Canvas.Handle, @BitmapPos, 0, @BlendFunction, ULW_ALPHA);
              end else
              begin
                Bitmap.Free;
                KillTimer(WndHandle, 0);
                PostQuitMessage(0);
                SetForegroundWindow(H);
              end;
            end;
        end;
        SetWindowPos(WndHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
      end;
  end;
  Result := DefWindowProc(hwn, msg, wpr, lpr);
end;

{ TSplashThread }

constructor TSplashThread.Create(WindowClass: string; ResourceName: string);
begin
  inherited Create(False);

  FResourceName := ResourceName;

  WatchWindowClass := WindowClass;
end;

procedure TSplashThread.Execute;
var
  Msg: TMsg;
  PngImage: TPngImage;
  WndClass: TWndClass;
  Inst: LongWord;
  ScreenWidth, ScreenHeight: Integer;
  ResStream: TResourceStream;
begin
  inherited;

  Inst := GetModuleHandle(nil);

  with WndClass do
  begin
    style := CS_CLASSDC or CS_PARENTDC;
    lpfnWndProc := @WindowProc;
    hInstance := Inst;
    hbrBackground := GetSysColorBrush(COLOR_3DFACE);
    lpszClassName := 'mistakeSplashScreen';
    hCursor := LoadCursor(0, IDC_ARROW);
  end;

  Windows.RegisterClass(WndClass);

  Bitmap := TBitmap.Create;
  PngImage := TPngImage.Create;
  try
    State := stFadeIn;

    ResStream := TResourceStream.Create(Inst, FResourceName, RT_RCDATA);

    PngImage.LoadFromStream(ResStream);
    bitmap.Assign(PngImage);

    PremultiplyBitmap(Bitmap);

    BitmapPos := Point(0, 0);
    BitmapSize.cx := Bitmap.Width;
    BitmapSize.cy := Bitmap.Height;

    BlendFunction.BlendOp := AC_SRC_OVER;
    BlendFunction.BlendFlags := 0;
    BlendFunction.SourceConstantAlpha := 0;
    BlendFunction.AlphaFormat := AC_SRC_ALPHA;

    ScreenWidth := GetSystemMetrics(SM_CXSCREEN);
    ScreenHeight := GetSystemMetrics(SM_CYSCREEN);

    WndHandle := CreateWindowEx(WS_EX_LAYERED or WS_EX_TOOLWINDOW, 'mistakeSplashScreen', '', WS_VISIBLE or WS_POPUP or WS_CHILD,
      ScreenWidth div 2 - Bitmap.Width div 2, ScreenHeight div 2 - Bitmap.Height div 2,
      Bitmap.Width, Bitmap.Height, 0, 0, Inst, nil);

    SetTimer(WndHandle, 0, 1, nil);
  finally
    PngImage.Free;
  end;

  while GetMessage(Msg, WndHandle, 0, 0) do
  begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;

procedure TSplashThread.PremultiplyBitmap(Bitmap: TBitmap);
var
  Row, Col: Integer;
  P: PRGBQuad;
  PreMult: array[Byte, Byte] of Byte;
begin
  for Row := 0 to 255 do
    for Col := Row to 255 do
    begin
      PreMult[Row, Col] := Row * Col div 255;
      if Row <> Col then
        PreMult[Col, Row] := PreMult[Row, Col];
    end;

  for Row := 0 to Bitmap.Height - 1 do
  begin
    Col := Bitmap.Width;
    P := Bitmap.ScanLine[Row];
    while (Col > 0) do
    begin
      P.rgbBlue := PreMult[P.rgbReserved, P.rgbBlue];
      P.rgbGreen := PreMult[P.rgbReserved, P.rgbGreen];
      P.rgbRed := PreMult[P.rgbReserved, P.rgbRed];
      Inc(P);
      Dec(Col);
    end;
  end;
end;

end.

