{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2013 Alexander Nottelmann

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
  Windows, Messages, SysUtils, Classes, Graphics, PngImage, MultiMon;

type
  TStates = (stFadeIn, stWaiting, stFadeOut);

  TSplashThread = class(TThread)
  private
    FResourceName: string;

    procedure PremultiplyBitmap(Bitmap: TBitmap);
  protected
    procedure Execute; override;
  public
    constructor Create(WindowClass: string; ResourceName: string; MainLeft, MainTop, MainWidth, MainHeight: Integer);
  end;

const
  FADE_TIME = 1000;
  FADE_WAIT_TIME = 1700;

var
  SplashWndHandle: LongWord;
  BitmapSize: TSize;
  SplashBitmap: TBitmap;
  SplashBitmapPos: TPoint;
  BlendFunction: TBlendFunction;
  State: TStates;
  WaitingStarted: Cardinal;
  StartPosLeft, StartPosTop, StartPosWidth, StartPosHeight: Integer;
  Monitors: array of TRect;
  Killed: Boolean;
  AnimationStart: Cardinal;
  EnumFoundWindow: Cardinal;
  FadeOutWaitStart: Cardinal;
  FocusWasSet: Boolean;
  MainWindowClass: string;
  FadeoutWait: Boolean;

implementation

function EnumWindowsProc(hHwnd: HWND; lParam : integer): boolean; stdcall;
var
  Pid: DWORD;
  ClassName: string;
  Len: Integer;
begin
  if (hHwnd = 0) then
  begin
    Result := False;
  end
  else
  begin
    Result := True;

    if hHwnd = SplashWndHandle then
      Exit;

    SetLength(ClassName, 255);
    GetWindowThreadProcessId(hHwnd, Pid);
    if (Pid = GetCurrentProcessId) and (IsWindowVisible(hHwnd)) then
    begin
      Len := GetClassName(hHwnd, @ClassName[1], 255);
      if Len > 0 then
      begin
        SetLength(ClassName, Len);
        if ClassName = 'TApplication' then
          Exit;
        if ClassName = MainWindowClass then
          FadeoutWait := True;

        EnumFoundWindow := hHwnd;
        Result := False;
      end;
    end;
  end;
end;

function GetWindow: Boolean;
begin
  EnumWindows(@EnumWindowsProc, 0);
  Result := EnumFoundWindow > 0;
end;

procedure FocusFoundWindow;
begin
  GetWindow;
  if EnumFoundWindow > 0 then
  begin
    FocusWasSet := True;
    SetForegroundWindow(EnumFoundWindow);
  end;
end;

function WindowProc(hwn, msg, wpr, lpr: Longint): Longint; stdcall;
var
  Val: Cardinal;
  TC: Cardinal;
begin
  if Killed then
  begin
    Result := 0;
    Exit;
  end;

  case msg of
    WM_TIMER:
      begin
        if not FocusWasSet then
          FocusFoundWindow;

        TC := GetTickCount;
        if AnimationStart = 0 then
          AnimationStart := GetTickCount;

        case State of
          stFadeIn:
            begin
              Val := Trunc(((TC - AnimationStart) / FADE_TIME) * 254);

              if TC - AnimationStart < FADE_TIME then
              begin
                BlendFunction.SourceConstantAlpha := Val;
                UpdateLayeredWindow(SplashWndHandle, 0, nil, @BitmapSize, SplashBitmap.Canvas.Handle, @SplashBitmapPos, 0, @BlendFunction, ULW_ALPHA);
              end else
              begin
                WaitingStarted := TC;
                State := stWaiting;
              end;
            end;
          stWaiting:
            begin
              if FocusWasSet then
              begin
                if FadeOutWaitStart = High(Cardinal) then
                  if FadeoutWait then
                  begin
                    FadeOutWaitStart := TC;
                  end else
                  begin
                    State := stFadeOut;
                    AnimationStart := TC;
                  end;

                if (FadeOutWaitStart <> High(Cardinal)) and (TC - FadeOutWaitStart > FADE_WAIT_TIME) then
                begin
                  State := stFadeOut;
                  AnimationStart := TC;
                end;
              end;
            end;
          stFadeOut:
            begin
              Val := 254 - Trunc(((TC - AnimationStart) / FADE_TIME) * 254);

              if TC - AnimationStart < FADE_TIME then
              begin
                BlendFunction.SourceConstantAlpha := Val;
                UpdateLayeredWindow(SplashWndHandle, 0, nil, @BitmapSize, SplashBitmap.Canvas.Handle, @SplashBitmapPos, 0, @BlendFunction, ULW_ALPHA);
              end else
              begin
                SplashBitmap.Free;
                KillTimer(SplashWndHandle, 0);
                Killed := True;
              end;
            end;
        end;
        SetWindowPos(SplashWndHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
      end;
  end;
  Result := DefWindowProc(hwn, msg, wpr, lpr);
end;

function MonitorEnumProc(hm: HMONITOR; dc: HDC; r: PRect; l: LPARAM): Boolean; stdcall;
begin
  SetLength(Monitors, Length(Monitors) + 1);
  Monitors[High(Monitors)].Left := r.Left;
  Monitors[High(Monitors)].Top := r.Top;
  Monitors[High(Monitors)].Right := r.Right;
  Monitors[High(Monitors)].Bottom := r.Bottom;
  Monitors[High(Monitors)].TopLeft := r.TopLeft;
  Monitors[High(Monitors)].BottomRight := r.BottomRight;
  Result := True;
end;

{ TSplashThread }

constructor TSplashThread.Create(WindowClass: string; ResourceName: string; MainLeft, MainTop, MainWidth, MainHeight: Integer);
begin
  inherited Create(False);

  FreeOnTerminate := True;

  FadeoutWait := False;

  MainWindowClass := WindowClass;

  FResourceName := ResourceName;

  StartPosLeft := MainLeft;
  StartPosTop := MainTop;
  StartPosWidth := MainWidth;
  StartPosHeight := MainHeight;
  Killed := False;
end;

procedure TSplashThread.Execute;
var
  i: Integer;
  Msg: TMsg;
  PngImage: TPngImage;
  WndClass: TWndClassEx;
  ResStream: TResourceStream;
  Monitor: Integer;
  DummyRect: TRect;
begin
  inherited;

  // Falls direkt was passiert, keinen Splash zeigen.
  Sleep(200);
  if GetWindow then
    Exit;

  FocusWasSet := False;
  FadeOutWaitStart := High(Cardinal);
  SetLength(Monitors, 0);
  EnumDisplayMonitors(0, nil, @MonitorEnumProc, 0);

  Monitor := -1;
  for i := 0 to High(Monitors) do
    if PtInRect(Monitors[i], Point(StartPosLeft + StartPosWidth div 2, StartPosTop + StartPosHeight div 2)) then
    begin
      Monitor := i;
      Break;
    end;

  if Monitor = -1 then
    for i := 0 to High(Monitors) do
      if IntersectRect(DummyRect, Monitors[i], Rect(StartPosLeft, StartPosTop, StartPosWidth + StartPosLeft, StartPosHeight + StartPosTop)) then
      begin
        Monitor := i;
        Break;
      end;

  if Monitor = -1 then
    Exit;

  WndClass.cbSize := SizeOf(TWndClassEx);
  WndClass.style := CS_CLASSDC or CS_PARENTDC;
  WndClass.lpfnWndProc := @WindowProc;
  WndClass.cbClsExtra := 0;
  WndClass.cbWndExtra := 0;
  WndClass.hInstance := HInstance;
  WndClass.hIcon := 0;
  WndClass.hCursor := LoadCursor(0, IDC_ARROW);
  WndClass.hbrBackground := COLOR_APPWORKSPACE;
  WndClass.lpszMenuName := nil;
  WndClass.lpszClassName := 'mistakeSplashScreen';
  WndClass.hIconSm := 0;

  RegisterClassEx(WndClass);

  SplashBitmap := TBitmap.Create;
  PngImage := TPngImage.Create;
  ResStream := TResourceStream.Create(HInstance, FResourceName, RT_RCDATA);
  try
    PngImage.LoadFromStream(ResStream);
    SplashBitmap.Assign(PngImage);

    PremultiplyBitmap(SplashBitmap);

    SplashBitmapPos := Point(0, 0);
    BitmapSize.cx := SplashBitmap.Width;
    BitmapSize.cy := SplashBitmap.Height;

    BlendFunction.BlendOp := AC_SRC_OVER;
    BlendFunction.BlendFlags := 0;
    BlendFunction.SourceConstantAlpha := 0;
    BlendFunction.AlphaFormat := AC_SRC_ALPHA;

    SplashWndHandle := CreateWindowEx(WS_EX_LAYERED or WS_EX_TOOLWINDOW, 'mistakeSplashScreen', '', WS_VISIBLE or WS_POPUP or WS_CHILD,
      (Monitors[Monitor].Left + Abs(Monitors[Monitor].Right - Monitors[Monitor].Left) div 2) - SplashBitmap.Width div 2,
      (Monitors[Monitor].Top + Abs(Monitors[Monitor].Bottom - Monitors[Monitor].Top) div 2) - SplashBitmap.Height div 2,
      SplashBitmap.Width, SplashBitmap.Height, 0, 0, HInstance, nil);

    UpdateLayeredWindow(SplashWndHandle, 0, nil, @BitmapSize, SplashBitmap.Canvas.Handle, @SplashBitmapPos, 0, @BlendFunction, ULW_ALPHA);

    SetTimer(SplashWndHandle, 0, 30, nil);
  finally
    PngImage.Free;
    ResStream.Free;
  end;

  AnimationStart := 0;

  while (not Killed) and GetMessage(Msg, SplashWndHandle, 0, 0) do
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

