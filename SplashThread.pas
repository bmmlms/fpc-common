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

unit SplashThread;

interface

uses
  JwaWinUser,
  Windows,
  Classes,
  DateUtils,
  Graphics,
  LanguageObjects,
  Messages,
  MultiMon,
  SysUtils,
  Types;

type

  { TSplashThread }

  TSplashThread = class(TThread)
  type
    TSplashStates = (ssFadingIn, ssVisible, ssFadingOut);
  private
  const
    FADE_DURATION = 500;
    SHOW_DURATION = 2000;
    SHOW_AFTER_MAIN_DURATION = 1000;
    WINDOW_CLASS = 'mistakeSplashScreen';
  private
    FResourceName: string;
    FVersion: string;

    FHandle: LongWord;
    FFadeStartedAt, FFadeoutAt: TDateTime;

    FBitmapSize: TSize;
    FSplashBitmap: TBitmap;
    FSplashBitmapPos: TPoint;
    FBlendFunction: TBlendFunction;
    FState: TSplashStates;
    FWindowClass: TWndClassEx;
    FStartPos: TRect;
    FMonitors: array of TRect;
    FAppWindow: Cardinal;
    FMainWindowClass: string;
    FMainWindowFound: Boolean;

    class function EnumWindowsProc(hHwnd: HWND; lParam: integer): boolean; stdcall; static;
    class function MonitorEnumProc(hm: HMONITOR; dc: HDC; r: PRect; l: LPARAM): Boolean; stdcall; static;
    class function WndProcWrapper(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; static;

    function FindAppWindow: Boolean;
    procedure FocusAppWindow;
    procedure SetAlpha(Value: Byte);
    function WndProc(uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;

    procedure PremultiplyBitmap(Bitmap: TBitmap);
  protected
    procedure Execute; override;
  public
    constructor Create(WindowClass, ResourceName, Codename, Version, GitSHA: string; MainLeft, MainTop, MainWidth, MainHeight: Integer);
    destructor Destroy; override;
  end;

implementation

{ TSplashThread }

class function TSplashThread.EnumWindowsProc(hHwnd: HWND; lParam: integer): boolean; stdcall;
var
  Pid: DWORD;
  ClassName: string;
  Len: Integer;
  SplashThread: TSplashThread;
begin
  SplashThread := TSplashThread(lParam);

  if (hHwnd = 0) then
    Exit(False);

  Result := True;

  if hHwnd = SplashThread.FHandle then
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

      if ClassName = SplashThread.FMainWindowClass then
        SplashThread.FMainWindowFound := True;

      SplashThread.FAppWindow := hHwnd;
      Result := False;
    end;
  end;
end;

class function TSplashThread.MonitorEnumProc(hm: HMONITOR; dc: HDC; r: PRect; l: LPARAM): Boolean; stdcall;
var
  SplashThread: TSplashThread;
begin
  SplashThread := TSplashThread(l);

  SetLength(SplashThread.FMonitors, Length(SplashThread.FMonitors) + 1);
  PRect(@SplashThread.FMonitors[High(SplashThread.FMonitors)])^ := r^;

  Result := True;
end;

class function TSplashThread.WndProcWrapper(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  Result := TSplashThread(GetWindowLongPtrW(hwnd, GWLP_USERDATA)).WndProc(uMsg, wParam, lParam);
end;

function TSplashThread.FindAppWindow: Boolean;
begin
  EnumWindows(@EnumWindowsProc, THandle(Self));
  Result := FAppWindow > 0;
end;

procedure TSplashThread.FocusAppWindow;
begin
  FindAppWindow;
  if FAppWindow > 0 then
    SetActiveWindow(FAppWindow);
end;

procedure TSplashThread.SetAlpha(Value: Byte);
begin
  FBlendFunction.SourceConstantAlpha := Value;
  UpdateLayeredWindow(FHandle, 0, nil, @FBitmapSize, FSplashBitmap.Canvas.Handle, @FSplashBitmapPos, 0, @FBlendFunction, ULW_ALPHA);
end;

function TSplashThread.WndProc(uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  N: TDateTime;
begin
  case uMsg of
    WM_TIMER:
    begin
      if FAppWindow = 0 then
        FocusAppWindow;

      N := Now;

      case FState of
        ssFadingIn:
          if MilliSecondsBetween(N, FFadeStartedAt) > FADE_DURATION then
          begin
            SetAlpha(High(Byte));
            FState := ssVisible;
            FFadeoutAt := IncMilliSecond(N, SHOW_DURATION);
          end else
            SetAlpha(Trunc((MilliSecondsBetween(N, FFadeStartedAt) / FADE_DURATION) * High(Byte)));
        ssVisible:
        begin
          if N >= FFadeoutAt then
          begin
            FFadeStartedAt := N;
            FState := ssFadingOut;
          end;

          if (FAppWindow > 0) and (FFadeStartedAt < N) then
          begin
            FFadeStartedAt := MaxDateTime;
            FFadeoutAt := IncMilliSecond(N, IfThen<Integer>(FMainWindowFound, SHOW_AFTER_MAIN_DURATION, 0));
          end;
        end;
        ssFadingOut:
          if MilliSecondsBetween(N, FFadeStartedAt) > FADE_DURATION then
          begin
            SetAlpha(Low(Byte));

            KillTimer(FHandle, 0);
            DestroyWindow(FHandle);
          end else
            SetAlpha(High(Byte) - Trunc((MilliSecondsBetween(N, FFadeStartedAt) / FADE_DURATION) * High(Byte)));
      end;

      SetWindowPos(FHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
    end;
    WM_NCDESTROY:
      PostQuitMessage(0);
  end;

  Result := DefWindowProc(FHandle, uMsg, wParam, lParam);
end;

constructor TSplashThread.Create(WindowClass, ResourceName, Codename, Version, GitSHA: string; MainLeft, MainTop, MainWidth, MainHeight: Integer);
begin
  inherited Create(False);

  FreeOnTerminate := True;

  FSplashBitmap := TBitmap.Create;
  FMainWindowClass := WindowClass;
  FResourceName := ResourceName;
  FStartPos := TRect.Create(MainLeft, MainTop, MainLeft + MainWidth, MainTop + MainHeight);

  FVersion := '© 2010-2021 A. Nottelmann et al. - V' + Version;
  if GitSHA <> '' then
    FVersion := FVersion + '-%s'.Format([GitSHA]);
  if Codename <> '' then
    FVersion := FVersion + ' ''%s'''.Format([Codename]);
end;

destructor TSplashThread.Destroy;
begin
  UnregisterClassA(WINDOW_CLASS, HINSTANCE);

  FSplashBitmap.Free;

  inherited Destroy;
end;

procedure TSplashThread.Execute;
var
  i: Integer;
  Msg: TMsg;
  PngImage: TPortableNetworkGraphic;
  ResStream: TResourceStream;
  Monitor: Integer;
  DummyRect: TRect;
begin
  // Falls direkt was passiert, keinen Splash zeigen.
  Sleep(200);
  if FindAppWindow then
    Exit;

  SetLength(FMonitors, 0);
  EnumDisplayMonitors(0, nil, @MonitorEnumProc, THandle(Self));

  Monitor := -1;
  for i := 0 to High(FMonitors) do
    if PtInRect(FMonitors[i], CenterPoint(FStartPos)) then
    begin
      Monitor := i;
      Break;
    end;

  if Monitor = -1 then
    for i := 0 to High(FMonitors) do
      if IntersectRect(DummyRect, FMonitors[i], FStartPos) then
      begin
        Monitor := i;
        Break;
      end;

  if Monitor = -1 then
    Exit;

  FWindowClass.cbSize := SizeOf(TWndClassEx);
  FWindowClass.style := CS_CLASSDC or CS_PARENTDC;
  FWindowClass.lpfnWndProc := @DefWindowProcW;
  FWindowClass.hInstance := HINSTANCE;
  FWindowClass.hCursor := LoadCursor(0, IDC_ARROW);
  FWindowClass.hbrBackground := COLOR_APPWORKSPACE;
  FWindowClass.lpszClassName := WINDOW_CLASS;
  RegisterClassEx(FWindowClass);

  PngImage := TPortableNetworkGraphic.Create;
  ResStream := TResourceStream.Create(HINSTANCE, FResourceName, RT_RCDATA);
  try
    PngImage.LoadFromStream(ResStream);

    FSplashBitmap.Assign(PngImage);

    PremultiplyBitmap(FSplashBitmap);

    FSplashBitmap.Canvas.Font.Name := 'Tahoma';
    FSplashBitmap.Canvas.Font.Color := clWhite;
    FSplashBitmap.Canvas.Font.Size := 8;
    SetBkMode(FSplashBitmap.Canvas.Handle, TRANSPARENT);
    FSplashBitmap.Canvas.TextOut(FSplashBitmap.Width - FSplashBitmap.Canvas.TextWidth(FVersion) - 25,
      FSplashBitmap.Height - FSplashBitmap.Canvas.TextHeight(FVersion) - 20, FVersion);

    FBitmapSize.cx := FSplashBitmap.Width;
    FBitmapSize.cy := FSplashBitmap.Height;

    FBlendFunction.BlendOp := AC_SRC_OVER;
    FBlendFunction.AlphaFormat := AC_SRC_ALPHA;

    FHandle := CreateWindowEx(WS_EX_LAYERED or WS_EX_TOOLWINDOW, WINDOW_CLASS, '', WS_VISIBLE or WS_POPUP or WS_CHILD, (FMonitors[Monitor].Left + Abs(FMonitors[Monitor].Right - FMonitors[Monitor].Left) div 2) -
      FSplashBitmap.Width div 2, (FMonitors[Monitor].Top + Abs(FMonitors[Monitor].Bottom - FMonitors[Monitor].Top) div 2) - FSplashBitmap.Height div 2, FSplashBitmap.Width, FSplashBitmap.Height, 0, 0, HINSTANCE, nil);
    if FHandle = 0 then
      raise Exception.Create('CreateWindowExW() failed: %d'.Format([GetLastError]));

    SetWindowLongPtrW(FHandle, GWLP_USERDATA, THandle(Self));
    if GetLastError <> 0 then
      raise Exception.Create('SetWindowLongPtrW() failed: %d'.Format([GetLastError]));

    SetLastError(0);
    if (SetWindowLongPtrW(FHandle, GWLP_WNDPROC, LONG_PTR(@WndProcWrapper)) = 0) and (GetLastError <> 0) then
      raise Exception.Create('SetWindowLongPtrW() failed: %d'.Format([GetLastError]));

    SetAlpha(0);

    SetTimer(FHandle, 0, 20, nil);
  finally
    PngImage.Free;
    ResStream.Free;
  end;

  FFadeStartedAt := Now;

  while Integer(GetMessage(Msg, FHandle, 0, 0)) > 0 do
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
    while Col > 0 do
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
