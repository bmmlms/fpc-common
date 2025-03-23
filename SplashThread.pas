{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2025 Alexander Nottelmann

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
  Classes,
  DateUtils,
  Graphics,
  LanguageObjects,
  MultiMon,
  SysUtils,
  Types,
  Windows;

type

  { TSplashThread }

  TSplashThread = class(TThread)
    type
      TSplashStates = (ssFadingIn, ssVisible, ssFadingOut);
  private
  const
    FADE_DURATION = 400;
    SHOW_DURATION = 2500;
    SHOW_AFTER_MAIN_DURATION = 1300;
    WINDOW_CLASS = 'mistakeSplashScreen';
  private
    FResourceName: string;
    FVersion: string;

    FHandle: LongWord;
    FFadeStartedAt, FFadeoutAt: TDateTime;

    FSplashImage: TPortableNetworkGraphic;
    FState: TSplashStates;
    FWindowClass: TWndClassEx;
    FStartPos: TRect;
    FMonitors: array of TRect;
    FAppWindow: Cardinal;
    FMainWindowClass: string;
    FMainWindowFound: Boolean;

    class function EnumWindowsProc(hHwnd: HWND; lParam: Integer): boolean; stdcall; static;
    class function MonitorEnumProc(hm: HMONITOR; dc: HDC; r: PRect; l: LPARAM): Boolean; stdcall; static;
    class function WndProcWrapper(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; static;

    function FindAppWindow: Boolean;
    procedure FocusAppWindow;
    procedure SetAlpha(Value: Byte);
    function WndProc(uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
  protected
    procedure Execute; override;
  public
    constructor Create(WindowClass, ResourceName, Codename, Version: string; MainLeft, MainTop, MainWidth, MainHeight: Integer);
    destructor Destroy; override;
  end;

implementation

{ TSplashThread }

class function TSplashThread.EnumWindowsProc(hHwnd: HWND; lParam: Integer): boolean; stdcall;
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
    SetForegroundWindow(FAppWindow);
end;

procedure TSplashThread.SetAlpha(Value: Byte);
begin
  SetLayeredWindowAttributes(FHandle, 0, Value, LWA_ALPHA);
end;

function TSplashThread.WndProc(uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  N: TDateTime;
  H: THandle;
  PaintInfo: PAINTSTRUCT;
begin
  case uMsg of
    WM_PAINT:
    begin
      H := BeginPaint(FHandle, PaintInfo);
      try
        BitBlt(H, 0, 0, FSplashImage.Width, FSplashImage.Height, FSplashImage.Canvas.Handle, 0, 0, SRCCOPY);
        FrameRect(H, RECT.Create(0, 0, FSplashImage.Width, FSplashImage.Height), GetStockObject(DKGRAY_BRUSH));
      finally
        EndPaint(FHandle, PaintInfo);
      end;
    end;
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

      if FState <> ssVisible then
        InvalidateRect(FHandle, nil, False);
    end;
    WM_NCDESTROY:
      PostQuitMessage(0);
  end;

  Result := DefWindowProc(FHandle, uMsg, wParam, lParam);
end;

constructor TSplashThread.Create(WindowClass, ResourceName, Codename, Version: string; MainLeft, MainTop, MainWidth, MainHeight: Integer);
begin
  inherited Create(False);

  FreeOnTerminate := True;

  FSplashImage := TPortableNetworkGraphic.Create;
  FMainWindowClass := WindowClass;
  FResourceName := ResourceName;
  FStartPos := TRect.Create(MainLeft, MainTop, MainLeft + MainWidth, MainTop + MainHeight);

  FVersion := '© 2010-2025 A. Nottelmann et al. - V' + Version;
  if Codename <> '' then
    FVersion := FVersion + ' ''%s'''.Format([Codename]);
end;

destructor TSplashThread.Destroy;
begin
  UnregisterClassA(WINDOW_CLASS, HINSTANCE);

  FSplashImage.Free;

  inherited Destroy;
end;

procedure TSplashThread.Execute;
var
  i: Integer;
  Msg: TMsg;
  ResStream: TResourceStream;
  Monitor: Integer;
  DummyRect: TRect;
begin
  // Falls direkt was passiert, keinen Splash zeigen.
  Sleep(200);
  if FindAppWindow then
    Exit;

  FMonitors := [];
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

  ResStream := TResourceStream.Create(HINSTANCE, FResourceName, Windows.RT_RCDATA);
  try
    FSplashImage.LoadFromStream(ResStream);

    FSplashImage.Canvas.Font.Quality := fqCleartypeNatural;
    FSplashImage.Canvas.Font.Color := clWhite;
    FSplashImage.Canvas.Font.Size := 7;
    FSplashImage.Canvas.AntialiasingMode := amOn;
    SetBkMode(FSplashImage.Canvas.Handle, TRANSPARENT);

    FSplashImage.Canvas.TextOut(FSplashImage.Width - FSplashImage.Canvas.TextWidth(FVersion) - 8, FSplashImage.Height - FSplashImage.Canvas.TextHeight(FVersion) - 8, FVersion);

    FHandle := CreateWindowEx(WS_EX_LAYERED or WS_EX_TOOLWINDOW, WINDOW_CLASS, '', WS_VISIBLE or WS_POPUP or WS_CHILD, (FMonitors[Monitor].Left + Abs(FMonitors[Monitor].Right - FMonitors[Monitor].Left) div 2) -
      FSplashImage.Width div 2, (FMonitors[Monitor].Top + Abs(FMonitors[Monitor].Bottom - FMonitors[Monitor].Top) div 2) - FSplashImage.Height div 2, FSplashImage.Width, FSplashImage.Height, 0, 0, HINSTANCE, nil);
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
    ResStream.Free;
  end;

  FFadeStartedAt := Now;

  while Integer(GetMessage(Msg, FHandle, 0, 0)) > 0 do
  begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;

end.
