unit WindowsFunctions;

interface

uses
  Windows;

type
  TShutdownBlockReasonCreate = function(hWnd: HWND; pwszReason: LPCWSTR): BOOL; stdcall;
  TShutdownBlockReasonDestroy = function(hWnd: HWND): BOOL; stdcall;

var
  ShutdownBlockReasonCreate: TShutdownBlockReasonCreate;
  ShutdownBlockReasonDestroy: TShutdownBlockReasonDestroy;

implementation

var
  User32Handle: THandle;
initialization
  ShutdownBlockReasonCreate := nil;
  ShutdownBlockReasonDestroy := nil;

  User32Handle := LoadLibrary(user32);
  if User32Handle <> 0 then
  begin
    ShutdownBlockReasonCreate := GetProcAddress(User32Handle, 'ShutdownBlockReasonCreate');
    ShutdownBlockReasonDestroy := GetProcAddress(User32Handle, 'ShutdownBlockReasonDestroy');
  end;
end.
