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

unit ExceptionHandler;

interface

uses
  Classes,
  Forms,
  Functions,
  shlobj,
  SysUtils,
  Windows;

type

  { TExceptionHandler }

  TExceptionHandler = class
  private
    procedure HandleException(Sender: TObject; E: Exception);
  public
    constructor Create;
  end;

implementation

var
  DesktopDir, AppVersion: string;

procedure UnhandledException(Obj: TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer);
var
  i: LongInt;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('Version: %s'.Format([AppVersion]));
    if Obj is Exception then
      SL.Add('%s: %s'.Format([Exception(Obj).ClassName, Exception(Obj).Message]));
    SL.Add('  %s'.Format([StringReplace(Trim(BackTraceStrFunc(Addr)), '  ', ' ', [rfReplaceAll])]));
    for i := 0 to FrameCount - 1 do
      SL.Add('  %s'.Format([StringReplace(Trim(BackTraceStrFunc(Frames[i])), '  ', ' ', [rfReplaceAll])]));

    try
      SL.SaveToFile(ConcatPaths([DesktopDir, 'crash_%s.txt'.Format([FormatDateTime('hhnnss', Now)])]));
      TFunctions.MsgBox('An unhandled exception occurred and the application needs to exit. A file containing information about the crash was saved to the desktop.', 'Error', MB_ICONERROR);
    except
      TFunctions.MsgBox('An unhandled exception occurred and the application needs to exit.'#13#10'%s'.Format([SL.Text]), 'Error', MB_ICONERROR);
    end;
  finally
    SL.Free;
  end;

  TerminateProcess(GetCurrentProcess, 1);
end;

{ TExceptionHandler }

procedure TExceptionHandler.HandleException(Sender: TObject; E: Exception);
begin
  UnhandledException(E, ExceptAddr, ExceptFrameCount, ExceptFrames);
end;

constructor TExceptionHandler.Create;
begin
  DesktopDir := TFunctions.GetShellFolder(CSIDL_DESKTOPDIRECTORY);
  AppVersion := TFunctions.GetFileVersion(Application.ExeName).AsString;

  ExceptProc := @UnhandledException;
  Application.AddOnExceptionHandler(HandleException);
end;

end.
