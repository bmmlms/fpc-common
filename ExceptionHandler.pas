{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2026 Alexander Nottelmann

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

const
  TH32CS_SNAPTHREAD = $00000004;
  THREAD_SUSPEND_RESUME = $0002;

type
  TThreadEntry32 = record
    dwSize: DWORD;
    cntUsage: DWORD;
    th32ThreadID: DWORD;
    th32OwnerProcessID: DWORD;
    tpBasePri: Longint;
    tpDeltaPri: Longint;
    dwFlags: DWORD;
  end;

function CreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWORD): THandle; stdcall; external kernel32;
function Thread32First(hSnapshot: THandle; var lpte: TThreadEntry32): BOOL; stdcall; external kernel32;
function Thread32Next(hSnapshot: THandle; var lpte: TThreadEntry32): BOOL; stdcall; external kernel32;
function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwThreadId: DWORD): THandle; stdcall; external kernel32;

var
  DesktopDir, ProductVersion: string;

procedure SuspendOtherThreads;
var
  Snapshot: THandle;
  Entry: TThreadEntry32;
  ThreadHandle: THandle;
  CurrentThreadID, CurrentProcessID: DWORD;
begin
  CurrentThreadID := GetCurrentThreadId;
  CurrentProcessID := GetCurrentProcessId;

  Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if Snapshot <> INVALID_HANDLE_VALUE then
  begin
    Entry.dwSize := SizeOf(TThreadEntry32);
    if Thread32First(Snapshot, Entry) then
      repeat
        if (Entry.th32OwnerProcessID = CurrentProcessID) and (Entry.th32ThreadID <> CurrentThreadID) then
        begin
          ThreadHandle := OpenThread(THREAD_SUSPEND_RESUME, False, Entry.th32ThreadID);
          if ThreadHandle <> 0 then
          begin
            SuspendThread(ThreadHandle);
            CloseHandle(ThreadHandle);
          end;
        end;
      until not Thread32Next(Snapshot, Entry);
    CloseHandle(Snapshot);
  end;
end;

function MsgBoxThread(Text: PChar): ptrint;
begin
  MessageBox(0, Text, 'Error', MB_ICONERROR);
  Result := 0;
end;

procedure ThreadedMsgBox(const Text: string);
var
  ThreadHandle: THandle;
  ThreadID: TThreadID;
begin
  ThreadHandle := BeginThread(@MsgBoxThread, PChar(Text), ThreadID);
  if ThreadHandle <> 0 then
  begin
    WaitForSingleObject(ThreadHandle, INFINITE);
    CloseHandle(ThreadHandle);
  end;
end;

procedure UnhandledException(Obj: TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer);
const
  Architecture: string = {$IF defined(CPU64)}'x86_64'{$ELSEIF defined(CPU32)}'i386'{$ELSE}{$ERROR Unknown architecture}{$ENDIF};
var
  i: LongInt;
  SL: TStringList;
begin
  SuspendOtherThreads;

  SL := TStringList.Create;
  try
    SL.Add('Version: %s (%s)'.Format([ProductVersion, Architecture]));
    if Obj is Exception then
      SL.Add('%s: %s'.Format([Exception(Obj).ClassName, Exception(Obj).Message]));
    SL.Add('  %s'.Format([StringReplace(Trim(BackTraceStrFunc(Addr)), '  ', ' ', [rfReplaceAll])]));
    for i := 0 to FrameCount - 1 do
      SL.Add('  %s'.Format([StringReplace(Trim(BackTraceStrFunc(Frames[i])), '  ', ' ', [rfReplaceAll])]));

    try
      SL.SaveToFile(ConcatPaths([DesktopDir, 'crash_%s.txt'.Format([FormatDateTime('hhnnss', Now)])]));
      ThreadedMsgBox('An unhandled exception occurred and the application needs to exit. A file containing information about the crash was saved to the desktop.');
    except
      ThreadedMsgBox('An unhandled exception occurred and the application needs to exit.'#13#10'%s'.Format([SL.Text]));
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
  ProductVersion := TFunctions.GetProductVersion;

  ExceptProc := @UnhandledException;
  Application.AddOnExceptionHandler(HandleException);
end;

end.
