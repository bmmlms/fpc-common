{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2016 Alexander Nottelmann

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

unit FileWatcher;

interface

uses
  Windows, SysUtils, Classes, SyncObjs, ShellApi, ComCtrls, Logging;

type
  TFileWatchEvent = procedure(Sender: TObject; Action: DWORD; RootDir, OldName, NewName: string) of object;

  TFileWatcher = class(TThread)
  private
    FPath: string;
    FFilename: string;
    FFilenameNew: string;
    FAction: DWORD;
    FFilter: DWORD;
    FTermEvent: TEvent;

    FOnEvent: TFileWatchEvent;

    procedure TriggerEvent;
  public
    constructor Create(const Path: string; Filter: DWORD);
    procedure Execute; override;
    procedure Terminate; reintroduce;

    property OnEvent: TFileWatchEvent read FOnEvent write FOnEvent;
  end;

  PFILE_NOTIFY_INFORMATION = ^FILE_NOTIFY_INFORMATION;
  FILE_NOTIFY_INFORMATION = packed record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FilenameLength: DWORD;
    Filename: WideString;
  end;

const
  WaitDir = WAIT_OBJECT_0;
  WaitTerm = WAIT_OBJECT_0 + 1;
  FILE_LIST_DIRECTORY = $0001;

implementation

constructor TFileWatcher.Create(const Path: string; Filter: DWORD);
begin
  inherited Create(True);
  FPath := IncludeTrailingBackslash(Path);
  FFilter := Filter;
  FreeOnTerminate := True;
end;

procedure TFileWatcher.Execute;
var
  WatchHandle: DWORD;
  Buffer: Pointer;
  BufLen: DWORD;
  Read: DWORD;
  Info: PFILE_NOTIFY_INFORMATION;
  NextOffset: DWORD;
  NameLen: DWORD;
  Overlap: TOverlapped;
  WaitResult: DWORD;
  EventArray: array [0..1] of THandle;
  FileEvent: TEvent;
begin
  try
    WatchHandle := CreateFile(PChar(FPath), FILE_LIST_DIRECTORY or GENERIC_READ,
      FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
      OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);

    if (WatchHandle = INVALID_HANDLE_VALUE) or (WatchHandle = 0) then
      Exit;

    FileEvent := TEvent.Create(nil, False, False, '');
    Overlap.hEvent := FileEvent.Handle;

    FTermEvent := TEvent.Create(nil, False, False, '');

    EventArray[0] := FileEvent.Handle;
    EventArray[1] := FTermEvent.Handle;

    BufLen := 65535;
    Buffer := AllocMem(BufLen);
    try
      while not Terminated do
      begin
        Read := 0;

        if ReadDirectoryChangesW(WatchHandle, Buffer, BufLen, True, FFilter, @Read, @Overlap, nil) then
        begin
          WaitResult := WaitForMultipleObjects(2, @EventArray, False, INFINITE);
          case WaitResult of
            WaitDir:
              begin
                Info := Buffer;
                repeat
                  NextOffset := Info.NextEntryOffset;
                  FAction := Info.Action;
                  NameLen := Info.FilenameLength;

                  // Wenn man im Explorer die File-Properties anzeigt und ID3-Tags ändert,
                  // löscht der die Datei und stellt sie dann wieder her. Das Sleep() ist doof,
                  // aber mir fällt auf die Schnelle nichts besseres ein...
                  if FAction = FILE_ACTION_REMOVED then
                    Sleep(100);

                  // Die letzte Bedingung ist für MusicBee. Da wird die Datei umbenannt und dann neu angelegt,
                  // das ignorieren wir dann.
                  if (((FAction = FILE_ACTION_REMOVED) and (not FileExists(FPath + WideCharLenToString(@Info.FileName, NameLen div 2)))) or
                       (FAction <> FILE_ACTION_REMOVED) and
                      not ((FAction = FILE_ACTION_ADDED) and (FPath + WideCharLenToString(@Info.FileName, NameLen div 2) = FFilename))) then
                  begin
                    if FAction <> FILE_ACTION_RENAMED_NEW_NAME then
                      FFilename := WideCharLenToString(@Info.FileName, NameLen div 2)
                    else
                      FFilenameNew := WideCharLenToString(@Info.Filename, NameLen div 2);

                    if Assigned(FOnEvent) then
                      Synchronize(TriggerEvent);
                  end;

                  Info := Pointer(Int64(Info) + NextOffset);

                  if Terminated then
                    Break;

                until NextOffset = 0;
              end;
            WaitTerm:
              begin
                Break;
              end
          else
            Break;
          end;
        end;
      end;
    finally
      CloseHandle(WatchHandle);
      FileEvent.Free;
      FTermEvent.Free;
      FreeMem(Buffer, BufLen);
    end;
  except

  end;
end;

procedure TFileWatcher.Terminate;
begin
  if FTermEvent <> nil then
    FTermEvent.SetEvent;

  inherited;
end;

procedure TFileWatcher.TriggerEvent;
begin
  if Assigned(FOnEvent) then
    FOnEvent(Self, FAction, FPath, FFilename, FFilenameNew);
end;

end.
