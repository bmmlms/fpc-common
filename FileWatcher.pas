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

unit FileWatcher;

interface

uses
  Classes,
  ComCtrls,
  Generics.Collections,
  JwaWinNT,
  Logging,
  SysUtils,
  Windows;

type
  TFileWatcherEventActions = (eaAdded, eaRemoved, eaModified, eaMoved);

  TFileWatcherEvent = procedure(Sender: TObject; Action: TFileWatcherEventActions; Path, PathNew: string) of object;

  { TDeletedFile }

  TDeletedFile = record
  public
    Expires: Int64;
    Path: string;

    constructor Create(Path: string);
  end;

  { TFileWatcher }

  TFileWatcher = class(TThread)
  private
  const
    WAIT_TERMINATE = WAIT_OBJECT_0;
    WAIT_DIR = WAIT_OBJECT_0 + 1;
    BUFFER_LEN = 65535;
  private
    FWatchPath: string;
    FPath, FPathNew: string;
    FAction: TFileWatcherEventActions;
    FFilter: DWORD;

    FTermEvent: THandle;
    FDeleted: TList<TDeletedFile>;

    FOnEvent: TFileWatcherEvent;

    procedure TriggerEvent;
  protected
    procedure Execute; override;
    procedure DoChangeDetected(Path: string; Action: DWORD); virtual;
    procedure ProcessDeleted; virtual;
  public
    constructor Create(WatchPath: string; Filter: DWORD); virtual;
    destructor Destroy; override;
    procedure Terminate; reintroduce;

    property OnEvent: TFileWatcherEvent read FOnEvent write FOnEvent;
  end;

implementation

{ TDeletedFile }

constructor TDeletedFile.Create(Path: string);
begin
  Self.Path := Path;
  Expires := GetTickCount64 + 1000;
end;

constructor TFileWatcher.Create(WatchPath: string; Filter: DWORD);
begin
  inherited Create(True);

  FWatchPath := WatchPath;
  FFilter := Filter;

  FTermEvent := CreateEvent(nil, False, False, nil);
  FDeleted := TList<TDeletedFile>.Create;

  FreeOnTerminate := True;
end;

destructor TFileWatcher.Destroy;
begin
  FDeleted.Free;
  CloseHandle(FTermEvent);

  inherited Destroy;
end;

procedure TFileWatcher.Execute;
var
  DirHandle, BytesRead, NextOffset, WaitResult: DWORD;
  Buffer: Pointer;
  Info: PFILE_NOTIFY_INFORMATION;
  Overlap: TOverlapped;
  EventArray: array [0..1] of THandle;
  ChangeEvent: THandle;
  Path: string;
begin
  ChangeEvent := CreateEvent(nil, False, False, nil);

  Overlap.hEvent := ChangeEvent;

  EventArray[0] := FTermEvent;
  EventArray[1] := ChangeEvent;

  try
    while not Terminated do
    begin
      DirHandle := CreateFile(PChar(FWatchPath), FILE_LIST_DIRECTORY or GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);

      if (DirHandle = INVALID_HANDLE_VALUE) or (DirHandle = 0) then
      begin
        if WaitForSingleObject(FTermEvent, 2000) = WAIT_TERMINATE then
          Exit;

        Continue;
      end;

      Buffer := AllocMem(BUFFER_LEN);
      try
        while not Terminated do
        begin
          if not ReadDirectoryChangesW(DirHandle, Buffer, BUFFER_LEN, True, FFilter, @BytesRead, @Overlap, nil) then
          begin
            if WaitForSingleObject(FTermEvent, 2000) = WAIT_TERMINATE then
              Exit;

            Break;
          end;

          WaitResult := WaitForMultipleObjects(2, @EventArray, False, IfThen<DWORD>(FDeleted.Count > 0, 1000, INFINITE));
          case WaitResult of
            WAIT_DIR:
            begin
              Info := Buffer;
              repeat
                if Info.Action = 0 then
                  Break;

                NextOffset := Info.NextEntryOffset;
                Path := ConcatPaths([FWatchPath, WideCharLenToString(@Info.FileName, Info.FileNameLength div 2)]);

                DoChangeDetected(Path, Info.Action);

                Info := Pointer(Info) + NextOffset;
              until NextOffset = 0;

              if Info.Action = 0 then
                Break;
            end;
            WAIT_TERMINATE:
              Exit;
          end;

          ProcessDeleted;
        end;
      finally
        CloseHandle(DirHandle);
        FreeMem(Buffer);
      end;
    end;
  finally
    CloseHandle(ChangeEvent);
  end;
end;

procedure TFileWatcher.Terminate;
begin
  if FTermEvent > 0 then
    SetEvent(FTermEvent);

  inherited;
end;

procedure TFileWatcher.TriggerEvent;
begin
  if (not Terminated) and Assigned(FOnEvent) then
    FOnEvent(Self, FAction, FPath, FPathNew);
end;

procedure TFileWatcher.DoChangeDetected(Path: string; Action: DWORD);
var
  i: Integer;
begin
  case Action of
    FILE_ACTION_REMOVED:
      FDeleted.Add(TDeletedFile.Create(Path));
    FILE_ACTION_ADDED:
    begin
      for i := 0 to FDeleted.Count - 1 do
        if FDeleted[i].Path = Path then
        begin
          FAction := eaModified;
          FPath := Path;

          FDeleted.Delete(i);

          Synchronize(TriggerEvent);

          Exit;
        end;

      for i := 0 to FDeleted.Count - 1 do
        if ExtractFileName(FDeleted[i].Path) = ExtractFileName(Path) then
        begin
          FAction := eaMoved;
          FPath := FDeleted[i].Path;
          FPathNew := Path;

          FDeleted.Delete(i);

          Synchronize(TriggerEvent);

          Exit;
        end;

      FAction := eaAdded;
      FPath := Path;

      Synchronize(TriggerEvent);
    end;
    FILE_ACTION_MODIFIED:
    begin
      FAction := eaModified;
      FPath := Path;

      Synchronize(TriggerEvent);
    end;
    FILE_ACTION_RENAMED_OLD_NAME:
      FPath := Path;
    FILE_ACTION_RENAMED_NEW_NAME:
    begin
      for i := 0 to FDeleted.Count - 1 do
        if FDeleted[i].Path = Path then
        begin
          FAction := eaModified;
          FPath := Path;

          FDeleted.Delete(i);

          Synchronize(TriggerEvent);

          Exit;
        end;

      FAction := eaMoved;
      FPathNew := Path;

      Synchronize(TriggerEvent);
    end;
  end;
end;

procedure TFileWatcher.ProcessDeleted;
var
  i: Integer;
begin
  for i := FDeleted.Count - 1 downto 0 do
  begin
    if GetTickCount64 < FDeleted[i].Expires then
      Continue;

    FAction := eaRemoved;
    FPath := FDeleted[i].Path;

    FDeleted.Delete(i);

    Synchronize(TriggerEvent);
  end;
end;

end.
