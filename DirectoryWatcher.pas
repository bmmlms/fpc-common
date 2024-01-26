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

unit DirectoryWatcher;

interface

uses
  Classes,
  Forms,
  Generics.Collections,
  JwaWinNT,
  SysUtils,
  Windows;

type
  TDirectoryWatcherActions = (dwaAdded, dwaRemoved, dwaModified, dwaMoved);

  { TDeletedFile }

  TDeletedFile = record
  public
    Expires: Int64;
    Path: string;

    constructor Create(const Path: string);
  end;

  { TNotification }

  TNotification = record
  public
    Path, PathNew: string;
    Action: TDirectoryWatcherActions;

    constructor Create(const Action: TDirectoryWatcherActions; const Path: string); overload;
    constructor Create(const Path: string; const PathNew: string); overload;
  end;

  TNotificationsEvent = procedure(const Sender: TObject; const Notification: TNotification) of object;

  { TDirectoryWatcher }

  TDirectoryWatcher = class(TThread)
  private
  const
    DELETED_TIMEOUT = 1000;
    BUFFER_LEN = 65535;
  private
    FDirectory: string;
    FDirHandle: THandle;
    FBuffer: Pointer;
    FOverlap: TOverlapped;
    FFilter: DWORD;

    FRenamedPath: string;

    FDeleted: TList<TDeletedFile>;
    FNotifications: TList<TNotification>;

    FTerminateEvent: THandle;

    FOnNotifications: TNotificationsEvent;

    procedure BeginRead;
    procedure ProcessBuffer(const Buffer: Pointer);
    procedure ProcessNotification(const Path: string; const Action: DWORD);
    procedure ProcessDeleted;
    procedure DoOnNotifications(NotificationsPtr: SizeInt);
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create(const Directory: string; const Filter: DWORD);
    destructor Destroy; override;

    property OnNotifications: TNotificationsEvent read FOnNotifications write FOnNotifications;
  end;

implementation

{ TDeletedFile }

constructor TDeletedFile.Create(const Path: string);
begin
  Self.Path := Path;
  Expires := GetTickCount64 + TDirectoryWatcher.DELETED_TIMEOUT;
end;

{ TNotification }

constructor TNotification.Create(const Action: TDirectoryWatcherActions; const Path: string);
begin
  Self.Action := Action;
  Self.Path := Path;
end;

constructor TNotification.Create(const Path: string; const PathNew: string);
begin
  Self.Action := dwaMoved;
  Self.Path := Path;
  Self.PathNew := PathNew;
end;

procedure NotificationCompletion(dwErrorCode, dwNumberOfBytesTransferred: DWORD; lpOverlapped: POVERLAPPED); stdcall;
var
  Watcher: TDirectoryWatcher;
  ReadBuffer: Pointer;
begin
  Watcher := TDirectoryWatcher(lpOverlapped.hEvent);

  ReadBuffer := Watcher.FBuffer;

  if dwErrorCode = 0 then
    Watcher.BeginRead;

  if dwNumberOfBytesTransferred > 0 then
    Watcher.ProcessBuffer(ReadBuffer);

  Freemem(ReadBuffer);
end;

{ TDirectoryWatcher }

constructor TDirectoryWatcher.Create(const Directory: string; const Filter: DWORD);
begin
  inherited Create(False);

  FTerminateEvent := CreateEvent(nil, False, False, nil);

  FDirectory := Directory;
  FFilter := Filter;
  FOverlap.hEvent := Windows.HANDLE(Self);

  FDeleted := TList<TDeletedFile>.Create;
  FNotifications := TList<TNotification>.Create;

  FreeOnTerminate := True;
end;

destructor TDirectoryWatcher.Destroy;
begin
  CloseHandle(FTerminateEvent);

  FDeleted.Free;
  FNotifications.Free;

  inherited Destroy;
end;

procedure TDirectoryWatcher.Execute;
var
  EventsArray: array[0..0] of THandle;
begin
  while not Terminated do
  begin
    FDirHandle := CreateFile(PChar(FDirectory), FILE_LIST_DIRECTORY or GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);

    if FDirHandle = INVALID_HANDLE_VALUE then
    begin
      if WaitForSingleObject(FTerminateEvent, 5000) = WAIT_OBJECT_0 then
        Exit;

      Continue;
    end;

    try
      BeginRead;

      EventsArray[0] := FTerminateEvent;

      while (not Terminated) and Assigned(FBuffer) do
      begin
        if WaitForMultipleObjectsEx(Length(EventsArray), @EventsArray, False, IfThen<DWORD>(FDeleted.Count > 0, DELETED_TIMEOUT, INFINITE), True) = WAIT_FAILED then
          Break;

        ProcessDeleted;

        if (not Terminated) and (FNotifications.Count > 0) then
        begin
          Application.QueueAsyncCall(DoOnNotifications, Int64(FNotifications));
          FNotifications := TList<TNotification>.Create;
        end;
      end;

      if Assigned(FBuffer) then
      begin
        CancelIo(FDirHandle);
        SleepEx(INFINITE, True);
      end;
    finally
      CloseHandle(FDirHandle);
    end;

    FNotifications.Clear;
    FDeleted.Clear;
  end;
end;

procedure TDirectoryWatcher.TerminatedSet;
begin
  inherited TerminatedSet;

  SetEvent(FTerminateEvent);
end;

procedure TDirectoryWatcher.BeginRead;
var
  BytesRead: DWORD;
begin
  FBuffer := GetMem(BUFFER_LEN);

  if not ReadDirectoryChangesW(FDirHandle, FBuffer, BUFFER_LEN, True, FFilter, @BytesRead, @FOverlap, @NotificationCompletion) then
    FreeMemAndNil(FBuffer);
end;

procedure TDirectoryWatcher.ProcessBuffer(const Buffer: Pointer);
var
  NextOffset: DWORD;
  Info: PFILE_NOTIFY_INFORMATION;
begin
  Info := Buffer;
  repeat
    ProcessNotification(ConcatPaths([FDirectory, WideCharLenToString(@Info.FileName, Info.FileNameLength div 2)]), Info.Action);

    NextOffset := Info.NextEntryOffset;
    Info := Pointer(Info) + NextOffset;
  until NextOffset = 0;
end;

procedure TDirectoryWatcher.ProcessNotification(const Path: string; const Action: DWORD);
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
          FNotifications.Add(TNotification.Create(dwaModified, Path));
          FDeleted.Delete(i);
          Exit;
        end;

      for i := 0 to FDeleted.Count - 1 do
        if ExtractFileName(FDeleted[i].Path) = ExtractFileName(Path) then
        begin
          FNotifications.Add(TNotification.Create(FDeleted[i].Path, Path));
          FDeleted.Delete(i);
          Exit;
        end;

      FNotifications.Add(TNotification.Create(dwaAdded, Path));
    end;
    FILE_ACTION_MODIFIED:
      FNotifications.Add(TNotification.Create(dwaModified, Path));
    FILE_ACTION_RENAMED_OLD_NAME:
      FRenamedPath := Path;
    FILE_ACTION_RENAMED_NEW_NAME:
    begin
      for i := 0 to FDeleted.Count - 1 do
        if FDeleted[i].Path = Path then
        begin
          FNotifications.Add(TNotification.Create(dwaModified, Path));
          FDeleted.Delete(i);
          Exit;
        end;

      FNotifications.Add(TNotification.Create(FRenamedPath, Path));
    end;
  end;
end;

procedure TDirectoryWatcher.ProcessDeleted;
var
  i: Integer;
begin
  for i := FDeleted.Count - 1 downto 0 do
  begin
    if GetTickCount64 < FDeleted[i].Expires then
      Continue;

    FNotifications.Add(TNotification.Create(dwaRemoved, FDeleted[i].Path));

    FDeleted.Delete(i);
  end;
end;

procedure TDirectoryWatcher.DoOnNotifications(NotificationsPtr: SizeInt);
var
  Notification: TNotification;
  Notifications: TList<TNotification> absolute NotificationsPtr;
begin
  try
    if Assigned(FOnNotifications) then
      for Notification in Notifications do
        FOnNotifications(Self, Notification);
  finally
    Notifications.Free;
  end;
end;

end.
