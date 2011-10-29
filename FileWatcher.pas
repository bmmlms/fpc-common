unit FileWatcher;

interface

uses
  Windows, SysUtils, Classes, SyncObjs, ShellApi, ComCtrls;

type
  TFileWatchEvent = procedure(Sender: TObject; Action: DWORD; OldName, NewName: string) of object;

  TFileWatcher = class(TThread)
  private
    FWatchHandle: DWORD;
    FPath: string;
    FFilename: string;
    FFilenameNew: string;
    FAction: DWORD;
    FFileEvent: THandle;
    FFilter: DWORD;
    FTermEvent: TEvent;

    FOnEvent: TFileWatchEvent;

    procedure TriggerEvent;
  public
    constructor Create(const Path: string; Filter: DWORD);
    destructor Destroy; override;
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
  Buffer: Pointer;
  BufLen: DWORD;
  Read: DWORD;
  Info: PFILE_NOTIFY_INFORMATION;
  NextOffset: DWORD;
  NameLen: DWORD;
  Overlap: TOverlapped;
  WaitResult: DWORD;
  EventArray: array [0..1] of THandle;
begin
  FWatchHandle := CreateFile(PChar(FPath), FILE_LIST_DIRECTORY or GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
    OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);

  if (FWatchHandle = INVALID_HANDLE_VALUE) or (FWatchHandle = 0) then
    Exit;

  FFileEvent := CreateEvent(nil, FALSE, FALSE, nil);
  Overlap.hEvent := FFileEvent;

  FTermEvent := TEvent.Create(nil, False, False, IntToStr(Handle) + 'Term');

  EventArray[0] := FFileEvent;
  EventArray[1] := FTermEvent.Handle;

  BufLen := 65535;
  Buffer := AllocMem(BufLen);
  try
    while not Terminated do
    begin
      Read := 0;
      if ReadDirectoryChangesW(FWatchHandle, Buffer, BufLen, True, FFilter, @Read, @Overlap, nil) then
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

                if ((FAction = FILE_ACTION_REMOVED) and (not FileExists(FPath + WideCharLenToString(@Info.FileName, NameLen div 2)))) or
                    (FAction <> FILE_ACTION_REMOVED) then
                begin
                  if FAction <> FILE_ACTION_RENAMED_NEW_NAME then
                    FFilename := WideCharLenToString(@Info.FileName, NameLen div 2)
                  else
                    FFilenameNew := WideCharLenToString(@Info.Filename, NameLen div 2);

                  if Assigned(FOnEvent) then
                    Synchronize(TriggerEvent);
                end;

                Info := Pointer(Int64(Info) + NextOffset);
              until NextOffset = 0;
            end;
          WaitTerm:
            Terminate;
        else
          Break;
        end;
      end;
    end;
  finally
    FreeMem(Buffer, BufLen);
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
    FOnEvent(Self, FAction, FFilename, FFilenameNew);
end;

destructor TFileWatcher.Destroy;
begin
  try
    if FWatchHandle <> INVALID_HANDLE_VALUE then
      CloseHandle(FWatchHandle);
  except end;
  try
    CloseHandle(FFileEvent);
    FTermEvent.Free;
  except end;
end;

end.
