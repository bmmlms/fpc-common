unit Service;

interface

uses
  Windows, SysUtils, WinSvc;

function IsServiceInstalled(ServiceName: string): Boolean;
function IsServiceRunning(ServiceName: string): Boolean;
function InstallService(ServiceName, DisplayName, ExePath: string): Boolean;
function StartService(ServiceName: string): Boolean;
function StopService(ServiceName: string): Boolean;
function IsServiceStartable(ServiceName: string): Boolean;
function UninstallService(ServiceName: string): Boolean;
procedure AdjustServiceFilename(ServiceName, Filename: string);

implementation

function IsServiceInstalled(ServiceName: string): Boolean;
var
  SCManager: THandle;
  Service: THandle;
begin
  Result := False;
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ENUMERATE_SERVICE);
  if SCManager = 0 then
    Exit;
  try
    Service := OpenService(SCManager, PChar(ServiceName), SERVICE_INTERROGATE);
    if Service <> 0 then
    begin
      Result := True;
      CloseServiceHandle(Service);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

function IsServiceRunning(ServiceName: string): Boolean;
var
  SCManager: THandle;
  Service: THandle;
  Status: TServiceStatus;
begin
  Result := False;
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
    Exit;
  try
    Service := OpenService(SCManager, PChar(ServiceName), SERVICE_ALL_ACCESS);
    if Service <> 0 then
    begin
      QueryServiceStatus(Service, Status);
      Result := Status.dwCurrentState <> SERVICE_STOPPED;
      //if (Status.dwCurrentState = SERVICE_RUNNING) or (Status.dwCurrentState = SERVICE_START_PENDING) then
      //  Result := True;
      CloseServiceHandle(Service);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

function UninstallService(ServiceName: string): Boolean;
var
  SCManager: THandle;
  Service: THandle;
begin
  Result := False;
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
    Exit;
  try
    Service := OpenService(SCManager, PChar(ServiceName), SERVICE_ALL_ACCESS);

    if Service = 0 then
      Exit;

    if IsServiceRunning(ServiceName) then
    begin
      if not StopService(ServiceName) then
        Exit;
    end;

    if DeleteService(Service) then
      Result := True;
    CloseServiceHandle(Service);
  finally
    CloseServiceHandle(SCManager);
  end;
end;

function InstallService(ServiceName, DisplayName, ExePath: string): Boolean;
var
  SCManager: THandle;
  Service: THandle;
begin
  Result := False;
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
    Exit;
  try
    Service := CreateService(SCManager, PChar(ServiceName), PChar(DisplayName), SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, SERVICE_AUTO_START, SERVICE_ERROR_IGNORE, PChar(ExePath), nil, nil, nil, nil, nil);
    if Service > 0 then
      Result := True;
    CloseServiceHandle(Service);
  finally
    CloseServiceHandle(SCManager);
  end;
end;

function StartService(ServiceName: string): Boolean;
var
  SCManager: THandle;
  Service: THandle;
  Dummy: PWideChar;
  Start: Cardinal;
begin
  if IsServiceRunning(ServiceName) then
    Exit(True);

  Result := False;
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
    Exit;
  try
    Service := OpenService(SCManager, PChar(ServiceName), SERVICE_ALL_ACCESS);
    try
      if Service > 0 then
      begin
        Dummy := nil;
        if WinSvc.StartService(Service, 0, Dummy) then
          Result := True;

        Start := GetTickCount;
        while (not IsServiceRunning(ServiceName)) do
        begin
          if GetTickCount > Start + 5000 then
            Break;
        end;

        Result := IsServiceRunning(ServiceName);
      end;
    finally
      CloseServiceHandle(Service);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

function StopService(ServiceName: string): Boolean;
var
  SCManager: THandle;
  Service: THandle;
  Status: TServiceStatus;
  Start: Cardinal;
begin
  if not IsServiceRunning(ServiceName) then
    Exit(True);

  Result := False;
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
    Exit;
  try
    Service := OpenService(SCManager, PChar(ServiceName), SERVICE_ALL_ACCESS);
    try
      if Service > 0 then
      begin
        if not ControlService(Service, SERVICE_CONTROL_STOP, Status) then
          Exit(not IsServiceRunning(ServiceName));

        Start := GetTickCount;
        while (IsServiceRunning(ServiceName)) do
        begin
          if GetTickCount > Start + 5000 then
            Break;
        end;

        Result := not IsServiceRunning(ServiceName);
      end;
    finally
      CloseServiceHandle(Service);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

function IsServiceStartable(ServiceName: string): Boolean;
var
  SCManager: THandle;
  Service: THandle;
  Status: TServiceStatus;
  Config: PQueryServiceConfig;
  Needed: Cardinal;
begin
  Result := False;
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
    Exit;
  try
    Service := OpenService(SCManager, PChar(ServiceName), SERVICE_ALL_ACCESS);
    try
      if Service > 0 then
      begin
        if not QueryServiceConfig(Service, Config, 0, Needed) then
        begin
          GetMem(Config, Needed);
          try
            if QueryServiceConfig(Service, Config, Needed, Needed) then
            begin
              Result := Config.dwStartType = SERVICE_AUTO_START;
            end;
          finally
            FreeMem(Config);
          end;
        end;
      end;
    finally
      CloseServiceHandle(Service);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

procedure AdjustServiceFilename(ServiceName, Filename: string);
var
  SCManager: THandle;
  Service: THandle;
  Status: TServiceStatus;
  Config: PQueryServiceConfig;
  Needed: Cardinal;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
    Exit;
  try
    Service := OpenService(SCManager, PChar(ServiceName), SERVICE_ALL_ACCESS);
    try
      if Service > 0 then
      begin
        if not QueryServiceConfig(Service, Config, 0, Needed) then
        begin
          GetMem(Config, Needed);
          try
            if QueryServiceConfig(Service, Config, Needed, Needed) then
            begin
              if LowerCase(Config.lpBinaryPathName) <> LowerCase(Filename) then
              begin
                ChangeServiceConfig(Service, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE, SERVICE_NO_CHANGE, PChar(Filename),
                  nil, nil, nil, nil, nil, nil);
              end;
            end;
          finally
            FreeMem(Config);
          end;
        end;
      end;
    finally
      CloseServiceHandle(Service);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

end.
