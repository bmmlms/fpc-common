unit DeviceNotificationListener;

interface

uses
  Classes,
  Generics.Collections,
  InterfaceBase,
  JwaDbt,
  SysUtils,
  Windows;

type
  TDeviceNotificationType = (dntAdded, dntRemoved);

  TDeviceNotificationEvent = procedure(const NotificationType: TDeviceNotificationType; const DeviceClass: TGUID; const DeviceName: string) of object;

  TDeviceNotification = record
    DeviceClass: TGUID;
    DeviceName: string;
  end;
  PDeviceNotification = ^TDeviceNotification;

  { TDeviceNotificationListener }

  TDeviceNotificationListener = class
  private
    FWndHandle: HWND;
    FNotificationHandle: HDEVNOTIFY;
    FDeviceAddedNotifications: TList<PDeviceNotification>;
    FOnDeviceNotification: TDeviceNotificationEvent;

    procedure WndProc(var Msg: TMessage);
    procedure AddAddedEvent(const DeviceClass: TGUID; const DeviceName: string);
    function RemoveAddedEvent(const DeviceName: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property OnDeviceNotification: TDeviceNotificationEvent read FOnDeviceNotification write FOnDeviceNotification;
  end;

implementation

function RegisterDeviceNotificationW(hRecipient: HANDLE; NotificationFilter: Pointer; Flags: DWORD): HDEVNOTIFY; stdcall; external user32;
function UnregisterDeviceNotification(Handle: HDEVNOTIFY): BOOL; stdcall; external user32;

{ TDeviceNotificationListener }

constructor TDeviceNotificationListener.Create;
begin
  FDeviceAddedNotifications := TList<PDeviceNotification>.Create;
end;

destructor TDeviceNotificationListener.Destroy;
begin
  Stop;
  FDeviceAddedNotifications.Free;

  inherited Destroy;
end;

procedure TDeviceNotificationListener.Start;
const
  DEVICE_NOTIFY_WINDOW_HANDLE = 0;
  DEVICE_NOTIFY_ALL_INTERFACE_CLASSES = 4;
var
  DBDI: DEV_BROADCAST_DEVICEINTERFACE;
  Size: Integer;
begin
  if FWndHandle <> 0 then
    Exit;

  FWndHandle := WidgetSet.AllocateHWnd(WndProc);
  if FWndHandle = 0 then
    raise Exception.Create('AllocateHWnd() failed: %s'.Format([SysErrorMessage(GetLastError)]));

  Size := SizeOf(DEV_BROADCAST_DEVICEINTERFACE);
  ZeroMemory(@DBDI, Size);
  DBDI.dbcc_size := Size;
  DBDI.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
  DBDI.dbcc_reserved := 0;
  DBDI.dbcc_name[0] := #0;

  FNotificationHandle := RegisterDeviceNotificationW(FWndHandle, @DBDI, DEVICE_NOTIFY_WINDOW_HANDLE or DEVICE_NOTIFY_ALL_INTERFACE_CLASSES);

  if FNotificationHandle = 0 then
  begin
    WidgetSet.DeallocateHWnd(FWndHandle);
    FWndHandle := 0;
    raise Exception.Create('RegisterDeviceNotificationW() failed: %s'.Format([SysErrorMessage(GetLastError)]));
  end;
end;

procedure TDeviceNotificationListener.Stop;
var
  Notification: PDeviceNotification;
begin
  if FNotificationHandle <> 0 then
  begin
    UnregisterDeviceNotification(FNotificationHandle);
    FNotificationHandle := 0;
  end;

  if FWndHandle <> 0 then
  begin
    KillTimer(FWndHandle, 0);
    WidgetSet.DeallocateHWnd(FWndHandle);
    FWndHandle := 0;
  end;

  for Notification in FDeviceAddedNotifications do
    Dispose(Notification);
  FDeviceAddedNotifications.Clear;
end;

procedure TDeviceNotificationListener.WndProc(var Msg: TMessage);
var
  DevBroadcastDeviceInterface: PDevBroadcastDeviceInterfaceW;
  Notification: PDeviceNotification;
  DeviceName: string;
begin
  if (Msg.msg = WM_DEVICECHANGE) and ((Msg.wParam = DBT_DEVICEARRIVAL) or (Msg.wParam = DBT_DEVICEREMOVECOMPLETE)) and (PDevBroadcastHdr(Msg.lParam)^.dbch_devicetype = DBT_DEVTYP_DEVICEINTERFACE) then
  begin
    DevBroadcastDeviceInterface := PDevBroadcastDeviceInterfaceW(Msg.lParam);

    DeviceName := PWideChar(@DevBroadcastDeviceInterface^.dbcc_name);

    if Msg.wParam = DBT_DEVICEARRIVAL then
      AddAddedEvent(DevBroadcastDeviceInterface.dbcc_classguid, DeviceName)
    else if (Msg.wParam = DBT_DEVICEREMOVECOMPLETE) and not RemoveAddedEvent(DeviceName) and Assigned(FOnDeviceNotification) then
      FOnDeviceNotification(dntRemoved, DevBroadcastDeviceInterface.dbcc_classguid, DeviceName);

    KillTimer(FWndHandle, 0);
    if FDeviceAddedNotifications.Count > 0 then
      SetTimer(FWndHandle, 0, 500, nil);

    Msg.Result := 0;
  end else if Msg.msg = WM_TIMER then
  begin
    KillTimer(FWndHandle, 0);

    for Notification in FDeviceAddedNotifications do
    begin
      if Assigned(FOnDeviceNotification) then
        FOnDeviceNotification(dntAdded, Notification.DeviceClass, Notification.DeviceName);
      Dispose(Notification);
    end;

    FDeviceAddedNotifications.Clear;
  end else
    Msg.Result := DefWindowProc(FWndHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TDeviceNotificationListener.AddAddedEvent(const DeviceClass: TGUID; const DeviceName: string);
var
  Notification: PDeviceNotification;
begin
  New(Notification);
  Notification.DeviceClass := DeviceClass;
  Notification.DeviceName := DeviceName;

  FDeviceAddedNotifications.Add(Notification);
end;

function TDeviceNotificationListener.RemoveAddedEvent(const DeviceName: string): Boolean;
var
  Notification: PDeviceNotification;
begin
  Result := False;

  for Notification in FDeviceAddedNotifications do
    if Notification.DeviceName = DeviceName then
    begin
      FDeviceAddedNotifications.Remove(Notification);
      Dispose(Notification);
      Exit(True);
    end;
end;

end.
