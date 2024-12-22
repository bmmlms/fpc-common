unit AudioEndpointNotificationListener;

interface

uses
  ActiveX,
  Classes,
  ComObj,
  InterfaceBase,
  SysUtils,
  Windows;

type
  EDataFlow = (eRender, eCapture, eAll, EDataFlow_enum_count);
  ERole = (eConsole, eMultimedia, eCommunications, ERole_enum_count);

  TPropertyKey = record
    fmtid: TGUID;
    pid: DWORD;
  end;

  { IPropertyStore }

  IPropertyStore = interface(IUnknown)
    function GetCount(out cProps: DWORD): HRESULT; stdcall;
    function GetAt(iProp: DWORD; out key: TPropertyKey): HRESULT; stdcall;
    function GetValue(const key: TPropertyKey; out Value: TPropVariant): HRESULT; stdcall;
  end;

  { IMMDevice }

  IMMDevice = interface(IUnknown)
    ['{D666063F-1587-4E43-81F1-B948E807363F}']
    function Activate(const iid: TGUID; dwClsCtx: DWORD; pActivationParams: PPropVariant; out EndpointVolume: IUnknown): HRESULT; stdcall;
    function OpenPropertyStore(stgmAccess: DWORD; out Properties: IPropertyStore): HRESULT; stdcall;
    function GetId(out strId: LPWSTR): HRESULT; stdcall;
    function GetState(out State: DWORD): HRESULT; stdcall;
  end;

  { IMMDeviceCollection }

  IMMDeviceCollection = interface(IUnknown)
    ['{0BD7A1BE-7A1A-44DB-8397-CC5392387B5E}']
    function GetCount(out cDevices: UINT): HRESULT; stdcall;
    function Item(nDevice: UINT; out Device: IMMDevice): HRESULT; stdcall;
  end;

  { IMMNotificationClient }

  IMMNotificationClient = interface(IUnknown)
    ['{7991EEC9-7E89-4D85-8390-6C703CEC60C0}']
    function OnDeviceStateChanged(pwstrDeviceId: LPWSTR; dwNewState: DWORD): HRESULT; stdcall;
    function OnDeviceAdded(pwstrDeviceId: LPWSTR): HRESULT; stdcall;
    function OnDeviceRemoved(pwstrDeviceId: LPWSTR): HRESULT; stdcall;
    function OnDefaultDeviceChanged(flow: EDataFlow; role: ERole; pwstrDefaultDeviceId: LPWSTR): HRESULT; stdcall;
    function OnPropertyValueChanged(pwstrDeviceId: LPWSTR; const key: TPropertyKey): HRESULT; stdcall;
  end;

  { IMMDeviceEnumerator }

  IMMDeviceEnumerator = interface(IUnknown)
    ['{A95664D2-9614-4F35-A746-DE8DB63617E6}']
    function EnumAudioEndpoints(dataFlow: EDataFlow; dwStateMask: DWORD; out Devices: IMMDeviceCollection): HRESULT; stdcall;
    function GetDefaultAudioEndpoint(EDF: EDataFlow; role: ERole; out EndPoint: IMMDevice): HRESULT; stdcall;
    function GetDevice(pwstrId: LPWSTR; out EndPoint: IMMDevice): HRESULT; stdcall;
    function RegisterEndpointNotificationCallback(const Client: IMMNotificationClient): HRESULT; stdcall;
    function UnregisterEndpointNotificationCallback(const Client: IMMNotificationClient): HRESULT; stdcall;
  end;

  { TMMNotificationClient }
  TMMNotificationClient = class(TInterfacedObject, IMMNotificationClient)
  private
    FWndHandle: HWND;
  protected
    function OnDeviceStateChanged(pwstrDeviceId: LPWSTR; dwNewState: DWORD): HRESULT; stdcall;
    function OnDeviceAdded(pwstrDeviceId: LPWSTR): HRESULT; stdcall;
    function OnDeviceRemoved(pwstrDeviceId: LPWSTR): HRESULT; stdcall;
    function OnDefaultDeviceChanged(flow: EDataFlow; role: ERole; pwstrDefaultDeviceId: LPWSTR): HRESULT; stdcall;
    function OnPropertyValueChanged(pwstrDeviceId: LPWSTR; const key: TPropertyKey): HRESULT; stdcall;
  public
    constructor Create(WndHandle: HWND);
  end;

  { TAudioEndpointNotificationListener }

  TAudioEndpointNotificationListener = class
  private
    FWndHandle: HWND;
    // These fields must be declared as interface types to make use of interface reference counting
    FMMDeviceEnumerator: IMMDeviceEnumerator;
    FMMNotificationClient: IMMNotificationClient;
    FOnDefaultDeviceChanged: TNotifyEvent;

    procedure WndProc(var Msg: TMessage);
  public
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property OnDefaultDeviceChanged: TNotifyEvent read FOnDefaultDeviceChanged write FOnDefaultDeviceChanged;
  end;

const
  CLASS_IMMDeviceEnumerator: TGUID = '{BCDE0395-E52F-467C-8E3D-C4579291692E}';
  WM_DEFAULT_DEVICE_CHANGED = WM_USER + 47;

implementation

{ TMMNotificationClient }

constructor TMMNotificationClient.Create(WndHandle: HWND);
begin
  FWndHandle := WndHandle;
end;

function TMMNotificationClient.OnDeviceStateChanged(pwstrDeviceId: LPWSTR; dwNewState: DWORD): HRESULT; stdcall;
begin
  Result := S_OK;
end;

function TMMNotificationClient.OnDeviceAdded(pwstrDeviceId: LPWSTR): HRESULT; stdcall;
begin
  Result := S_OK;
end;

function TMMNotificationClient.OnDeviceRemoved(pwstrDeviceId: LPWSTR): HRESULT; stdcall;
begin
  Result := S_OK;
end;

function TMMNotificationClient.OnDefaultDeviceChanged(flow: EDataFlow; role: ERole; pwstrDefaultDeviceId: LPWSTR): HRESULT; stdcall;
begin
  if (Flow = eRender) and (role = eMultimedia) then
    PostMessage(FWndHandle, WM_DEFAULT_DEVICE_CHANGED, 0, 0);

  Result := S_OK;
end;

function TMMNotificationClient.OnPropertyValueChanged(pwstrDeviceId: LPWSTR; const key: TPropertyKey): HRESULT; stdcall;
begin
  Result := S_OK;
end;

{ TAudioEndpointNotificationListener }

destructor TAudioEndpointNotificationListener.Destroy;
begin
  Stop;

  inherited Destroy;
end;

procedure TAudioEndpointNotificationListener.Start;
begin
  if FWndHandle <> 0 then
    Exit;

  FWndHandle := WidgetSet.AllocateHWnd(WndProc);
  if FWndHandle = 0 then
    raise Exception.Create('AllocateHWnd() failed: %s'.Format([SysErrorMessage(GetLastError)]));

  FMMDeviceEnumerator := CreateComObject(CLASS_IMMDeviceEnumerator) as IMMDeviceEnumerator;
  FMMNotificationClient := TMMNotificationClient.Create(FWndHandle);

  if not Succeeded(FMMDeviceEnumerator.RegisterEndpointNotificationCallback(FMMNotificationClient)) then
  begin
    Stop;

    raise Exception.Create('RegisterEndpointNotificationCallback() failed');
  end;
end;

procedure TAudioEndpointNotificationListener.Stop;
begin
  if Assigned(FMMDeviceEnumerator) and Assigned(FMMNotificationClient) then
    FMMDeviceEnumerator.UnregisterEndpointNotificationCallback(FMMNotificationClient);

  FMMDeviceEnumerator := nil;
  FMMNotificationClient := nil;

  if FWndHandle <> 0 then
  begin
    WidgetSet.DeallocateHWnd(FWndHandle);
    FWndHandle := 0;
  end;
end;

procedure TAudioEndpointNotificationListener.WndProc(var Msg: TMessage);
begin
  if Msg.msg = WM_DEFAULT_DEVICE_CHANGED then
  begin
    if Assigned(FOnDefaultDeviceChanged) then
      FOnDefaultDeviceChanged(Self);

    Msg.Result := 0;
  end else
    Msg.Result := DefWindowProc(FWndHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

end.
