unit SystemCritical;

interface

uses
  Windows, Messages;

type
  TSetThreadExecutionState = function(ESFlags: DWORD): DWORD; stdcall;

  TSystemCritical = class
  private
    FCritical: Boolean;

    FFunc: TSetThreadExecutionState;

    procedure FSetCritical(Value: Boolean);
    procedure DecCritical;
    procedure IncCritical;
  public
    constructor Create;
    destructor Destroy; override;

    property Critical: Boolean read FCritical write FSetCritical;
  end;

const
  ES_SYSTEM_REQUIRED = $00000001;
  ES_DISPLAY_REQUIRED = $00000002;
  ES_USER_PRESENT = $00000004;
  ES_AWAYMODE_REQUIRED = $00000040;
  ES_CONTINUOUS = $80000000;

var
  Critical: TSystemCritical;

implementation

{ TSystemCritical }

constructor TSystemCritical.Create;
var
  H: THandle;
begin
  inherited;

  FFunc := nil;
  H := GetModuleHandle('kernel32.dll');
  if H <> 0 then
  begin
    @FFunc := GetProcAddress(H, 'SetThreadExecutionState');
  end;
end;

destructor TSystemCritical.Destroy;
begin
  Critical := False;

  inherited;
end;

procedure TSystemCritical.FSetCritical(Value: Boolean);
begin
  if Assigned(@FFunc) then
  begin
    if Value then
      FFunc(ES_SYSTEM_REQUIRED or ES_CONTINUOUS)
    else
      FFunc(ES_CONTINUOUS);
  end;
end;

initialization
begin
  Critical := TSystemCritical.Create;
end;

finalization
begin
  Critical.Free;
  Critical := nil;
end;

end.
