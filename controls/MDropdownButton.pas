unit MDropdownButton;

interface

uses
  Classes,
  CommCtrl,
  Controls,
  LCLType,
  Menus,
  StdCtrls,
  Win32Int,
  Win32WSControls,
  Win32WSStdCtrls,
  Windows,
  WSLCLClasses;

type

  { TWin32WSMDropdownButton }

  TWin32WSMDropdownButton = class(TWin32WSButton)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
  end;

  { TMDropdownButton }

  TMDropdownButton = class(TButton)
  private
    FSplit: Boolean;
    FPopupMenu: TPopupMenu;

    procedure FSetSplit(Value: Boolean);
  protected
    class procedure WSRegisterClass; override;
    function HandleAllocated: Boolean;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
  public
    procedure Click; override;
  published
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property Split: Boolean read FSplit write FSetSplit;
  end;

procedure Register;

implementation

procedure Register;
const
  Classes: array of TComponentClass = [TMDropdownButton];
begin
  RegisterComponents('MControls', Classes);
end;

function MDropdownButtonParentMsgHandler(const AWinControl: TWinControl; Window: HWnd; Msg: UInt; WParam: Windows.WParam;
  LParam: Windows.LParam; var MsgResult: Windows.LResult; var WinProcess: Boolean): Boolean;
var
  P: TPoint;
begin
  Result := False;
  if Msg = WM_NOTIFY then
    if LCLType.PNMHDR(LParam)^.code = BCN_DROPDOWN then
    begin
      Result := True;
      if not Assigned(TMDropdownButton(AWinControl).PopupMenu) then
        Exit;

      P := TPoint.Create(AWinControl.Left + AWinControl.Width, AWinControl.Top + AWinControl.Height);
      ClientToScreen(Window, P);

      TrackPopupMenu(TMDropdownButton(AWinControl).PopupMenu.Handle, TPM_RIGHTALIGN or TPM_TOPALIGN, P.x, P.y, 0, Window, nil);
    end;
end;

{ TWin32WSMDropdownButton }

class function TWin32WSMDropdownButton.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  PrepareCreateWindow(AWinControl, AParams, Params);
  with Params do
  begin
    Flags := WS_CHILD or BS_DEFSPLITBUTTON or WS_VISIBLE;
    pClassName := @ButtonClsName[0];
    WindowTitle := StrCaption;
  end;
  FinishCreateWindow(AWinControl, Params, False);
  Params.WindowInfo^.ParentMsgHandler := @MDropdownButtonParentMsgHandler;
  Result := Params.Window;

  TMDropdownButton(AWinControl).Split := False;
end;

{ TMDropdownButton }

class procedure TMDropdownButton.WSRegisterClass;
begin
  inherited WSRegisterClass;

  RegisterWSComponent(TMDropdownButton, TWin32WSMDropdownButton);
end;

function TMDropdownButton.HandleAllocated: Boolean;
begin
  Result := inherited;

  Split := False;
end;

procedure TMDropdownButton.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  PreferredWidth += 8;
end;

procedure TMDropdownButton.FSetSplit(Value: Boolean);
var
  SplitInfo: tagBUTTON_SPLITINFO;
begin
  FSplit := Value;

  SplitInfo.mask := BCSIF_STYLE;
  if Value then
    SplitInfo.uSplitStyle := 0
  else
    SplitInfo.uSplitStyle := BCSS_NOSPLIT;

  Button_SetSplitInfo(Handle, @SplitInfo);

  Invalidate;
end;

procedure TMDropdownButton.Click;
var
  P: TPoint;
begin
  inherited Click;

  if (not Assigned(PopupMenu)) or FSplit then
    Exit;

  P := TPoint.Create(Width, Height);
  P := ClientToScreen(P);

  PopupMenu.Alignment := paRight;
  PopupMenu.PopUp(P.x, P.y);
end;

end.
