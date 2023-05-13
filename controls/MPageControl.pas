unit MPageControl;

interface

uses
  Buttons,
  Classes,
  ComboEx,
  ComCtrls,
  Controls,
  Forms,
  Generics.Collections,
  Graphics,
  GraphType,
  ImgList,
  LMessages,
  Math,
  MControlFocuser,
  Menus,
  MStringFunctions,
  StdCtrls,
  SysUtils,
  Themes,
  Windows;

type

  { TMTabSheet }

  TMTabSheet = class(TTabSheet)
  private
    FCaption: TCaption;
    FShowCloseButton: Boolean;
    FShownOnce: Boolean;
    FButtonRect: TRect;

    FFocusedControlBeforeChange: TWinControl;

    procedure UpdateProperties;
    procedure FSetCaption(Value: TCaption);
    procedure FSetShowCloseButton(Value: Boolean);
  protected
    procedure DoShow; override;

    procedure ShownFirst; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    function CanClose: Boolean; virtual;
    function ProcessShortCut(Msg: TWMKey): Boolean; virtual;

    property ShowCloseButton: Boolean read FShowCloseButton write FSetShowCloseButton;
  published
    property Caption: TCaption read FCaption write FSetCaption;
  end;

  { TMPageControl }

  TMPageControl = class(TPageControl)
  private
  const
    WM_CLOSETAB = WM_USER + 1245;
    CT_ACTIVE = 0;
    CT_ALL = 1;
    CT_ALL_BUT_ACTIVE = 2;
    PADDING_CHAR = ' ';
  private
    FRemoving: Boolean;
    FMaxTabWidth: Integer;
    FFocusList: TList<TTabSheet>;
    FPressedButton: Integer;
    FMouseInButton: Boolean;

    procedure DrawButton(Canvas: TCanvas; R: TRect; State: TButtonState);
    procedure RemoveTab(Tab: TMTabSheet); virtual;
    function GetCloseButtonTabIndex(const P: TPoint): Integer;
    procedure FSetMaxTabWidth(Value: Integer);
    function FGetTabSheet(Index: Integer): TMTabSheet;
  protected
    function CanChange: Boolean; override;
    procedure DoChange; override;
    procedure WndProc(var Message: TMessage); override;
    procedure PaintWindow(DC: HDC); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CloseTab(Idx: Integer);
    procedure CloseAll;
    procedure CloseAllButActive;

    property MaxTabWidth: Integer read FMaxTabWidth write FSetMaxTabWidth;
    property Pages[Index: Integer]: TMTabSheet read FGetTabSheet;
  end;

procedure Register;

implementation

const
  TABSHEET_PADDING = 3;

procedure Register;
const
  Classes: array of TComponentClass = [TMPageControl];
begin
  RegisterComponents('MControls', Classes);
end;

{ TMTabSheet }

function TMTabSheet.CanClose: Boolean;
begin
  Result := True;
end;

procedure TMTabSheet.DoShow;
begin
  inherited DoShow;

  if not FShownOnce then
  begin
    ShownFirst;
    FShownOnce := True;
  end;
end;

procedure TMTabSheet.ShownFirst;
begin

end;

constructor TMTabSheet.Create(AOwner: TComponent);
begin
  inherited;

  FShowCloseButton := True;
  FCaption := '';
  Color := clWindow;
end;

procedure TMTabSheet.FSetCaption(Value: TCaption);
begin
  FCaption := Trim(Value);
  UpdateProperties;
end;

procedure TMTabSheet.FSetShowCloseButton(Value: Boolean);
begin
  FShowCloseButton := Value;
  UpdateProperties;
end;

function TMTabSheet.ProcessShortCut(Msg: TWMKey): Boolean;
begin
  Update;

  Result := False;
end;

procedure TMTabSheet.UpdateProperties;
var
  TabRect: TRect;
  ButtonWidth: Integer;
begin
  if not Assigned(PageControl) then
    Exit;

  TabRect := PageControl.TabRect(PageIndex);

  if TabRect.Height = 0 then
    Exit;

  ButtonWidth := PageControl.TabHeight - MulDiv(TABSHEET_PADDING, Screen.PixelsPerInch, 96) * 2;

  FButtonRect := TRect.Create(TPoint.Create(TabRect.Right - ButtonWidth - MulDiv(TABSHEET_PADDING, Screen.PixelsPerInch, 96) * 3, TabRect.Top + (TabRect.Height div 2) - Floor(ButtonWidth / 2)), ButtonWidth, ButtonWidth);

  if PageControl.ActivePageIndex = PageIndex then
    FButtonRect.Offset(0, -2);

  inherited Caption := FCaption;
end;

{ TMPageControl }

procedure TMPageControl.DrawButton(Canvas: TCanvas; R: TRect; State: TButtonState);
var
  uType: Integer;
  uState: Integer;
  Details: TThemedElementDetails;
  Win: TThemedToolTip;
begin
  if ThemeServices.ThemesEnabled then
  begin
    case State of
      bsDown:
        Win := tttClosePressed;
      bsHot:
        Win := tttCloseHot;
      else
        Win := tttCloseNormal;
    end;

    R.Right := R.Right + 2;
    R.Bottom := R.Bottom + 2;

    Details := ThemeServices.GetElementDetails(Win);
    ThemeServices.DrawElement(Canvas.Handle, Details, R);
  end else
  begin
    uType := DFC_CAPTION;
    uState := DFCS_CAPTIONCLOSE;

    case State of
      bsDown:
        uState := uState or DFCS_PUSHED;
      bsHot:
        uState := uState or DFCS_MONO;
      else
        uState := uState or DFCS_FLAT;
    end;

    DrawFrameControl(Canvas.Handle, R, uType, uState);
  end;
end;

function TMPageControl.CanChange: Boolean;
begin
  if (GetAsyncKeyState(VK_LBUTTON) < 0) and (GetCloseButtonTabIndex(ScreenToClient(Mouse.CursorPos)) > -1) then
    Exit(False);

  Result := inherited;

  if Result then
    TMTabSheet(ActivePage).FFocusedControlBeforeChange := Screen.ActiveControl;
end;

procedure TMPageControl.PaintWindow(DC: HDC);
var
  R: TRect;
  i: Integer;
  Canvas: TCanvas;
begin
  inherited PaintWindow(DC);

  Canvas := TCanvas.Create;
  Canvas.Handle := DC;
  try
    for i := 0 to PageCount - 1 do
    begin
      Pages[i].UpdateProperties;

      R := Pages[i].FButtonRect;
      if (not Pages[i].FShowCloseButton) or (R.Top >= 0) then
        Continue;

      if FPressedButton = i then
        DrawButton(Canvas, R, bsDown)
      else if R.Contains(ScreenToClient(Mouse.CursorPos)) then
        DrawButton(Canvas, R, bsHot)
      else
        DrawButton(Canvas, R, bsUp);
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TMPageControl.DoChange;
begin
  inherited;

  if TMTabSheet(ActivePage).FFocusedControlBeforeChange <> nil then
    TMTabSheet(ActivePage).FFocusedControlBeforeChange.ApplyFocus;

  if not FRemoving then
    FFocusList.Add(ActivePage);

  Invalidate;
end;

procedure TMPageControl.CloseTab(Idx: Integer);
begin
  PostMessage(Handle, WM_CLOSETAB, CT_ACTIVE, Idx);
end;

constructor TMPageControl.Create(AOwner: TComponent);
begin
  inherited;

  Options := Options + [nboDoChangeOnSetIndex];
  FPressedButton := -1;
  FFocusList := TList<TTabSheet>.Create;
end;

destructor TMPageControl.Destroy;
begin
  FFocusList.Free;

  inherited;
end;

procedure TMPageControl.CloseAll;
begin
  PostMessage(Handle, WM_CLOSETAB, CT_ALL, 0);
end;

procedure TMPageControl.CloseAllButActive;
begin
  PostMessage(Handle, WM_CLOSETAB, CT_ALL_BUT_ACTIVE, 0);
end;

procedure TMPageControl.FSetMaxTabWidth(Value: Integer);
var
  i: Integer;
begin
  FMaxTabWidth := Value;
  for i := 0 to PageCount - 1 do
    Pages[i].UpdateProperties;
end;

function TMPageControl.FGetTabSheet(Index: Integer): TMTabSheet;
begin
  Result := TMTabSheet(inherited Pages[Index]);
end;

procedure TMPageControl.RemoveTab(Tab: TMTabSheet);
var
  Idx: Integer;
  i: Integer;
begin
  if not Tab.CanClose then
    Exit;

  FRemoving := True;
  LockWindowUpdate(Handle);
  try
    if Tab = ActivePage then
    begin
      if PageCount - 1 > ActivePageIndex then
        Idx := ActivePageIndex
      else
        Idx := ActivePageIndex - 1;
    end else if Tab.PageIndex <= ActivePageIndex then
      Idx := ActivePageIndex - 1
    else
      Idx := ActivePageIndex;

    if Idx < 0 then
      Idx := 0;
    if PageCount = 0 then
      Idx := -1;

    for i := FFocusList.Count - 1 downto 0 do
      if FFocusList[i] = Tab then
        FFocusList.Delete(i);

    Tab.Free;

    if FFocusList.Count > 0 then
      Idx := FFocusList[FFocusList.Count - 1].PageIndex;
    ActivePageIndex := Idx;
  finally
    LockWindowUpdate(0);
    FRemoving := False;
  end;
end;

function TMPageControl.GetCloseButtonTabIndex(const P: TPoint): Integer;
var
  i: Integer;
begin
  for i := 0 to PageCount - 1 do
    if Pages[i].FShowCloseButton and Pages[i].FButtonRect.Contains(P) then
      Exit(i);
  Exit(-1);
end;

procedure TMPageControl.WndProc(var Message: TMessage);
var
  i: Integer;
  Text: UnicodeString;
  NewText: string;
  Item: PTCITEM;
  Org: Pointer = nil;
begin
  if Message.Msg = WM_CLOSETAB then
  begin
    case Message.WParam of
      CT_ACTIVE:
        RemoveTab(Pages[Message.LParam]);
      CT_ALL:
        for i := PageCount - 1 downto 0 do
          RemoveTab(Pages[i]);
      CT_ALL_BUT_ACTIVE:
        for i := PageCount - 1 downto 0 do
          if Pages[i] <> ActivePage then
            RemoveTab(Pages[i]);
    end;
    Exit;
  end;

  if (Message.Msg = TCM_INSERTITEMW) or (Message.Msg = TCM_SETITEMW) then
  begin
    Item := PTCITEM(Message.lParam);

    Org := Item.pszText;
    Text := PWideChar(Item.pszText);

    NewText := Text;
    NewText := NewText.TrimRight(PADDING_CHAR);

    if (FMaxTabWidth = 0) and Pages[Message.wParam].FShowCloseButton then
      NewText := NewText + TMStringFunctions.StringForWidth(PADDING_CHAR, Pages[Message.wParam].FButtonRect.Width + Scale96ToFont(TABSHEET_PADDING) * 2, Font)
    else if (FMaxTabWidth > 0) and Pages[Message.wParam].FShowCloseButton then
      NewText := TMStringFunctions.TruncateText(NewText, FMaxTabWidth - (TabRect(Message.wParam).Width - Pages[Message.wParam].FButtonRect.Left), Font) + TMStringFunctions.StringForWidth(
        PADDING_CHAR, Pages[Message.wParam].FButtonRect.Width, Font)
    else if (FMaxTabWidth > 0) and (not Pages[Message.wParam].FShowCloseButton) then
      NewText := TMStringFunctions.TruncateText(NewText, FMaxTabWidth, Font);

    Text := NewText;
    Item.pszText := PChar(Text);
  end;

  inherited;

  if Org <> nil then
    Item.pszText := Org;
end;

procedure TMPageControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ButtonIndex: Integer;
begin
  inherited MouseMove(Shift, X, Y);

  ButtonIndex := GetCloseButtonTabIndex(TPoint.Create(X, Y));

  if (ButtonIndex > -1) <> FMouseInButton then
    Invalidate;

  FMouseInButton := ButtonIndex > -1;
end;

procedure TMPageControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button <> mbLeft then
    Exit;

  FPressedButton := GetCloseButtonTabIndex(TPoint.Create(X, Y));
  if FPressedButton > -1 then
  begin
    MouseCapture := True;
    Invalidate;
  end;
end;

procedure TMPageControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if FPressedButton > -1 then
  begin
    if Pages[FPressedButton].FButtonRect.Contains(TPoint.Create(X, Y)) then
      RemoveTab(Pages[FPressedButton]);

    FPressedButton := -1;
    MouseCapture := False;

    Invalidate;
  end;
end;

procedure TMPageControl.MouseLeave;
begin
  inherited MouseLeave;

  if FMouseInButton then
    Invalidate;
end;

end.
