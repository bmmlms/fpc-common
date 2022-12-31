{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2021 Alexander Nottelmann

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

unit MControls;

interface

uses
  Buttons,
  Classes,
  ComboEx,
  ComCtrls,
  Controls,
  DragDrop,
  DragDropFile,
  Forms,
  Functions,
  Generics.Collections,
  Graphics,
  GraphType,
  ImgList,
  LanguageObjects,
  LMessages,
  Math,
  Menus,
  Messages,
  StdCtrls,
  SysUtils,
  Themes,
  Types,
  VirtualTrees,
  Windows;

type

  { TMTabSheet }

  TMTabSheet = class(TTabSheet)
  private
    FCaption: TCaption;
    FShowCloseButton: Boolean;
    FButtonRect: TRect;

    FFocusedControlBeforeChange: TWinControl;

    procedure UpdateProperties;
    procedure FSetCaption(Value: TCaption);
    procedure FSetShowCloseButton(Value: Boolean);
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
    PADDING_CHAR = ' ';
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

  { TMVirtualStringTree }

  TMVirtualStringTree = class(TVirtualStringTree, IPostTranslatable)
  private
    FSuppressNodeEdit: Boolean;
  protected
    procedure WMRButtonUp(var Message: TLMRButtonUp); message LM_RBUTTONUP;
  public
    constructor Create(AOwner: TComponent); override;

    function CanEdit(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;

    procedure PostTranslate; virtual;
  end;

  TMShowHidePanel = class(TCustomControl)
  private
    FShowCaption: string;
    FHideCaption: string;

    FBuffer: Graphics.TBitmap;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property ShowCaption: string read FShowCaption write FShowCaption;
    property HideCaption: string read FHideCaption write FHideCaption;
  end;

  { TComboBoxExEditable }

  TComboBoxExEditable = class(TComboBoxEx, IFPObserver)
  private
  const
    WM_ALIGNEDIT = WM_USER + 1;
    WM_SETTEXTBYINDEX = WM_USER + 2;
  private
    FSettingText: Boolean;
    FEditRect: TRect;
    FItemIndexBeforeDropDown: Integer;

    function FGetFocusedItemData: TCustomData;
  protected
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure Select; override;
    procedure SetTextByIndex(var Msg: TMessage); message WM_SETTEXTBYINDEX;
    procedure AlignEdit(var Msg: TMessage); message WM_ALIGNEDIT;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetItemIndex(const Val: Integer); override;
    procedure DropDown; override;
    procedure CloseUp; override;
    procedure Change; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);

    property FocusedItemData: TCustomData read FGetFocusedItemData;
  end;

  TWinControlFocuser = class helper for TWinControl
  public
    procedure ApplyFocus;
  end;

const
  TABSHEET_PADDING = 3;

implementation

{ TMTabSheet }

function TMTabSheet.CanClose: Boolean;
begin
  Result := True;
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
      NewText := NewText + TFunctions.StringForWidth(PADDING_CHAR, Pages[Message.wParam].FButtonRect.Width + MulDiv(TABSHEET_PADDING, Screen.PixelsPerInch, 96), Font)
    else if (FMaxTabWidth > 0) and Pages[Message.wParam].FShowCloseButton then
      NewText := TFunctions.TruncateText(NewText, FMaxTabWidth - (TabRect(Message.wParam).Width - Pages[Message.wParam].FButtonRect.Left), Font) + TFunctions.StringForWidth(PADDING_CHAR, Pages[Message.wParam].FButtonRect.Width, Font)
    else if (FMaxTabWidth > 0) and (not Pages[Message.wParam].FShowCloseButton) then
      NewText := TFunctions.TruncateText(NewText, FMaxTabWidth, Font);

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

{ TMVirtualStringTree }

constructor TMVirtualStringTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoScrollDelay := 50;
  AutoScrollInterval := 400;
  IncrementalSearch := isVisibleOnly;
  HintMode := hmTooltip;
  ShowHint := True;

  TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages, toHideFocusRect];
  TreeOptions.AutoOptions := [toAutoScroll, toAutoScrollOnExpand];
  TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toFullRowSelect];

  Header.Options := [hoColumnResize, hoDrag, hoAutoResize, hoHotTrack, hoShowSortGlyphs, hoVisible];
end;

function TMVirtualStringTree.CanEdit(Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  if FSuppressNodeEdit then
    Exit(False);

  Result := inherited;
end;

procedure TMVirtualStringTree.WMRButtonUp(var Message: TLMRButtonUp);
begin
  FSuppressNodeEdit := True;
  try
    inherited;
  finally
    FSuppressNodeEdit := False;
  end;
end;

procedure TMVirtualStringTree.PostTranslate;
begin
  Invalidate;
end;

{ TMShowHidePanel }

procedure TMShowHidePanel.Click;
begin
  inherited;

end;

constructor TMShowHidePanel.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TMShowHidePanel.Destroy;
begin
  FBuffer.Free;

  inherited;
end;

procedure TMShowHidePanel.Paint;
begin
  inherited;

  if FBuffer = nil then
    Exit;

  FBuffer.Canvas.Brush.Color := clRed;
  FBuffer.Canvas.FillRect(Classes.Rect(0, 0, 10, 10));

  SetBkMode(FBuffer.Canvas.Handle, TRANSPARENT);
  FBuffer.Canvas.TextOut(20, 0, 'Show filters...');

  FBuffer.Canvas.PenPos := Classes.Point(30, 8);
  FBuffer.Canvas.LineTo(100, 8);

  Canvas.Draw(0, 0, FBuffer);
end;

procedure TMShowHidePanel.Resize;
begin
  inherited;

  if FBuffer <> nil then
    FBuffer.Free;

  FBuffer := Graphics.TBitmap.Create;
  FBuffer.Width := ClientWidth;
  FBuffer.Height := ClientHeight;
end;

{ TComboBoxExEditable }

procedure TComboBoxExEditable.AlignEdit(var Msg: TMessage);
var
  EditHandle: THandle;
  EditTextRect: TRect;
begin
  EditHandle := FindWindowEx(Handle, 0, 'Edit', nil);

  GetWindowRect(EditHandle, @FEditRect);

  Windows.ScreenToClient(GetParent(EditHandle), @FEditRect.TopLeft);
  Windows.ScreenToClient(GetParent(EditHandle), @FEditRect.BottomRight);

  SendMessage(EditHandle, EM_GETRECT, 0, LPARAM(@EditTextRect));

  if Assigned(Images) then
    MoveWindow(EditHandle, 16 + FEditRect.Left * 2, ClientRect.Height div 2 - EditTextRect.Height div 2, FEditRect.Width - 16 - FEditRect.Left * 2, EditTextRect.Height, False)
  else
    MoveWindow(EditHandle, FEditRect.Left, ClientRect.Height div 2 - EditTextRect.Height div 2, FEditRect.Width, EditTextRect.Height, False);

  Repaint;
end;

function TComboBoxExEditable.FGetFocusedItemData: TCustomData;
begin
  if ItemIndex = -1 then
    Exit(nil);

  Exit(ItemsEx[ItemIndex].Data);
end;

procedure TComboBoxExEditable.WMPaint(var Msg: TLMPaint);
begin
  inherited;

  if not Assigned(Images) then
    Exit;

  Canvas.Brush.Color := clWindow;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(FEditRect.Left, FEditRect.Top, FEditRect.Left + 16, FEditRect.Top + 16);

  if ItemIndex > -1 then
    Images.Resolution[16].Draw(Canvas, FEditRect.Left, ClientRect.Height div 2 - 16 div 2, ItemsEx[ItemIndex].ImageIndex, gdeNormal)
  else if FItemIndexBeforeDropDown > -1 then
    Images.Resolution[16].Draw(Canvas, FEditRect.Left, ClientRect.Height div 2 - 16 div 2, ItemsEx[FItemIndexBeforeDropDown].ImageIndex, gdeNormal);
end;

procedure TComboBoxExEditable.Select;
begin
  inherited Select;

  if not DroppedDown then
    FItemIndexBeforeDropDown := -2;

  PostMessage(Handle, WM_SETTEXTBYINDEX, 0, 0);
end;

procedure TComboBoxExEditable.SetTextByIndex(var Msg: TMessage);
begin
  if ItemIndex > -1 then
  begin
    FSettingText := True;
    Text := ItemsEx[ItemIndex].Caption;
    FSettingText := False;
    if Focused then
      SelectAll;
  end;
end;

procedure TComboBoxExEditable.DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);

  if not HandleAllocated then
    Exit;

  PostMessage(Handle, WM_ALIGNEDIT, 0, 0);
end;

procedure TComboBoxExEditable.SetItemIndex(const Val: Integer);
begin
  if FSettingText then
    Exit;

  inherited SetItemIndex(Val);

  PostMessage(Handle, WM_SETTEXTBYINDEX, 0, 0);
end;

procedure TComboBoxExEditable.DropDown;
begin
  FItemIndexBeforeDropDown := ItemIndex;

  inherited DropDown;
end;

procedure TComboBoxExEditable.CloseUp;
begin
  inherited CloseUp;

  if FItemIndexBeforeDropDown > -2 then
    ItemIndex := FItemIndexBeforeDropDown;
end;

procedure TComboBoxExEditable.Change;
begin
  inherited Change;

  FItemIndexBeforeDropDown := -2;

  Repaint;
end;

constructor TComboBoxExEditable.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FItemIndexBeforeDropDown := -2;
  TCustomComboBox(Self).Style := csOwnerDrawEditableFixed;

  FWinControlFlags += [wcfEraseBackground];

  ItemsEx.FPOAttachObserver(Self);
end;

destructor TComboBoxExEditable.Destroy;
begin
  ItemsEx.FPODetachObserver(Self);

  inherited Destroy;
end;

procedure TComboBoxExEditable.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
var
  Item: TComboExItem absolute Data;
begin
  if HandleAllocated and (Operation = ooChange) and Assigned(Item) and (Item.Index = ItemIndex) then
    PostMessage(Handle, WM_SETTEXTBYINDEX, 0, 0);
end;

{ TWinControlFocuser }

procedure TWinControlFocuser.ApplyFocus;
begin
  try
    SetFocus;
  except
  end;
end;

end.
