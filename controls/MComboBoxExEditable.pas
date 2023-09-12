unit MComboBoxExEditable;

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
  LCLIntf,
  LCLType,
  LMessages,
  Math,
  Menus,
  StdCtrls,
  SysUtils,
  Themes,
  Types,
  Windows;

type

  { TMComboBoxExEditable }

  TMComboBoxExEditable = class(TComboBoxEx, IFPObserver)
  private
  const
    WM_ALIGNEDIT = WM_USER + 1;
    WM_SETTEXTBYINDEX = WM_USER + 2;
  private
    FSettingText: Boolean;
    FEditRect: TRect;
    FItemIndexBeforeDropDown: Integer;
    FBuffer: Graphics.TBitmap;

    function FGetFocusedItemData: TCustomData;
  protected
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure WMWindowPosChanging(var Message: TLMWindowPosChanging); message LM_WINDOWPOSCHANGING;
    procedure SetTextByIndex(var Msg: TMessage); message WM_SETTEXTBYINDEX;
    procedure AlignEdit(var Msg: TMessage); message WM_ALIGNEDIT;
    procedure Select; override;
    procedure SetItemIndex(const Val: Integer); override;
    procedure DropDown; override;
    procedure CloseUp; override;
    procedure Change; override;
    procedure DrawItem(Index: Integer; ARect: TRect; State: StdCtrls.TOwnerDrawState); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);

    property FocusedItemData: TCustomData read FGetFocusedItemData;
  end;

procedure Register;

implementation

procedure Register;
const
  Classes: array of TComponentClass = [TMComboBoxExEditable];
begin
  RegisterComponents('MControls', Classes);
end;

{ TMComboBoxExEditable }

procedure TMComboBoxExEditable.AlignEdit(var Msg: TMessage);
var
  EditTextRect: TRect;
  ComboBoxInfo: TComboboxInfo;
begin
  ComboBoxInfo.cbSize := SizeOf(ComboBoxInfo);
  GetComboBoxInfo(Handle, @ComboBoxInfo);

  FEditRect := ComboBoxInfo.rcItem;

  SendMessage(ComboBoxInfo.hwndItem, EM_GETRECT, 0, LPARAM(@EditTextRect));

  if Assigned(Images) then
    MoveWindow(ComboBoxInfo.hwndItem, Scale96ToFont(20) + FEditRect.Left, ClientRect.Height div 2 - EditTextRect.Height div 2, FEditRect.Width - Scale96ToFont(20) - FEditRect.Left, EditTextRect.Height, False)
  else
    MoveWindow(ComboBoxInfo.hwndItem, FEditRect.Left, ClientRect.Height div 2 - EditTextRect.Height div 2, FEditRect.Width, EditTextRect.Height, False);

  Repaint;
end;

procedure TMComboBoxExEditable.WMWindowPosChanging(var Message: TLMWindowPosChanging);
begin
  inherited;

  PostMessage(Handle, WM_ALIGNEDIT, 0, 0);
end;

function TMComboBoxExEditable.FGetFocusedItemData: TCustomData;
begin
  if ItemIndex = -1 then
    Exit(nil);

  Exit(ItemsEx[ItemIndex].Data);
end;

procedure TMComboBoxExEditable.WMPaint(var Msg: TLMPaint);
begin
  inherited;

  if not Assigned(Images) then
    Exit;

  FBuffer.Canvas.Brush.Color := clWindow;
  FBuffer.Canvas.Brush.Style := bsSolid;
  FBuffer.Canvas.FillRect(0, 0, Scale96ToFont(16), Scale96ToFont(16));

  if (not DroppedDown) and (ItemIndex > -1) then
    Images.Resolution[Scale96ToFont(16)].Draw(FBuffer.Canvas, 0, 0, ItemsEx[ItemIndex].ImageIndex, gdeNormal)
  else if FItemIndexBeforeDropDown > -1 then
    Images.Resolution[Scale96ToFont(16)].Draw(FBuffer.Canvas, 0, 0, ItemsEx[FItemIndexBeforeDropDown].ImageIndex, gdeNormal);

  Canvas.Draw(FEditRect.Left + 2, ClientRect.Height div 2 - Scale96ToFont(16) div 2, FBuffer);
end;

procedure TMComboBoxExEditable.Select;
begin
  inherited Select;

  if not DroppedDown then
    FItemIndexBeforeDropDown := -2;

  PostMessage(Handle, WM_SETTEXTBYINDEX, 0, 0);
end;

procedure TMComboBoxExEditable.SetTextByIndex(var Msg: TMessage);
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

procedure TMComboBoxExEditable.SetItemIndex(const Val: Integer);
begin
  if FSettingText then
    Exit;

  inherited SetItemIndex(Val);

  PostMessage(Handle, WM_SETTEXTBYINDEX, 0, 0);
end;

procedure TMComboBoxExEditable.DropDown;
begin
  FItemIndexBeforeDropDown := ItemIndex;

  inherited DropDown;
end;

procedure TMComboBoxExEditable.CloseUp;
begin
  inherited CloseUp;

  if FItemIndexBeforeDropDown > -2 then
    ItemIndex := FItemIndexBeforeDropDown;
end;

procedure TMComboBoxExEditable.Change;
begin
  inherited Change;

  FItemIndexBeforeDropDown := -2;

  Repaint;
end;

// Copied from base class with modified drawing of icons
procedure TMComboBoxExEditable.DrawItem(Index: Integer; ARect: TRect; State: StdCtrls.TOwnerDrawState);
const
  caThemes: array [Boolean] of TThemedButton = (tbPushButtonDisabled, tbPushButtonNormal);
  cItemIndent: SmallInt = 2;
var
  aDetail: TThemedElementDetails;
  aDropped: Boolean;
  aEnabled: Boolean;
  aFlags: Cardinal;
  aFocusedEditableMainItemNoDD: Boolean;
  aImgPoint: TPoint;
  aIndent: SmallInt;
  aItemIndex: SmallInt;
  aMainItem: Boolean;
  anyRect: TRect;
  ImagesSize: TSize;
begin
  aDropped := DroppedDown;
  aEnabled := IsEnabled;
  aMainItem := (ARect.Left > 0);
  {$IF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
  aFocusedEditableMainItemNoDD := (Focused and aMainItem and not aDropped);
  {$ELSE}
  aFocusedEditableMainItemNoDD := False;
  {$ENDIF}
  if aDropped and not aMainItem or aFocusedEditableMainItemNoDD then
  begin
    if not (LCLType.odSelected in State) then
      Canvas.Brush.Color := clWindow;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ARect);
  end;
  aDetail := ThemeServices.GetElementDetails(caThemes[aEnabled]);
  if FNeedMeasure then
  begin
    FTextHeight := Canvas.TextHeight('ŠjÁÇ');
    FNeedMeasure := False;
  end;
  if not aMainItem
  then
    aIndent := TComboExItem(ItemsEx[Index]).Indent
  else
    aIndent := -1;
  if aIndent < 0 then
    aIndent := 0;
  Inc(aIndent, cItemIndent);
  if assigned(Images) then
  begin
    aItemIndex := -1;
    ImagesSize := Images.SizeForPPI[16, Font.PixelsPerInch];
    if (aMainItem or (LCLType.odSelected in State)) and
      ((ItemsEx[Index].SelectedImageIndex >= 0) and (ItemsEx[Index].SelectedImageIndex < Images.Count))
    then
      aItemIndex := ItemsEx[Index].SelectedImageIndex;
    if aItemIndex < 0 then
      aItemIndex := ItemsEx[Index].ImageIndex;
    if aItemIndex >= 0 then
    begin
      if not FRightToLeft
      then
        aImgPoint.X := ARect.Left + aIndent
      else
        aImgPoint.X := ARect.Right - aIndent - ImagesSize.cx;
      aImgPoint.Y := (ARect.Bottom + ARect.Top - ImagesSize.cy) div 2;
      //ThemeServices.DrawIcon(Canvas, aDetail, aImgPoint, Images, aItemIndex);
      Images.Resolution[Scale96ToFont(16)].Draw(Canvas, aImgPoint.X, aImgPoint.Y, aItemIndex, True);
    end;
    Inc(aIndent, ImagesSize.cx + 2 * cItemIndent);
  end;
  Canvas.Brush.Style := bsClear;
  if (not (LCLType.odSelected in State) or not aDropped) and not aFocusedEditableMainItemNoDD
  then
    Canvas.Font.Color := clWindowText
  else
    Canvas.Font.Color := clHighlightText;
  if aFocusedEditableMainItemNoDD then
  begin
    LCLIntf.SetBkColor(Canvas.Handle, ColorToRGB(clBtnFace));
    LCLIntf.DrawFocusRect(Canvas.Handle, aRect);
  end;
  aFlags := DT_END_ELLIPSIS + DT_VCENTER + DT_SINGLELINE + DT_NOPREFIX;
  if not FRightToLeft then
  begin
    anyRect.Left := ARect.Left + aIndent;
    anyRect.Right := ARect.Right;
  end else
  begin
    anyRect.Right := ARect.Right - aIndent;
    anyRect.Left := ARect.Left;
    aFlags := aFlags or DT_RIGHT or DT_RTLREADING;
  end;
  anyRect.Top := (ARect.Top + ARect.Bottom - FTextHeight) div 2;
  anyRect.Bottom := anyRect.Top + FTextHeight;
  DrawTextW(Canvas.Handle, PWideChar(UnicodeString(ItemsEx[Index].Caption)), Length(ItemsEx[Index].Caption), anyRect, aFlags);
end;

constructor TMComboBoxExEditable.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  ItemHeight := Scale96ToFont(17);
  FItemIndexBeforeDropDown := -2;
  TCustomComboBox(Self).Style := csOwnerDrawEditableFixed;
  FBuffer := Graphics.TBitmap.Create;
  FBuffer.SetSize(Scale96ToFont(16), Scale96ToFont(16));

  FWinControlFlags += [wcfEraseBackground];

  ItemsEx.FPOAttachObserver(Self);
end;

destructor TMComboBoxExEditable.Destroy;
begin
  FBuffer.Free;
  ItemsEx.FPODetachObserver(Self);

  inherited Destroy;
end;

procedure TMComboBoxExEditable.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
var
  Item: TComboExItem absolute Data;
begin
  if HandleAllocated and (Operation = ooChange) and Assigned(Item) and (Item.Index = ItemIndex) then
    PostMessage(Handle, WM_SETTEXTBYINDEX, 0, 0);
end;

end.
