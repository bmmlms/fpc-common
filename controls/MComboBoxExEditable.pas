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
  LMessages,
  Math,
  Menus,
  StdCtrls,
  SysUtils,
  Themes,
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
    MoveWindow(ComboBoxInfo.hwndItem, 20 + FEditRect.Left, ClientRect.Height div 2 - EditTextRect.Height div 2, FEditRect.Width - 20 - FEditRect.Left, EditTextRect.Height, False)
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
  FBuffer.Canvas.FillRect(0, 0, 16, 16);

  if (not DroppedDown) and (ItemIndex > -1) then
    Images.Resolution[16].Draw(FBuffer.Canvas, 0, 0, ItemsEx[ItemIndex].ImageIndex, gdeNormal)
  else if FItemIndexBeforeDropDown > -1 then
    Images.Resolution[16].Draw(FBuffer.Canvas, 0, 0, ItemsEx[FItemIndexBeforeDropDown].ImageIndex, gdeNormal);

  Canvas.Draw(FEditRect.Left + 2, ClientRect.Height div 2 - 16 div 2, FBuffer);
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

constructor TMComboBoxExEditable.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  ItemHeight := 17;
  FItemIndexBeforeDropDown := -2;
  TCustomComboBox(Self).Style := csOwnerDrawEditableFixed;
  FBuffer := Graphics.TBitmap.Create;
  FBuffer.SetSize(16, 16);

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
