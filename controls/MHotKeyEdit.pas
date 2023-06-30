unit MHotkeyEdit;

interface

uses
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  ExtCtrls,
  Graphics,
  Math,
  Menus,
  StdCtrls,
  SysUtils,
  Themes,
  Windows;

type
  TMHKModifier = (hkShift, hkCtrl, hkAlt, hkExt);
  TMHKModifiers = set of TMHKModifier;
  TMHKInvalidKey = (hcNone, hcShift, hcCtrl, hcAlt, hcShiftCtrl, hcShiftAlt, hcCtrlAlt, hcShiftCtrlAlt);
  TMHKInvalidKeys = set of TMHKInvalidKey;

  { TMHotkeyEdit }

  TMHotkeyEdit = class(TCustomEdit)
  private
    FInternalUpdate: Integer;
    FHotkeyUpdate: Integer;
    FHotKey: Word;
    FModifiers: TMHKModifiers;
    FHotModifiers: TMHKModifiers;
    FInvalidKeys: TMHKInvalidKeys;

    procedure UpdateHotKey(Value: TShortcut);
    function GetHotKey: TShortcut;
    procedure SetHotKey(Value: TShortcut);
    procedure ShortcutToHotKey(Value: TShortcut);
    procedure UpdateHotText;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure RealSetText(const AValue: TCaption); override;
    procedure KeyPress(var Key: char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    property HotKey: TShortcut read GetHotKey write SetHotKey;
  published
    property Align;
    property OnChange;
  end;

function ShortcutToText(Shortcut: TShortcut): string;

implementation

const
  DefaultModifiers = [hkAlt];
  DefaultInvalidKeys = [hcNone, hcShift];

const
  skNone = '';
  skBkSp = 'BkSp';
  skTab = 'Tab';
  skEsc = 'Esc';
  skEnter = 'Enter';
  skSpace = 'Space';
  skPgUp = 'PgUp';
  skPgDn = 'PgDn';
  skEnd = 'End';
  skHome = 'Home';
  skLeft = 'Left';
  skUp = 'Up';
  skRight = 'Right';
  skDown = 'Down';
  skIns = 'Ins';
  skDel = 'Del';
  skShift = 'Shift+';
  skCtrl = 'Ctrl+';
  skAlt = 'Alt+';
  skMediaNext = 'Next track';
  skMediaPrev = 'Previous track';
  skMediaStop = 'Stop';
  skMediaPlayPause = 'Play/pause';

  M_LBUTTON = 1;
  M_RBUTTON = 2;
  M_SHIFT = 4;
  M_CONTROL = 8;
  M_MBUTTON = 16;
  M_MENU = 32;

type
  TMKeyCap = (kcBkSp, kcTab, kcEsc, kcEnter, kcSpace, kcPgUp, kcPgDn, kcEnd, kcHome, kcLeft, kcUp, kcRight, kcDown, kcIns, kcDel, kcShift, kcCtrl, kcAlt, kcMediaNext, kcMediaPrev, kcMediaStop, kcMediaPlayPause);

var
  KeyCaps: array[TMKeyCap] of string = (skBkSp, skTab, skEsc, skEnter, skSpace, skPgUp, skPgDn, skEnd, skHome, skLeft, skUp, skRight, skDown, skIns, skDel, skShift, skCtrl, skAlt, skMediaNext, skMediaPrev, skMediaStop, skMediaPlayPause);

function Shortcut(Key: Word; Shift: TShiftState): TShortcut;
begin
  Result := 0;
  if WordRec(Key).Hi <> 0 then
    Exit;
  Result := Key;
  if ssShift in Shift then
    Inc(Result, scShift);
  if ssCtrl in Shift then
    Inc(Result, scCtrl);
  if ssAlt in Shift then
    Inc(Result, scAlt);
end;

function GetSpecialKeyName(Shortcut: TShortcut): string;
var
  ScanCode: Integer;
  KeyName: array[0..255] of Char;
begin
  Result := '';
  ScanCode := MapVirtualKey(WordRec(Shortcut).Lo, 0) shl 16;
  if ScanCode <> 0 then
  begin
    GetKeyNameText(ScanCode, KeyName, SizeOf(KeyName));
    Result := KeyName;
  end;
end;

function ShortcutToText(Shortcut: TShortcut): string;
var
  Name: string;
begin
  case WordRec(Shortcut).Lo of
    $08, $09:
      Name := KeyCaps[TMKeyCap(Ord(kcBkSp) + WordRec(Shortcut).Lo - $08)];
    $0D: Name := KeyCaps[kcEnter];
    $1B: Name := KeyCaps[kcEsc];
    $20..$28:
      Name := KeyCaps[TMKeyCap(Ord(kcSpace) + WordRec(Shortcut).Lo - $20)];
    $2D..$2E:
      Name := KeyCaps[TMKeyCap(Ord(kcIns) + WordRec(Shortcut).Lo - $2D)];
    $30..$39: Name := Chr(WordRec(Shortcut).Lo - $30 + Ord('0'));
    $41..$5A: Name := Chr(WordRec(Shortcut).Lo - $41 + Ord('A'));
    $60..$69: Name := Chr(WordRec(Shortcut).Lo - $60 + Ord('0'));
    $70..$87: Name := 'F' + IntToStr(WordRec(Shortcut).Lo - $6F);
    $B0..$B3: Name := KeyCaps[TMKeyCap(Ord(kcMediaNext) + WordRec(Shortcut).Lo - $B0)];
    else
      Name := GetSpecialKeyName(Shortcut);
  end;

  if Name <> '' then
  begin
    Result := '';
    if Shortcut and scCtrl <> 0 then
      Result := Result + KeyCaps[kcCtrl];
    if Shortcut and scShift <> 0 then
      Result := Result + KeyCaps[kcShift];
    if Shortcut and scAlt <> 0 then
      Result := Result + KeyCaps[kcAlt];
    Result := Result + Name;
  end else
    Result := '';
end;

function ShiftStateToKeys(Shift: TShiftState): Word;
begin
  Result := 0;
  if ssShift in Shift then
    Result := Result or M_SHIFT;
  if ssCtrl in Shift then
    Result := Result or M_CONTROL;
  if ssLeft in Shift then
    Result := Result or M_LBUTTON;
  if ssRight in Shift then
    Result := Result or M_RBUTTON;
  if ssMiddle in Shift then
    Result := Result or M_MBUTTON;
  if ssAlt in Shift then
    Result := Result or M_MENU;
end;

function KeysToShiftState(Keys: Word): TShiftState;
begin
  Result := [];
  if Keys and MK_SHIFT <> 0 then
    Include(Result, ssShift);
  if Keys and MK_CONTROL <> 0 then
    Include(Result, ssCtrl);
  if Keys and MK_LBUTTON <> 0 then
    Include(Result, ssLeft);
  if Keys and MK_RBUTTON <> 0 then
    Include(Result, ssRight);
  if Keys and MK_MBUTTON <> 0 then
    Include(Result, ssMiddle);
  if GetKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);
end;

function KeyDataToShiftState(KeyData: Longint): TShiftState;
const
  AltMask = $20000000;
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then
    Include(Result, ssCtrl);
  if KeyData and AltMask <> 0 then
    Include(Result, ssAlt);
end;

function KeyboardStateToShiftState(const KeyboardState: TKeyboardState): TShiftState;
begin
  Result := [];
  if KeyboardState[VK_SHIFT] and $80 <> 0 then
    Include(Result, ssShift);
  if KeyboardState[VK_CONTROL] and $80 <> 0 then
    Include(Result, ssCtrl);
  if KeyboardState[VK_MENU] and $80 <> 0 then
    Include(Result, ssAlt);
  if KeyboardState[VK_LBUTTON] and $80 <> 0 then
    Include(Result, ssLeft);
  if KeyboardState[VK_RBUTTON] and $80 <> 0 then
    Include(Result, ssRight);
  if KeyboardState[VK_MBUTTON] and $80 <> 0 then
    Include(Result, ssMiddle);
end;

{ TMHotkeyEdit }

procedure TMHotkeyEdit.UpdateHotKey(Value: TShortcut);
begin
  if FHotkeyUpdate = 0 then
  begin
    Inc(FHotkeyUpdate);
    try
      SetHotKey(Value);
    finally
      Dec(FHotkeyUpdate);
    end;
  end;
end;

function TMHotkeyEdit.GetHotKey: TShortcut;
var
  Shift: TShiftState;
begin
  Shift := [];

  if hkShift in FHotModifiers then
    Include(Shift, ssShift);
  if hkCtrl in FHotModifiers then
    Include(Shift, ssCtrl);
  if hkAlt in FHotModifiers then
    Include(Shift, ssAlt);

  if (Shift = []) then
  begin
    if hkShift in FModifiers then
      Include(Shift, ssShift);
    if hkCtrl in FModifiers then
      Include(Shift, ssCtrl);
    if hkAlt in FModifiers then
      Include(Shift, ssAlt);
  end;

  Result := Shortcut(FHotKey, Shift);
end;

procedure TMHotkeyEdit.SetHotKey(Value: TShortcut);
begin
  ShortcutToHotKey(Value);
  UpdateHotText;
end;

procedure TMHotkeyEdit.ShortcutToHotKey(Value: TShortcut);
begin
  if (Value = VK_BACK) or (Value = VK_DELETE) then
    Value := 0;

  FHotKey := Value and not (scShift + scCtrl + scAlt);
  FHotModifiers := [];

  if Value and scShift <> 0 then
    Include(FHotModifiers, hkShift);
  if Value and scCtrl <> 0 then
    Include(FHotModifiers, hkCtrl);
  if Value and scAlt <> 0 then
    Include(FHotModifiers, hkAlt);

  if (hcShiftCtrlAlt in FInvalidKeys) and (FHotModifiers = [hkShift, hkCtrl, hkAlt]) then
    FHotModifiers := [];

  if (hcCtrlAlt in FInvalidKeys) and (FHotModifiers = [hkCtrl, hkAlt]) then
    FHotModifiers := [];

  if (hcShiftAlt in FInvalidKeys) and (FHotModifiers = [hkShift, hkAlt]) then
    FHotModifiers := [];

  if (hcShiftCtrl in FInvalidKeys) and (FHotModifiers = [hkShift, hkCtrl]) then
    FHotModifiers := [];

  if (hcAlt in FInvalidKeys) and (FHotModifiers = [hkAlt]) then
    FHotModifiers := [];

  if (hcCtrl in FInvalidKeys) and (FHotModifiers = [hkCtrl]) then
    FHotModifiers := [];

  if (hcShift in FInvalidKeys) and (FHotModifiers = [hkShift]) then
    FHotModifiers := [];

  if (hcNone in FInvalidKeys) and (FHotModifiers = []) then
    FHotModifiers := [];

  if (FHotModifiers = []) then
  begin
    if hkShift in FModifiers then
      Include(FHotModifiers, hkShift);
    if hkCtrl in FModifiers then
      Include(FHotModifiers, hkCtrl);
    if hkAlt in FModifiers then
      Include(FHotModifiers, hkAlt);
  end;
end;

procedure TMHotkeyEdit.UpdateHotText;
var
  S: String;
begin
  Inc(FInternalUpdate);
  try
    if FHotKey <> 0 then
      S := ShortcutToText(GetHotKey)
    else
    begin
      if hkCtrl in FHotModifiers then
        S := S + skCtrl;
      if hkShift in FHotModifiers then
        S := S + skShift;
      if hkAlt in FHotModifiers then
        S := S + skAlt;
    end;

    RealSetText(S);
  finally
    Dec(FInternalUpdate);
  end;
end;

procedure TMHotkeyEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  K: Word;
begin
  K := Key;
  Key := 0;
  FHotModifiers := [];

  if K in [VK_SHIFT, VK_CONTROL, VK_MENU] then
    K := 0;

  UpdateHotKey(Shortcut(K, Shift));
end;

procedure TMHotkeyEdit.RealSetText(const AValue: TCaption);
begin
  if FInternalUpdate > 0 then
  begin
    inherited RealSetText(AValue);

    SetCaretPos(TPoint.Create(Length(Text), 0));
  end;
end;

procedure TMHotkeyEdit.KeyPress(var Key: char);
begin
  Key := #0;

  inherited KeyPress(Key);
end;

procedure TMHotkeyEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  Key := 0;
  if FHotKey = 0 then
  begin
    FHotModifiers := [];
    UpdateHotText;
  end;
end;

end.
