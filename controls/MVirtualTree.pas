unit MVirtualTree;

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
  VirtualTrees,
  Windows;

type

  { TMStringEditLink }

  TMStringEditLink = class(TStringEditLink)
  public
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override; stdcall;
  end;

  { TMVirtualTree }

  TMVirtualTree = class(TVirtualStringTree)
  private
    FPaintedOnce: Boolean;
    FAsyncCallPending: Boolean;
    FSuppressNodeEdit: Boolean;
    FSelectFirst: Boolean;

    FOnSelectionChange: TNotifyEvent;
  protected
    procedure DoChange(Node: PVirtualNode); override;
    function DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink; override;
    function DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode; var Effect: LongWord): Boolean; override;
    procedure DoBeforePaint(Canvas: TCanvas); override;
    procedure WMRButtonDown(var Message: TLMRButtonDown); message LM_RBUTTONDOWN;
    procedure WMRButtonUp(var Message: TLMRButtonUp); message LM_RBUTTONUP;
    procedure WMContextMenu(var Message: TLMContextMenu); message LM_CONTEXTMENU;
    procedure WMSetFocus(var Msg: TLMSetFocus); message LM_SETFOCUS;
    procedure HandleMouseUp(Keys: PtrUInt; const HitInfo: THitInfo); override;
    procedure WndProc(var Message: TLMessage); override;

    procedure AfterChange(Data: PtrInt);
    procedure ResetColors;
  public
    constructor Create(AOwner: TComponent); override;

    function CanEdit(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;

    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

procedure Register;

implementation

procedure Register;
const
  Classes: array of TComponentClass = [TMVirtualTree];
begin
  RegisterComponents('MControls', Classes);
end;

{ TMStringEditLink }

function TMStringEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
begin
  Result := inherited PrepareEdit(Tree, Node, Column);

  if Assigned(Edit) then
    Edit.Font := Tree.Font;
end;

{ TMVirtualTree }

constructor TMVirtualTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoScrollDelay := 50;
  AutoScrollInterval := 400;
  IncrementalSearch := isVisibleOnly;
  HintMode := hmTooltip;

  TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages, toHideFocusRect];
  TreeOptions.AutoOptions := [toAutoScroll, toAutoScrollOnExpand];
  TreeOptions.SelectionOptions := [toMultiSelect, toFullRowSelect];

  Header.Options := [hoColumnResize, hoDrag, hoAutoResize, hoHotTrack, hoShowSortGlyphs, hoVisible];
end;

function TMVirtualTree.CanEdit(Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  if FSuppressNodeEdit then
    Exit(False);

  Result := inherited;
end;

procedure TMVirtualTree.DoChange(Node: PVirtualNode);
begin
  inherited;

  if not FAsyncCallPending then
    Application.QueueAsyncCall(AfterChange, 0);
end;

function TMVirtualTree.DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink;
begin
  Result := TMStringEditLink.Create;
end;

function TMVirtualTree.DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode; var Effect: LongWord): Boolean;
var
  HI: THitInfo;
begin
  GetHitTestInfoAt(Pt.X, Pt.Y, True, HI);
  Exit(not ((hiAbove in HI.HitPositions) or (hiBelow in HI.HitPositions) or (hiToLeft in HI.HitPositions) or (hiToRight in HI.HitPositions)));
end;

procedure TMVirtualTree.WMRButtonDown(var Message: TLMRButtonDown);
var
  HitInfo: THitInfo;
begin
  inherited;

  if not CanFocus then
    Exit;

  GetHitTestInfoAt(Message.XPos, Message.YPos, True, HitInfo);

  if Assigned(HitInfo.HitNode) and not Selected[HitInfo.HitNode] then
  begin
    ClearSelection;
    Selected[HitInfo.HitNode] := True;
    SetFocus;
  end else if not Assigned(HitInfo.HitNode) then
    ClearSelection;
end;

procedure TMVirtualTree.WMRButtonUp(var Message: TLMRButtonUp);
begin
  FSuppressNodeEdit := True;
  try
    inherited;
  finally
    FSuppressNodeEdit := False;
  end;
end;

procedure TMVirtualTree.WMContextMenu(var Message: TLMContextMenu);
var
  P: TPoint;
  HitInfo: THitInfo;
begin
  if Message.Result <> 0 then
    Exit;

  P := GetMousePosFromMessage(Message.Pos);
  if P.X <> -1 then
    P := ScreenToClient(P);

  GetHitTestInfoAt(P.X, P.Y, True, HitInfo);

  if not (hiToRight in HitInfo.HitPositions) then
    inherited;
end;

procedure TMVirtualTree.WMSetFocus(var Msg: TLMSetFocus);
begin
  inherited;

  FSuppressNodeEdit := True;
end;

procedure TMVirtualTree.HandleMouseUp(Keys: PtrUInt; const HitInfo: THitInfo);
begin
  if FSuppressNodeEdit then
  begin
    Self.TreeStates := Self.TreeStates - [tsEditPending];
    FSuppressNodeEdit := False;
  end;

  inherited HandleMouseUp(Keys, HitInfo);
end;

procedure TMVirtualTree.WndProc(var Message: TLMessage);
begin
  inherited WndProc(Message);

  if (Message.msg >= WM_KEYFIRST) and (Message.msg <= WM_KEYLAST) then
    FSuppressNodeEdit := False;
end;

procedure TMVirtualTree.DoBeforePaint(Canvas: TCanvas);
begin
  // Since some trees get populated with a delay (ChartsTree, LogTree) initially select the first node this way.
  if not FPaintedOnce and (RootNodeCount > 0) then
  begin
    FPaintedOnce := True;

    if FSelectFirst then
      Selected[GetFirst] := True;
    Self.OffsetY := 0;
  end;

  inherited DoBeforePaint(Canvas);
end;

procedure TMVirtualTree.AfterChange(Data: PtrInt);
begin
  FAsyncCallPending := False;

  if (not Application.Terminated) and Assigned(OnSelectionChange) then
    OnSelectionChange(Self);
end;

procedure TMVirtualTree.ResetColors;
var
  Colors: TVTColors;
begin
  Colors := TVTColors.Create(Self);
  try
    Self.Colors.Assign(Colors);
    Font.Color := clWindowText;
    Color := clWindow;
  finally
    Colors.Free;
  end;
end;

end.
