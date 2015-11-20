{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2016 Alexander Nottelmann

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
  Windows, SysUtils, Classes, Messages, ComCtrls, ActiveX, Controls, Buttons,
  StdCtrls, Menus, VirtualTrees, DragDrop, DragDropFile, ShellApi, Types,
  Themes, ImgList, GUIFunctions, LanguageObjects, Graphics, Forms,
  Generics.Collections;

type
  TMTabSheet = class;

  TMTabSheetCloseButton = class(TSpeedButton)
  private
    FHotTrack: Boolean;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    property HotTrack: Boolean read FHotTrack write FHotTrack default True;
  end;

  TMPageControl = class(TPageControl)
  private
    FMaxTabWidth: Integer;
    FFocusList: TList<TMTabSheet>;

    procedure AlignButtons;
    procedure RemoveTab(Tab: TMTabSheet); virtual;
    procedure FSetMaxTabWidth(Value: Integer);

    procedure FSetActivePage(Value: TTabSheet);
    function FGetActivePage: TTabSheet;
  protected
    function CanChange: Boolean; override;
    procedure Change; override;

    procedure TabClosed(Tab: TMTabSheet); virtual;

    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure CloseTab(Idx: Integer);
    procedure CloseAll;
    procedure CloseAllButActive;

    property MaxTabWidth: Integer read FMaxTabWidth write FSetMaxTabWidth;
    property ActivePage: TTabSheet read FGetActivePage write FSetActivePage;
  end;

  TMTabSheet = class(TTabSheet)
  private
    FCaption: string;
    FButtonWidth: Integer;
    FMaxWidth: Integer;
    Button: TMTabSheetCloseButton;
    FShowCloseButton: Boolean;
    FOnClosed: TNotifyEvent;

    FFocusedControlBeforeChange: TWinControl;

    procedure SetCaptionInternal(Value: string);
    procedure FSetMaxWidth(Value: Integer);
    function FGetCaption: string;
    procedure FSetCaption(Value: string);
    procedure FSetShowCloseButton(Value: Boolean);

    procedure AlignButton;
  public
    constructor Create(AOwner: TComponent); reintroduce; virtual;
    destructor Destroy; override;
    procedure AfterCreate; virtual;

    function CanClose: Boolean; virtual;

    property Caption: string read FGetCaption write FSetCaption;
    property MaxWidth: Integer read FMaxWidth write FSetMaxWidth;
    property ShowCloseButton: Boolean read FShowCloseButton write FSetShowCloseButton;
    property OnClosed: TNotifyEvent read FOnClosed write FOnClosed;
  end;

  TMVirtualStringTree = class(TVirtualStringTree)
  private
  protected
    procedure NodeSelected(Node: PVirtualNode); virtual;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
  public
  end;

  TMStatusBar = class(TStatusBar)
  private
    function ShortenString(Panel: TStatusPanel; const Rect: TRect): string;
  protected
    procedure DrawPanel(Panel: TStatusPanel; const Rect: TRect); override;
  public
  end;

  TMShowHidePanel = class(TCustomControl)
  private
    FShowCaption: string;
    FHideCaption: string;

    FBuffer: TBitmap;
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

  TWinControlFocuser = class helper for TWinControl
  public
    procedure ApplyFocus;
  end;

implementation

{ TMTabSheetCloseButton }

constructor TMTabSheetCloseButton.Create(AOwner: TComponent);
begin
  inherited;
  FHotTrack := True;
  Width := 14;
  Height := 14;
  Flat := false;
end;

procedure TMTabSheetCloseButton.Click;
begin
  inherited;
  TMPageControl(Parent).CloseTab(TMTabSheet(Owner).PageIndex);
end;

procedure TMTabSheetCloseButton.Paint;
var
  uType: Integer;
  uState: Integer;
  Details: TThemedElementDetails;
  Win: TThemedWindow;
begin
  if ThemeServices.ThemesEnabled then
  begin
    if Enabled then
    begin
      if (Down) or (FState = bsDown) then
        Win := twSmallCloseButtonPushed
      else if MouseInControl then
        Win :=  twSmallCloseButtonHot
      else
        Win :=  twSmallCloseButtonNormal;
    end else
      Win :=  twCloseButtonNormal;
    Details := ThemeServices.GetElementDetails(Win);
    ThemeServices.DrawElement(Canvas.Handle, Details, ClientRect);
  end else
  begin
    uType := DFC_CAPTION;
    uState := DFCS_CAPTIONCLOSE;
    if Enabled then
    begin
      if (Down) or (FState = bsDown) then
        uState := uState or DFCS_PUSHED
      else if MouseInControl then
      begin
        if FHotTrack then
          uState := uState or DFCS_MONO
        else
          uState := uState or DFCS_FLAT;
      end else
        uState := uState or DFCS_FLAT;
    end else
      uState := uState or DFCS_INACTIVE;
    DrawFrameControl(Canvas.Handle, ClientRect, uType, uState);
  end;
end;

{ TMPageControl }

procedure TMPageControl.AlignButtons;
var
  i: Integer;
  P: TMTabSheet;
begin
  for i := 0 to PageCount - 1 do
  begin
    P := TMTabSheet(Pages[i]);
    P.AlignButton;
  end;
end;

function TMPageControl.CanChange: Boolean;
begin
  TMTabSheet(ActivePage).FFocusedControlBeforeChange := Screen.ActiveControl;

  Result := inherited;
end;

procedure TMPageControl.Change;
begin
  inherited;

  if TMTabSheet(ActivePage).FFocusedControlBeforeChange <> nil then
    TMTabSheet(ActivePage).FFocusedControlBeforeChange.ApplyFocus;

  FFocusList.Add(TMTabSheet(ActivePage));

  AlignButtons;
end;

procedure TMPageControl.CloseTab(Idx: Integer);
begin
  PostMessage(Handle, WM_USER + 1245, 0, Idx)
end;

constructor TMPageControl.Create(AOwner: TComponent);
begin
  inherited;

  FFocusList := TList<TMTabSheet>.Create;
end;

destructor TMPageControl.Destroy;
begin
  FFocusList.Free;

  inherited;
end;

procedure TMPageControl.CloseAll;
begin
  PostMessage(Handle, WM_USER + 1245, 1, 0)
end;

procedure TMPageControl.CloseAllButActive;
begin
  PostMessage(Handle, WM_USER + 1245, 2, 0);
end;

procedure TMPageControl.FSetMaxTabWidth(Value: Integer);
var
  i: Integer;
begin
  FMaxTabWidth := Value;
  for i := 0 to PageCount - 1 do
    TMTabSheet(Pages[i]).MaxWidth := Value;
end;

function TMPageControl.FGetActivePage: TTabSheet;
begin
  Result := inherited ActivePage;
end;

procedure TMPageControl.FSetActivePage(Value: TTabSheet);
begin
  inherited ActivePage := Value;

  Change;
end;

procedure TMPageControl.RemoveTab(Tab: TMTabSheet);
var
  Idx: Integer;
  i: Integer;
begin
  if not Tab.CanClose then
    Exit;

  LockWindowUpdate(Handle);
  try
    if Assigned(TMTabSheet(Tab).FOnClosed) then
      TMTabSheet(Tab).FOnClosed(Tab);

    if Tab = ActivePage then
    begin
      if PageCount - 1 > ActivePageIndex then
        Idx := ActivePageIndex
      else
        Idx := ActivePageIndex - 1;
    end else
    begin
      if Tab.PageIndex <= ActivePageIndex then
        Idx := ActivePageIndex - 1
      else
        Idx := ActivePageIndex;
    end;

    if Idx < 0 then
      Idx := 0;
    if PageCount = 0 then
      Idx := -1;

    for i := FFocusList.Count - 1 downto 0 do
      if FFocusList[i] = Tab then
        FFocusList.Delete(i);

    Tab.Parent := nil;
    TabClosed(TMTabSheet(Tab));
    Tab.Free;

    if FFocusList.Count > 0 then
      Idx := FFocusList[FFocusList.Count - 1].PageIndex;
    ActivePageIndex := Idx;
  finally
    LockWindowUpdate(0);
  end;
end;

procedure TMPageControl.TabClosed(Tab: TMTabSheet);
begin

end;

procedure TMPageControl.WndProc(var Message: TMessage);
var
  ActiveClosed: Boolean;
  i: Integer;
begin
  //ActiveClosed := False;

  if Message.Msg = WM_PAINT then
    AlignButtons;
  if Message.Msg = WM_USER + 1245 then
  begin
    case Message.WParam of
      0: // Aktives schließen
        begin
          if ActivePage = TMTabSheet(Pages[Message.LParam]) then
            ActiveClosed := True;

          RemoveTab(TMTabSheet(Pages[Message.LParam]));
        end;
      1: // Alle schließen
        begin
          ActiveClosed := True;
          for i := PageCount - 1 downto 0 do
            RemoveTab(TMTabSheet(Pages[i]));
        end;
      2: // Alle außer aktivem schließen
        for i := PageCount - 1 downto 0 do
          if Pages[i] <> ActivePage then
            RemoveTab(TMTabSheet(Pages[i]));
    end;

    //if ActiveClosed and (FFocusList.Count > 0) then
    //  ActivePage := FFocusList[FFocusList.Count - 1];

    AlignButtons;
  end;
  inherited;
end;

{ TMTabSheet }

procedure TMTabSheet.AfterCreate;
begin

end;

procedure TMTabSheet.AlignButton;
var
  PageControl: TPageControl;
begin
  if not FShowCloseButton then
    Exit;
  PageControl := TPageControl(Owner);
  if PageControl.ActivePage = Self then
  begin
    // Diese Abfrage muss, sonst wird ein WM_PAINT ausgelöst, welches dann wieder hier endet (Endlosschleife)
    if PageControl.TabRect(TabIndex).Right - FButtonWidth - 4 <> Button.Left then
    begin
      Button.Left := PageControl.TabRect(TabIndex).Right - FButtonWidth - 4;
      Button.Top := (((PageControl.TabRect(TabIndex).Top + PageControl.TabRect(TabIndex).Bottom) div 2) - FButtonWidth div 2) + PageControl.TabRect(TabIndex).Top - 3;
    end;
  end else
  begin
    // Diese Abfrage muss, sonst wird ein WM_PAINT ausgelöst, welches dann wieder hier endet (Endlosschleife)
    if Button.Left <> PageControl.TabRect(TabIndex).Right - FButtonWidth - 6 then
    begin
      Button.Left := PageControl.TabRect(TabIndex).Right - FButtonWidth - 6;
      Button.Top := (((PageControl.TabRect(TabIndex).Top + PageControl.TabRect(TabIndex).Bottom) div 2) - FButtonWidth div 2) + PageControl.TabRect(TabIndex).Top - 2;
    end;
  end;
end;

function TMTabSheet.CanClose: Boolean;
begin
  Result := True;
end;

constructor TMTabSheet.Create(AOwner: TComponent);
begin
  inherited;
  FShowCloseButton := True;
  FButtonWidth := 12;
  FCaption := '';
  Button := TMTabSheetCloseButton.Create(Self);
  Button.Parent := TWinControl(AOwner);
  Button.ShowHint := True;
  Button.Width := FButtonWidth;
  Button.Height := FButtonWidth;
  Button.Hint := _('Close tab');
  AlignButton;
  Button.Show;
  FMaxWidth := TMPageControl(AOwner).FMaxTabWidth;
end;

destructor TMTabSheet.Destroy;
begin

  inherited;
end;

function TMTabSheet.FGetCaption: string;
var
  s: string;
begin
  s := inherited Caption;
  Result := Trim(s);
end;

procedure TMTabSheet.FSetCaption(Value: string);
begin
  FCaption := Value;
  SetCaptionInternal(Value);
end;

procedure TMTabSheet.FSetMaxWidth(Value: Integer);
begin
  FMaxWidth := Value;
  SetCaptionInternal(FCaption);
end;

procedure TMTabSheet.FSetShowCloseButton(Value: Boolean);
begin
  FShowCloseButton := Value;
  AlignButton;
end;

procedure TMTabSheet.SetCaptionInternal(Value: string);
var
  s, s2: string;
  minsw: Integer;
begin
  if FMaxWidth > 0 then
    s2 := TruncateText(Value, FMaxWidth, PageControl.Canvas.Font)
  else
    s2 := Value;

  if FShowCloseButton then
  begin
    s := ' ';
    minsw := PageControl.Canvas.TextWidth(s);
    while minsw < FButtonWidth + 4 do
    begin
      s := s + ' ';
      minsw := PageControl.Canvas.TextWidth(s);
    end;
  end;

  inherited Caption := s2 + s;
end;


{ TMStatusBar }

procedure TMStatusBar.DrawPanel(Panel: TStatusPanel; const Rect: TRect);
begin
  inherited;
  Canvas.FillRect(Rect);
  Canvas.TextRect(Rect, Rect.Left, Rect.Top, ShortenString(Panel, Rect));
end;

function TMStatusBar.ShortenString(Panel: TStatusPanel; const Rect: TRect): string;
var
  s: string;
  w: Integer;
  sw: Integer;
begin
  s := Panel.Text;
  w := Panel.Width;
  sw := Canvas.TextWidth(s);

  if sw > w then
  begin
    SetLength(s, Length(s) - 1);
    s := Trim(s);
    s := s + '...';
    sw := Canvas.TextWidth(s);
  end;

  while sw > w do
  begin
    s := Copy(s, 1, Length(s) - 4) + '...';
    s := Trim(s);
    if Length(s) = 3 then
    begin
      s := '';
      Break;
    end;
    sw := Canvas.TextWidth(s);
  end;

  Result := s;
end;

{ TMVirtualStringTree }

procedure TMVirtualStringTree.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TMVirtualStringTree.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if ((Key = VK_UP) or (Key = VK_DOWN) or
      (Key = VK_RIGHT) or (Key = VK_LEFT) or
      (Key = VK_SPACE) or (Key = VK_RETURN) or
      (Key = VK_END) or (Key = VK_HOME) or
      (Key = VK_PRIOR) or (Key = VK_NEXT)) and
     (FocusedNode <> nil) then
  begin
    NodeSelected(FocusedNode);
  end;
end;

procedure TMVirtualStringTree.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TMVirtualStringTree.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  inherited;
  Node := GetNodeAt(X, Y);
  if (Node <> nil) and (Node = FocusedNode) then
    NodeSelected(Node);
end;

procedure TMVirtualStringTree.NodeSelected(Node: PVirtualNode);
begin

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
  FBuffer.Canvas.FillRect(Rect(0, 0, 10, 10));

  SetBkMode(FBuffer.Canvas.Handle, TRANSPARENT);
  FBuffer.Canvas.TextOut(20, 0, 'Show filters...');

  FBuffer.Canvas.PenPos := Point(30, 8);
  FBuffer.Canvas.LineTo(100, 8);

  Canvas.Draw(0, 0, FBuffer);
end;

procedure TMShowHidePanel.Resize;
begin
  inherited;

  if FBuffer <> nil then
    FBuffer.Free;

  FBuffer := TBitmap.Create;
  FBuffer.Width := ClientWidth;
  FBuffer.Height := ClientHeight;
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
