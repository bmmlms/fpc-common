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
  Windows, SysUtils, Classes, Messages, ComCtrls, ActiveX, Controls, Buttons,
  StdCtrls, Menus, VirtualTrees, DragDrop, DragDropFile, ShellApi, Types,
  Themes, ImgList, GUIFunctions, LanguageObjects, Graphics, Forms,
  Generics.Collections, Math;

type

  { TMTabSheet }

  TMTabSheet = class(TTabSheet)
  private
    FCaption: string;
    FShowCloseButton: Boolean;
    FButtonRect: TRect;

    FFocusedControlBeforeChange: TWinControl;

    procedure UpdateProperties;
    procedure FSetCaption(Value: string);
    procedure FSetShowCloseButton(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;

    function CanClose: Boolean; virtual;

    function ProcessShortCut(Msg: TWMKey): Boolean; virtual;   // TODO: testen ob das noch funzt.

    property Caption: string read FCaption write FSetCaption;
    property ShowCloseButton: Boolean read FShowCloseButton write FSetShowCloseButton;
  end;

  { TMPageControl }

  TMPageControl = class(TPageControl)
  private const
    WM_CLOSETAB = WM_USER + 1245;
    CT_ACTIVE = 0;
    CT_ALL = 1;
    CT_ALL_BUT_ACTIVE = 2;
  private
    FPainted: Boolean;
    FMaxTabWidth: Integer;
    FFocusList: TList<TTabSheet>;
    FPressedButton: Integer;

    procedure DrawButton(Canvas: TCanvas; R: TRect; State: TButtonState);
    procedure RemoveTab(Tab: TMTabSheet); virtual;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CloseTab(Idx: Integer);
    procedure CloseAll;
    procedure CloseAllButActive;

    property MaxTabWidth: Integer read FMaxTabWidth write FSetMaxTabWidth;
    property Pages[Index: Integer]: TMTabSheet read FGetTabSheet;
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
  Color := clWhite;
end;

procedure TMTabSheet.FSetCaption(Value: string);
begin
  FCaption := Value.Trim;
  UpdateProperties;
end;

procedure TMTabSheet.FSetShowCloseButton(Value: Boolean);
begin
  FShowCloseButton := Value;
  UpdateProperties;
end;

function TMTabSheet.ProcessShortCut(Msg: TWMKey): Boolean;
begin
  self.Update;

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

  ButtonWidth := GetTextSize('Wyg', PageControl.Font).Height - 2;

  FButtonRect := TRect.Create(TPoint.Create(TabRect.Right - ButtonWidth - 5, TabRect.Top + (TabRect.Height div 2) - Math.Floor(ButtonWidth / 2)), ButtonWidth, ButtonWidth);

  if PageControl.ActivePageIndex = PageIndex then
    FButtonRect.Offset(0, -2);

  if (TMPageControl(PageControl).FMaxTabWidth = 0) and FShowCloseButton then
    inherited Caption := FCaption + StringForWidth(' ', FButtonRect.Width, PageControl.Font)
  else if (TMPageControl(PageControl).FMaxTabWidth > 0) and FShowCloseButton then
    inherited Caption := TruncateText(FCaption, TMPageControl(PageControl).FMaxTabWidth - (TabRect.Width - FButtonRect.Left), PageControl.Font) + StringForWidth(' ', FButtonRect.Width, PageControl.Font)
  else if (TMPageControl(PageControl).FMaxTabWidth > 0) and (not FShowCloseButton) then
    inherited Caption := TruncateText(FCaption, TMPageControl(PageControl).FMaxTabWidth, PageControl.Font)
  else
    inherited Caption := FCaption;
end;

{ TMPageControl }

procedure TMPageControl.DrawButton(Canvas: TCanvas; R: TRect;
  State: TButtonState);
var
  uType: Integer;
  uState: Integer;
  Details: TThemedElementDetails;
  Win: TThemedWindow;
begin
  if ThemeServices.ThemesEnabled then
  begin
    case State of
      bsDown:
        Win := twSmallCloseButtonPushed;
      bsHot:
        Win := twSmallCloseButtonHot;
      else
        Win := twSmallCloseButtonNormal;
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
var
  i: Integer;
begin
  if GetAsyncKeyState(VK_LBUTTON) < 0 then
    for i := 0 to PageCount - 1 do
      if TMTabSheet(Pages[i]).FButtonRect.Contains(ScreenToClient(Mouse.CursorPos)) then
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
      begin
        DrawButton(Canvas, R, bsHot);
      end
      else
        DrawButton(Canvas, R, bsUp);
    end;
  finally
    Canvas.Free;
  end;

  FPainted := True;
end;

procedure TMPageControl.DoChange;
begin
  inherited;

  if TMTabSheet(ActivePage).FFocusedControlBeforeChange <> nil then
    TMTabSheet(ActivePage).FFocusedControlBeforeChange.ApplyFocus;

  FFocusList.Add(ActivePage);

  Invalidate;
end;

procedure TMPageControl.CloseTab(Idx: Integer);
begin
  PostMessage(Handle, WM_CLOSETAB, CT_ACTIVE, Idx)
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
  PostMessage(Handle, WM_CLOSETAB, CT_ALL, 0)
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

procedure TMPageControl.RemoveTab(Tab: TMTabSheet);    // TODO: ich sollte RemovePage(idx) überschreiben. und das hier sollte dann removepage aufrufen.
var
  Idx: Integer;
  i: Integer;
begin
  if not Tab.CanClose then
    Exit;

  LockWindowUpdate(Handle);
  try
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

    Tab.Free;

    if FFocusList.Count > 0 then
      Idx := FFocusList[FFocusList.Count - 1].PageIndex;
    ActivePageIndex := Idx;
  finally
    LockWindowUpdate(0);
  end;
end;

procedure TMPageControl.WndProc(var Message: TMessage);
var
  i: Integer;
begin
  if Message.Msg = WM_CLOSETAB then
  begin
    case Message.WParam of
      CT_ACTIVE:
        begin
          RemoveTab(Pages[Message.LParam]);
        end;
      CT_ALL:
        begin
          for i := PageCount - 1 downto 0 do
            RemoveTab(Pages[i]);
        end;
      CT_ALL_BUT_ACTIVE:
        for i := PageCount - 1 downto 0 do
          if Pages[i] <> ActivePage then
            RemoveTab(Pages[i]);
    end;
    Exit;
  end;

  inherited;
end;

procedure TMPageControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  Invalidate;
end;

procedure TMPageControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button <> mbLeft then
    Exit;

  FPressedButton := -1;
  for i := 0 to PageCount - 1 do
    if Pages[i].FShowCloseButton and Pages[i].FButtonRect.Contains(ScreenToClient(Mouse.CursorPos)) then
    begin
      FPressedButton := i;
      MouseCapture := True;
      Invalidate;
      Break;
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
