unit MPageControl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Themes, StdCtrls, Generics.Defaults, Generics.Collections,
  ExtCtrls, ImgList, GUIFunctions, UxTheme, Math, AdvancedRect;

type
  TMPageControl = class;

  TMTabSheet = class(TCustomControl)
  private
    FPageControl: TMPageControl;

    FCaption: string;
    FImageIndex: Integer;
    FShowCloseButton: Boolean;
    FMaxWidth: Integer;
    FPageIndex: Integer;

    FHeaderRect: TAdvancedRect;
    FImageRect: TAdvancedRect;
    FTextRect: TAdvancedRect;
    FCloseButtonRect: TAdvancedRect;

    FOnClosed: TNotifyEvent;

    procedure FSetCaption(Value: string);
    procedure FSetImageIndex(Value: Integer);
    procedure FSetMaxWidth(Value: Integer);
    procedure FSetShowCloseButton(Value: Boolean);
    procedure FSetPageControl(Value: TMPageControl);
  protected

  public
    constructor Create(AOwner: TComponent); overload; virtual;
    constructor Create(AOwner: TComponent; Caption: string); overload; virtual;

    property Caption: string read FCaption write FSetCaption;
    property ImageIndex: Integer read FImageIndex write FSetImageIndex;
    property ShowCloseButton: Boolean read FShowCloseButton write FSetShowCloseButton;
    property MaxWidth: Integer read FMaxWidth write FSetMaxWidth;
    property Color;
    property PageControl: TMPageControl read FPageControl write FSetPageControl;

    property OnClosed: TNotifyEvent read FOnClosed write FOnClosed;
  end;

  TMouseDownTypes = (dtNothing, dtPage, dtCloseButton);

  TMPageControl = class(TWinControl)
  private
    FPages: TList<TMTabSheet>;
    FImages: TImageList;

    FHeaderRect: TAdvancedRect;
    FActivePage: TMTabSheet;
    FHeaderHeight: Integer;
    FHoverTab: TMTabSheet;
    FHoverButton: TMTabSheet;
    FBorderColor: TColor;
    FHeaderBackgroundColor: TColor;
    FCloseButtonWidth: Integer;
    FCloseButtonHeight: Integer;
    FLeftRightPadding: Integer;
    FBorderWidth: Integer;

    FMouseDownType: TMouseDownTypes;
    FMouseDownOn: TMTabSheet;

    function GetTabAt(X, Y: Integer): TMTabSheet;
    function GetButtonPageAt(X, Y: Integer): TMTabSheet;
    procedure MouseToHeader(var X, Y: Integer);

    procedure PaintControl;
    procedure PagesNotify(Sender: TObject; const Item: TMTabSheet; Action: TCollectionNotification);

    procedure FSetActivePage(Value: TMTabSheet);

    function FGetPageCount: Integer;
  protected
    procedure Resize; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;

    procedure WMNCCalcSize(var Message: TWMNCCalcSize);
      message WM_NCCALCSIZE;
    procedure WMNCMouseMove(var Message: TWMNCMouseMove);
      message WM_NCMOUSEMOVE;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown);
      message WM_NCLBUTTONDOWN;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNcmouseleave(var Message: TMessage);
      message WM_NCMOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCLButtonUp(var Message: TWMNCLButtonUp);
      message WM_NCLBUTTONUP;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    property Pages: TList<TMTabSheet> read FPages;
    property Images: TImageList read FImages write FImages;
    property ActivePage: TMTabSheet read FActivePage write FSetActivePage;
    property HeaderBackgroundColor: TColor read FHeaderBackgroundColor write FHeaderBackgroundColor;
    property PageCount: Integer read FGetPageCount;
  end;

implementation

{ TMPageControl }

constructor TMPageControl.Create(AOwner: TComponent);
var
  D: TThemedElementDetails;
  B: TBitmap;
begin
  inherited;

  FPages := TList<TMTabSheet>.Create;
  FPages.OnNotify := PagesNotify;

  // TODO: Nur wenn themeservices an sind!!
  B := TBitmap.Create;
  try
    B.Width := 10;
    B.Height := 10;
    D := ThemeServices.GetElementDetails(ttPane);
    ThemeServices.DrawElement(B.Canvas.Handle, D, Rect(0, 0, 9, 9));
    FBorderColor := B.Canvas.Pixels[0, 0];
  finally
    B.Free;
  end;

  FHeaderHeight := 23;
  FHeaderBackgroundColor := clBtnFace;
  FCloseButtonHeight := 12;
  FCloseButtonWidth := 12;
  FLeftRightPadding := 4;
  FBorderWidth := 1;
end;

destructor TMPageControl.Destroy;
begin
  FPages.Free;

  inherited;
end;

function TMPageControl.FGetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TMPageControl.FSetActivePage(Value: TMTabSheet);
var
  i: Integer;
  Repaint: Boolean;
begin
  for i := 0 to Pages.Count - 1 do
    Pages[i].Visible := False;

  Repaint := Value <> FActivePage;

  FActivePage := Value;

  if Repaint then
    PaintControl;

  if FActivePage <> nil then
    FActivePage.Visible := True;
  FActivePage.BringToFront;

end;

function TMPageControl.GetButtonPageAt(X, Y: Integer): TMTabSheet;
var
  Page: TMTabSheet;
begin
  Result := nil;
  for Page in FPages do
    if PtInRect(Page.FCloseButtonRect.Rect, Point(X, Y)) then
    begin
      Result := Page;
      Break;
    end;
end;

function TMPageControl.GetTabAt(X, Y: Integer): TMTabSheet;
var
  Page: TMTabSheet;
begin
  Result := nil;
  for Page in FPages do
    if PtInRect(Page.FHeaderRect.Rect, Point(X, Y)) then
    begin
      Result := Page;
      Break;
    end;
end;

procedure TMPageControl.MouseToHeader(var X, Y: Integer);
var
  P: TPoint;
begin
  P.X := X;
  P.Y := Y;
  P := ScreenToClient(P);
  P.X := P.X + FBorderWidth;
  P.Y := P.Y + FHeaderRect.Height;

  X := P.X;
  Y := P.Y;
end;

procedure TMPageControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Page, Page2: TMTabSheet;
begin
  inherited;

  for Page in FPages do
  begin
    if (FMouseDownType = dtCloseButton) and (FMouseDownOn = Page) and Page.ShowCloseButton then
      if PtInRect(Page.FCloseButtonRect.Rect, Point(X, Y)) then
      begin
        FPages.Remove(Page);
        Page.Free;
        Break;
      end;
  end;

  FMouseDownType := dtNothing;
  FMouseDownOn := nil;
end;

procedure TMPageControl.PagesNotify(Sender: TObject; const Item: TMTabSheet;
  Action: TCollectionNotification);
begin
  if csDestroying in ComponentState then
    Exit;

  case Action of
    cnAdded:
      begin
        if FPages.Count = 1 then
          FActivePage := Item;

        Item.Parent := Self;
        Item.Align := alClient;
        Item.FPageControl := Self;
        Item.Visible := Item = FActivePage;
      end;
    cnRemoved:
      begin
        if Item = FActivePage then
        begin
          if FPages.Count = 0 then
            ActivePage := nil
          else if (FPages.Count > 0) and (Item.FPageIndex = 0) then
            ActivePage := FPages[0] // War die Seite ganz links, also wieder ganz links aktivieren
          else if FPages.Count > Item.FPageIndex then
            ActivePage := FPages[Item.FPageIndex] // Es gibt rechts noch was, also das aktivieren
          else if FPages.Count > Item.FPageIndex - 1 then
            ActivePage := FPages[Item.FPageIndex - 1] // Das links von der Seite aktivieren
          else if FPages.Count > 0 then
            ActivePage := FPages[0];
        end;
        if Assigned(Item.FOnClosed) then
          Item.FOnClosed(Item);

        if Parent <> nil then
        begin
          FHoverTab := GetTabAt(ScreenToClient(Mouse.CursorPos).X, ScreenToClient(Mouse.CursorPos).Y);
          FHoverButton := GetButtonPageAt(ScreenToClient(Mouse.CursorPos).X, ScreenToClient(Mouse.CursorPos).Y);
          PaintControl;
        end;
      end;
  end;
end;

procedure TMPageControl.PaintControl;
  procedure PaintCloseButton(Page: TMTabSheet; C: TCanvas);
  var
    uType: Integer;
    uState: Integer;
    Details: TThemedElementDetails;
    Win: TThemedWindow;
    MouseOver: Boolean;
  begin
    MouseOver := Page = FHoverButton; // (X <> -1) and (Y <> -1) and PtInRect(Page.FCloseButtonRect.Rect, Point(X, Y));

    if ThemeServices.ThemesEnabled then
    begin
      if Enabled then
      begin
        if MouseOver and (FMouseDownType = dtCloseButton) and (FMouseDownOn = Page) then
          Win := twSmallCloseButtonPushed
        else if MouseOver then
          Win :=  twSmallCloseButtonHot
        else
          Win :=  twSmallCloseButtonNormal;
      end else
        Win :=  twCloseButtonNormal;
      Details := ThemeServices.GetElementDetails(Win);
      ThemeServices.DrawElement(C.Handle, Details, Page.FCloseButtonRect.Rect);
    end else
    begin
      uType := DFC_CAPTION;
      uState := DFCS_CAPTIONCLOSE;
      if Enabled then
      begin
        if MouseOver and (FMouseDownType = dtCloseButton) and (FMouseDownOn = Page) then
          uState := uState or DFCS_PUSHED
        else if MouseOver then
        begin
          if True then //FHotTrack then
            uState := uState or DFCS_MONO
          else
            uState := uState or DFCS_FLAT;
        end else
          uState := uState or DFCS_FLAT;
      end else
        uState := uState or DFCS_INACTIVE;
      DrawFrameControl(C.Handle, Page.FCloseButtonRect.Rect, uType, uState);
    end;
  end;
  procedure PaintText(Page: TMTabSheet; C: TCanvas; Text: string);
  var
    L: Integer;
  begin
    L := Page.FHeaderRect.Left + 4;
    if Page.ImageIndex > -1 then
      L := L + FImages.Width + 4;

    SetBkMode(C.Handle, TRANSPARENT);

    C.TextOut(Page.FTextRect.Left, Page.FTextRect.Top, Text);
  end;
  procedure PaintImage(Page: TMTabSheet; C: TCanvas);
  begin
    FImages.Draw(C, Page.FImageRect.Left, Page.FImageRect.Top + 1, Page.FImageIndex);
  end;
  procedure DrawPage(Page: TMTabSheet; B: TBitmap);
  var
    D: TThemedElementDetails;
  begin
    if ThemeServices.ThemesEnabled then
    begin
      if Page = FActivePage then
        D := ThemeServices.GetElementDetails(ttTabItemBothEdgeSelected)
      else if Page = FHoverTab then
        D := ThemeServices.GetElementDetails(ttTabItemBothEdgeHot)
      else
        D := ThemeServices.GetElementDetails(ttTabItemBothEdgeNormal);

      ThemeServices.DrawElement(B.Canvas.Handle, D, Page.FHeaderRect.Rect);
    end else
    begin
      if Page = FActivePage then
        B.Canvas.Brush.Color := clWindow
      else if Page = FHoverTab then
        B.Canvas.Brush.Color := clRed
      else
        B.Canvas.Brush.Color := clInactiveCaption;

      B.Canvas.Rectangle(Page.FHeaderRect.Rect);
    end;

    if Page.FImageIndex > -1 then
      PaintImage(Page, B.Canvas);
//    PaintText(Page, B.Canvas, T);
    if Page.FMaxWidth > 0 then
      PaintText(Page, B.Canvas, TruncateText(Page.FCaption, Page.FMaxWidth, B.Canvas.Font))
    else
      PaintText(Page, B.Canvas, Page.FCaption);
    if Page.FShowCloseButton then
      PaintCloseButton(Page, B.Canvas);
  end;
var
  i, DrawX: Integer;
  D: TThemedElementDetails;
  T: string;
  Page: TMTabSheet;
  B: TBitmap;
  C: TCanvas;
  DC: HDC;
begin
  B := TBitmap.Create;
  c := TCanvas.Create;
  try
    DC := GetWindowDC(Handle);
    c.Handle := DC;

    B.Width := Width;
    B.Height := FHeaderHeight;

    // Tableiste Hintergrund malen
    B.Canvas.Pen.Color := FHeaderBackgroundColor;
    B.Canvas.Brush.Color := FHeaderBackgroundColor;
    B.Canvas.FillRect(FHeaderRect.Rect);

    // Linie unter Tabs malen
    B.Canvas.Pen.Width := FBorderWidth;
    B.Canvas.Pen.Color := FBorderColor;
    B.Canvas.MoveTo(0, FHeaderHeight - 1);
    B.Canvas.LineTo(Width, FHeaderHeight - 1);


{    if (FPages.Count > 0) and (Pages[0] <> FActivePage) then
      DrawX := 2
    else
    }
      DrawX := 0;

    for i := 0 to FPages.Count - 1 do
    begin
      Page := FPages[i];

      Page.FPageIndex := i;
      T := Page.FCaption;
      if Page.FMaxWidth > 0 then
      begin
        T := TruncateText(T, Page.FMaxWidth, C.Font);
      end;

      // Erstmal die Abmessungen anhand des Textes bestimmen
      if Page = FActivePage then
        Page.FHeaderRect.Top := 0
      else
        Page.FHeaderRect.Top := 3;
      Page.FHeaderRect.Left := DrawX;
      Page.FHeaderRect.Width := C.TextWidth(T) + FLeftRightPadding * 2;
      Page.FHeaderRect.Height := Max(FHeaderRect.Height, C.TextHeight(T) + FLeftRightPadding * 2) - Page.FHeaderRect.Top - 1;
      if Page = FActivePage then
      begin
        Page.FHeaderRect.Height := Page.FHeaderRect.Height + 2;
      end;

      Page.FTextRect.Left := Page.FHeaderRect.Left + FLeftRightPadding;
      Page.FTextRect.Width := C.TextWidth(T);
      Page.FTextRect.Height := C.TextHeight(T);
      Page.FTextRect.Top := Page.FHeaderRect.Top + Page.FHeaderRect.Height div 2 - Page.FTextRect.Height div 2;

      if Page = FActivePage then
      begin
        Page.FTextRect.Top := Page.FTextRect.Top - 2;
        Page.FTextRect.Bottom := Page.FTextRect.Bottom - 2;
      end;

      // Bild berechnen
      if Page.ImageIndex > -1 then
      begin
        Page.FHeaderRect.Width := Page.FHeaderRect.Width + FImages.Width + FLeftRightPadding;

        Page.FImageRect.Top := Page.FHeaderRect.Top + Page.FHeaderRect.Height div 2 - FImages.Height div 2;
        Page.FImageRect.Left := Page.FHeaderRect.Left + FLeftRightPadding;
        Page.FImageRect.Height := FImages.Height;
        Page.FImageRect.Width := FImages.Width;

        Page.FTextRect.Left := Page.FTextRect.Left + Page.FImageRect.Width + FLeftRightPadding;

        if Page = FActivePage then
        begin
          Page.FImageRect.Top := Page.FImageRect.Top - 2;
          Page.FImageRect.Bottom := Page.FImageRect.Bottom - 2;
        end;
      end;

      // CloseButton berechnen
      if Page.FShowCloseButton then
      begin
        Page.FHeaderRect.Width := Page.FHeaderRect.Width + FCloseButtonWidth + FLeftRightPadding * 2;

        Page.FCloseButtonRect.Top := Page.FHeaderRect.Top + Page.FHeaderRect.Height div 2 - FCloseButtonHeight div 2;
        Page.FCloseButtonRect.Left := Page.FHeaderRect.Right - FCloseButtonWidth - FLeftRightPadding;
        Page.FCloseButtonRect.Width := FCloseButtonWidth;
        Page.FCloseButtonRect.Height := FCloseButtonHeight;

        if Page = FActivePage then
        begin
          Page.FCloseButtonRect.Top := Page.FCloseButtonRect.Top - 2;
          Page.FCloseButtonRect.Bottom := Page.FCloseButtonRect.Bottom - 2;
        end;
      end;

      DrawX := DrawX + Page.FHeaderRect.Right - Page.FHeaderRect.Left;
    end;



    for Page in Pages do
    begin
      if Page = FActivePage then
      begin
        Continue;
      end;

      if Page = Pages[0] then
      begin
        Page.FHeaderRect.Left := Page.FHeaderRect.Left + 2;
        Page.FHeaderRect.Right := Page.FHeaderRect.Right + 2;
      end;

      DrawPage(Page, B);
    end;

    if FActivePage <> nil then
    begin
      if FActivePage <> Pages[0] then
      begin
        FActivePage.FHeaderRect.Left := FActivePage.FHeaderRect.Left - 1;
        FActivePage.FHeaderRect.Right := FActivePage.FHeaderRect.Right + 2;
      end else
        FActivePage.FHeaderRect.Right := FActivePage.FHeaderRect.Right + 2;

      DrawPage(FActivePage, B);
    end;

    C.Draw(0, 0, B);


    C.Pen.Color := FBorderColor;
    C.Pen.Style := psSolid;
    // Rand rechts
    C.MoveTo(Width - FBorderWidth, FHeaderRect.Height - 1);
    C.LineTo(Width - FBorderWidth, ClientHeight + FHeaderRect.Height);
    // Rand unten
    C.LineTo(0, ClientHeight + FHeaderRect.Height);
    // Rand links
    C.LineTo(0, FHeaderRect.Height - 1);

  finally
    ReleaseDC(Handle,DC);
    B.Free;
    c.free;
  end;
end;

procedure TMPageControl.Resize;
begin
  inherited;

  FHeaderRect.Top := 0;
  FHeaderRect.Left := 0;
  FHeaderRect.Width := Width;
  FHeaderRect.Bottom := FHeaderHeight;
end;

procedure TMPageControl.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  Inc(Message.CalcSize_Params^.rgrc[0].Top, FHeaderHeight);
  Inc(Message.CalcSize_Params^.rgrc[0].Left, FBorderWidth);
  Dec(Message.CalcSize_Params^.rgrc[0].Right, FBorderWidth);
  Dec(Message.CalcSize_Params^.rgrc[0].Bottom, FBorderWidth);
end;

procedure TMPageControl.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTBORDER;
end;

procedure TMPageControl.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  Paint: Boolean;
  Page, Page2: TMTabSheet;
  X, Y: Integer;
begin
  inherited;

  X := Message.XCursor;
  Y := Message.YCursor;

  MouseToHeader(X, Y);

  begin
    Paint := False;
    FMouseDownType := dtNothing;
    FMouseDownOn := nil;

    for Page in FPages do
    begin
      if Page.ShowCloseButton and (PtInRect(Page.FCloseButtonRect.Rect, Point(X, Y))) then
      begin
        Paint := True;
        FMouseDownType := dtCloseButton;
        FMouseDownOn := Page;
      end else if PtInRect(Page.FHeaderRect.Rect, Point(X, Y)) then
      begin
        FMouseDownType := dtPage;
        FMouseDownOn := Page;
      end;

      if not ((Page.ShowCloseButton) and (PtInRect(Page.FCloseButtonRect.Rect, Point(X, Y)))) and
             PtInRect(Page.FHeaderRect.Rect, Point(X, Y)) and (Page <> FActivePage) then
      begin
        for Page2 in FPages do
          Page2.Visible := False;
        ActivePage := Page;
        Paint := True;
        Break;
      end;
    end;

    if Paint then
      PaintControl;
  end;
end;

procedure TMPageControl.WMNCLButtonUp(var Message: TWMNCLButtonUp);
var
  X, Y: Integer;
  Page, Page2: TMTabSheet;
begin
  inherited;

  X := Message.XCursor;
  Y := Message.YCursor;

  MouseToHeader(X, Y);

  for Page in FPages do
  begin
    if (FMouseDownType = dtCloseButton) and (FMouseDownOn = Page) and Page.ShowCloseButton then
      if PtInRect(Page.FCloseButtonRect.Rect, Point(X, Y)) then
      begin
        FPages.Remove(Page);
        Page.Free;
        Break;
      end;
  end;

  FMouseDownType := dtNothing;
  FMouseDownOn := nil;
end;

procedure TMPageControl.WMNcmouseleave(var Message: TMessage);
begin
  FMouseDownType := dtNothing;
  FMouseDownOn := nil;

  FHoverTab := nil;
  PaintControl;
end;

procedure TMPageControl.WMNCMouseMove(var Message: TWMNCMouseMove);
var
  X, Y: Integer;
  Paint: Boolean;
  T, B: TMTabSheet;
  tme: TTRACKMOUSEEVENT;
begin
  X := Message.XCursor;
  Y := Message.YCursor;

  MouseToHeader(X, Y);

  T := GetTabAt(X, Y);
  B := GetButtonPageAt(X, Y);

  if T <> FHoverTab then
  begin
    Paint := True;
  end else if B <> FHoverButton then
  begin
    Paint := True;
  end;

  if Paint then
  begin
    FHoverTab := T;
    FHoverButton := B;
    PaintControl;
  end;

  TME.cbSize := SizeOf(TME);
  TME.dwFlags := TME_LEAVE or TME_NONCLIENT;
  TME.hwndTrack := Handle;
  TrackMouseEvent(TME);
end;

procedure TMPageControl.WMPaint(var Message: TWMPaint);
begin
  inherited;

  PaintControl;
end;

{ TMPage }

constructor TMTabSheet.Create(AOwner: TComponent; Caption: string);
var
  B: TBitmap;
  D: TThemedElementDetails;
begin
  inherited Create(AOwner);

  ControlStyle := [csDoubleClicks, csOpaque, csPannable, csGestures];

  FCaption := Caption;
  FImageIndex := -1;
  FShowCloseButton := False;
  BorderWidth := 0;

  // Hintergrundfarbe holen
  B := TBitmap.Create; // TODO: oder per api das ttPane malen.
  try
    B.Width := 20;
    B.Height := 10;

    D := ThemeServices.GetElementDetails(ttTabItemSelected);
    ThemeServices.DrawElement(B.Canvas.Handle, D, Rect(0, 0, B.Width, B.Height));

    Color := B.Canvas.Pixels[5, 9];
  finally
    B.Free;
  end;
end;

procedure TMTabSheet.FSetCaption(Value: string);
begin
  FCaption := Value;
end;

procedure TMTabSheet.FSetImageIndex(Value: Integer);
begin
  FImageIndex := Value;
end;

procedure TMTabSheet.FSetMaxWidth(Value: Integer);
begin
  FMaxWidth := Value;
end;

procedure TMTabSheet.FSetPageControl(Value: TMPageControl);
begin
  Value.Pages.Add(Self);
end;

procedure TMTabSheet.FSetShowCloseButton(Value: Boolean);
begin
  FShowCloseButton := Value;
  if FPageControl <> nil then
    FPageControl.PaintControl;
end;

constructor TMTabSheet.Create(AOwner: TComponent);
begin
  Create(AOwner, '');
end;

end.
