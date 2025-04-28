unit MSeekBar;

interface

uses
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  ExtCtrls,
  Forms,
  Graphics,
  GraphUtil,
  Math,
  Menus,
  MVirtualTree,
  SysUtils,
  Themes,
  UxTheme,
  VirtualTrees,
  Windows;

type
  TGripperStates = (gsUnknown, gsNormal, gsHot, gsDown);

  { TMSeekBar }

  TMSeekBar = class(TCustomControl)
  private
    FMax: Int64;
    FPosition: Int64;
    FOrientation: TScrollBarKind;

    FPositionBeforeDrag: Int64;

    FLastGripperPos: Integer;
    FDragFrom: Integer;
    FGripperVisible: Boolean;
    FGripperDown: Boolean;
    FNotifyOnMove: Boolean;
    FNotifyOnDown: Boolean;

    FLastGripperState: TGripperStates;

    FSetting: Boolean;
    FOnPositionChanged: TNotifyEvent;

    procedure PaintBackground(const Bmp: Graphics.TBitmap; const Rect: TRect);
    procedure PaintGripper(const Bmp: Graphics.TBitmap; const Rect: TRect);

    function GetBackgroundRect: TRect;
    function GetGripperRect: TRect;
    function GetGripperState: TGripperStates;

    procedure FSetPosition(Value: Int64);
    procedure FSetGripperVisible(Value: Boolean);

    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EraseBackground(DC: HDC); override;
    property Max: Int64 read FMax write FMax;
    property Position: Int64 read FPosition write FSetPosition;
    property PositionBeforeDrag: Int64 read FPositionBeforeDrag write FPositionBeforeDrag;
    property Orientation: TScrollBarKind read FOrientation write FOrientation;
    property GripperVisible: Boolean read FGripperDown write FSetGripperVisible;
    property NotifyOnMove: Boolean read FNotifyOnMove write FNotifyOnMove;
    property NotifyOnDown: Boolean read FNotifyOnDown write FNotifyOnDown;
    property OnPositionChanged: TNotifyEvent read FOnPositionChanged write FOnPositionChanged;
  end;

procedure Register;

implementation

procedure Register;
const
  Classes: array of TComponentClass = [TMSeekBar];
begin
  RegisterComponents('MControls', Classes);
end;

{ TMSeekBar }

procedure TMSeekBar.Paint;
var
  Bmp: Graphics.TBitmap;
begin
  inherited;

  Bmp := Graphics.TBitmap.Create;
  try
    Bmp.Width := ClientWidth;
    Bmp.Height := ClientHeight;

    if Assigned(@DrawThemeParentBackground) then
      DrawThemeParentBackground(Handle, Bmp.Canvas.Handle, nil)
    else
    begin
      Bmp.Transparent := True;
      Bmp.TransparentColor := clFuchsia;
      Bmp.Canvas.Brush.Style := bsSolid;
      Bmp.Canvas.Brush.Color := Bmp.TransparentColor;
      Bmp.Canvas.FillRect(TRect.Create(0, 0, Bmp.Width, Bmp.Height));
    end;

    PaintBackground(Bmp, GetBackgroundRect);

    if FGripperVisible then
      PaintGripper(Bmp, GetGripperRect);

    Canvas.Draw(0, 0, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TMSeekBar.PaintBackground(const Bmp: Graphics.TBitmap; const Rect: TRect);
var
  R: TRect;
begin
  R := Rect;

  if FOrientation = sbHorizontal then
  begin
    R.Top := R.Top + R.Height div 2 - 3;
    R.Bottom := R.Top + 6;
  end else
  begin
    R.Left := R.Left + R.Width div 2 - 3;
    R.Right := R.Left + 6;
  end;

  Bmp.Canvas.Brush.Color := clWindow;
  Bmp.Canvas.Pen.Width := 1;
  Bmp.Canvas.Pen.Color := ColorAdjustLuma(clWindowFrame, 100, False);
  Bmp.Canvas.Rectangle(R);
end;

procedure TMSeekBar.PaintGripper(const Bmp: Graphics.TBitmap; const Rect: TRect);

  procedure DrawButton(Canvas: TCanvas; R: TRect; State: TButtonState);
  var
    uType, uState: Integer;
    Details: TThemedElementDetails;
    Win: TThemedButton;
  begin
    if ThemeServices.ThemesEnabled then
    begin
      case State of
        bsDown:
          Win := tbPushButtonPressed;
        bsHot:
          Win := tbPushButtonHot;
        else
          Win := tbPushButtonNormal;
      end;

      if FOrientation = sbHorizontal then
      begin
        R.Left := R.Left - 1;
        R.Right := R.Right + 1;
      end else
      begin
        R.Top := R.Top - 1;
        R.Bottom := R.Bottom + 1;
      end;

      Details := ThemeServices.GetElementDetails(Win);
      ThemeServices.DrawElement(Canvas.Handle, Details, R);
    end else
    begin
      uType := DFC_BUTTON;
      uState := DFCS_BUTTONPUSH;

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

var
  i: Integer;
  Pt: TPoint;
begin
  if FMax <= 0 then
    Exit;

  case GetGripperState of
    gsHot:
      DrawButton(Bmp.Canvas, Rect, bsHot);
    gsDown:
      DrawButton(Bmp.Canvas, Rect, bsDown);
    else
      DrawButton(Bmp.Canvas, Rect, bsUp);
  end;

  Bmp.Canvas.Pen.Color := IfThen<TColor>(GetGripperState <> gsNormal, clBtnShadow, clBtnText);

  Pt := TPoint.Create(Rect.Left + Rect.Width div 2 - 3, Rect.Top + Rect.Height div 2 - 2);

  for i := 0 to 2 do
    if FOrientation = sbHorizontal then
    begin
      Bmp.Canvas.Line(Pt, TPoint.Create(Pt.X, Pt.Y + 6));
      Pt.Offset(2, 0);
    end else
    begin
      Bmp.Canvas.Line(Pt, TPoint.Create(Pt.X + 6, Pt.Y));
      Pt.Offset(0, 2);
    end;

  FLastGripperState := GetGripperState;
  FLastGripperPos := FPosition;
end;

function TMSeekBar.GetBackgroundRect: TRect;
begin
  Result := TRect.Create(TPoint.Create(BorderSpacing.Left, BorderSpacing.Top), ClientWidth - BorderSpacing.Left - BorderSpacing.Right, ClientHeight - BorderSpacing.Top - BorderSpacing.Bottom);
end;

function TMSeekBar.GetGripperRect: TRect;
var
  P: Integer;
begin
  Result := GetBackgroundRect;

  if FOrientation = sbHorizontal then
  begin
    P := Trunc((FPosition / FMax) * (Result.Width - Result.Height));

    Result.Left := Result.Left + P;
    Result.Right := Result.Left + Result.Height;
  end else
  begin
    P := Trunc((FPosition / FMax) * (Result.Height - Result.Width));

    Result.Top := Result.Top + P;
    Result.Bottom := Result.Top + Result.Width;
  end;
end;

procedure TMSeekBar.WMMouseWheel(var Msg: TWMMouseWheel);
begin
  if FOrientation = sbHorizontal then
    FPosition := FPosition + Trunc(Msg.WheelDelta / 30)
  else
    FPosition := FPosition - Trunc(Msg.WheelDelta / 30);

  if FPosition < 0 then
    FPosition := 0;
  if FPosition > FMax then
    FPosition := FMax;

  if FNotifyOnMove then
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self);

  if (FLastGripperState <> GetGripperState) or (FLastGripperPos <> FPosition) then
    Repaint;
end;

procedure TMSeekBar.EraseBackground(DC: HDC);
begin

end;

function TMSeekBar.GetGripperState: TGripperStates;
begin
  Result := gsUnknown;

  if not FGripperVisible then
    Exit;

  if not FGripperDown and PtInRect(GetGripperRect, ScreenToClient(Mouse.CursorPos)) then
    Result := gsHot
  else if FGripperDown then
    Result := gsDown
  else
    Result := gsNormal;
end;

constructor TMSeekBar.Create(AOwner: TComponent);
begin
  inherited;

  FMax := 0;
  FPositionBeforeDrag := -1;
  FOrientation := sbHorizontal;
end;

procedure TMSeekBar.FSetGripperVisible(Value: Boolean);
begin
  if Value <> FGripperVisible then
  begin
    FGripperVisible := Value;
    Repaint;
  end;
end;

procedure TMSeekBar.FSetPosition(Value: Int64);
begin
  if FSetting then
    Exit;

  FPosition := Value;

  if FNotifyOnMove then
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self);

  if (FLastGripperState <> GetGripperState) or (FLastGripperPos <> FPosition) then
    Repaint;
end;

procedure TMSeekBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
var
  BackgroundRect, GripperRect: TRect;
begin
  inherited;

  if (not FGripperVisible) or (Button <> mbLeft) then
    Exit;

  BackgroundRect := GetBackgroundRect;
  GripperRect := GetGripperRect;

  FGripperDown := True;

  if PtInRect(GripperRect, ScreenToClient(Mouse.CursorPos)) then
  begin
    if FOrientation = sbHorizontal then
      FDragFrom := X - GripperRect.Left
    else
      FDragFrom := Y - GripperRect.Top;

    Repaint;

    FSetting := True;
  end else if PtInRect(BackgroundRect, ScreenToClient(Mouse.CursorPos)) then
  begin
    FDragFrom := (IfThen<Integer>(FOrientation = sbHorizontal, GripperRect.Width, GripperRect.Height)) div 2;

    if FOrientation = sbHorizontal then
      FPosition := Trunc((X - BorderSpacing.Left - FDragFrom) / (BackgroundRect.Width - GripperRect.Width) * FMax)
    else
      FPosition := Trunc((Y - BorderSpacing.Top - FDragFrom) / (BackgroundRect.Height - GripperRect.Height) * FMax);

    if FPositionBeforeDrag = -1 then
      FPositionBeforeDrag := FPosition;

    if FPosition < 0 then
      FPosition := 0;
    if FPosition > FMax then
      FPosition := FMax;

    if FNotifyOnDown then
      if Assigned(FOnPositionChanged) then
        FOnPositionChanged(Self);

    Repaint;

    FSetting := True;
  end;
end;

procedure TMSeekBar.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;

  if FSetting then
  begin
    if FPositionBeforeDrag = -1 then
      FPositionBeforeDrag := FPosition;

    if FOrientation = sbHorizontal then
      FPosition := Trunc(((X - BorderSpacing.Left - FDragFrom) / (GetBackgroundRect.Width - GetGripperRect.Width)) * FMax)
    else
      FPosition := Trunc(((Y - BorderSpacing.Top - FDragFrom) / (GetBackgroundRect.Height - GetGripperRect.Height)) * FMax);

    if FPosition < 0 then
      FPosition := 0;
    if FPosition > FMax then
      FPosition := FMax;

    if FNotifyOnMove then
      if Assigned(FOnPositionChanged) then
        FOnPositionChanged(Self);
  end;

  if (FLastGripperState <> GetGripperState) or (FLastGripperPos <> FPosition) then
    Repaint;
end;

procedure TMSeekBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;

  SetFocus;

  if Button = mbLeft then
  begin
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self);

    FPositionBeforeDrag := -1;

    FSetting := False;
    FGripperDown := False;

    Repaint;
  end;
end;

procedure TMSeekBar.MouseLeave;
begin
  inherited MouseLeave;

  Repaint;
end;

end.
