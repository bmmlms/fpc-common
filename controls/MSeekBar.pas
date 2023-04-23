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

    FGripperPos, FLastGripperPos: Integer;
    FDragFrom: Integer;
    FGripperVisible: Boolean;
    FGripperDown: Boolean;
    FNotifyOnMove: Boolean;
    FNotifyOnDown: Boolean;

    FLastGripperState: TGripperStates;

    FSetting: Boolean;
    FOnPositionChanged: TNotifyEvent;

    procedure PaintBackground(const Bmp: Graphics.TBitmap; const ClipRect: TRect);
    procedure PaintGripper(const Bmp: Graphics.TBitmap; const ClipRect: TRect);

    function GetGripperState: TGripperStates;

    procedure FSetPosition(Value: Int64);
    procedure FSetGripperVisible(Value: Boolean);

    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
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
  R, ClipRect: TRect;
begin
  inherited;

  Bmp := Graphics.TBitmap.Create;
  try
    Bmp.Width := ClientWidth;
    Bmp.Height := ClientHeight;

    R := TRect.Create(0, 0, Bmp.Width, Bmp.Height);

    if ThemeServices.ThemesEnabled then
      ThemeServices.DrawParentBackground(Handle, BMP.Canvas.Handle, nil, False)
    else
    begin
      Bmp.Canvas.Brush.Style := bsSolid;
      Bmp.Canvas.Brush.Color := clBtnFace;
      Bmp.Canvas.FillRect(R);
    end;

    ClipRect := R;
    ClipRect.Top := BorderSpacing.Top;
    ClipRect.Bottom := ClipRect.Bottom - BorderSpacing.Bottom;
    ClipRect.Left := BorderSpacing.Left;
    ClipRect.Right := ClipRect.Right - BorderSpacing.Right;

    PaintBackground(Bmp, ClipRect);

    if FGripperVisible then
      PaintGripper(Bmp, ClipRect);

    Canvas.Draw(0, 0, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TMSeekBar.PaintBackground(const Bmp: Graphics.TBitmap; const ClipRect: TRect);
var
  R: TRect;
begin
  Bmp.Canvas.Brush.Color := clBlack;
  Bmp.Canvas.Pen.Color := clBlack;

  case FOrientation of
    sbHorizontal:
    begin
      // Rand links und oben
      Bmp.Canvas.MoveTo(0, ClipRect.Height div 2 + 3); // Unten links
      Bmp.Canvas.LineTo(0, ClipRect.Height div 2 - 3); // Nach oben malen
      Bmp.Canvas.LineTo(ClipRect.Width - Bmp.Canvas.Pen.Width, ClipRect.Height div 2 - 3); // Nach rechts malen
      // Rand rechts und unten
      Bmp.Canvas.Pen.Color := clGray;
      Bmp.Canvas.LineTo(ClipRect.Width - Bmp.Canvas.Pen.Width, ClipRect.Height div 2 + 3);
      Bmp.Canvas.LineTo(0, ClipRect.Height div 2 + 3);

      R.Left := Canvas.Pen.Width;
      R.Top := ClipRect.Height div 2 - 3 + Bmp.Canvas.Pen.Width;
      R.Bottom := ClipRect.Height div 2 + 3;
      R.Right := ClipRect.Width - Bmp.Canvas.Pen.Width;
    end;
    sbVertical:
    begin
      // Rand links und oben
      Bmp.Canvas.MoveTo(ClipRect.Width div 2 - 3, ClipRect.Height - Bmp.Canvas.Pen.Width);
      Bmp.Canvas.LineTo(ClipRect.Width div 2 - 3, 0);
      Bmp.Canvas.LineTo(ClipRect.Width div 2 + 3, 0);
      // Rand rechts und unten
      Bmp.Canvas.Pen.Color := clGray;
      Bmp.Canvas.LineTo(ClipRect.Width div 2 + 3, ClipRect.Height - Bmp.Canvas.Pen.Width);
      Bmp.Canvas.LineTo(ClipRect.Width div 2 - 3, ClipRect.Height - Bmp.Canvas.Pen.Width);

      R.Left := ClipRect.Width div 2 - 3 + Canvas.Pen.Width;
      R.Top := Bmp.Canvas.Pen.Width;
      R.Bottom := ClipRect.Height - Bmp.Canvas.Pen.Width;
      R.Right := ClipRect.Width div 2 + 3 - Bmp.Canvas.Pen.Width;
    end;
  end;

  Bmp.Canvas.Brush.Color := clBtnFace;
  Bmp.Canvas.FillRect(R);
end;

procedure TMSeekBar.PaintGripper(const Bmp: Graphics.TBitmap; const ClipRect: TRect);
var
  i, P: Integer;
  R: TRect;
  D: TThemedElementDetails;
  Pt: TPoint;
begin
  if FMax <= 0 then
    Exit;

  if FOrientation = sbHorizontal then
  begin
    P := Trunc((FPosition / FMax) * (ClipRect.Width - ClipRect.Height));

    R.Top := ClipRect.Top;
    R.Left := P + ClipRect.Left;
    R.Bottom := ClipRect.Height;
    R.Right := P + ClipRect.Height;

    Pt := TPoint.Create(R.Left + ClipRect.Height div 2 - 2, ClipRect.Height div 2 - 3);
  end else
  begin
    P := Trunc((FPosition / FMax) * (ClipRect.Height - ClipRect.Width));

    R.Top := P + ClipRect.Top;
    R.Left := ClipRect.Left;
    R.Bottom := P + ClipRect.Width;
    R.Right := ClipRect.Width;

    Pt := TPoint.Create(ClipRect.Width div 2 - 3, R.Top + ClipRect.Width div 2 - 2);
  end;

  case GetGripperState of
    gsHot:
      Bmp.Canvas.Brush.Color := ColorAdjustLuma(clScrollBar, -30, False);
    gsDown:
      Bmp.Canvas.Brush.Color := ColorAdjustLuma(clScrollBar, -60, False);
    else
      Bmp.Canvas.Brush.Color := clScrollBar;
  end;

  Bmp.Canvas.FillRect(R);

  Bmp.Canvas.Pen.Color := IfThen<TColor>(GetGripperState <> gsNormal, GetHighLightColor(clBtnShadow, 50), clBtnShadow);

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

procedure TMSeekBar.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
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

function TMSeekBar.GetGripperState: TGripperStates;
var
  P: LongInt;
  R: TRect;
begin
  Result := gsUnknown;

  if not FGripperVisible then
    Exit;

  if FOrientation = sbHorizontal then
  begin
    P := Trunc((FPosition / FMax) * (ClientWidth - 20));

    R.Top := 2;
    R.Left := P;
    R.Bottom := ClientHeight;
    R.Right := 20 + R.Left;
  end else
  begin
    P := Trunc((FPosition / FMax) * (ClientHeight - 20));

    R.Top := P;
    R.Left := 2;
    R.Bottom := P + 20;
    R.Right := ClientWidth;
  end;

  if not FGripperDown and PtInRect(R, ScreenToClient(Mouse.CursorPos)) then
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

  Constraints.MinHeight := 21;
  Constraints.MinWidth := 21;
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
  if FMax = 0 then
    FGripperPos := 0
  else if FOrientation = sbHorizontal then
    FGripperPos := Trunc((FPosition / FMax) * (ClientWidth - 20))
  else
    FGripperPos := Trunc((FPosition / FMax) * (ClientHeight - 20));

  if FNotifyOnMove then
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self);

  if (FLastGripperState <> GetGripperState) or (FLastGripperPos <> FPosition) then
    Repaint;
end;

procedure TMSeekBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
var
  V: Integer;
begin
  inherited;

  if not FGripperVisible then
    Exit;

  if Button = mbLeft then
  begin
    FGripperDown := True;

    if FOrientation = sbHorizontal then
    begin
      V := X;
      FGripperPos := Trunc((FPosition / FMax) * (ClientWidth - 20));
    end else
    begin
      V := Y;
      FGripperPos := Trunc((FPosition / FMax) * (ClientHeight - 20));
    end;

    if (V > FGripperPos) and (V < FGripperPos + 20) then
      FDragFrom := Min(Abs(V - FGripperPos), Abs(FGripperPos - V))
    else
    begin
      FDragFrom := 10;

      if FPositionBeforeDrag = -1 then
        FPositionBeforeDrag := FPosition;

      if FOrientation = sbHorizontal then
        FPosition := Trunc(((V - FDragFrom) / (ClientWidth - 20)) * Max)
      else
        FPosition := Trunc(((V - FDragFrom) / (ClientHeight - 20)) * Max);
      FGripperPos := V - FDragFrom;

      if FPosition < 0 then
        FPosition := 0;
      if FPosition > FMax then
        FPosition := FMax;

      if FNotifyOnDown then
        if Assigned(FOnPositionChanged) then
          FOnPositionChanged(Self);
    end;

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
    begin
      FPosition := Trunc(((X - FDragFrom) / (ClientWidth - 20)) * Max);
      FGripperPos := X - FDragFrom;
    end else
    begin
      FPosition := Trunc(((Y - FDragFrom) / (ClientHeight - 20)) * Max);
      FGripperPos := Y - FDragFrom;
    end;

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
