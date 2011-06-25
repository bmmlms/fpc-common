unit AdvancedRect;

interface

uses
  Windows;

type
  TAdvancedRect = record
  private
    FLeft, FRight, FTop, FBottom, FWidth, FHeight: Integer;

    procedure CalcWidthHeight;

    procedure FSetLeft(Value: Integer);
    procedure FSetRight(Value: Integer);
    procedure FSetTop(Value: Integer);
    procedure FSetBottom(Value: Integer);
    procedure FSetWidth(Value: Integer);
    procedure FSetHeight(Value: Integer);

    function FGetRect: TRect;
  public
    property Left: Integer read FLeft write FSetLeft;
    property Right: Integer read FRight write FSetRight;
    property Top: Integer read FTop write FSetTop;
    property Bottom: Integer read FBottom write FSetBottom;
    property Width: Integer read FWidth write FSetWidth;
    property Height: Integer read FHeight write FSetHeight;

    property Rect: TRect read FGetRect;
  end;

implementation

{ TAdvancedRect }

procedure TAdvancedRect.CalcWidthHeight;
begin
  FWidth := FRight - FLeft;
  FHeight := FBottom - FTop;
end;

function TAdvancedRect.FGetRect: TRect;
begin
  Result.Top := FTop;
  Result.Left := FLeft;
  Result.Right := FRight;
  Result.Bottom := FBottom;
end;

procedure TAdvancedRect.FSetBottom(Value: Integer);
begin
  FBottom := Value;
  CalcWidthHeight;
end;

procedure TAdvancedRect.FSetHeight(Value: Integer);
begin
  FBottom := FTop + Value;
  FHeight := Value;
end;

procedure TAdvancedRect.FSetLeft(Value: Integer);
begin
  FLeft := Value;
  CalcWidthHeight;
end;

procedure TAdvancedRect.FSetRight(Value: Integer);
begin
  FRight := Value;
  CalcWidthHeight;
end;

procedure TAdvancedRect.FSetTop(Value: Integer);
begin
  FTop := Value;
  CalcWidthHeight;
end;

procedure TAdvancedRect.FSetWidth(Value: Integer);
begin
  FRight := FLeft + Value;
  FWidth := Value;
end;

end.
