unit MToolbarForcedHorizontal;

interface

uses
  Classes,
  ComCtrls,
  Toolwin;

type
  { TMToolbarForcedHorizontal }

  TMToolbarForcedHorizontal = class(TToolBar)
  public
    constructor Create(TheOwner: TComponent); override;

    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean); override;
  protected
    function IsVertical: Boolean; override;
    procedure AdjustClientRect(var ARect: TRect); override;
  end;

procedure Register;

implementation

procedure Register;
const
  Classes: array of TComponentClass = [TMToolbarForcedHorizontal];
begin
  RegisterComponents('MControls', Classes);
end;

{ TMToolbarForcedHorizontal }

constructor TMToolbarForcedHorizontal.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Indent := 0;
  EdgeBorders := [];
 // EdgeInner := esNone;
 // EdgeOuter := esNone;
  AutoSize := True;
  ButtonWidth := 23;
  ButtonHeight := 24;
end;

procedure TMToolbarForcedHorizontal.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean);
begin
  //  Constraints.MinHeight := ButtonHeight - 1;
  //  Constraints.MaxHeight := ButtonHeight - 1;

  inherited;
end;

function TMToolbarForcedHorizontal.IsVertical: Boolean;
begin
  Result := False;
end;

procedure TMToolbarForcedHorizontal.AdjustClientRect(var ARect: TRect);
begin
  inherited AdjustClientRect(ARect);

  ARect.Bottom += 1;
end;

end.
