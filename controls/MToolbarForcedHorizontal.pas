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
  protected
    function IsVertical: Boolean; override;
    procedure AdjustClientRect(var ARect: TRect); override;
    procedure Loaded; override;
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
  AutoSize := True;
  ButtonWidth := Scale96ToFont(23);
  ButtonHeight := Scale96ToFont(24);
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

procedure TMToolbarForcedHorizontal.Loaded;
begin
  inherited Loaded;

  ButtonWidth := Scale96ToFont(ButtonWidth);
  ButtonHeight := Scale96ToFont(ButtonHeight);
end;

end.
