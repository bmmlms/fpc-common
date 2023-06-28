unit MSpeedButton;

interface

uses
  Buttons,
  Classes;

type
  { TMSpeedButton }

  TMSpeedButton = class(TSpeedButton)
  public
    constructor Create(AOwner: TComponent); override;
  protected
    procedure Loaded; override;
    procedure PaintBackground(var PaintRect: TRect); override;
  end;

procedure Register;

implementation

procedure Register;
const
  Classes: array of TComponentClass = [TMSpeedButton];
begin
  RegisterComponents('MControls', Classes);
end;

{ TMSpeedButton }

constructor TMSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Constraints.MinWidth := Scale96ToFont(23);
  Constraints.MinHeight := Scale96ToFont(23);
end;

procedure TMSpeedButton.Loaded;
begin
  inherited Loaded;

  Constraints.MinWidth := Scale96ToFont(23);
  Constraints.MinHeight := Scale96ToFont(23);
end;

procedure TMSpeedButton.PaintBackground(var PaintRect: TRect);
begin
  PaintRect.Bottom += 1;

  inherited PaintBackground(PaintRect);
end;

end.
