unit MSpeedButton;

interface

uses
  Buttons,
  Classes;

type
  { TMSpeedButton }

  TMSpeedButton = class(TSpeedButton)
  protected
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

procedure TMSpeedButton.PaintBackground(var PaintRect: TRect);
begin
  PaintRect.Bottom += 1;

  inherited PaintBackground(PaintRect);
end;

end.
