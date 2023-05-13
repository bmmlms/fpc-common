unit MEditButton;

interface

uses
  Classes,
  Controls,
  EditBtn,
  MSpeedButton;

type
  { TMEditButton }

  TMEditButton = class(TEditButton)
  protected
    function GetBuddyClassType: TControlClass; override;
  end;

procedure Register;

implementation

procedure Register;
const
  Classes: array of TComponentClass = [TMEditButton];
begin
  RegisterComponents('MControls', Classes);
end;

{ TMEditButton }

function TMEditButton.GetBuddyClassType: TControlClass;
begin
  Result := TMSpeedButton;
end;

end.
