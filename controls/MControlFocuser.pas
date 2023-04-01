unit MControlFocuser;

interface

uses
  Controls;

type

  TWinControlFocuser = class helper for TWinControl
  public
    procedure ApplyFocus;
  end;

implementation

{ TWinControlFocuser }

procedure TWinControlFocuser.ApplyFocus;
begin
  try
    SetFocus;
  except
  end;
end;

end.
