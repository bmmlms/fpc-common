unit MLabeledEdit;

interface

uses
  Classes,
  ComboEx,
  Controls,
  Dialogs,
  EditBtn,
  ExtCtrls,
  Forms,
  Graphics,
  IDEIntf,
  LResources,
  MEditButton,
  MHotkeyEdit,
  PropEdits,
  Spin,
  StdCtrls,
  SysUtils,
  TypInfo;

type

  { TMLabeledControl }

  TMLabeledControl<T> = class(TCustomPanel)
  private
    FCaption: string;
    FEdit: TEdit;
    FLabel: TLabel;

    procedure FSetCaption(Value: string);
  protected
    FControl: T;

    procedure Paint; override;
    procedure Loaded; override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Enabled;

    property Caption: string read FCaption write FSetCaption;
  end;

  TMLabeledEdit = class(TMLabeledControl<TEdit>)
  published
    property Control: TEdit read FControl;
  end;

  TMLabeledSpinEdit = class(TMLabeledControl<TSpinEdit>)
  published
    property Control: TSpinEdit read FControl;
  end;

  TMLabeledEditButton = class(TMLabeledControl<TMEditButton>)
  published
    property Control: TMEditButton read FControl;
  end;

  TMLabeledHotkeyEdit = class(TMLabeledControl<TMHotkeyEdit>)
  published
    property Control: TMHotkeyEdit read FControl;
  end;

  { TMLabeledComboBoxEx }

  TMLabeledComboBoxEx = class(TMLabeledControl<TComboBoxEx>)
  public
    constructor Create(TheOwner: TComponent); override;
  protected
    procedure Loaded; override;
  published
    property Control: TComboBoxEx read FControl;
  end;

procedure Register;

implementation

procedure UnlistPublishedProperty(ComponentClass: TPersistentClass; const PropertyName: String);
var
  pi: PPropInfo;
begin
  pi := TypInfo.GetPropInfo(ComponentClass, PropertyName);
  if (pi <> nil) then
    RegisterPropertyEditor(pi^.PropType, ComponentClass, PropertyName, PropEdits.THiddenPropertyEditor);
end;

procedure Register;
const
  Classes: array of TComponentClass = [TMLabeledEdit, TMLabeledSpinEdit, TMLabeledEditButton, TMLabeledHotkeyEdit, TMLabeledComboBoxEx];
var
  C: TComponentClass;
begin
  RegisterComponents('MControls', Classes);

  for C in Classes do
  begin
    UnlistPublishedProperty(C, 'HelpContext');
    UnlistPublishedProperty(C, 'HelpKeyword');
    UnlistPublishedProperty(C, 'HelpType');
    UnlistPublishedProperty(C, 'Hint');
  end;
end;

{ TMLabeledControl }

procedure TMLabeledControl<T>.FSetCaption(Value: string);
begin
  FCaption := Value;
  if Assigned(FLabel) then
    FLabel.Caption := FCaption;
end;

procedure TMLabeledControl<T>.Paint;
begin

end;

procedure TMLabeledControl<T>.Loaded;
begin
  inherited Loaded;

  FControl.Width := Scale96ToFont(FControl.Width);

  FSetCaption(FCaption);
end;

procedure TMLabeledControl<T>.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);

  if not (csLoading in ComponentState) and (FCaption = '') then
    FSetCaption(Value);
end;

constructor TMLabeledControl<T>.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  AutoSize := True;
  BevelOuter := bvNone;

  FLabel := TLabel.Create(Self);
  FLabel.Align := alTop;
  FLabel.AutoSize := True;
  FLabel.BorderSpacing.Bottom := Scale96ToFont(3);
  FLabel.Parent := Self;

  FControl := T.Create(Self);
  FControl.Align := alLeft;
  FControl.Parent := Self;
  FControl.SetSubComponent(True);
end;

{ TMLabeledComboBoxEx }

constructor TMLabeledComboBoxEx.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Control.ItemHeight := 17;
end;

procedure TMLabeledComboBoxEx.Loaded;
begin
  inherited Loaded;

  Control.ItemHeight := Scale96ToFont(Control.ItemHeight);
end;

end.
