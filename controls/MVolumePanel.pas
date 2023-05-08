unit MVolumePanel;

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
  MSeekBar,
  MVirtualTree,
  SysUtils,
  Themes,
  Windows;

type
  TOnGetVolumeBeforeMute = function(Sender: TObject): Integer of object;

  { TMVolumePanel }

  TMVolumePanel = class(TPanel)
  private
    FTrackBar: TMSeekBar;
    FMute: TSpeedButton;
    FVolume: Integer;
    FVolumeBeforeDrag: Integer;
    FVolumeChange: TNotifyEvent;
    FImageIndexSound: Integer;
    FImageIndexSoundLow: Integer;
    FImageIndexMute: Integer;

    FOnGetVolumeBeforeMute: TOnGetVolumeBeforeMute;

    procedure MuteClick(Sender: TObject);
    procedure VolumeChange(Sender: TObject);
    procedure RefreshButtonState;
    procedure FSetVolume(Volume: Integer);
    procedure FSetNotifyOnMove(Value: Boolean);
    function FGetVolume: Integer;
    procedure FSetImages(Value: TImageList);
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); reintroduce;

    property OnVolumeChange: TNotifyEvent read FVolumeChange write FVolumeChange;
    property Volume: Integer read FGetVolume write FSetVolume;
    property VolumeBeforeDrag: Integer read FVolumeBeforeDrag;
    property NotifyOnMove: Boolean write FSetNotifyOnMove;
    property OnGetVolumeBeforeMute: TOnGetVolumeBeforeMute read FOnGetVolumeBeforeMute write FOnGetVolumeBeforeMute;
    property Images: TImageList write FSetImages;
    property ImageIndexSound: Integer read FImageIndexSound write FImageIndexSound;
    property ImageIndexSoundLow: Integer read FImageIndexSoundLow write FImageIndexSoundLow;
    property ImageIndexMute: Integer read FImageIndexMute write FImageIndexMute;
  end;

procedure Register;

implementation

procedure Register;
const
  Classes: array of TComponentClass = [TMVolumePanel];
begin
  RegisterComponents('MControls', Classes);
end;

{ TMVolumePanel }

procedure TMVolumePanel.SetEnabled(Value: Boolean);
begin
  inherited;

  FMute.Enabled := Value;
  FTrackBar.GripperVisible := Value;
end;

procedure TMVolumePanel.MuteClick(Sender: TObject);
var
  P: Integer;
begin
  if FMute.Down then
  begin
    FTrackBar.PositionBeforeDrag := FTrackBar.Position;
    FTrackBar.Position := 0;

    FMute.ImageIndex := FImageIndexMute;
    FMute.Down := True;
  end else
  begin
    P := FOnGetVolumeBeforeMute(Self);
    FTrackBar.Position := P;
    FMute.ImageIndex := IfThen(P > 50, FImageIndexSound, FImageIndexSoundLow);
  end;
end;

procedure TMVolumePanel.VolumeChange(Sender: TObject);
begin
  FVolume := FTrackBar.Position;
  FVolumeBeforeDrag := FTrackBar.PositionBeforeDrag;

  RefreshButtonState;

  if Assigned(OnVolumeChange) then
    OnVolumeChange(Self);
end;

procedure TMVolumePanel.RefreshButtonState;
begin
  if Volume = 0 then
  begin
    FMute.Down := True;
    FMute.ImageIndex := FImageIndexMute;
  end else
  begin
    FMute.Down := False;
    FMute.ImageIndex := IfThen(Volume > 50, FImageIndexSound, FImageIndexSoundLow);
  end;
end;

procedure TMVolumePanel.FSetVolume(Volume: Integer);
begin
  FTrackBar.Position := Volume;
  RefreshButtonState;
end;

function TMVolumePanel.FGetVolume: Integer;
begin
  Result := FTrackBar.Position;
end;

procedure TMVolumePanel.FSetImages(Value: TImageList);
begin
  FMute.Images := Value;
end;

procedure TMVolumePanel.FSetNotifyOnMove(Value: Boolean);
begin
  FTrackBar.NotifyOnMove := Value;
end;

constructor TMVolumePanel.Create(AOwner: TComponent);
begin
  inherited;

  BevelOuter := bvNone;

  FMute := TSpeedButton.Create(Self);
  FMute.Hint := 'Mute';
  FMute.Flat := True;
  FMute.Align := alLeft;
  FMute.GroupIndex := 1;
  FMute.AllowAllUp := True;
  FMute.Down := True;
  FMute.OnClick := MuteClick;
  FMute.AutoSize := True;
  FMute.Parent := Self;

  FTrackBar := TMSeekBar.Create(Self);
  FTrackBar.Max := 100;
  FTrackBar.Align := alClient;
  FTrackBar.OnPositionChanged := VolumeChange;
  FTrackBar.GripperVisible := True;
  FTrackBar.NotifyOnMove := True;
  FTrackBar.NotifyOnDown := True;
  FTrackBar.BorderSpacing.Bottom := 1;
  FTrackBar.Parent := Self;

  Constraints.MinHeight := 21;
  Constraints.MaxHeight := 21;

  RefreshButtonState;
end;

end.
