{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MControls;

{$warn 5023 off : no warning about unused units}
interface

uses
  MLabeledEdit, MHotKeyEdit, MPageControl, MControlFocuser, MStringFunctions, 
  MComboBoxExEditable, MVirtualTree, MSeekBar, MVolumePanel, 
  MToolbarForcedHorizontal, MSpeedButton, MEditButton, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('MLabeledEdit', @MLabeledEdit.Register);
  RegisterUnit('MPageControl', @MPageControl.Register);
  RegisterUnit('MComboBoxExEditable', @MComboBoxExEditable.Register);
  RegisterUnit('MVirtualTree', @MVirtualTree.Register);
  RegisterUnit('MSeekBar', @MSeekBar.Register);
  RegisterUnit('MVolumePanel', @MVolumePanel.Register);
  RegisterUnit('MToolbarForcedHorizontal', @MToolbarForcedHorizontal.Register);
  RegisterUnit('MSpeedButton', @MSpeedButton.Register);
  RegisterUnit('MEditButton', @MEditButton.Register);
end;

initialization
  RegisterPackage('MControls', @Register);
end.
