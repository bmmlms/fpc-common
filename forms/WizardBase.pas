{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2025 Alexander Nottelmann

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 3
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
    ------------------------------------------------------------------------
}

unit WizardBase;

interface

uses
  AppData,
  AppDataBase,
  Buttons,
  Classes,
  ComboEx,
  ComCtrls,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  LanguageObjects,
  MControlFocuser,
  MControls,
  MLabeledEdit,
  SettingsStorage,
  shlobj,
  StdCtrls,
  SysUtils,
  Variants,
  Windows;

type
  TStep = class
  private
    FCaption: string;
    FDescription: string;
    FPanel: TPanel;
  protected
  public
    constructor Create(Caption: string; Panel: TPanel);
    property Caption: string read FCaption;
    property Description: string read FDescription write FDescription;
    property Panel: TPanel read FPanel;
  end;

  TStepList = class(TList)
  private
    function Get2(Index: Integer): TStep;
    procedure Put2(Index: Integer; Item: TStep);
  public
    property Items[Index: Integer]: TStep read Get2 write Put2; default;
  end;

  { TfrmWizardBase }

  TfrmWizardBase = class(TForm)
    lblDesc: TLabel;
    lstLanguages: TMLabeledComboBoxEx;
    pnlLanguage: TPanel;
    pnlNav: TPanel;
    Bevel2: TBevel;
    pnlStorage: TPanel;
    pnlUpdates: TPanel;
    chkAutoUpdate: TCheckBox;
    pnlHeader: TPanel;
    Shape1: TShape;
    lblTop: TLabel;
    optAppData: TRadioButton;
    optPortable: TRadioButton;
    lblAppData: TLabel;
    lblPortable: TLabel;
    btnNext: TBitBtn;
    btnBack: TBitBtn;
    procedure FormDestroy(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure lstLanguagesSelect(Sender: TObject);
    procedure lblPortableClick(Sender: TObject);
    procedure lblAppDataClick(Sender: TObject);
  private
    FInitializedSteps: TList;
    procedure SetText;
    procedure FSetDescription(Value: string);
  protected
    FStepList: TStepList;
    FActiveIndex: Integer;
    FActiveSetup: TStep;
    procedure NextStep;
    procedure PrevStep;
    procedure SetStep(Idx: Integer);
    procedure RegisterSteps; virtual;
    procedure Finish; virtual;
    function IsValid(Step: TStep): Boolean; virtual;
    procedure InitStep(Step: TStep); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    property Description: string write FSetDescription;
  end;

implementation

{$R *.lfm}

procedure TfrmWizardBase.btnBackClick(Sender: TObject);
begin
  PrevStep;
end;

procedure TfrmWizardBase.btnNextClick(Sender: TObject);
begin
  NextStep;
end;

constructor TfrmWizardBase.Create(AOwner: TComponent);
begin
  inherited;

  FInitializedSteps := TList.Create;
  FActiveSetup := nil;

  FStepList := TStepList.Create;

  RegisterSteps;

  Language.Translate(Self);

  Constraints.MinWidth := Scale96ToFont(Constraints.MinWidth);
  Constraints.MinHeight := Scale96ToFont(Constraints.MinHeight);
end;

procedure TfrmWizardBase.Finish;
begin
  if lstLanguages.Control.ItemIndex > -1 then
  begin
    Language.CurrentLanguage := TLanguage(lstLanguages.Control.ItemsEx[lstLanguages.Control.ItemIndex].Data);
    AppGlobals.Language := Language.CurrentLanguage.ID;
  end;

  AppGlobals.AutoUpdate := chkAutoUpdate.Checked;
  AppGlobals.FirstStartShown := True;
  AppGlobals.WasSetup := True;

  AppGlobals.Save(Handle);

  Close;
end;

procedure TfrmWizardBase.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmWizardBase.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not AppGlobals.WasSetup then
    if TFunctions.MsgBox(_('Closing the wizard will exit the application.'#13#10'Do you really want to exit now?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON2) = IDNO then
      CanClose := False;
end;

procedure TfrmWizardBase.FormCreate(Sender: TObject);
var
  n, i: Integer;
  ComboItem: TComboExItem;
begin
  for n := 0 to ControlCount - 1 do
    for i := 0 to FStepList.Count - 1 do
      if Controls[n] = FStepList[i].Panel then
      begin
        TPanel(Controls[n]).Enabled := False;
        TPanel(Controls[n]).BevelOuter := bvNone;
        TPanel(Controls[n]).Align := alClient;
        Break;
      end;

  lstLanguages.Control.Clear;
  lstLanguages.Control.Images := AppGlobals.LanguageIcons.List;
  for i := 0 to LanguageList.Count - 1 do
    if LanguageList[i].Available then
    begin
      ComboItem := lstLanguages.Control.ItemsEx.Add;
      ComboItem.Caption := LanguageList[i].Name;
      ComboItem.Data := LanguageList[i];
      ComboItem.ImageIndex := AppGlobals.LanguageIcons.GetIconIndex(LanguageList[i].ID);
    end;

  for i := 0 to lstLanguages.Control.ItemsEx.Count - 1 do
    if Language.CurrentLanguage.ID = TLanguage(lstLanguages.Control.ItemsEx[i].Data).ID then
    begin
      lstLanguages.Control.ItemIndex := i;
      Break;
    end;

  lstLanguages.Control.ItemsEx.SortType := stText;

  SetStep(0);
end;

procedure TfrmWizardBase.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  FInitializedSteps.Free;
  for i := 0 to FStepList.Count - 1 do
    TStep(FStepList[i]).Free;
  FStepList.Free;
end;

procedure TfrmWizardBase.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmWizardBase.FormShow(Sender: TObject);
begin
  btnNext.ApplyFocus;
  SetText;
end;

procedure TfrmWizardBase.FSetDescription(Value: string);
begin
  lblDesc.Caption := Value;
end;

procedure TfrmWizardBase.InitStep(Step: TStep);
begin
  if Step.Panel = pnlStorage then
  begin
    optAppData.Checked := True;
    optPortable.Enabled := AppGlobals.PortableAllowed;
    lblPortable.Enabled := AppGlobals.PortableAllowed;

    optAppData.Checked := True;
    case AppGlobals.Portable of
      poYes:
        optPortable.Checked := True;
      poNo:
        optAppData.Checked := True;
      poUndefined:
        if AppGlobals.RunningFromInstalledLocation then
          optAppData.Checked := True
        else if AppGlobals.PortableAllowed then
          optPortable.Checked := True;
    end;
  end;

  if Step.Panel = pnlUpdates then
    chkAutoUpdate.Checked := AppGlobals.AutoUpdate;
end;

function TfrmWizardBase.IsValid(Step: TStep): Boolean;
begin
  Result := True;

  if Step.Panel = pnlStorage then
  begin
    if optAppData.Checked then
      AppGlobals.Portable := poNo;
    if optPortable.Checked then
      AppGlobals.Portable := poYes;
    FInitializedSteps.Clear;
  end;
end;

procedure TfrmWizardBase.lblAppDataClick(Sender: TObject);
begin
  optAppData.Checked := True;
end;

procedure TfrmWizardBase.lblPortableClick(Sender: TObject);
begin
  optPortable.Checked := True;
end;

procedure TfrmWizardBase.lstLanguagesSelect(Sender: TObject);
begin
  if lstLanguages.Control.ItemIndex > -1 then
  begin
    Language.CurrentLanguage := TLanguage(lstLanguages.Control.ItemsEx[lstLanguages.Control.ItemIndex].Data);
    Language.Translate(Self);

    SetText;
  end;
end;

procedure TfrmWizardBase.NextStep;
begin
  if IsValid(FActiveSetup) then
    SetStep(FActiveIndex + 1);
end;

procedure TfrmWizardBase.PrevStep;
begin
  SetStep(FActiveIndex - 1);
end;

procedure TfrmWizardBase.RegisterSteps;
begin
  FStepList.Add(TStep.Create('Select language', pnlLanguage));
  FStepList.Add(TStep.Create('Configure settings', pnlStorage));
  FStepList.Add(TStep.Create('Configure updates', pnlUpdates));

  FStepList[0].Description := Format(_('Welcome to %s!'#13#10'This wizard will guide you through the initial setup.'#13#10'Please select your language now.'), [AppGlobals.AppName]);
  FStepList[1].Description := 'Please select where application data should be saved.';
  FStepList[2].Description := 'Please choose whether automatic search for updates should be enabled.';
end;

procedure TfrmWizardBase.SetStep(Idx: Integer);
begin
  if (FActiveSetup <> nil) and (Idx > -1) then
    FActiveSetup.Panel.Enabled := False;

  if Idx = -1 then
  begin
    Close;
    Exit;
  end else if Idx > FStepList.Count - 1 then
  begin
    Finish;
    Exit;
  end;

  if FInitializedSteps.IndexOf(FStepList[Idx]) = -1 then
  begin
    FInitializedSteps.Add(FStepList[Idx]);
    InitStep(FStepList[Idx]);
  end;

  if Idx = 0 then
    btnBack.Caption := _('&Cancel')
  else
    btnBack.Caption := _('&Back');

  if Idx = FStepList.Count - 1 then
    btnNext.Caption := _('&Finish')
  else
    btnNext.Caption := _('&Next');

  FActiveIndex := Idx;
  FActiveSetup := FStepList[Idx];
  FActiveSetup.Panel.Enabled := True;
  FActiveSetup.Panel.BringToFront;
  lblTop.Caption := _(FActiveSetup.Caption);

  if FActiveSetup.FDescription <> '' then
  begin
    lblDesc.Caption := _(FActiveSetup.FDescription);
    lblDesc.Visible := True;
  end else
    lblDesc.Visible := False;
end;

procedure TfrmWizardBase.SetText;
begin
  lblDesc.Caption := Format(_('Welcome to %s!'#13#10'This wizard will guide you through the initial setup.'#13#10'Please select your language now.'), [AppGlobals.AppName]);
  FStepList[0].Description := Format(_('Welcome to %s!'#13#10'This wizard will guide you through the initial setup.'#13#10'Please select your language now.'), [AppGlobals.AppName]);

  lblAppData.Caption := Format(_('Settings will be saved to "%s" and/or to the registry to "%s".'#13#10'This makes sense if the application was installed.'),
    [TSettingsInstalled.GetDataDir(CSIDL_APPDATA, AppGlobals.AppName), 'HKCU' + TSettingsInstalled.GetRegPath(AppGlobals.AppName)]);
  lblPortable.Caption := Format(_('Settings will be saved to application folder which is "%s" at the moment.'#13#10'This makes sense if the application will be used in portable mode.'), [TSettingsPortable.GetDataDir]);
  lblTop.Caption := _(FActiveSetup.FCaption);
end;

{ TStepList }

function TStepList.Get2(Index: Integer): TStep;
begin
  Result := TStep(inherited Get(Index));
end;

procedure TStepList.Put2(Index: Integer; Item: TStep);
begin
  inherited Put(Index, Item);
end;

{ TStep }

constructor TStep.Create(Caption: string; Panel: TPanel);
begin
  FCaption := Caption;
  FPanel := Panel;
end;

end.
