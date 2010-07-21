{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010 Alexander Nottelmann

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, LanguageObjects, AppData,
  Functions, ComCtrls, AppDataBase, SettingsStorage;

type
  TStep = class
  private
    FCaption: string;
    FPanel: TPanel;
  protected
  public
    constructor Create(Caption: string; Panel: TPanel);
    property Caption: string read FCaption;
    property Panel: TPanel read FPanel;
  end;

  TStepList = class(TList)
  private
    function Get2(Index: Integer): TStep;
    procedure Put2(Index: Integer; Item: TStep);
  public
    property Items[Index: Integer]: TStep read Get2 write Put2; default;
  end;

  TfrmWizardBase = class(TForm)
    pnlLanguage: TPanel;
    lblLanguage: TLabel;
    pnlNav: TPanel;
    btnBack: TBitBtn;
    btnNext: TBitBtn;
    Bevel2: TBevel;
    pnlStorage: TPanel;
    pnlUpdates: TPanel;
    chkAutoUpdate: TCheckBox;
    pnlHeader: TPanel;
    Shape1: TShape;
    lblTop: TLabel;
    lstLanguages: TComboBoxEx;
    lblLanguageList: TLabel;
    optAppData: TRadioButton;
    lblData: TLabel;
    optPortable: TRadioButton;
    lblAppData: TLabel;
    lblPortable: TLabel;
    lblUpdates: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure lstLanguagesSelect(Sender: TObject);
  private
    FInitializedSteps: TList;
    procedure SetText;
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
  private

    { Private-Deklarationen }
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

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
end;

procedure TfrmWizardBase.Finish;
begin
  if lstLanguages.ItemIndex > -1 then
  begin
    Language.CurrentLanguage := TLanguage(lstLanguages.ItemsEx[lstLanguages.ItemIndex].Data);
    AppGlobals.Language := Language.CurrentLanguage.ID;
  end;

  AppGlobals.AutoUpdate := chkAutoUpdate.Checked;

  // Das passt so, sonst kommt das FirstStart Form zwei mal.
  AppGlobals.FirstStartShown := True;

  AppGlobals.WasSetup := True;
  AppGlobals.Save;

  Close;
end;

procedure TfrmWizardBase.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmWizardBase.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if not AppGlobals.WasSetup then
  begin
    if MsgBox(Handle, _('Closing the wizard will exit the application.'#13#10'Do you really want to exit now?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON2) = IDNO then
      CanClose := False;
  end;
end;

procedure TfrmWizardBase.FormCreate(Sender: TObject);
var
  n, i: Integer;
  ComboItem: TComboExItem;
begin
  //SetWindowLong(Handle, GWL_HWNDPARENT, 0);


  ClientWidth := 400;
  ClientHeight := 300;

  for n := 0 to ControlCount - 1 do
    for i := 0 to FStepList.Count - 1 do
      if Controls[n] = FStepList[i].Panel then
      begin
        TPanel(Controls[n]).Enabled := False;
        TPanel(Controls[n]).BorderStyle := bsNone;
        TPanel(Controls[n]).BevelOuter := bvNone;
        TPanel(Controls[n]).Align := alClient;
        Break;
      end;

  lstLanguages.Clear;
  lstLanguages.Images := AppGlobals.LanguageIcons.List;
  for i := 0 to LanguageList.Count - 1 do
    if LanguageList[i].Available then
    begin
      ComboItem := lstLanguages.ItemsEx.Add;
      ComboItem.Caption := LanguageList[i].Name;
      ComboItem.Data := LanguageList[i];
      ComboItem.ImageIndex := AppGlobals.LanguageIcons.GetIconIndex(LanguageList[i].ID);
      if Language.CurrentLanguage.ID = LanguageList[i].ID then
        lstLanguages.ItemIndex := ComboItem.Index;
    end;

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

procedure TfrmWizardBase.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmWizardBase.FormShow(Sender: TObject);
begin
  btnNext.SetFocus;
  SetText;
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
        begin
          if AppGlobals.RunningFromInstalledLocation then
            optAppData.Checked := True
          else if AppGlobals.PortableAllowed then
            optPortable.Checked := True;
        end;
    end;
  end;
  if Step.Panel = pnlUpdates then
  begin
    chkAutoUpdate.Checked := AppGlobals.AutoUpdate;
  end;
end;

function TfrmWizardBase.IsValid(Step: TStep): Boolean;
begin
  Result := True;
  if Step.Panel = pnlLanguage then
  begin

  end;
  if Step.Panel = pnlStorage then
  begin
    if optAppData.Checked then
      AppGlobals.Portable := poNo;
    if optPortable.Checked then
      AppGlobals.Portable := poYes;
    FInitializedSteps.Clear;
  end;
  if Step.Panel = pnlUpdates then
  begin

  end;
end;

procedure TfrmWizardBase.lstLanguagesSelect(Sender: TObject);
begin
  if lstLanguages.ItemIndex > -1 then
  begin
    Language.CurrentLanguage := TLanguage(lstLanguages.ItemsEx[lstLanguages.ItemIndex].Data);
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
end;

procedure TfrmWizardBase.SetStep(Idx: Integer);
begin
  if FActiveSetup <> nil then
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
end;

procedure TfrmWizardBase.SetText;
begin
  lblAppData.Caption := Format(_('Settings will be saved to "%s" and/or to the registry to "%s".'#13#10'This makes sense if the application was installed previously.'),
    [TSettingsInstalled.GetDataDir(AppGlobals.AppName), 'HKCU' + TSettingsInstalled.GetRegPath(AppGlobals.AppName)]);
  lblPortable.Caption := Format(_('Settings will be saved to application folder which is "%s" at the moment.'#13#10'This makes sense if the application was not installed and should be portable.'),
    [TSettingsPortable.GetDataDir]);
  lblTop.Caption := _(FActiveSetup.FCaption);
  lblLanguage.Caption := Format(_('Welcome to %s!'#13#10'This wizard will guide you through the initial setup.'#13#10'Please select your language now.'), [AppGlobals.AppName]);
  lblData.Caption := _('Please select where application data should be saved.');
  lblUpdates.Caption := _('Please choose if automatic search for updates should be enabled.'#13#10'No personal information will be transferred and you will be asked before an update will be downloaded or applied.');
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
