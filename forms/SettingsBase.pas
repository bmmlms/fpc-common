{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2024 Alexander Nottelmann

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

unit SettingsBase;

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
  Generics.Collections,
  Graphics,
  ImgList,
  LanguageObjects,
  LCLType,
  MControlFocuser,
  MControls,
  MLabeledEdit,
  MStringFunctions, MVirtualTree,
  SettingsStorage,
  StdCtrls,
  StreamHelper,
  SysUtils,
  Variants,
  VirtualTrees;

type
  TPage = class
  private
    FCaption: string;
    FOriginalCaption: string;
    FPanel: TPanel;
    FNode: PVirtualNode;
    FImageIndex: Integer;
    FParent: TPage;
  protected
  public
    constructor Create(OriginalCaption: string; Panel: TPanel; ImageIndex: Integer); overload;
    constructor Create(OriginalCaption: string; Panel: TPanel; ImageIndex: Integer; Parent: TPage); overload;
    property Caption: string read FCaption;
    property Panel: TPanel read FPanel;
    property Node: PVirtualNode read FNode write FNode;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property Parent: TPage read FParent;
  end;

  TPageList = class(TList<TPage>)
  public
    function Find(P: TPanel): TPage;
  end;

  TPageTree = class(TVirtualStringTree)
  private
    FColName: TVirtualTreeColumn;
    FPages: TPageList;
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: string); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure Resize; override;
    function DoCollapsing(Node: PVirtualNode): Boolean; override;
  public
    constructor Create(AOwner: TComponent; Pages: TPageList); reintroduce;
    function AddChild(Parent: PVirtualNode; UserData: Pointer = nil): PVirtualNode; override;
    procedure Setup;
  end;

  { TfrmSettingsBase }

  TfrmSettingsBase = class(TForm, IPostTranslatable)
    btnCopyProfile: TButton;
    btnDeleteProfile: TButton;
    btnExportProfile: TButton;
    btnImportProfile: TButton;
    FlowPanelPages: TFlowPanel;
    lstLanguages: TMLabeledComboBoxEx;
    pnlProfileBottom: TPanel;
    pnlProfileButtons: TPanel;
    txtHost: TMLabeledEdit;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    pnlLeft: TPanel;
    pnlGeneral: TPanel;
    chkAutoUpdateCheck: TCheckBox;
    lblPortable: TLabel;
    chkProxy: TCheckBox;
    chkCheckCertificate: TCheckBox;
    txtPort: TMLabeledSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure lstLanguagesChange(Sender: TObject);
    procedure btnCopyProfileClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure chkProxyClick(Sender: TObject);
    procedure btnDeleteProfileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnExportProfileClick(Sender: TObject);
    procedure btnImportProfileClick(Sender: TObject);
  private
    FSaveSettings: Boolean;
    FTreeView: TPageTree;
    FImportFilename: string;

    FOnSaveForExport: TNotifyEvent;

    procedure TreeViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  protected
    FPageList: TPageList;
    FActivePage: TPage;
    FImages: TImageList;
    procedure SetPage(Page: TPage); overload; virtual;
    procedure SetPage(Panel: TPanel); overload; virtual;
    procedure RegisterPages; virtual;
    procedure RegisterGeneralPage(ImageIndex: Integer); virtual;
    procedure Finish; virtual;
    function CanFinish: Boolean; virtual;
    procedure PostTranslate; virtual;
    procedure GetExportDataHeader(Stream: TMemoryStream); virtual;
    procedure GetExportData(Stream: TMemoryStream); virtual;
    function CheckImportFile(Filename: string): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent; Images: TImageList; ShowGeneral: Boolean); reintroduce;

    property SaveSettings: Boolean read FSaveSettings;
    property ImportFilename: string read FImportFilename;

    property OnSaveForExport: TNotifyEvent read FOnSaveForExport write FOnSaveForExport;
  end;

implementation

{$R *.lfm}

{ TPage }

constructor TPage.Create(OriginalCaption: string; Panel: TPanel; ImageIndex: Integer);
begin
  inherited Create;

  FOriginalCaption := OriginalCaption;
  FCaption := _(FOriginalCaption);
  FPanel := Panel;
  FImageIndex := ImageIndex;
end;

constructor TPage.Create(OriginalCaption: string; Panel: TPanel; ImageIndex: Integer; Parent: TPage);
begin
  Create(OriginalCaption, Panel, ImageIndex);

  FParent := Parent;
end;

{ TPageList }

function TPageList.Find(P: TPanel): TPage;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].FPanel = P then
    begin
      Result := Items[i];
      Break;
    end;
end;

{ TfrmSettingsBase }

procedure TfrmSettingsBase.PostTranslate;
var
  i: Integer;
begin
  if ((AppGlobals.Portable = poNo) and (not AppGlobals.PortableAllowed)) or (AppGlobals.Portable = poUndefined) then
  begin
    lblPortable.Visible := False;
    btnCopyProfile.Visible := False;
  end else if AppGlobals.Portable = poYes then
    lblPortable.Caption := _('This application is using portable settings. ' + 'To copy these settings to the registry/application data folder, press ''Copy profile''.')
  else if AppGlobals.Portable = poNo then
    lblPortable.Caption := _('This application is using settings from the registry/application data folder. ' + 'To copy these settings to a portable profile, press ''Copy profile''.');

  for i := 0 to FPageList.Count - 1 do
  begin
    FPageList[i].FCaption := _(FPageList[i].FOriginalCaption);
    FTreeView.Invalidate;
  end;
end;

procedure TfrmSettingsBase.btnCopyProfileClick(Sender: TObject);
var
  S: TSettingsInstalled;
  S2: TSettingsPortable;
begin
  if ((AppGlobals.Portable = poYes) and (TSettingsInstalled.Active(AppGlobals.AppName)) or (AppGlobals.Portable = poNo) and (TSettingsPortable.Active(AppGlobals.AppName))) then
    if TFunctions.MsgBox(_('All settings of the other profile will be replaced by the current profile''s settings.'#13#10 + 'Please be aware that existing settings and files containing data will be overwritten.'#13#10 + 'Proceed?'),
      'Question', MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON2) = IDNO then
      Exit;

  try
    S := TSettingsInstalled.Create(AppGlobals.AppName, AppGlobals.AppPath, AppGlobals.CommandLine);
    S2 := TSettingsPortable.Create(AppGlobals.AppName, AppGlobals.AppPath, AppGlobals.CommandLine);
    try
      if AppGlobals.Portable = poYes then
        S.Assign(S2)
      else
        S2.Assign(S);
    finally
      S.Free;
      S2.Free;
    end;

    TFunctions.MsgBox(_('The profile was copied successfully. To use the copied profile, restart the application.'), _('Info'), MB_ICONINFORMATION);
  except
    TFunctions.MsgBox(_('An error occured while copying the settings.'#13#10 + 'It is possible that some settings were copied before the error occured.'), _('Error'), MB_ICONERROR);
  end;
end;

procedure TfrmSettingsBase.btnOKClick(Sender: TObject);
begin
  if CanFinish then
    Finish;
end;

procedure TfrmSettingsBase.btnDeleteProfileClick(Sender: TObject);
begin
  if TFunctions.MsgBox(_('All data saved in the currently used profile will be deleted.'#13#10 + 'Are you sure you want to delete this profile?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON2) = IDYES then
  begin
    AppGlobals.Storage.DeleteProfile;
    AppGlobals.SkipSave := True;
    TFunctions.MsgBox(_('The profile was deleted.'#13#10 + 'When you exit the application, no data will be saved so that the profil will not be recreated.'), _('Info'), MB_ICONINFORMATION);
  end;
end;

procedure TfrmSettingsBase.btnExportProfileClick(Sender: TObject);
var
  S: TMemoryStream;
  Dlg: TSaveDialog;
  Lst: TSettingsList;
begin
  try
    Dlg := TSaveDialog.Create(Self);
    Dlg.Title := _('Save file');
    Dlg.Filter := 'streamWriter profile (*.dat)|*.dat';
    Dlg.Options := Dlg.Options + [ofOverwritePrompt];
    Dlg.DefaultExt := '.dat';
    S := TMemoryStream.Create;
    Lst := TSettingsList.Create;

    AppGlobals.Storage.IgnoreFields.Clear;
    AppGlobals.Storage.IgnoreFields.Add('User');
    AppGlobals.Storage.IgnoreFields.Add('Pass');
    try
      if Dlg.Execute and (Dlg.FileName <> '') then
      begin
        GetExportDataHeader(S);
        S.Write(Cardinal(1), False);
        AppGlobals.Storage.GetData(Lst);
        Lst.Save(S);
        GetExportData(S);
        S.SaveToFile(Dlg.FileName);
        TFunctions.MsgBox(_('The profile was exported successfully.'), _('Info'), MB_ICONINFORMATION);
      end;
    finally
      AppGlobals.Storage.IgnoreFields.Clear;
      Lst.Free;
      S.Free;
      Dlg.Free;
    end;
  except
    TFunctions.MsgBox(_('An error occured while exporting the profile.'), _('Error'), MB_ICONERROR);
  end;
end;

procedure TfrmSettingsBase.btnImportProfileClick(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  if TFunctions.MsgBox(_('The profile currently in use will be replaced with the imported one. After successful import streamWriter will restart.'#13#10'Do you want to continue?'), _('Question'), MB_ICONQUESTION or MB_YESNO) = IDYES then
  begin
    Dlg := TOpenDialog.Create(Self);
    Dlg.Title := _('Open file');
    Dlg.Filter := 'streamWriter profile (*.dat)|*.dat';
    try
      if Dlg.Execute then
        if (Dlg.FileName <> '') and (CheckImportFile(Dlg.FileName)) then
        begin
          FImportFilename := Dlg.FileName;
          Close;
        end;
    finally
      Dlg.Free;
    end;
  end;
end;

function TfrmSettingsBase.CanFinish: Boolean;
begin
  Result := True;

  if not Assigned(FPageList.Find(pnlGeneral)) then
    Exit;

  if chkProxy.Checked then
    if (Trim(txtHost.Control.Text) = '') or (Trim(txtPort.Control.Text) = '') or (StrToIntDef(txtPort.Control.Text, 0) <= 0) then
    begin
      TFunctions.MsgBox(_('You need to supply a host and a port (must be a positive number) to connect to if the use of a HTTP proxy is enabled.'), _('Info'), MB_ICONINFORMATION);
      SetPage(FPageList.Find(TPanel(txtHost.Parent)));
      if Trim(txtHost.Control.Text) = '' then
        txtHost.ApplyFocus
      else
        txtPort.ApplyFocus;
      Exit(False);
    end;
end;

function TfrmSettingsBase.CheckImportFile(Filename: string): Boolean;
begin
  Result := True;
end;

procedure TfrmSettingsBase.chkProxyClick(Sender: TObject);
begin
  txtHost.Enabled := chkProxy.Checked;
  txtPort.Enabled := chkProxy.Checked;
end;

constructor TfrmSettingsBase.Create(AOwner: TComponent; Images: TImageList; ShowGeneral: Boolean);
var
  i: Integer;
begin
  inherited Create(AOwner);

  FImages := Images;

  for i := FlowPanelPages.ControlCount - 1 downto 0 do
    FlowPanelPages.Controls[i].Parent := Self;

  // Alle Panels verstecken
  for i := 0 to ControlCount - 1 do
    if Controls[i].InheritsFrom(TPanel) and (Controls[i] <> pnlLeft) and (Controls[i] <> pnlNav) then
      Controls[i].Visible := False;

  FActivePage := nil;

  FPageList := TPageList.Create;

  RegisterPages;

  pnlLeft.Width := Scale96ToFont(190);

  FTreeView := TPageTree.Create(pnlLeft, FPageList);
  FTreeView.Parent := pnlLeft;
  FTreeView.Images := FImages;
  FTreeView.Align := alClient;
  FTreeView.OnChange := TreeViewChange;
  FTreeView.Show;

  btnCopyProfile.Enabled := not AppGlobals.Storage.DataDirOverridden;
  btnDeleteProfile.Enabled := not AppGlobals.Storage.DataDirOverridden;
  btnExportProfile.Enabled := not AppGlobals.Storage.DataDirOverridden;
  btnImportProfile.Enabled := not AppGlobals.Storage.DataDirOverridden;

  Language.Translate(Self);
end;

procedure TfrmSettingsBase.Finish;
begin
  if Assigned(FPageList.Find(pnlGeneral)) then
  begin
    AppGlobals.AutoUpdate := chkAutoUpdateCheck.Checked;

    AppGlobals.Lock;
    try
      AppGlobals.CheckCertificate := chkCheckCertificate.Checked;
      AppGlobals.ProxyEnabled := chkProxy.Checked;
      AppGlobals.ProxyHost := txtHost.Control.Text;
      AppGlobals.ProxyPort := StrToIntDef(txtPort.Control.Text, 8080);
    finally
      AppGlobals.Unlock;
    end;

    if lstLanguages.Control.ItemIndex > -1 then
      AppGlobals.Language := Language.CurrentLanguage.ID;
  end;

  FSaveSettings := True;

  Close;
end;

procedure TfrmSettingsBase.FormCreate(Sender: TObject);
var
  i: Integer;
  ComboItem: TComboExItem;
begin
  chkAutoUpdateCheck.Checked := AppGlobals.AutoUpdate;

  chkCheckCertificate.Checked := AppGlobals.CheckCertificate;
  chkProxy.Checked := AppGlobals.ProxyEnabled;
  txtHost.Enabled := chkProxy.Checked;
  txtPort.Enabled := chkProxy.Checked;
  txtHost.Control.Text := AppGlobals.ProxyHost;
  txtPort.Control.Text := IntToStr(AppGlobals.ProxyPort);

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
end;

procedure TfrmSettingsBase.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FPageList.Count - 1 do
    TPage(FPageList[i]).Free;
  FPageList.Free;
  Language.CurrentLanguage := LanguageList.FindLanguage(AppGlobals.Language);
end;

procedure TfrmSettingsBase.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;

  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmSettingsBase.FormShow(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ControlCount - 1 do
    if Controls[i].InheritsFrom(TPanel) and (Controls[i] <> pnlLeft) and (Controls[i] <> pnlNav) then
    begin
      Controls[i].Align := alClient;
      TPanel(Controls[i]).BevelOuter := bvNone;
    end;

  FTreeView.Setup;
  SetPage(FPageList[0]);
  FTreeView.ApplyFocus;
end;

procedure TfrmSettingsBase.GetExportData(Stream: TMemoryStream);
begin
  if Assigned(FOnSaveForExport) then
    FOnSaveForExport(Self);
end;

procedure TfrmSettingsBase.GetExportDataHeader(Stream: TMemoryStream);
begin

end;

procedure TfrmSettingsBase.lstLanguagesChange(Sender: TObject);
begin
  if lstLanguages.Control.ItemIndex > -1 then
  begin
    Language.CurrentLanguage := TLanguage(lstLanguages.Control.ItemsEx[lstLanguages.Control.ItemIndex].Data);
    Language.Translate(Self);
  end;
end;

procedure TfrmSettingsBase.TreeViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  i: Integer;
begin
  for i := 0 to FPageList.Count - 1 do
    if (FPageList[i].Node = Node) then
    begin
      SetPage(FPageList[i]);
      Break;
    end;
end;

procedure TfrmSettingsBase.RegisterPages;
begin

end;

procedure TfrmSettingsBase.RegisterGeneralPage(ImageIndex: Integer);
begin
  FPageList.Add(TPage.Create('General', pnlGeneral, ImageIndex));
end;

procedure TfrmSettingsBase.SetPage(Page: TPage);
begin
  if Page = FActivePage then
    Exit;
  if FActivePage <> nil then
    FActivePage.Panel.Visible := False;

  FActivePage := Page;
  FActivePage.Panel.Visible := True;
  FActivePage.Panel.BringToFront;

  FTreeView.FocusedNode := FActivePage.Node;
  FTreeView.Selected[FActivePage.Node] := True;
end;

procedure TfrmSettingsBase.SetPage(Panel: TPanel);
var
  Page: TPage;
begin
  for Page in FPageList do
    if Page.Panel = Panel then
    begin
      SetPage(Page);
      Break;
    end;
end;

{ TPageTree }

function TPageTree.AddChild(Parent: PVirtualNode; UserData: Pointer): PVirtualNode;
begin
  Result := inherited;

  Expanded[Result] := True;
  if Parent <> nil then
    Expanded[Parent] := True;
end;

constructor TPageTree.Create(AOwner: TComponent; Pages: TPageList);
begin
  inherited Create(AOwner);

  FPages := Pages;

  FColName := Header.Columns.Add;

  TreeOptions.SelectionOptions := [toDisableDrawSelection, toFullRowSelect];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop] + [toFullRowDrag];

  DefaultNodeHeight := TMStringFunctions.GetTextSize(MeasureTextHeightString, Font).cy + 12;
end;

function TPageTree.DoCollapsing(Node: PVirtualNode): Boolean;
begin
  Result := False;
end;

function TPageTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList;
var
  i: Integer;
begin
  Result := inherited;

  if Node <> nil then
    if ((Kind = ikNormal) or (Kind = ikSelected)) and (Column = 0) then
      for i := 0 to FPages.Count - 1 do
        if FPages[i].Node = Node then
          Index := FPages[i].ImageIndex;
end;

procedure TPageTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: string);
var
  i: Integer;
begin
  inherited;

  if Column = 0 then
    for i := 0 to FPages.Count - 1 do
      if FPages[i].Node = Node then
        Text := FPages[i].Caption;
end;

procedure TPageTree.Resize;
begin
  inherited;

  if Assigned(FColName) then
    FColName.Width := ClientWidth;
end;

procedure TPageTree.Setup;
var
  i: Integer;
begin
  for i := 0 to FPages.Count - 1 do
    if FPages[i].Parent = nil then
      FPages[i].Node := AddChild(nil)
    else if FPages[i].Parent <> nil then
      FPages[i].Node := AddChild(FPages[i].Parent.Node);
end;

end.
