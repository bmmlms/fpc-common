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
unit SettingsBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls, LanguageObjects,
  AppData, AppDataBase, SettingsStorage, Functions, ListActns, pngimage,
  PngImageList, ImgList, VirtualTrees;

type
  TPage = class
  private
    FCaption: string;
    FOriginalCaption: string;
    FPanel: TPanel;
    FNode: PVirtualNode;
    FButton: TSpeedButton;
    FResName: string;
  protected
  public
    constructor Create(OriginalCaption: string; Panel: TPanel; ResName: string);
    property Caption: string read FCaption;
    property Panel: TPanel read FPanel;
    property Node: PVirtualNode read FNode write FNode;
    property Button: TSpeedButton read FButton write FButton;
    property ResName: string read FResName;
  end;

  TPageList = class(TList)
  private
    function Get2(Index: Integer): TPage;
    procedure Put2(Index: Integer; Item: TPage);
  public
    property Items[Index: Integer]: TPage read Get2 write Put2; default;
    function Find(P: TPanel): TPage;
  end;

  TPageTree = class(TVirtualStringTree)
  private
    FColName: TVirtualTreeColumn;
    FPages: TPageList;
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: string); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean;
      var Index: Integer): TCustomImageList; override;
    procedure DoInitNode(Parent: PVirtualNode; Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;
    procedure Resize; override;
    procedure DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode;
      var NodeHeight: Integer); override;
  public
    constructor Create(AOwner: TComponent; Pages: TPageList);
  end;

  TfrmSettingsBase = class(TForm)
    pnlHeader: TPanel;
    Shape1: TShape;
    lblTop: TLabel;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    pnlLeft: TPanel;
    pnlGeneral: TPanel;
    lstLanguages: TComboBoxEx;
    lblLanguage: TLabel;
    chkAutoUpdateCheck: TCheckBox;
    lblPortable: TLabel;
    btnCopyProfile: TButton;
    Panel1: TPanel;
    txtPort: TLabeledEdit;
    txtHost: TLabeledEdit;
    chkProxy: TCheckBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure lstLanguagesChange(Sender: TObject);
    procedure btnCopyProfileClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure chkProxyClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSaveSettings: Boolean;
    FShowGeneral: Boolean;
    FTreeView: TPageTree;
    procedure NavButtonClick(Sender: TObject);
    procedure TreeViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  protected
    FPageList: TPageList;
    FActivePage: TPage;
    FUseTree: Boolean;
    FTreeImages: TPngImageList;
    procedure SetPage(Page: TPage); overload;
    procedure SetPage(Panel: TPanel); overload;
    //procedure EnablePanel(Panel: TPanel; Enable: Boolean);
    procedure RegisterPages; virtual;
    procedure Finish; virtual;
    function CanFinish: Boolean; virtual;
    procedure PreTranslate; virtual;
    procedure PostTranslate; virtual;
  public
    constructor Create(AOwner: TComponent; ShowGeneral: Boolean); reintroduce;

    property SaveSettings: Boolean read FSaveSettings;
  end;

implementation

{$R *.dfm}

{ TPage }

constructor TPage.Create(OriginalCaption: string; Panel: TPanel; ResName: string);
begin
  inherited Create;

  FOriginalCaption := OriginalCaption;
  FCaption := _(FOriginalCaption);
  FPanel := Panel;
  FResName := ResName;
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

function TPageList.Get2(Index: Integer): TPage;
begin
  Result := TPage(inherited Get(Index));
end;

procedure TPageList.Put2(Index: Integer; Item: TPage);
begin
  inherited Put(Index, Item);
end;

{ TfrmSettingsBase }

procedure TfrmSettingsBase.PreTranslate;
begin

end;

procedure TfrmSettingsBase.PostTranslate;
var
  i: Integer;
begin
  if ((AppGlobals.Portable = poNo) and (not AppGlobals.PortableAllowed)) or (AppGlobals.Portable = poUndefined) then
  begin
    lblPortable.Visible := False;
    btnCopyProfile.Visible := False;
    Exit;
  end;
  if AppGlobals.Portable = poYes then
    lblPortable.Caption := _('This application is using portable settings. ' + 'To copy these settings to the registry/application data folder, press ''Copy profile''.')
  else if AppGlobals.Portable = poNo then
    lblPortable.Caption := _('This application is using settings from the registry/application data folder. ' + 'To copy these settings to a portable profile, press ''Copy profile''.');

  for i := 0 to FPageList.Count - 1 do
  begin
    FPageList[i].FCaption := _(FPageList[i].FOriginalCaption);
    if FUseTree then
    begin
      FTreeView.Invalidate;
    end else
      FPageList[i].Button.Caption := FPageList[i].FCaption;
  end;
end;

procedure TfrmSettingsBase.btnCopyProfileClick(Sender: TObject);
var
  S: TSettingsInstalled;
  S2: TSettingsPortable;
begin
  if ((AppGlobals.Portable = poYes) and (TSettingsInstalled.Active(AppGlobals.AppName)) or
      (AppGlobals.Portable = poNo) and (TSettingsPortable.Active(AppGlobals.AppName))) then
    if MsgBox(Handle, _('All settings of the other profile will be replaced by the current profile''s settings.'#13#10 +
                        'Please be aware that existing settings and files containing data will be overwritten.'#13#10 +
                        'Proceed?'), 'Question', MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON2) = IDNO then
    begin
      Exit;
    end;

  try
    S := TSettingsInstalled.Create(AppGlobals.AppName, AppGlobals.AppPath);
    S2 := TSettingsPortable.Create(AppGlobals.AppName, AppGlobals.AppPath);
    try
      if AppGlobals.Portable = poYes then
        S.Assign(S2)
      else
        S2.Assign(S);
    finally
      S.Free;
      S2.Free;
    end;

    MsgBox(Handle, _('The profile was copied successfully. To use the copied profile, restart the application.'), _('Info'), MB_ICONINFORMATION);
  except
    MsgBox(Handle, _('An error occured while copying the settings.'#13#10 +
                     'It is possible that some settings were copied before the error occured.'), _('Error'), MB_ICONERROR);
  end;
end;

procedure TfrmSettingsBase.btnOKClick(Sender: TObject);
begin
  if CanFinish then
    Finish;
end;

procedure TfrmSettingsBase.Button1Click(Sender: TObject);
begin
  if MsgBox(Handle, _('All data saved in the currently used profile will be deleted.'#13#10 +
                      'Are you sure you want to delete this profile?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON2) = IDYES then
  begin
    AppGlobals.Storage.DeleteProfile;
    AppGlobals.SkipSave := True;
    MsgBox(Handle, _('The profile was deleted.'#13#10 +
                     'When you exit the application, no data will be saved so that the profil will not be recreated.'), _('Info'), MB_ICONINFORMATION);
  end;
end;

function TfrmSettingsBase.CanFinish: Boolean;
begin
  if FShowGeneral then
  begin
    Result := False;
    if chkProxy.Checked then
    begin
      if (Trim(txtHost.Text) = '') or (Trim(txtPort.Text) = '') or (StrToIntDef(txtPort.Text, 0) <= 0) then
      begin
        MsgBox(Handle, _('You need to supply a host and a port (must be a positive number) to connect to if the use of a HTTP proxy is enabled.'), _('Info'), MB_ICONINFORMATION);
        SetPage(FPageList.Find(TPanel(txtHost.Parent)));
        if Trim(txtHost.Text) = '' then
          txtHost.SetFocus
        else
          txtPort.SetFocus;
        Exit;
      end;
    end;
  end;
  Result := True;
end;

procedure TfrmSettingsBase.chkProxyClick(Sender: TObject);
begin
  txtHost.Enabled := chkProxy.Checked;
  txtPort.Enabled := chkProxy.Checked;
end;

constructor TfrmSettingsBase.Create(AOwner: TComponent; ShowGeneral: Boolean);
var
  i: Integer;
  Btn: TSpeedButton;
  HIco: THandle;
  Ico: TIcon;
  Item: TTreeNode;
  Res: TResourceStream;
  Png: TPngImage;
begin
  inherited Create(AOwner);

  FShowGeneral := ShowGeneral;
  FActivePage := nil;

  FTreeImages := TPngImageList.Create(Self);

  FPageList := TPageList.Create;

  RegisterPages;

  if FUseTree then
  begin
    pnlLeft.Width := 160;

    FTreeView := TPageTree.Create(pnlLeft, FPageList);
    FTreeView.Parent := pnlLeft;
    FTreeView.Images := FTreeImages;
    FTreeView.Align := alClient;
    FTreeView.OnChange := TreeViewChange;
    FTreeView.Show;
  end;

  for i := 0 to FPageList.Count - 1 do
  begin
    if FUseTree then
    begin
      Res := TResourceStream.Create(HInstance, FPageList[i].ResName, MakeIntResource(RT_RCDATA));
      Png := TPngImage.Create;
      try
        Png.LoadFromStream(Res);
        FTreeImages.AddPng(Png);
      finally
        Png.Free;
        Res.Free;
      end;
    end else
    begin
      Btn := TSpeedButton.Create(pnlLeft);
      Btn.Parent := pnlLeft;
      Btn.OnClick := NavButtonClick;
      Btn.Caption := FPageList[i].Caption;
      Btn.Flat := True;
      Btn.GroupIndex := 1;
      Btn.Height := 50;
      Btn.Align := alTop;

      HIco := LoadImage(HInstance, PChar(FPageList[i].ResName), IMAGE_ICON,
        16, 16, LR_DEFAULTCOLOR);
      if HIco > 0 then
      begin
        Ico := TIcon.Create;
        try
          Ico.Handle := HIco;
          Btn.Glyph.Assign(Ico);
          Btn.Glyph.TransparentMode := tmAuto;
          Btn.Glyph.Transparent := True;
          Btn.Glyph.TransparentColor := Btn.Glyph.Canvas.Pixels[0, 0];
          Btn.Layout := Buttons.blGlyphTop;
          DestroyIcon(HIco);
        finally
          Ico.Free;
        end;
      end;

      Btn.Show;
      FPageList[i].Button := Btn;
    end;
  end;

  Language.Translate(Self, PreTranslate, PostTranslate);
end;

{
procedure TfrmSettingsBase.EnablePanel(Panel: TPanel; Enable: Boolean);
var
  i: Integer;
begin
  for i := 0 to FPageList.Count - 1 do
    if FPageList[i].FPanel = Panel then
    begin
      FPageList[i].FNode.Enabled := Enable;
      Break;
    end;
end;
}

procedure TfrmSettingsBase.Finish;
begin
  if FShowGeneral then
  begin
    AppGlobals.AutoUpdate := chkAutoUpdateCheck.Checked;

    AppGlobals.ProxyEnabled := chkProxy.Checked;
    AppGlobals.ProxyHost := txtHost.Text;
    AppGlobals.ProxyPort := StrToIntDef(txtPort.Text, 8080);

    if lstLanguages.ItemIndex > -1 then
    begin
      AppGlobals.Language := Language.CurrentLanguage.ID;
    end;
  end;

  FSaveSettings := True;

  Close;
end;

procedure TfrmSettingsBase.FormCreate(Sender: TObject);
var
  i, n: Integer;
  ComboItem: TComboExItem;
begin
  chkAutoUpdateCheck.Checked := AppGlobals.AutoUpdate;

  chkProxy.Checked := AppGlobals.ProxyEnabled;
  txtHost.Enabled := chkProxy.Checked;
  txtPort.Enabled := chkProxy.Checked;
  txtHost.Text := AppGlobals.ProxyHost;
  txtPort.Text := IntToStr(AppGlobals.ProxyPort);

  lstLanguages.Clear;
  lstLanguages.Images := AppGlobals.LanguageIcons.List;
  for i := 0 to LanguageList.Count - 1 do
    if LanguageList[i].Available then
    begin
      ComboItem := lstLanguages.ItemsEx.Add;
      ComboItem.Caption := LanguageList[i].Name;
      ComboItem.Data := LanguageList[i];
      ComboItem.ImageIndex := AppGlobals.LanguageIcons.GetIconIndex(LanguageList[i].ID);
    end;
  lstLanguages.ItemsEx.SortType := stText;
  lstLanguages.ItemsEx.Sort;
  for i := 0 to lstLanguages.ItemsEx.Count - 1 do
    if Language.CurrentLanguage.ID = TLanguage(lstLanguages.ItemsEx[i].Data).ID then
    begin
      lstLanguages.ItemIndex := i;
      Break;
    end;
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

procedure TfrmSettingsBase.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
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
  i, n: Integer;
begin
  for n := 0 to ControlCount - 1 do
    for i := 0 to FPageList.Count - 1 do
      if Controls[n] = FPageList[i].Panel then
      begin
        TPanel(Controls[n]).Enabled := False;
        TPanel(Controls[n]).Align := alClient;
        TPanel(Controls[n]).BevelOuter := bvNone;
        // Vermeidet Flackern nach anklicken im Tree/aktivieren.
        // Das Align := alClient wird da wohl erst aktiv.
        TPanel(Controls[n]).SendToBack;
        Break;
      end;

  SetPage(FPageList[0]);
  if FUseTree then
    FTreeView.SetFocus;
end;

procedure TfrmSettingsBase.lstLanguagesChange(Sender: TObject);
begin
  if lstLanguages.ItemIndex > -1 then
  begin
    Language.CurrentLanguage := TLanguage(lstLanguages.ItemsEx[lstLanguages.ItemIndex].Data);
    Language.Translate(Self, PreTranslate, PostTranslate);
  end;
end;

procedure TfrmSettingsBase.TreeViewChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
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

procedure TfrmSettingsBase.NavButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FPageList.Count - 1 do
    if FPageList[i].Button = Sender then
    begin
      SetPage(FPageList[i]);
      Break;
    end;
end;

procedure TfrmSettingsBase.RegisterPages;
begin
  if FShowGeneral then
    if FUseTree then
      FPageList.Add(TPage.Create('General', pnlGeneral, 'SETTINGS'))
    else
      FPageList.Add(TPage.Create('&General', pnlGeneral, 'SETTINGS'));
end;

procedure TfrmSettingsBase.SetPage(Page: TPage);
begin
  if Page = FActivePage then
    Exit;
  if FActivePage <> nil then
    FActivePage.Panel.Enabled := False;

  FActivePage := Page;
  FActivePage.Panel.Enabled := True;
  FActivePage.Panel.Visible := True;
  FActivePage.Panel.BringToFront;

  if FUseTree then
  begin
    FTreeView.FocusedNode := FActivePage.Node;
    FTreeView.Selected[FActivePage.Node] := True;
  end else
  begin
    FActivePage.Button.Down := True;
  end;
end;

procedure TfrmSettingsBase.SetPage(Panel: TPanel);
var
  Page: TPage;
  i: Integer;
begin
  Page := nil;
  for i := 0 to FPageList.Count - 1 do
    if FPageList[i].Panel = Panel then
    begin
      Page := FPageList[i];
      Break;
    end;
  if Page = nil then
    Exit;

  if FActivePage <> nil then
    FActivePage.Panel.Enabled := False;

  FActivePage := Page;
  FActivePage.Button.Down := True;
  FActivePage.Panel.Enabled := True;
  FActivePage.Panel.BringToFront;
end;

{ TPageTree }

constructor TPageTree.Create(AOwner: TComponent; Pages: TPageList);
var
  i: Integer;
begin
  inherited Create(AOwner);

  FPages := Pages;

  for i := 0 to Pages.Count - 1 do
    Pages[i].Node := AddChild(nil);

  FColName := Header.Columns.Add;

  TreeOptions.SelectionOptions := [toDisableDrawSelection, toFullRowSelect];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toAcceptOLEDrop] + [toFullRowDrag];
end;

function TPageTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
begin
  Result := inherited;

  if Node <> nil then
    if ((Kind = ikNormal) or (Kind = ikSelected)) and (Column = 0) then
      Index := Node.Index;
end;

procedure TPageTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: string);
begin
  inherited;

  if Column = 0 then
    Text := FPages[Node.Index].FCaption;
end;

procedure TPageTree.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
begin
  inherited;

end;

procedure TPageTree.DoMeasureItem(TargetCanvas: TCanvas;
  Node: PVirtualNode; var NodeHeight: Integer);
begin
  inherited;

  NodeHeight := 27;
end;

procedure TPageTree.Resize;
begin
  inherited;

  FColName.Width := ClientWidth;
end;

end.
