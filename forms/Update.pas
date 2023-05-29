{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2023 Alexander Nottelmann

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

unit Update;

interface

uses
  AppData,
  ChangeLog,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  LanguageObjects,
  StdCtrls,
  SysUtils,
  UpdateClient,
  Variants,
  Windows;

type

  { TfrmUpdate }

  TfrmUpdate = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    lblNewestVersion: TLabel;
    lblState: TLabel;
    lblVersion: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    lblChangeLog: TLabel;
    pnlNav: TPanel;
    Bevel2: TBevel;
    cmdOK: TButton;
    cmdCancel: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cmdCancelClick(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lblChangeLogClick(Sender: TObject);
  private
    Updater: TUpdateClient;
    procedure UpdaterUpdateFound(Sender: TObject);
    procedure UpdaterNoUpdateFound(Sender: TObject);
    procedure UpdaterDownloadProgress(Sender: TObject);
    procedure UpdaterUpdateDownloaded(Sender: TObject);
    procedure UpdaterError(Sender: TObject);
  public
    Exit: Boolean;
    Updated: Boolean;
    constructor Create(AOwner: TComponent; Version: string = ''; UpdateURL: string = ''); reintroduce;
  end;

implementation

{$R *.lfm}

procedure TfrmUpdate.cmdCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmUpdate.cmdOKClick(Sender: TObject);
begin
  case cmdOK.Tag of
    0:
    begin
      lblState.Caption := _('Searching for new version...');
      Updater.Start(uaVersion, True);
    end;
    1:
      if AppGlobals.RunningFromInstalledLocation then
      begin
        lblState.Caption := _('Downloading update...');
        ProgressBar1.Tag := -1;
        ProgressBar1.Position := 0;
        Updater.Start(uaUpdate, True);
      end else
      begin
        TFunctions.ShellExecute(0, 'open', AppGlobals.ProjectLink);
        Close;
      end;
  end;
  cmdOK.Enabled := False;
end;

constructor TfrmUpdate.Create(AOwner: TComponent; Version: string; UpdateURL: string);
begin
  inherited Create(AOwner);

  Exit := False;
  Updated := False;
  lblVersion.Caption := AppGlobals.AppVersion.AsString;
  lblNewestVersion.Caption := '';
  lblChangeLog.Visible := False;

  Updater := TUpdateClient.Create;
  Updater.UpdateURL := UpdateURL;
  Updater.Language := Language.CurrentLanguage.ID;
  Updater.OnUpdateFound := UpdaterUpdateFound;
  Updater.OnNoUpdateFound := UpdaterNoUpdateFound;
  Updater.OnDownloadProgress := UpdaterDownloadProgress;
  Updater.OnUpdateDownloaded := UpdaterUpdateDownloaded;
  Updater.OnError := UpdaterError;
  if Version <> '' then
  begin
    Updater.FoundVersion := TFunctions.ParseVersion(Version);
    UpdaterUpdateFound(nil);
    cmdOK.Click;
  end else
    Updater.Start(uaVersion, True);

  Constraints.MinWidth := Scale96ToFont(Constraints.MinWidth);
end;

procedure TfrmUpdate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Updater.Free;
  Action := caHide;
end;

procedure TfrmUpdate.FormCreate(Sender: TObject);
begin
  Language.Translate(Self);
  lblChangeLog.Left := ClientWidth div 2 - lblChangeLog.Width div 2;

  ProgressBar1.Visible := AppGlobals.RunningFromInstalledLocation;
end;

procedure TfrmUpdate.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmUpdate.lblChangeLogClick(Sender: TObject);
var
  F: TfrmChangeLog;
begin
  F := TfrmChangeLog.Create(Self, Updater.ChangeLog);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TfrmUpdate.UpdaterDownloadProgress(Sender: TObject);
begin
  if Updater.Action = uaUpdate then
  begin
    if ProgressBar1.Tag = -1 then
    begin
      if Updater.UpdateLength = -1 then
        ProgressBar1.Style := pbstMarquee
      else
        ProgressBar1.Style := pbstNormal;
      ProgressBar1.Tag := 0;
    end;

    if ProgressBar1.Style = pbstNormal then
    begin
      if Updater.Percent < 100 then
        ProgressBar1.Position := Updater.Percent + 1;
      ProgressBar1.Position := Updater.Percent;
    end;
  end;
end;

procedure TfrmUpdate.UpdaterError(Sender: TObject);
begin
  lblState.Caption := _('Error');
  //ProgressBar1.State := pbsError;
  case Updater.Action of
    uaVersion:
    begin
      cmdOK.Caption := _('&Retry');
      cmdOK.Tag := 0;
    end;
    uaUpdate:
    begin
      cmdOK.Caption := _('&Retry');
      cmdOK.Tag := 1;
    end;
  end;
  cmdOK.Enabled := True;
end;

procedure TfrmUpdate.UpdaterNoUpdateFound(Sender: TObject);
begin
  lblState.Caption := _('No newer version was found');
  lblNewestVersion.Caption := Updater.FoundVersion.AsString;
end;

procedure TfrmUpdate.UpdaterUpdateDownloaded(Sender: TObject);
var
  Res: Integer;
begin
  lblState.Caption := _('Download complete');
  Res := TFunctions.MsgBox(_('The update was downloaded successfully. Do you want to exit the application and install the update now?'#13#10 + 'If you select "No", the update will be installed on the next start of the application.'),
    _('Question'), MB_ICONQUESTION or MB_YESNO);
  if Res = IDYES then
  begin
    Exit := True;
    Updated := True;
  end else
    Updated := True;
  Close;
end;

procedure TfrmUpdate.UpdaterUpdateFound(Sender: TObject);
begin
  lblState.Caption := _('New version found');
  lblNewestVersion.Caption := Updater.FoundVersion.AsString;
  cmdOK.Caption := _('&Download');
  cmdOK.Enabled := True;
  cmdOK.Tag := 1;
  cmdOK.Default := True;
  lblChangeLog.Visible := Updater.ChangeLog <> '';
end;

end.
