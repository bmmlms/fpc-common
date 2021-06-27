{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2021 Alexander Nottelmann

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

unit MsgDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, AppData, LanguageObjects, ShellAPI, MControls;

const
  mrDontShow = 100;

type

  TfrmMsgDlg = class(TForm)
    chkNotShowAgain: TCheckBox;
    txtText: TMemo;
    pnlNav: TPanel;
    Bevel2: TBevel;
    cmdNoCancel: TButton;
    cmdYesOK: TButton;
    imgIcon: TImage;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cmdYesOKClick(Sender: TObject);
    procedure cmdNoCancelClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FID: Integer;
    FButtons: TMsgDlgButtons;
    FButtonToFocus: TButton;
    FResult: TModalResult;
    function ShowMsgInternal: TModalResult;
  public
    constructor Create(AOwner: TComponent; Text: string; MsgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; ID: Integer); reintroduce;

    class function ShowMsg(Owner: TCustomForm; Text: string; MsgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; ID: Integer = -1): TModalResult;
  end;

implementation

uses
  Functions;

{$R *.lfm}

procedure TfrmMsgDlg.cmdNoCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMsgDlg.cmdYesOKClick(Sender: TObject);
begin
  if (mbYes in FButtons) then
    FResult := mrYes
  else
    FResult := mrOk;

  Close;
end;

procedure TfrmMsgDlg.FormActivate(Sender: TObject);
begin
  if not chkNotShowAgain.Visible then
    txtText.Height := pnlNav.Top - 4 - txtText.Top
  else
    txtText.Height := chkNotShowAgain.Top - 4 - txtText.Top;
end;

procedure TfrmMsgDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FID > -1 then
    if chkNotShowAgain.Checked and ((FResult = mrYes) or (FResult = mrOk)) then
      AppGlobals.InfoShown[FID] := True;
end;

procedure TfrmMsgDlg.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Self.Close;
  end;
end;

procedure TfrmMsgDlg.FormShow(Sender: TObject);
begin
  if FButtonToFocus <> nil then
    FButtonToFocus.ApplyFocus;
end;

class function TfrmMsgDlg.ShowMsg(Owner: TCustomForm; Text: string; MsgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; ID: Integer): TModalResult;
var
  M: TfrmMsgDlg;
  CheckButtons: TMsgDlgButtons;
begin
  CheckButtons := Buttons + [DefButton];
  if (mbAbort in CheckButtons) or (mbRetry in CheckButtons) or (mbIgnore in CheckButtons) or
     (mbAll in CheckButtons) or (mbNoToAll in CheckButtons) or (mbYesToAll in CheckButtons) or
     (mbClose in CheckButtons)
  then
    raise Exception.Create('Unsupported button supplied');

  if ((mbYes in Buttons) and (mbCancel in Buttons)) or
     ((mbOK in Buttons) and (mbNo in Buttons))
  then
    raise Exception.Create('Unsupported button set supplied');

  if not (DefButton in Buttons) then
    raise Exception.Create('DefButton not in button set');

  if ID > -1 then
    if AppGlobals.InfoShown[ID] then
    begin
      Result := mrDontShow;
      Exit;
    end;

  M := TfrmMsgDlg.Create(Owner, Text, MsgType, Buttons, DefButton, ID);
  try
    if (Owner = nil) or (not Owner.Visible) or (IsIconic(Owner.Handle)) then
      M.Position := poScreenCenter;
    Result := M.ShowMsgInternal;
  finally
    M.Free;
  end;
end;

function TfrmMsgDlg.ShowMsgInternal: TModalResult;
begin
  Self.ShowModal;
  Result := FResult;
end;

constructor TfrmMsgDlg.Create(AOwner: TComponent; Text: string; MsgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefButton: TMsgDlgBtn; ID: Integer);
var
  Icon: TIcon;
begin
  inherited Create(AOwner);

  FID := ID;
  FButtons := Buttons;

  cmdNoCancel.Visible := (mbNo in Buttons) or (mbCancel in Buttons);
  if mbNo in Buttons then
    cmdNoCancel.Caption := '&No';
  if mbYes in Buttons then
    cmdYesOK.Caption := '&Yes';

  FButtonToFocus := nil;
  if (DefButton = mbOK) or (DefButton = mbYes) then
  begin
    cmdYesOK.Default := True;
    FButtonToFocus := cmdYesOK;
  end;
  if (DefButton = mbNo) or (DefButton = mbCancel) then
  begin
    cmdNoCancel.Default := True;
    FButtonToFocus := cmdNoCancel;
  end;

  FResult := mrCancel;
  if (mbNo in FButtons) then
    FResult := mrNo;

  Icon := TIcon.Create;
  try
    case MsgType of
      mtWarning:
        begin
          Caption := 'Warning';
          Icon.Handle := LoadIcon(Icon.Handle, PChar(IDI_EXCLAMATION));
        end;
      mtError:
        begin
          Caption := 'Error';
          Icon.Handle := LoadIcon(Icon.Handle, PChar(IDI_HAND));
        end;
      mtInformation:
        begin
          Caption := 'Information';
          Icon.Handle := LoadIcon(Icon.Handle, PChar(IDI_ASTERISK));
        end;
      mtConfirmation:
        begin
          Caption := 'Question';
          Icon.Handle := LoadIcon(Icon.Handle, PChar(IDI_QUESTION));
        end;
      mtCustom:
        raise Exception.Create('mtCustom not supported');
    end;
    imgIcon.Picture.Icon := Icon;
  finally
    Icon.Free;
  end;

  chkNotShowAgain.Visible := ID > -1;

  Language.Translate(Self);

  txtText.Text := Text;
end;

end.
