{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2011 Alexander Nottelmann

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
  Dialogs, StdCtrls, ExtCtrls, AppData, LanguageObjects;

type
  TMsgButtons = (btOK, btOKCancel);
  TMsgRetTypes = (mtOK, mtCancel, mtDontShow);

  TfrmMsgDlg = class(TForm)
    chkNotShowAgain: TCheckBox;
    Bevel1: TBevel;
    cmdOK: TButton;
    cmdCancel: TButton;
    txtText: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure cmdCancelClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
  private
    FID: Integer;
    FRes: TMsgRetTypes;
    function ShowMsgInternal(Text: string; ID: Integer; Buttons: TMsgButtons): TMsgRetTypes; overload;
    function ShowMsgInternal(Text: string; Buttons: TMsgButtons): TMsgRetTypes; overload;
  public
    class function ShowMsg(Owner: TCustomForm; Text: string; ID: Integer; Buttons: TMsgButtons): TMsgRetTypes; overload;
    class function ShowMsg(Owner: TCustomForm; Text: string; Buttons: TMsgButtons): TMsgRetTypes; overload;
  end;

implementation

uses
  Functions;

{$R *.dfm}

procedure TfrmMsgDlg.cmdCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMsgDlg.cmdOKClick(Sender: TObject);
begin
  FRes := mtOK;
  Close;
end;

procedure TfrmMsgDlg.FormActivate(Sender: TObject);
begin
  if not chkNotShowAgain.Visible then
    txtText.Height := Bevel1.Top - 4 - txtText.Top
  else
    txtText.Height := chkNotShowAgain.Top - 4 - txtText.Top;
end;

procedure TfrmMsgDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FID > -1 then
    if chkNotShowAgain.Checked and (FRes = mtOK) then
      AppGlobals.InfoShown[FID] := True;
end;

procedure TfrmMsgDlg.FormCreate(Sender: TObject);
begin
  Language.Translate(Self);
  FRes := mtCancel;
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

class function TfrmMsgDlg.ShowMsg(Owner: TCustomForm; Text: string; ID: Integer; Buttons: TMsgButtons): TMsgRetTypes;
var
  M: TfrmMsgDlg;
begin
  if ID > -1 then
    if AppGlobals.InfoShown[ID] then
    begin
      Result := mtDontShow;
      Exit;
    end;

  M := TfrmMsgDlg.Create(Owner);
  try
    if (Owner = nil) or (not Owner.Visible) or (IsIconic(Owner.Handle)) then
      M.Position := poScreenCenter;
    M.ShowMsgInternal(Text, ID, Buttons);
    Result := M.FRes;
  finally
    M.Free;
  end;
end;

class function TfrmMsgDlg.ShowMsg(Owner: TCustomForm; Text: string; Buttons: TMsgButtons): TMsgRetTypes;
begin
  Result := ShowMsg(Owner, Text, -1, Buttons);
end;

function TfrmMsgDlg.ShowMsgInternal(Text: string; ID: Integer; Buttons: TMsgButtons): TMsgRetTypes;
begin
  FID := ID;

  cmdCancel.Visible := Buttons = btOKCancel;
  chkNotShowAgain.Visible := ID > -1;

  txtText.Text := Text;
  Self.ShowModal;

  Result := Self.FRes;
end;

function TfrmMsgDlg.ShowMsgInternal(Text: string; Buttons: TMsgButtons): TMsgRetTypes;
begin
  Result := ShowMsgInternal(Text, -1, Buttons);
end;

end.
