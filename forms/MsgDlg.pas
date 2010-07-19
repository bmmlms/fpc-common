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
unit MsgDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, AppData, LanguageObjects;

type
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
  private
    FID: Integer;
    FRes: TMsgRetTypes;
    function ShowMsgInternal(Text: string; Buttons, ID: Integer): TMsgRetTypes;
  public
    class function ShowMsg(Owner: TForm; Text: string; Buttons, ID: Integer):
      TMsgRetTypes;
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

procedure TfrmMsgDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if chkNotShowAgain.Checked then
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

class function TfrmMsgDlg.ShowMsg(Owner: TForm; Text: string; Buttons,
  ID: Integer): TMsgRetTypes;
var
  M: TfrmMsgDlg;
begin
  if AppGlobals.InfoShown[ID] then
  begin
    Result := mtDontShow;
    Exit;
  end;

  M := TfrmMsgDlg.Create(Owner);
  try
    if (Owner = nil) or (not Owner.Visible) or (IsIconic(Owner.Handle)) then
      M.Position := poScreenCenter;
    M.ShowMsgInternal(Text, Buttons, ID);
    Result := M.FRes;
  finally
    M.Free;
  end;
end;

function TfrmMsgDlg.ShowMsgInternal(Text: string; Buttons, ID: Integer): TMsgRetTypes;
begin
  FID := ID;

  if Buttons = 2 then
    Self.cmdCancel.Visible := True
  else
    Self.cmdCancel.Visible := False;

  txtText.Text := Text;
  Self.ShowModal;

  Result := Self.FRes;
end;

end.
