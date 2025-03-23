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

unit Changelog;

interface

uses
  Buttons,
  Classes,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  LanguageObjects,
  StdCtrls,
  SysUtils,
  Variants,
  Windows;

type
  TfrmChangeLog = class(TForm)
    txtChangeLog: TMemo;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnClose: TBitBtn;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent; ChangeLog: string); reintroduce;
  end;

implementation

{$R *.lfm}

procedure TfrmChangeLog.btnCloseClick(Sender: TObject);
begin
  Close;
end;

constructor TfrmChangeLog.Create(AOwner: TComponent; ChangeLog: string);
begin
  inherited Create(AOwner);
  txtChangeLog.Text := ChangeLog;
end;

procedure TfrmChangeLog.FormCreate(Sender: TObject);
begin
  Language.Translate(Self);
end;

procedure TfrmChangeLog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmChangeLog.FormShow(Sender: TObject);
begin
  SendMessage(txtChangeLog.Handle, WM_VSCROLL, SB_TOP, 0);
end;

end.
