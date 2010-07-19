{
    ------------------------------------------------------------------------
    streamWriter
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
unit Debug;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DebugView, ExtCtrls, LanguageObjects;

type
  TfrmDebug = class(TForm)
    btnClear: TButton;
    pnlDebug: TPanel;
    btnCopy: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    lstDebug: TDebugView;
  public
    constructor Create(AOwner: TComponent; Name: string); reintroduce;
    procedure Add(Time: TDateTime; Text, Data: string);
  end;

implementation

{$R *.dfm}

procedure TfrmDebug.Add(Time: TDateTime; Text, Data: string);
begin
  lstDebug.AddData(Time, Text, Data);
end;

procedure TfrmDebug.btnClearClick(Sender: TObject);
begin
  lstDebug.Clear;
end;

procedure TfrmDebug.btnCopyClick(Sender: TObject);
begin
  lstDebug.Copy;
end;

constructor TfrmDebug.Create(AOwner: TComponent; Name: string);
begin
  inherited Create(AOwner);
  if Name <> '' then
    Caption := Caption + ' - ' + Name;
end;

procedure TfrmDebug.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmDebug.FormCreate(Sender: TObject);
begin
  Language.Translate(Self);

  lstDebug := TDebugView.Create(pnlDebug);
  lstDebug.Parent := pnlDebug;
  lstDebug.Align := alClient;
  lstDebug.Show;
end;

procedure TfrmDebug.FormDestroy(Sender: TObject);
begin
  FreeAndNil(lstDebug);
end;

procedure TfrmDebug.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

end.
