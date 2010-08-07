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
unit About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, LanguageObjects, StdCtrls, AppData, ExtCtrls, ShellAPI, ComCtrls,
  Buttons;

type
  TfrmAbout = class(TForm)
    pagAbout: TPageControl;
    tabAbout: TTabSheet;
    lblAbout: TLabel;
    imgLogo: TImage;
    lblForumLink: TLabel;
    lblProjectLink: TLabel;
    lblHelpLink: TLabel;
    tabLicense: TTabSheet;
    txtAbout: TMemo;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnClose: TBitBtn;
    lblGPL: TLabel;
    lblVersion: TLabel;
    lblCopyright: TLabel;
    lblHomepage: TLabel;
    procedure lblProjectLinkClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lblGPLClick(Sender: TObject);
    procedure lblHelpLinkClick(Sender: TObject);
    procedure lblForumLinkClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure lblHomepageClick(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent; Caption: string); reintroduce;
  end;

implementation

{$R *.dfm}

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

constructor TfrmAbout.Create(AOwner: TComponent; Caption: string);
var
  Icon: TIcon;
begin
  inherited Create(AOwner);

  Language.Translate(Self);

  Self.Caption := Caption;
  lblAbout.Caption := AppGlobals.AppName;
  lblVersion.Caption := _('Version') + ' ' + AppGlobals.AppVersion.AsString;
  lblGPL.Caption := _('Distributed under the terms of the GNU General Public License');

  txtAbout.Text := Format(_('%s'#13#10 +
                            'Copyright (c) 2010 Alexander Nottelmann'#13#10#13#10 +
                            'This program is free software: you can redistribute it and/or modify'#13#10 +
                            'it under the terms of the GNU General Public License as published by'#13#10 +
                            'the Free Software Foundation, either version 3 of the License, or'#13#10 +
                            '(at your option) any later version.'#13#10#13#10 +
                            'This program is distributed in the hope that it will be useful,'#13#10 +
                            'but WITHOUT ANY WARRANTY; without even the implied warranty of'#13#10 +
                            'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the'#13#10 +
                            'GNU General Public License for more details.'#13#10#13#10 +
                            'You should have received a copy of the GNU General Public License'#13#10 +
                            'along with this program.  If not, see <http://www.gnu.org/licenses/>.'), [AppGlobals.AppName]);;

  lblHelpLink.Caption := _('Help');
  lblProjectLink.Caption := _('Information, changelog and updates');
  lblForumLink.Caption := _('Report problems or request new features');

  Icon := TIcon.Create;
  try
    Icon.LoadFromResourceName(HInstance, 'A');
    imgLogo.Picture.Assign(Icon);
    imgLogo.Left := tabAbout.ClientWidth - imgLogo.Width - lblVersion.Left;
  finally
    Icon.Free;
  end;

  pagAbout.ActivePageIndex := 0;
end;

procedure TfrmAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmAbout.lblHomepageClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://mistake.ws/', '', '', 1);
end;

procedure TfrmAbout.lblGPLClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://www.gnu.org/licenses/gpl-3.0.html', '', '', 1);
end;

procedure TfrmAbout.lblProjectLinkClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(AppGlobals.ProjectLink), '', '', 1);
end;

procedure TfrmAbout.lblHelpLinkClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(AppGlobals.ProjectHelpLink), '', '', 1);
end;

procedure TfrmAbout.lblForumLinkClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://mistake.ws/forum/', '', '', 1);
end;

end.
