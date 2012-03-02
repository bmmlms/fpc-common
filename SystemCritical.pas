{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2012 Alexander Nottelmann

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
unit SystemCritical;

interface

uses
  Windows, Messages;

type
  TSetThreadExecutionState = function(ESFlags: DWORD): DWORD; stdcall;

  TSystemCritical = class
  private
    FCritical: Boolean;

    FFunc: TSetThreadExecutionState;

    procedure FSetCritical(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    property Critical: Boolean read FCritical write FSetCritical;
  end;

const
  ES_SYSTEM_REQUIRED = $00000001;
  ES_DISPLAY_REQUIRED = $00000002;
  ES_USER_PRESENT = $00000004;
  ES_AWAYMODE_REQUIRED = $00000040;
  ES_CONTINUOUS = $80000000;

var
  Critical: TSystemCritical;

implementation

{ TSystemCritical }

constructor TSystemCritical.Create;
var
  H: THandle;
begin
  inherited;

  FFunc := nil;
  H := GetModuleHandle('kernel32.dll');
  if H <> 0 then
  begin
    @FFunc := GetProcAddress(H, 'SetThreadExecutionState');
  end;
end;

destructor TSystemCritical.Destroy;
begin
  Critical := False;

  inherited;
end;

procedure TSystemCritical.FSetCritical(Value: Boolean);
begin
  Exit; // TODO: Wieder rausmachen!
  if Assigned(@FFunc) then
  begin
    if Value then
      FFunc(ES_SYSTEM_REQUIRED or ES_CONTINUOUS)
    else
      FFunc(ES_CONTINUOUS);
  end;
end;

initialization
begin
  Critical := TSystemCritical.Create;
end;

finalization
begin
  Critical.Free;
  Critical := nil;
end;

end.
