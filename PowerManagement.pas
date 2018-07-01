{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2018 Alexander Nottelmann

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

unit PowerManagement;

interface

uses
  Windows, Messages, SysUtils, Classes, Generics.Collections, DateUtils;

type
  TSetThreadExecutionState = function(ESFlags: DWORD): DWORD; stdcall;

  TWakeupEvent = class
  private
    FHandle: Cardinal;
  public
    constructor Create(Time: TDateTime);
    destructor Destroy; override;
  end;

  TPowerManagement = class
  private
    FCritical: Boolean;
    FWakeupList: TList<TWakeupEvent>;

    FFunc: TSetThreadExecutionState;

    procedure FSetCritical(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    function AddWakeup(Time: TDateTime): Cardinal;
    procedure RemoveWakeup(Handle: Cardinal);

    property Critical: Boolean read FCritical write FSetCritical;
  end;

const
  ES_SYSTEM_REQUIRED = $00000001;
  ES_DISPLAY_REQUIRED = $00000002;
  ES_USER_PRESENT = $00000004;
  ES_AWAYMODE_REQUIRED = $00000040;
  ES_CONTINUOUS = $80000000;

var
  Power: TPowerManagement;

implementation

{ TPowerManagement }

function TPowerManagement.AddWakeup(Time: TDateTime): Cardinal;
var
  WE: TWakeupEvent;
begin
  WE := TWakeupEvent.Create(Time);
  FWakeupList.Add(WE);
  Result := WE.FHandle;
end;

constructor TPowerManagement.Create;
var
  H: THandle;
begin
  inherited;

  FWakeupList := TList<TWakeupEvent>.Create;

  FFunc := nil;
  H := GetModuleHandle('kernel32.dll');
  if H <> 0 then
  begin
    @FFunc := GetProcAddress(H, 'SetThreadExecutionState');
  end;
end;

destructor TPowerManagement.Destroy;
var
  i: Integer;
begin
  FCritical := False;

  for i := 0 to FWakeupList.Count - 1 do
    FWakeupList[i].Free;
  FWakeupList.Free;

  inherited;
end;

procedure TPowerManagement.FSetCritical(Value: Boolean);
begin
  FCritical := Value;
  if Assigned(@FFunc) then
  begin
    if Value then
      FFunc(ES_SYSTEM_REQUIRED or ES_CONTINUOUS)
    else
      FFunc(ES_CONTINUOUS);
  end;
end;

procedure TPowerManagement.RemoveWakeup(Handle: Cardinal);
var
  i: Integer;
begin
  for i := 0 to FWakeupList.Count - 1 do
    if FWakeupList[i].FHandle = Handle then
    begin
      FWakeupList[i].Free;
      FWakeupList.Delete(i);
      Exit;
    end;
end;

{ TWakeupEvent }

constructor TWakeupEvent.Create(Time: TDateTime);
var
  ST: TSystemTime;
  FT: TFileTime;
begin
  inherited Create;

  FHandle := CreateWaitableTimer(nil, True, nil);
  if FHandle = INVALID_HANDLE_VALUE then
    raise Exception.Create('Error CreateWaitableTimer()');

  DateTimeToSystemTime(Time, ST);
  if not SystemTimeToFileTime(ST, FT) then
  begin
    CloseHandle(FHandle);
    FHandle := 0;
    raise Exception.Create('Error SystemTimeToFileTime()');
  end;

  if not LocalFileTimeToFileTime(FT, FT) then
  begin
    CloseHandle(FHandle);
    FHandle := 0;
    raise Exception.Create('Error LocalFileTimeToFileTime()');
  end;

  if not SetWaitableTimer(FHandle, Int64(FT), 0, nil, nil, True) then
  begin
    CloseHandle(FHandle);
    FHandle := 0;
    raise Exception.Create('Error SystemTimeToFileTime()');
  end;
end;

destructor TWakeupEvent.Destroy;
begin
  CloseHandle(FHandle);

  inherited;
end;

initialization
begin
  Power := TPowerManagement.Create;
end;

finalization
begin
  Power.Free;
  Power := nil;
end;

end.
