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

unit Logging;

interface

uses
  SysUtils,
  Windows;

type

  { TLogger }

  TLogger = class
  const
    UTF8_BOM: array[0..2] of Byte = ($EF, $BB, $BF);
  class var
    FFilename: string;
    FHandle: THandle;
  public
    class procedure SetFilename(const Value: string);
    class procedure Write(Data: string);
  end;

implementation

{ TLogger }

class procedure TLogger.SetFilename(const Value: string);
var
  W: Cardinal;
begin
  if FHandle <> 0 then
    CloseHandle(FHandle);

  if string.IsNullOrWhiteSpace(Value) then
    Exit;

  FFilename := Value;

  FHandle := CreateFile(PChar(Value), FILE_APPEND_DATA, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

  if (FHandle <> 0) and (GetLastError <> ERROR_ALREADY_EXISTS) then
    WriteFile(FHandle, UTF8_BOM[0], Length(UTF8_BOM), W, nil);
end;

class procedure TLogger.Write(Data: string);
var
  W: Cardinal = 0;
begin
  if (FHandle = 0) or string.IsNullOrWhiteSpace(Data) then
    Exit;

  Data += #13#10;

  WriteFile(FHandle, Data[1], Length(Data), W, nil);
end;

end.
