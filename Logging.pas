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
  Classes,
  SysUtils,
  Windows;

type
  TLogger = class
  public
    class procedure SetFilename(LogFile: string);
    class procedure Write(Data: string);
  end;

var
  LoggingFile: string;

implementation

//var
//  CS: _RTL_CRITICAL_SECTION;

{ TLogger }

class procedure TLogger.SetFilename(LogFile: string);
begin
  LoggingFile := LogFile;
end;

class procedure TLogger.Write(Data: string);
const
  FILE_APPEND_DATA = 4;
var
  H: THandle;
  W: Cardinal;
begin
  if LoggingFile = '' then
    Exit;

  //  EnterCriticalSection(CS);
  try
    H := CreateFileW(PWideChar(UnicodeString(LoggingFile)), FILE_APPEND_DATA, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if H = INVALID_HANDLE_VALUE then
    begin
      H := CreateFileW(PWideChar(UnicodeString(LoggingFile)), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
      Data := UTF8Encode(#$FEFF) + Data;
    end;

    if H <> INVALID_HANDLE_VALUE then
    begin
      Data := Data + #13#10;
      WriteFile(H, Data[1], Length(Data) * SizeOf(Char), W, nil);
      FileClose(H);
    end;
  finally
    //    LeaveCriticalSection(CS);
  end;
end;

initialization
  LoggingFile := '';
  //  InitializeCriticalSection(CS);

end.
