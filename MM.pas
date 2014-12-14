{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2015 Alexander Nottelmann

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

unit MM;

interface

uses
  {$IFDEF DEBUG}FastMM4,{$ENDIF} SysUtils, GUIFunctions, ShlObj;

implementation

function GetDesktopDir: string;
begin
  Result := GetShellFolder(CSIDL_DESKTOP);
  if (Trim(Result) <> '') then
  begin
    Result := IncludeTrailingPathDelimiter(Result);
  end;
end;

begin
  {$IFDEF FullDebugMode}
  SetMMLogFileName(PAnsiChar(AnsiString(GetDesktopDir + 'streamwriter_fastmm.txt')));
  {$ENDIF}
end.
