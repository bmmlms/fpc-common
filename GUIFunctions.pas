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
unit GUIFunctions;

interface

uses
  Windows, SysUtils, Controls, Graphics, ShellAPI, ShlObj, ActiveX;

type
  TAccessCanvas = class(TCanvas);

function TruncateText(Text: string; MaxWidth: Integer; SrcCanvas: TCanvas): string;
function BrowseDialog(Handle: HWnd; Title: string; Flag: Integer): string;
function GetShellFolder(CSIDL: Integer): string;

implementation

function TruncateText(Text: string; MaxWidth: Integer; SrcCanvas: TCanvas): string;
var
  w: Integer;
  Canvas: TAccessCanvas;
  Size2: TSize;
begin
  Canvas := TAccessCanvas.Create;
  try
    Canvas.Handle := GetDC(GetDesktopWindow);
    SelectObject(Canvas.Handle, SrcCanvas.Font.Handle);

    if MaxWidth > -1 then
    begin
      GetTextExtentPoint32W(Canvas.Handle, Text, Length(Text), Size2);
      w := Size2.cx;
      if w > MaxWidth then
      begin
        SetLength(Text, Length(Text) - 1);
        Text := Text + '...';
      end;

      GetTextExtentPoint32W(Canvas.Handle, Text, Length(Text), Size2);
      w := Size2.cx;
      while w > MaxWidth do
      begin
        Text := Copy(Text, 1, Length(Text) - 4) + '...';
        if Length(Text) = 3 then
        begin
          Text := '';
          Break;
        end;
        GetTextExtentPoint32W(Canvas.Handle, Text, Length(Text), Size2);
        w := Size2.cx;
      end;
    end;
    Result := Text;

    ReleaseDC(GetDesktopWindow, Canvas.Handle);
  finally
    Canvas.Free;
  end;
end;

function BrowseDialog(Handle: HWnd; Title: string; Flag: Integer): string;
var
  lpItemID : PItemIDList;
  BrowseInfo : TBrowseInfo;
  DisplayName : array[0..MAX_PATH] of Char;
  TempPath : array[0..MAX_PATH] of Char;
begin
  Result:='';
  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  with BrowseInfo do begin
    hwndOwner := Handle;
    pszDisplayName := @DisplayName;
    lpszTitle := PChar(Title);
    ulFlags := Flag;
  end;
  lpItemID := SHBrowseForFolder(BrowseInfo);
  if lpItemId <> nil then begin
    SHGetPathFromIDList(lpItemID, TempPath);
    Result := TempPath;
    GlobalFreePtr(lpItemID);
  end;
end;

function GetShellFolder(CSIDL: integer): string;
var
  pidl: PItemIdList;
  SystemFolder: Integer;
  Malloc: IMalloc;
begin
  Malloc := nil;
  Result := '';
  SHGetMalloc(Malloc);
  if Malloc = nil then
  begin
    Exit;
  end;
  try
    SystemFolder := CSIDL;
    if SUCCEEDED(SHGetSpecialFolderLocation(0, SystemFolder, pidl)) then
    begin
      SetLength(Result, MAX_PATH);
      if SHGetPathFromIDList(pidl, PChar(Result)) then
      begin
        SetLength(Result, Length(PChar(Result)));
      end else
        Result := '';
    end;
  finally
    Malloc.Free(pidl);
  end;
end;

end.
