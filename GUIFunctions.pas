{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2011 Alexander Nottelmann

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
  Windows, SysUtils, Classes, Controls, Graphics, ShellAPI, ShlObj, ActiveX;

type
  TAccessCanvas = class(TCanvas);

function GetTextSize(Text: string; Font: TFont): TSize;
function TruncateText(Text: string; MaxWidth: Integer; Font: TFont): string;
function BrowseDialog(Handle: HWnd; Title: string; Flag: Integer): string;
procedure PropertiesDialog(Filename: string);
function GetShellFolder(CSIDL: Integer): string;
function Recycle(Handle: Cardinal; Filename: string): Boolean; overload;
function GetUserDir: string;

implementation

function GetTextSize(Text: string; Font: TFont): TSize;
var
  Canvas: TAccessCanvas;
  Size2: TSize;
begin
  Result.cx := 0;
  Result.cy := 0;
  Canvas := TAccessCanvas.Create;
  try
    Canvas.Handle := GetDC(GetDesktopWindow);
    SelectObject(Canvas.Handle, Font.Handle);
    GetTextExtentPoint32W(Canvas.Handle, Text, Length(Text), Size2);
    Result := Size2;
    ReleaseDC(GetDesktopWindow, Canvas.Handle);
  finally
    Canvas.Free;
  end;
end;

function TruncateText(Text: string; MaxWidth: Integer; Font: TFont): string;
var
  w: Integer;
  Canvas: TAccessCanvas;
  Size2: TSize;
begin
  Canvas := TAccessCanvas.Create;
  try
    Canvas.Handle := GetDC(GetDesktopWindow);
    SelectObject(Canvas.Handle, Font.Handle);

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
  lpItemID: PItemIDList;
  BrowseInfo: TBrowseInfo;
  DisplayName: array[0..MAX_PATH] of Char;
  TempPath: array[0..MAX_PATH] of Char;
begin
  Result := '';
  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  with BrowseInfo do
  begin
    hwndOwner := Handle;
    pszDisplayName := @DisplayName;
    lpszTitle := PChar(Title);
    ulFlags := Flag or BIF_NEWDIALOGSTYLE;
  end;
  lpItemID := SHBrowseForFolder(BrowseInfo);
  if lpItemId <> nil then
  begin
    SHGetPathFromIDList(lpItemID, TempPath);
    Result := TempPath;
    GlobalFreePtr(lpItemID);
  end;
end;

procedure PropertiesDialog(Filename: string);
var
  sei: ShellExecuteInfo;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.lpFile := PChar(Filename);
  sei.lpVerb := 'properties';
  sei.fMask  := SEE_MASK_INVOKEIDLIST;
  ShellExecuteEx(@sei);
end;

function GetShellFolder(CSIDL: Integer): string;
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

function Recycle(Handle: Cardinal; Filename: string): Boolean;
var
  Ret: Integer;
  Operation: TSHFileOpStruct;
begin
  with Operation do
  begin
    Wnd := Handle;
    wFunc := FO_DELETE;
    pFrom := PChar(Filename + #0#0#0#0);
    pTo := nil;
    fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION;
    hNameMappings := nil;
    lpszProgressTitle := nil;
  end;
  Ret := SHFileOperation(Operation);
  // True wenn Erfolg oder Datei nicht gefunden
  Result := (Ret = 0) or (Ret = 2);
end;

function GetUserDir: string;
begin
  Result := GetShellFolder(CSIDL_APPDATA);
  if (Trim(Result) <> '') then
  begin
    Result := IncludeTrailingPathDelimiter(Result);
  end;
end;

end.

