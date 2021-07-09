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

unit GUIFunctions;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, ShellAPI, ShlObj, ActiveX,
  ComObj, Types;

function GetTextSize(Text: string; Font: TFont): TSize;
function TruncateText(Text: string; MaxWidth: Integer; Font: TFont): string;
function StringForWidth(const c: Char; const Width: Integer; const Font: TFont): string;
function BrowseDialog(Handle: HWnd; Title: string; Flag: Integer): string;
procedure PropertiesDialog(Filename: string);
function GetShellFolder(CSIDL: Integer): string;
function Recycle(Handle: Cardinal; Filename: string): Boolean; overload;
function GetUserDir: string;
function ResizeBitmap(Bitmap: TBitmap; MaxSize: Integer): TBitmap;
function CreateLink(Executable, Dest, Name, Args: string; Delete: Boolean): Boolean;
procedure GetMaxTransparent(Icon: TIcon; var Top, Right: Integer);

implementation

// TODO: nen helper draus machen.
function TruncateText(Text: string; MaxWidth: Integer; Font: TFont): string;
var
  Canvas: TCanvas;
  EW: Integer;
begin
  Text := Text.Trim;
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetDC(GetDesktopWindow);
    try
      Canvas.Font.Assign(Font);

      if Canvas.TextWidth(Text) <= MaxWidth then
        Exit(Text);

      EW := Canvas.TextWidth('...');

      Exit(Text.Substring(0, Canvas.TextFitInfo(Text, MaxWidth - EW)).Trim + '...');
    finally
      ReleaseDC(GetDesktopWindow, Canvas.Handle);
    end;
  finally
    Canvas.Free;
  end;
end;

function StringForWidth(const c: Char; const Width: Integer; const Font: TFont): string;
var
  Canvas: TCanvas;

begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetDC(GetDesktopWindow);
    try
      Canvas.Font.Assign(Font);

      Result := c;
      while Canvas.TextWidth(Result) < Width do
        Result += c;
    finally
      ReleaseDC(GetDesktopWindow, Canvas.Handle);
    end;
  finally
    Canvas.Free;
  end;
end;

function GetTextSize(Text: string; Font: TFont): TSize;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetDC(GetDesktopWindow);
    try
      Canvas.Font.Assign(Font);

      Exit(Canvas.TextExtent(Text));
    finally
      ReleaseDC(GetDesktopWindow, Canvas.Handle);
    end;
  finally
    Canvas.Free;
  end;
end;

function BrowseDialog(Handle: HWnd; Title: string; Flag: Integer): string;  // TODO: gibts ne kompoinente für
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
  //ShellExecuteEx(@sei);
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

function ResizeBitmap(Bitmap: TBitmap; MaxSize: Integer): TBitmap;
var
  FW, FH: Integer;
  Res: TBitmap;
begin
  if Bitmap.Width >= Bitmap.Height then
  begin
    FW := MaxSize;
    FH := Trunc((Bitmap.Height / Bitmap.Width) * MaxSize);
  end else
  begin
    FH := MaxSize;
    FW := Trunc((Bitmap.Width / Bitmap.Height) * MaxSize);
  end;

  Res := TBitmap.Create;
  Res.Width := FW;
  Res.Height := FH;
  Res.Canvas.StretchDraw(Rect(0, 0, FW, FH), Bitmap);
  Result := Res;
end;

function CreateLink(Executable, Dest, Name, Args: string; Delete: Boolean): Boolean;
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  LinkName: WideString;
begin
  Dest := IncludeTrailingPathDelimiter(Dest);
  if Delete then
  begin
    Result := DeleteFile(Dest + Name + '.lnk');
  end else
  begin
    DeleteFile(Dest + Name + '.lnk');

    IObject := CreateComObject(CLSID_ShellLink);
    ISLink := IObject as IShellLink;
    IPFile := IObject as IPersistFile;

    ISLink.SetPath(PChar(Executable));
    ISLink.SetWorkingDirectory(PChar(ExtractFilePath(Executable)));
    ISLink.SetArguments(PChar(Args));

    // TODO:
    //LinkName := Dest + Name + '.lnk';
    //Result := IPFile.Save(PChar(LinkName), False) = S_OK;
  end;
end;

procedure GetMaxTransparent(Icon: TIcon; var Top, Right: Integer);
var
  i, n: Integer;
  C: TColor;
begin
  Top := Icon.Height;
  Right := -1;

  C := Icon.Canvas.Pixels[0, 0];
  for i := 0 to Icon.Height - 1 do
    for n := 0 to Icon.Width - 1 do
      if Icon.Canvas.Pixels[n, i] <> C then
        if n > Right then
          Right := n;

  for i := Icon.Height - 1 downto 0 do
    for n := 0 to Icon.Width - 1 do
      if Icon.Canvas.Pixels[n, i] <> C then
        if i < Top then
          Top := i;
end;

end.

