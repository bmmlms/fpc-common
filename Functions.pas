{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2023 Alexander Nottelmann

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

unit Functions;

interface

uses
  ActiveX,
  Classes,
  ComObj,
  Controls,
  DateUtils,
  Dialogs,
  FileUtil,
  Forms,
  Graphics,
  GraphUtil,
  IdURI,
  LazUTF8,
  LConvEncoding,
  regexpr,
  ShellAPI,
  ShlObj,
  shlwapi,
  StrUtils,
  SysUtils,
  Windows,
  ZStream;

type
  TPatternReplace = record
    C: string;
    Replace: string;
  end;
  TPatternReplaceArray = array of TPatternReplace;

  TAppVersion = record
    Major, Minor, Revision, Build: Cardinal;
    AsString: AnsiString;
  end;

  TParseURLRes = record
    URL: string;
    Host: string;
    Port: Integer;
    Data: string;
    PortDetected: Boolean;
    Secure: Boolean;
    Success: Boolean;
  end;

  TRunProcessResults = (rpWin, rpFail, rpTerminated, rpTimeout);

  TReadCallback = procedure(Data: AnsiString) of object;

  TShutdownBlockReasonCreate = function(hWnd: HWND; pwszReason: LPCWSTR): BOOL; stdcall;
  TShutdownBlockReasonDestroy = function(hWnd: HWND): BOOL; stdcall;

  TTzSpecificLocalTimeToSystemTime = function(lpTimeZoneInformation: PTIMEZONEINFORMATION; lpLocalTime, lpUniversalTime: PSYSTEMTIME): BOOL; stdcall;

  { TFunctions }

  TFunctions = class
  private
  class var
    FShutdownBlockReasonCreate: TShutdownBlockReasonCreate;
    FShutdownBlockReasonDestroy: TShutdownBlockReasonDestroy;

    FTzSpecificLocalTimeToSystemTime: TTzSpecificLocalTimeToSystemTime;
  public
    class function MsgBox(Text, Title: string; uType: Cardinal): Integer; static;
    class function ValidURL(URL: string): Boolean; static;
    class function StripURL(URL: string): string; static;
    class function ParseURL(URL: string): TParseURLRes; static;
    class function GetSystemDir: string; static;
    class function GetTempDir: string; static;
    class function ExtractLastDirName(s: string): string; static;
    class function RemoveFileExt(const s: string): string; static;
    class function StringToMask(s: string): string; static;
    class function Like(AString, APattern: string): Boolean; static;
    class function MakeSize(Size: Int64): string; static;
    class function DiskSpaceOkay(Path: string; MinSpaceGB: Int64): Boolean; static;
    class procedure FindFiles(PathPattern: string; Files: TStringList; SubDirs: Boolean = False; TerminateFlag: PByteBool = nil); static;
    class function RunProcess(Filename, WorkingDir: string; Timeout: Cardinal; var Output: AnsiString; var ExitCode: DWORD; TerminateFlag: PByteBool; KillOnTimeout: Boolean;
      ReadCallback: TReadCallback = nil): TRunProcessResults; overload; static;
    class function RunProcess(Filename: string; out Handle: Cardinal; Hide: Boolean = False): Boolean; overload; static;
    class function RunProcess(Filename: string; Hide: Boolean = False): Boolean; overload; static;
    class function GetCPUCount: DWord; static;
    class function BeautifyFilePath(const s: string; MaxPathChars: Integer): string; static;
    class function HashString(Value: string): Cardinal; static;
    class function IsAnsi(const s: string): Boolean; static;
    class function OccurenceCount(C: Char; Str: string): Integer; static;
    class function PatternReplace(S: string; ReplaceList: TPatternReplaceArray): string; static;
    class function PatternReplaceNew(S: string; ReplaceList: TPatternReplaceArray): string; static;
    class function IsAdmin: LongBool; static;
    class function HTML2Color(const HTML: string): Integer; static;
    class function SimilarColor(const Color: TColor; Diff: Integer): TColor; static;
    class function GetFileSize(const AFilename: string; out Size: Int64): Boolean; static;
    class function CmpInt(const A, B: Int64; R: Boolean = False): Integer; static;
    class function CmpUInt64(const A, B: UInt64; R: Boolean = False): Integer; static;
    class function ParseVersion(const Version: string): TAppVersion; overload; static;
    class function ParseVersion(const Major, Minor, Revision, Build: Cardinal): TAppVersion; overload; static;
    class function IsVersionNewer(const Current, Found: TAppVersion; const ConsiderBuild: Boolean = True): Boolean; static;
    class function BuildPattern(const s: string; out Hash: Cardinal; out NumChars: Integer; AlwaysAlter: Boolean): string; static;
    class function TryRelativePath(const s: string; IsFile: Boolean; OnlyIfRemovable: Boolean): string; static;
    class function TryUnRelativePath(const s: string): string; static;
    class function FixPathName(Path: string): string; static;
    class function GetFileVersion(Filename: string): TAppVersion; static;
    class function ShortenString(Str: string; Len: Integer): string; static;
    class procedure Explode(const Separator, S: string; Lst: TStringList); static;
    class function RegExReplace(RegEx, ReplaceWith, Data: string): string; static;
    class function ContainsRegEx(RegEx, Data: string): Boolean; static;
    class procedure CompressStream(InStream, OutStream: TStream; CompressionLevel: TCompressionLevel); static;
    class procedure DecompressStream(InStream, OutStream: TStream); static;
    class function MoveFile(const Source, Dest: string; const ReplaceIfExists: Boolean): Boolean; static;
    class function IsHTTPUrl(const s: string): Boolean; static;
    class function FilterHTTPUrls(s: string; out URLs: TStringArray): Boolean; static;
    class function FilterEndsWith(const Source, FilterList: TStringArray; out Results: TStringArray): Boolean; static;
    class function GetStringGuessEncoding(const Value: PByte; const Len: Integer): string; static;

    class function BrowseDialog(Owner: TComponent; Title: string): string; static;
    class procedure PropertiesDialog(Filename: string); static;
    class function GetShellFolder(const CSIDL: Integer): string; static;
    class function Recycle(Handle: Cardinal; Filename: string): Boolean; overload; static;
    class function GetUserDir: string; static;
    class function ResizeBitmap(Bitmap: Graphics.TBitmap; MaxSize: Integer): Graphics.TBitmap; static;
    class function CreateLink(Executable, Dir, Name, Args: string; Delete: Boolean): Boolean; static;
    class procedure GetMaxTransparent(Icon: TIcon; var Top, Right: Integer); static;
    class function WindowIsFullscreen: Boolean; static;
    class function ShellExecute(const Handle: THandle; const Operation, Filename: string; const Parameters: string = ''): Boolean;
    class function ReadDataObjectFiles(const DataObject: IDataObject; out Files: TStringArray): Boolean;
    class function ReadDataObjectText(const DataObject: IDataObject; out Text: string): Boolean;

    class function ShutdownBlockReasonCreate(hWnd: HWND; pwszReason: string): Boolean; static;
    class function ShutdownBlockReasonDestroy(hWnd: HWND): Boolean; static;

    class function LocalToUTC(const DateTime: TDateTime): TDateTime; static;
    class function DateTimeToFileTime(const DateTime: TDateTime): FILETIME; static;
  end;

implementation

class function TFunctions.MsgBox(Text, Title: string; uType: Cardinal): Integer;
var
  ParentHandle: THandle = 0;
begin
  if Assigned(Screen.ActiveForm) then
    ParentHandle := Screen.ActiveForm.Handle;

  Result := MessageBox(ParentHandle, PChar(Text), PChar(Title), uType);
end;

class function TFunctions.ValidURL(URL: string): Boolean;
begin
  Result := IsAnsi(URL);
  if Result then
    Result := ParseURL(URL).Success;
end;

class function TFunctions.StripURL(URL: string): string;
var
  i: Integer;
begin
  Result := '';
  if Length(URL) > 1 then
  begin
    URL := LowerCase(URL);
    for i := 1 to Length(URL) do
      if (CharInSet(Url[i], ['a'..'z'])) or (CharInSet(Url[i], ['0'..'9'])) then
        Result := Result + URL[i];
  end;
end;

class function TFunctions.ParseURL(URL: string): TParseURLRes;
var
  U: TIdURI;
begin
  Result.URL := '';
  Result.Host := '';
  Result.Port := 0;
  Result.Data := '';
  Result.PortDetected := False;
  Result.Secure := False;
  Result.Success := False;

  if (Copy(LowerCase(URL), 0, 7) <> 'http://') and (Copy(LowerCase(URL), 0, 8) <> 'https://') then
    URL := 'http://' + URL;

  U := TIdURI.Create(URL);
  try
    try
      Result.URL := U.URI;
      Result.Host := U.Host;
      Result.Data := U.Path + U.Document;
      if Length(U.Params) > 0 then
        Result.Data := Result.Data + '?' + U.Params;
      Result.Secure := U.Protocol = 'https';

      Result.Port := 0;
      if Length(U.Port) > 0 then
      try
        Result.Port := StrToInt(U.Port);
        Result.PortDetected := True;
      except
      end;

      if not Result.PortDetected then
        if U.Protocol = 'http' then
          Result.Port := 80
        else if U.Protocol = 'https' then
          Result.Port := 443;

      Result.Success := (Length(Result.Host) > 0) and (Result.Port > 0);
    except
    end;
  finally
    U.Free;
  end;
end;

class function TFunctions.GetSystemDir: string;
var
  Dir: array [0..MAX_PATH - 1] of Char;
begin
  Result := '';
  if GetSystemDirectory(Dir, MAX_PATH) <> 0 then
    Result := Dir;
end;

class function TFunctions.GetTempDir: string;
var
  Dir: array [0..MAX_PATH - 1] of Char;
begin
  Result := '';
  if GetTempPath(MAX_PATH, Dir) <> 0 then
    Result := Dir;
end;

class function TFunctions.ExtractLastDirName(s: string): string;
var
  i: Integer;
begin
  Result := '';
  if s = '' then
    Exit;
  if s[Length(s)] = '\' then
    SetLength(s, Length(s) - 1);

  for i := Length(s) downto 1 do
    if s[i] = '\' then
    begin
      Result := Copy(s, i + 1, MAX_PATH);
      Break;
    end;
  if Result = '' then
    Result := s;
end;

class function TFunctions.RemoveFileExt(const s: string): string;
var
  i: Integer;
begin
  i := RPos('.', s);
  if i > 1 then
    Exit(Copy(s, 1, i - 1));
  Exit(s);
end;

class function TFunctions.StringToMask(s: string): string;
begin
  Result := '*' + StringReplace(Trim(s), ' ', '*', [rfReplaceAll]) + '*';
end;

class function TFunctions.Like(AString, APattern: string): Boolean;
var
  StringPtr, PatternPtr: PChar;
  StringRes, PatternRes: PChar;
begin
  AString := WideLowerCase(AString);
  APattern := WideLowerCase(APattern);
  Result := False;
  StringPtr := PChar(AString);
  PatternPtr := PChar(APattern);
  StringRes := nil;
  PatternRes := nil;
  repeat
    repeat // ohne vorangegangenes "*"
      case PatternPtr^ of
        #0:
        begin
          Result := StringPtr^ = #0;
          if Result or (StringRes = nil) or (PatternRes = nil) then
            Exit;
          StringPtr := StringRes;
          PatternPtr := PatternRes;
          Break;
        end;
        '*':
        begin
          Inc(PatternPtr);
          PatternRes := PatternPtr;
          Break;
        end;
        '?':
        begin
          if StringPtr^ = #0 then
            Exit;
          Inc(StringPtr);
          Inc(PatternPtr);
        end;
        else
        begin
          if StringPtr^ = #0 then
            Exit;
          if StringPtr^ <> PatternPtr^ then
          begin
            if (StringRes = nil) or (PatternRes = nil) then
              Exit;
            StringPtr := StringRes;
            PatternPtr := PatternRes;
            Break;
          end else
          begin
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        end;
      end;
    until False;
    repeat
      case PatternPtr^ of
        #0:
        begin
          Result := True;
          Exit;
        end;
        '*':
        begin
          Inc(PatternPtr);
          PatternRes := PatternPtr;
        end;
        '?':
        begin
          if StringPtr^ = #0 then
            Exit;
          Inc(StringPtr);
          Inc(PatternPtr);
        end;
        else
        begin
          repeat
            if StringPtr^ = #0 then
              Exit;
            if StringPtr^ = PatternPtr^ then
              Break;
            Inc(StringPtr);
          until False;
          Inc(StringPtr);
          StringRes := StringPtr;
          Inc(PatternPtr);
          Break;
        end;
      end;
    until False;
  until False;
end;

class function TFunctions.MakeSize(Size: Int64): string;
begin
  if Size < 1048576 then
    Result := Format('%f KB', [Size / (1024)])
  else if Size < 1073741824 then
    Result := Format('%f MB', [Size / (1024 * 1024)])
  else
    Result := Format('%f GB', [Size / (1024 * 1024 * 1024)]);
end;

class function TFunctions.DiskSpaceOkay(Path: string; MinSpaceGB: Int64): Boolean;
var
  Res: Int64;
begin
  Path := LowerCase(Path);
  if (Length(Path) = 0) or (not CharInSet(Path[1], ['a'..'z'])) then
    Exit(True);

  Res := DiskFree(Byte(Path[1]) - 96);
  if Res = -1 then
    Exit(True);

  Result := Res > MinSpaceGB * 1073741824;
end;

class procedure TFunctions.FindFiles(PathPattern: string; Files: TStringList; SubDirs: Boolean = False; TerminateFlag: PByteBool = nil);
var
  SR: TSearchRec;
  Dir, Pattern: string;
  Dirs: TStringList;
begin
  Files.Clear;

  Dir := ExtractFilePath(PathPattern);
  Pattern := ExtractFileName(PathPattern);
  Dirs := TStringList.Create;
  try
    repeat
      if (TerminateFlag <> nil) and TerminateFlag^ then
        Exit;

      if FindFirst(ConcatPaths([Dir, Pattern]), faAnyFile, SR) = 0 then
      begin
        repeat
          if (SR.Name <> '.') and (SR.Name <> '..') and (not (SR.Attr and faDirectory = faDirectory)) then
            if SubDirs then
              Files.Add(ConcatPaths([Dir, SR.Name]))
            else
              Files.Add(SR.Name);
        until FindNext(SR) <> 0;
        SysUtils.FindClose(SR);
      end;

      if SubDirs then
        if FindFirst(ConcatPaths([Dir, '*']), faDirectory, SR) = 0 then
        begin
          repeat
            if (SR.Name <> '.') and (SR.Name <> '..') and (SR.Attr and faDirectory = faDirectory) then
              Dirs.Add(ConcatPaths([Dir, SR.Name]));
          until FindNext(SR) <> 0;
          SysUtils.FindClose(SR);
        end;

      if Dirs.Count > 0 then
      begin
        Dir := Dirs[0];
        Dirs.Delete(0);
      end else
        Dir := '';
    until Dir = '';
  finally
    Dirs.Free;
  end;
end;

class function TFunctions.RunProcess(Filename, WorkingDir: string; Timeout: Cardinal; var Output: AnsiString; var ExitCode: DWORD; TerminateFlag: PByteBool; KillOnTimeout: Boolean;
  ReadCallback: TReadCallback = nil): TRunProcessResults; overload;
var
  OK: Boolean;
  Handle: Cardinal;
  SI: TStartupInfo;
  PI: TProcessInformation;
  SA: TSecurityAttributes;
  SD: TSecurityDescriptor;

  ReadPipeOut, WritePipeOut: THandle;
  ReadPipeIn, WritePipeIn: THandle;
  ReadCount: DWORD;
  Avail: DWORD;
  Tmp: AnsiString;
  Started: UInt64;
begin
  Result := rpFail;
  Output := '';
  if Filename = '' then
    Exit;

  if not InitializeSecurityDescriptor(@SD, SECURITY_DESCRIPTOR_REVISION) then
    Exit;
  if not SetSecurityDescriptorDacl(@SD, True, nil, False) then
    Exit;
  SA.lpSecurityDescriptor := @SD;

  SA.nLength := SizeOf(TSecurityAttributes);
  SA.bInheritHandle := True;

  if not CreatePipe(ReadPipeOut, WritePipeOut, @SA, 0) then
    Exit;

  if not CreatePipe(ReadPipeIn, WritePipeIn, @SA, 0) then
  begin
    CloseHandle(ReadPipeOut);
    CloseHandle(WritePipeOut);
    Exit;
  end;

  FillChar(SI, SizeOf(TStartupInfo), #0);
  FillChar(PI, SizeOf(TProcessInformation), #0);
  SI.cb := SizeOf(TStartupInfo);
  SI.dwFlags := STARTF_FORCEOFFFEEDBACK or STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_HIDE;
  SI.hStdOutput := WritePipeOut;
  SI.hStdError := WritePipeOut;
  SI.hStdInput := ReadPipeIn;
  OK := CreateProcess(nil, PChar(Filename), nil, nil, True, CREATE_NEW_PROCESS_GROUP or NORMAL_PRIORITY_CLASS or CREATE_NO_WINDOW, nil, PChar(WorkingDir), SI, PI);
  try
    if OK then
    begin
      Handle := PI.hProcess;

      Result := rpWin;

      Started := GetTickCount64;
      while WaitForSingleObject(Handle, 100) = WAIT_TIMEOUT do
      begin
        try
          if (TerminateFlag <> nil) and TerminateFlag^ then
          begin
            TerminateProcess(Handle, 0);
            Sleep(500); // Wichtig - manchmal sind nach TerminateProcess() scheinbar noch Handles offen
            Result := rpTerminated;
            Exit;
          end;
        except
        end;

        PeekNamedPipe(ReadPipeOut, nil, 0, nil, @Avail, nil);
        if Avail > 0 then
        begin
          SetLength(Tmp, Avail);
          ReadFile(ReadPipeOut, Tmp[1], Avail, ReadCount, nil);

          if Assigned(ReadCallback) then
            ReadCallback(Tmp);

          Output := Output + Tmp;

          Started := GetTickCount64;
        end;

        if Timeout < High(Cardinal) then
          if Started + Timeout < GetTickCount64 then
          begin
            if KillOnTimeout then
            begin
              TerminateProcess(Handle, 0);
              Sleep(500); // Wichtig - manchmal sind nach TerminateProcess() scheinbar noch Handles offen
            end;

            Result := rpTimeout;
            Exit;
          end;
      end;

      PeekNamedPipe(ReadPipeOut, nil, 0, nil, @Avail, nil);
      if Avail > 0 then
      begin
        SetLength(Tmp, Avail);
        ReadFile(ReadPipeOut, Tmp[1], Avail, ReadCount, nil);
        Output := Output + Tmp;
      end;

      GetExitCodeProcess(PI.hProcess, ExitCode);
    end;
  finally
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
    CloseHandle(ReadPipeOut);
    CloseHandle(WritePipeOut);
    CloseHandle(ReadPipeIn);
    CloseHandle(WritePipeIn);
  end;
end;

class function TFunctions.RunProcess(Filename: string; out Handle: Cardinal; Hide: Boolean): Boolean;
var
  OK: Boolean;
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  Result := False;
  Handle := High(Cardinal);
  if Filename = '' then
    Exit;
  FillChar(SI, SizeOf(TStartupInfo), #0);
  FillChar(PI, SizeOf(TProcessInformation), #0);
  SI.cb := SizeOf(TStartupInfo);
  if Hide then
  begin
    SI.dwFlags := STARTF_USESHOWWINDOW;
    SI.wShowWindow := SW_HIDE;
  end;
  OK := CreateProcess(nil, PChar(Filename), nil, nil, False, CREATE_NEW_PROCESS_GROUP or NORMAL_PRIORITY_CLASS, nil, nil, SI, PI);
  Result := OK;
  if OK then
  begin
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
    Handle := PI.hProcess;
  end;
end;

class function TFunctions.RunProcess(Filename: string; Hide: Boolean): Boolean;
var
  Dummy: Cardinal;
begin
  Result := RunProcess(Filename, Dummy, Hide);
end;

class function TFunctions.GetCPUCount: DWord;
var
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  Result := SysInfo.dwNumberOfProcessors;
end;

class function TFunctions.BeautifyFilePath(const s: string; MaxPathChars: Integer): string;
var
  i: Integer;
  CharSep: Integer;
begin
  Result := s;
  CharSep := UTF8RPos('\', s);

  if (CharSep > 0) and (CharSep - 1 > MaxPathChars) then
    Result := UTF8Copy(s, 1, MaxPathChars - 3) + '...' + UTF8Copy(s, CharSep, UTF8LengthFast(s) - CharSep + 1);
end;

class function TFunctions.HashString(Value: string): Cardinal;
var
  i: Integer;
  x: Cardinal;
begin
  Result := 0;
  for i := 1 to Length(Value) do
  begin
    Result := (Result shl 4) + Ord(Value[i]);
    x := Result and $F0000000;
    if (x <> 0) then
      Result := Result xor (x shr 24);
    Result := Result and (not x);
  end;
end;

class function TFunctions.IsAnsi(const s: string): Boolean;
var
  P: PByte;
begin
  Result := True;

  if S.Length = 0 then
    Exit;

  P := @s[1];
  while P < @s[1] + Length(s) do
  begin
    if P^ > 127 then
      Exit(False);
    P += 1;
  end;
end;

class function TFunctions.OccurenceCount(C: Char; Str: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Str) do
    if Str[i] = C then
      Inc(Result);
end;

class function TFunctions.PatternReplace(S: string; ReplaceList: TPatternReplaceArray): string;
var
  C: Char;
  i, n, j: Integer;
  Replace: string;
  TokenIndices: array of Integer;
begin
  Result := s;

  TokenIndices := [];
  for i := 1 to Length(Result) - 1 do
    if Result[i] = '%' then
      for j := 0 to High(ReplaceList) do
        if (Length(Result) >= i + 1) and (Result[i + 1] = ReplaceList[j].C) then
        begin
          SetLength(TokenIndices, Length(TokenIndices) + 1);
          TokenIndices[High(TokenIndices)] := i;
        end;

  for n := 0 to High(TokenIndices) do
  begin
    C := Result[TokenIndices[n] + 1];
    for j := 0 to High(ReplaceList) do
      if ReplaceList[j].C = C then
      begin
        Replace := ReplaceList[j].Replace;
        Break;
      end;

    Result := Copy(Result, 1, TokenIndices[n] - 1) + Replace + Copy(Result, TokenIndices[n] + 2, Length(Result));
    for j := 0 to High(TokenIndices) do
      if TokenIndices[j] > TokenIndices[n] then
        TokenIndices[j] := TokenIndices[j] + Length(Replace) - 2;
  end;
end;

class function TFunctions.PatternReplaceNew(S: string; ReplaceList: TPatternReplaceArray): string;
var
  i, n, j: Integer;
  D: Integer;
  Str: string;
  TokenIndices: array of Integer;
begin
  Result := s;

  TokenIndices := [];
  for i := 1 to Length(Result) do
    if Result[i] = '%' then
      TokenIndices += [i];

  if Length(TokenIndices) < 2 then
    Exit;

  for i := 0 to High(TokenIndices) - 1 do
  begin
    if i mod 2 <> 0 then
      Continue;

    if TokenIndices[i + 1] - TokenIndices[i] > 1 then
    begin
      Str := LowerCase(Copy(Result, TokenIndices[i] + 1, TokenIndices[i + 1] - TokenIndices[i] - 1));
      for n := 0 to High(ReplaceList) do
        if LowerCase(ReplaceList[n].C) = Str then
        begin
          D := Length(Result);
          Result := Copy(Result, 1, TokenIndices[i] - 1) + ReplaceList[n].Replace + Copy(Result, TokenIndices[i + 1] + 1, Length(Result));
          for j := i + 2 to High(TokenIndices) do
            TokenIndices[j] := TokenIndices[j] + Length(Result) - D;
          Break;
        end;
    end;
  end;
end;

class function TFunctions.IsAdmin: LongBool;

  function GetAdminSid: PSID;
  const
    SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
    SECURITY_BUILTIN_DOMAIN_RID: DWORD = $00000020;
    DOMAIN_ALIAS_RID_ADMINS: DWORD = $00000220;
  begin
    Result := nil;
    AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, Result);
  end;

var
  TokenHandle: THandle = 0;
  ReturnLength: DWORD;
  TokenInformation: PTokenGroups;
  AdminSid: PSID;
  Loop: Integer;
begin
  Result := False;

  if not OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
    Exit;

  try
    ReturnLength := 0;
    GetTokenInformation(TokenHandle, TokenGroups, nil, 0, ReturnLength);
    TokenInformation := GetMemory(ReturnLength);
    if Assigned(TokenInformation) then
      try
        if GetTokenInformation(TokenHandle, TokenGroups, TokenInformation, ReturnLength, ReturnLength) then
        begin
          AdminSid := GetAdminSid;
          try
            for Loop := 0 to TokenInformation^.GroupCount - 1 do
              {$PUSH}
              {$RANGECHECKS OFF}
              if EqualSid(TokenInformation^.Groups[Loop].Sid, AdminSid) then
              {$POP}
                Exit(True);
          finally
            FreeSid(AdminSid);
          end;
        end;
      finally
        FreeMemory(TokenInformation);
      end;
  finally
    CloseHandle(TokenHandle);
  end;
end;

class function TFunctions.HTML2Color(const HTML: string): Integer;
var
  Offset: Integer;
begin
  if Copy(HTML, 1, 1) = '#' then
    Offset := 1
  else
    Offset := 0;
  Result := Integer(StrToInt('$' + Copy(HTML, Offset + 1, 2))) + Integer(StrToInt('$' + Copy(HTML, Offset + 3, 2))) shl 8 + Integer(StrToInt('$' + Copy(HTML, Offset + 5, 2))) shl 16;
end;

class function TFunctions.SimilarColor(const Color: TColor; Diff: Integer): TColor;
var
  H, S, L: Word;
  IsDark: Boolean;
begin
  GraphUtil.ColorRGBToHLS(ColorToRGB(Color), H, L, S);

  IsDark := L < 32;

  if L < 32 then
    L := 32;

  Diff := IfThen<Integer>(IsDark, 100 + Diff, 100 - Diff);

  Result := GraphUtil.ColorHLSToRGB(H, (Cardinal(L) * Diff) div 100, S);
end;

class function TFunctions.GetFileSize(const AFilename: string; out Size: Int64): Boolean;
var
  FileStream: TFileStream;
begin
  Size := 0;
  Result := False;

  try
    FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      Size := FileStream.Size;
      Result := True;
    finally
      FileStream.Free;
    end;
  except
  end;
end;

class function TFunctions.CmpInt(const A, B: Int64; R: Boolean): Integer;
begin
  if not R then
  begin
    if A > B then
      Result := 1
    else if A < B then
      Result := -1
    else
      Result := 0;
  end else if A < B then
    Result := 1
  else if A > B then
    Result := -1
  else
    Result := 0;
end;

class function TFunctions.CmpUInt64(const A, B: UInt64; R: Boolean): Integer;
begin
  if not R then
  begin
    if A > B then
      Result := 1
    else if A < B then
      Result := -1
    else
      Result := 0;
  end else if A < B then
    Result := 1
  else if A > B then
    Result := -1
  else
    Result := 0;
end;

class function TFunctions.ParseVersion(const Version: string): TAppVersion;
var
  Dots: array[0..2] of Integer;
  i: Integer;
begin
  Dots[0] := Pos('.', string(Version));
  Dots[1] := PosEx('.', string(Version), Dots[0] + 1);
  Dots[2] := PosEx('.', string(Version), Dots[1] + 1);
  for i := 0 to High(Dots) do
    if Dots[i] < i then
      raise Exception.Create('Error parsing version.');
  Result.Major := StrToInt(Copy(string(Version), 0, Dots[0] - 1));
  Result.Minor := StrToInt(Copy(string(Version), Dots[0] + 1, Dots[1] - 1 - Dots[0]));
  Result.Revision := StrToInt(Copy(string(Version), Dots[1] + 1, Dots[2] - 1 - Dots[1]));
  Result.Build := StrToInt(Copy(string(Version), Dots[2] + 1, Length(Version) - Dots[2]));
  Result.AsString := AnsiString(Format('%d.%d.%d.%d', [Result.Major, Result.Minor, Result.Revision, Result.Build]));
end;

class function TFunctions.ParseVersion(const Major, Minor, Revision, Build: Cardinal): TAppVersion;
begin
  Result.Major := Major;
  Result.Minor := Minor;
  Result.Revision := Revision;
  Result.Build := Build;
  Result.AsString := AnsiString(Format('%d.%d.%d.%d', [Major, Minor, Revision, Build]));
end;

class function TFunctions.IsVersionNewer(const Current, Found: TAppVersion; const ConsiderBuild: Boolean = True): Boolean;
var
  MajorEq, MinorEq, RevisionEq: Boolean;
begin
  MajorEq := Found.Major = Current.Major;
  MinorEq := Found.Minor = Current.Minor;
  RevisionEq := Found.Revision = Current.Revision;

  Result := (Found.Major > Current.Major)
    or (MajorEq and (Found.Minor > Current.Minor))
    or (MajorEq and MinorEq and (Found.Revision > Current.Revision))
    or (ConsiderBuild and MajorEq and MinorEq and RevisionEq and (Found.Build > Current.Build));
end;

class function TFunctions.BuildPattern(const s: string; out Hash: Cardinal; out NumChars: Integer; AlwaysAlter: Boolean): string;

  function TrimChars(const s: string; const c: Char): string;
  var
    n, Counter: Integer;
    F: Boolean;
  begin
    F := False;
    Counter := 1;
    SetLength(Result, Length(s));
    for n := 1 to Length(s) do
      if s[n] = c then
      begin
        if F then
        else
        begin
          Result[Counter] := c;
          Inc(Counter);
        end;
        F := True;
      end else
      begin
        F := False;
        Result[Counter] := s[n];
        Inc(Counter);
      end;
    if Counter = 0 then
      Exit;
    SetLength(Result, Counter - 1);
  end;

var
  P: Integer;
begin
  P := 1;
  if not AlwaysAlter then
  begin
    P := Pos('*', s);
    if P = 0 then
      P := Pos('?', s);
  end;

  Result := Trim(LowerCase(s));
  Result := AnsiLowerCase(Result);
  if (AlwaysAlter) or (not AlwaysAlter and (P = 0)) then
  begin
    Result := '*' + Result + '*';
    Result := StringReplace(Result, ' ', '*', [rfReplaceAll]);
  end;
  Result := TrimChars(Result, '*');
  NumChars := Length(Result) - OccurenceCount('*', Result) - OccurenceCount('?', Result);
  Hash := HashString(Result);
end;

class function TFunctions.TryRelativePath(const s: string; IsFile: Boolean; OnlyIfRemovable: Boolean): string;
var
  From: string;
  T: Integer;
  OutData: array[0..MAX_PATH - 1] of Char;
begin
  if (not OnlyIfRemovable) or (GetDriveType(PChar(IncludeTrailingPathDelimiter(ExtractFileDrive(s)))) = DRIVE_REMOVABLE) then
  begin
    if IsFile then
      T := FILE_ATTRIBUTE_NORMAL
    else
      T := FILE_ATTRIBUTE_DIRECTORY;

    From := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

    if PathRelativePathToA(OutData, PChar(From), T, PChar(s), 0) then
      Result := OutData
    else
      Result := s;
  end else
    Result := s;
end;

class function TFunctions.TryUnRelativePath(const s: string): string;
var
  From: string;
  OutData: array[0..MAX_PATH - 1] of Char;
begin
  if (((Length(s) >= 2) and (s[1] = '.') and (s[2] = '\')) or ((Length(s) >= 3) and (s[1] = '.') and (s[2] = '.') and (s[3] = '\'))) then
  begin
    From := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

    if PathCanonicalizeA(OutData, PChar(From + s)) then
      Result := OutData
    else
      Result := s;
  end else
    Result := s;
end;

class function TFunctions.FixPathName(Path: string): string;
var
  i, LastI: Integer;
  Drive, Part: string;
  Parts: array of string = [];
begin
  Result := '';
  Drive := '';

  // Laufwerk/Share-Anfang ermitteln
  if (Length(Path) >= 3) and (Copy(Path, 2, 2) = ':\') then
  begin
    Drive := Copy(Path, 1, 3);
    if Length(Path) >= 4 then
      Path := Copy(Path, 4, Length(Path) - 3)
    else
      Path := '';
  end else if (Length(Path) >= 2) and (Copy(Path, 1, 2) = '\\') then
  begin
    Drive := Copy(Path, 1, 2);
    if Length(Path) >= 3 then
      Path := Copy(Path, 3, Length(Path) - 2)
    else
      Path := '';
  end;

  // In Parts aufsplitten
  LastI := 1;
  for i := 1 to Length(Path) do
    if Path[i] = '\' then
    begin
      Parts += [Trim(Copy(Path, LastI, i - LastI))];
      LastI := i + 1;
    end;

  // Wenn am Ende noch Rest ist, ist das auch ein Part
  if Length(Path) > LastI then
    Parts += [Trim(Copy(Path, LastI, Length(Path) - LastI + 1))];

  // '.' und ' ' am Anfang und am Ende entfernen
  for i := 0 to High(Parts) do
  begin
    while (Length(Parts[i]) > 0) and ((Parts[i][Length(Parts[i])] = '.') or (Parts[i][Length(Parts[i])] = ' ')) do
      SetLength(Parts[i], Length(Parts[i]) - 1);

    { Punkte sind am Anfang zugelassen, also das hier nicht machen.
    while (Length(Parts[i]) > 0) and ((Parts[i][1] = '.') or (Parts[i][1] = ' ')) do
    begin
      if Length(Parts[i]) > 1 then
        Parts[i] := Copy(Parts[i], 2, Length(Parts[i]) - 1)
      else
        Parts[i] := '';
    end;
    }
    Parts[i] := Trim(Parts[i]);
  end;

  // Den Pfad aus den Parts zusammenbauen
  Result := Result + Drive;
  for Part in Parts do
    if Part <> '' then
      if (Length(Result) > 1) and (Result[Length(Result)] <> '\') then
        Result := Result + '\' + Part
      else
        Result := Result + Part;

  // Wenn in der Eingangsvariable am Ende ein '\' war, dann auch anhängen
  if (Length(Path) >= 1) and (Path[Length(Path)] = '\') and (Result <> '') then
    Result := Result + '\';
end;

class function TFunctions.GetFileVersion(Filename: string): TAppVersion;
var
  VerInfoSize: Integer;
  VerValueSize: DWord;
  Dummy: DWord;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(Filename), Dummy);
  Result.Major := 0;
  Result.Minor := 0;
  Result.Revision := 0;
  Result.Build := 0;
  Result.AsString := '0.0.0.0';
  if VerInfoSize <> 0 then
  begin
    GetMem(VerInfo, VerInfoSize);
    try
      if GetFileVersionInfo(PChar(Filename), 0, VerInfoSize, VerInfo) then
        if VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then
        begin
          with VerValue^ do
          begin
            Result.Major := dwFileVersionMS shr 16;
            Result.Minor := dwFileVersionMS and $FFFF;
            Result.Revision := dwFileVersionLS shr 16;
            Result.Build := dwFileVersionLS and $FFFF;
          end;
          Result.AsString := AnsiString(Format('%d.%d.%d.%d', [Result.Major, Result.Minor, Result.Revision, Result.Build]));
        end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
  end;

  if (Result.Major = 0) and (Result.Minor = 0) and (Result.Revision = 0) and (Result.Build = 0) then
    raise Exception.Create('');
end;

class function TFunctions.ShortenString(Str: string; Len: Integer): string;
begin
  Result := Trim(Str);
  if Length(Result) > Len then
    Result := Trim(Copy(Result, 1, Len - 3)) + '...';
end;

class procedure TFunctions.Explode(const Separator, S: string; Lst: TStringList);
var
  SepLen: Integer;
  F, P: PChar;
  ALen, Index: Integer;
  Res: array of string = [];
  i: Integer;
begin
  Lst.Clear;

  try
    if S = '' then
      Exit;

    if Separator = '' then
    begin
      SetLength(Res, 1);
      Res[0] := S;
      Exit;
    end;

    SepLen := Length(Separator);
    ALen := 0;
    SetLength(Res, ALen);

    Index := 0;
    P := PChar(S);
    while P^ <> #0 do
    begin
      F := P;
      P := AnsiStrPos(P, PChar(Separator));
      if P = nil then
        P := StrEnd(F);
      if Index >= ALen then
      begin
        Inc(ALen, 5);
        SetLength(Res, ALen);
      end;
      SetString(Res[Index], F, P - F);
      Inc(Index);
      if P^ <> #0 then
        Inc(P, SepLen);
    end;
    if Index < ALen then
      SetLength(Res, Index);
  finally
    for i := 0 to High(Res) do
      if Length(Trim(Res[i])) > 0 then
        Lst.Add(Res[i]);
  end;
end;

class function TFunctions.RegExReplace(RegEx, ReplaceWith, Data: string): string;
var
  R: TRegExpr;
  LastResult: string;
begin
  Result := Data;

  R := TRegExpr.Create(RegEx);
  R.ModifierI := True;
  try
    // Das muss so. Sonst wird z.B. aus 'ft. ft. ft. ft.' ein 'Feat. ft. Feat. ft.'
    try
      repeat
        LastResult := Result;
        Result := R.Replace(Result, ReplaceWith, True);
      until LastResult = Result;
    except
    end;
  finally
    R.Free;
  end;
end;

class function TFunctions.ContainsRegEx(RegEx, Data: string): Boolean;
var
  R: TRegExpr;
begin
  Result := False;
  R := TRegExpr.Create(RegEx);
  try
    R.ModifierI := True;
    try
      Result := R.Exec(Data);
    except
    end;
  finally
    R.Free;
  end;
end;

class procedure TFunctions.CompressStream(InStream, OutStream: TStream; CompressionLevel: TCompressionLevel);
var
  ZStream: TCompressionStream;
begin
  ZStream := TCompressionStream.Create(CompressionLevel, OutStream);
  try
    ZStream.CopyFrom(InStream, 0);
  finally
    ZStream.Free;
  end;
end;

class procedure TFunctions.DecompressStream(InStream, OutStream: TStream);
const
  BufferSize = 32768;
var
  Count: Integer;
  ZStream: TDecompressionStream;
  Buffer: array[0..BufferSize - 1] of Byte;
begin
  ZStream := TDecompressionStream.Create(InStream);
  try
    while True do
    begin
      Count := ZStream.Read(Buffer, BufferSize);
      if Count <> 0 then
        OutStream.WriteBuffer(Buffer, Count)
      else
        Break;
    end;
  finally
    ZStream.Free;
  end;
end;

class function TFunctions.MoveFile(const Source, Dest: string; const ReplaceIfExists: Boolean): Boolean;
begin
  if FileExists(Dest) and ((ReplaceIfExists and (not SysUtils.DeleteFile(Dest))) or (not ReplaceIfExists)) then
    Exit(False);

  Exit(RenameFile(Source, Dest));
end;

class function TFunctions.IsHTTPUrl(const s: string): Boolean;
begin
  Result := S.ToLower.StartsWith('http://') or S.ToLower.StartsWith('https://');
end;

class function TFunctions.FilterHTTPUrls(s: string; out URLs: TStringArray): Boolean;
begin
  URLs := [];

  for s in s.Split(LineEnding) do
    if IsHTTPUrl(s) then
      URLs += [s];

  Result := Length(URLs) > 0;
end;

class function TFunctions.FilterEndsWith(const Source, FilterList: TStringArray; out Results: TStringArray): Boolean;
var
  S, F: string;
begin
  Results := [];

  for S in Source do
    for F in FilterList do
      if EndsText(F, S) then
        Results += [S];

  Result := Length(Results) > 0;
end;

class function TFunctions.GetStringGuessEncoding(const Value: PByte; const Len: Integer): string;
var
  Dummy: Boolean;
begin
  SetCodePage(RawByteString(Result), CP_NONE);
  SetLength(Result, Len);
  Move(Value^, Result[1], Len);
  Result := ConvertEncodingToUTF8(Result, GuessEncoding(Result), Dummy);
end;

class function TFunctions.BrowseDialog(Owner: TComponent; Title: string): string;
var
  Dlg: TSelectDirectoryDialog;
begin
  Result := '';
  Dlg := TSelectDirectoryDialog.Create(Owner);
  try
    Dlg.Options := [ofEnableSizing, ofPathMustExist];
    Dlg.Title := Title;

    if not Dlg.Execute then
      Exit;

    Result := Dlg.FileName;
  finally
    Dlg.Free;
  end;
end;

class procedure TFunctions.PropertiesDialog(Filename: string);
var
  Info: TSHELLEXECUTEINFO;
begin
  FillChar(Info, SizeOf(Info), #0);
  Info.cbSize := SizeOf(Info);
  Info.lpFile := PChar(Filename);
  Info.lpVerb := 'properties';
  Info.fMask := SEE_MASK_INVOKEIDLIST;
  ShellExecuteExA(@Info);
end;

class function TFunctions.GetShellFolder(const CSIDL: Integer): string;
var
  Buf: array[0..MAX_PATH - 1] of Char;
begin
  if Failed(SHGetFolderPath(0, csidl, 0, SHGFP_TYPE_CURRENT, Buf)) then
    raise Exception.Create('SHGetFolderPath() failed');
  Result := Buf;
end;

class function TFunctions.Recycle(Handle: Cardinal; Filename: string): Boolean;
var
  Ret: Integer;
  Operation: TSHFileOpStruct;
  Buf: array[0..MAX_PATH - 1] of Char;
begin
  FillChar(Buf, SizeOf(Buf), #0);
  Move(Filename[1], Buf, Filename.Length);
  with Operation do
  begin
    hwnd := Handle;
    wFunc := FO_DELETE;
    pFrom := Buf;
    pTo := nil;
    fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT;
    hNameMappings := nil;
    lpszProgressTitle := nil;
  end;
  Ret := SHFileOperation(@Operation);
  // True wenn Erfolg oder Datei nicht gefunden
  Result := (Ret = 0) or (Ret = 2);
end;

class function TFunctions.GetUserDir: string;
begin
  Result := GetShellFolder(CSIDL_APPDATA);
end;

class function TFunctions.ResizeBitmap(Bitmap: Graphics.TBitmap; MaxSize: Integer): Graphics.TBitmap;
var
  FW, FH: Integer;
  Res: Graphics.TBitmap;
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

  Res := Graphics.TBitmap.Create;
  Res.Width := FW;
  Res.Height := FH;
  Res.Canvas.StretchDraw(Classes.Rect(0, 0, FW, FH), Bitmap);
  Result := Res;
end;

class function TFunctions.CreateLink(Executable, Dir, Name, Args: string; Delete: Boolean): Boolean;
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  Filename: string;
begin
  Filename := ConcatPaths([Dir, '%s.lnk'.Format([Name])]);
  if Delete then
    Exit(SysUtils.DeleteFile(Filename));

  SysUtils.DeleteFile(Filename);

  IObject := CreateComObject(CLSID_ShellLink);
  ISLink := IObject as IShellLink;
  IPFile := IObject as IPersistFile;

  ISLink.SetPath(PChar(Executable));
  ISLink.SetWorkingDirectory(PChar(ExtractFilePath(Executable)));
  ISLink.SetArguments(PChar(Args));

  Result := IPFile.Save(PWideChar(UnicodeString(Filename)), False) = S_OK;
end;

class procedure TFunctions.GetMaxTransparent(Icon: TIcon; var Top, Right: Integer);
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

class function TFunctions.WindowIsFullscreen: Boolean;

  function RectMatches(R: TRect; R2: TRect): Boolean;
  begin
    Result := (R.Left = R2.Left) and (R.Top = R2.Top) and (R.Right = R2.Right) and (R.Bottom = R2.Bottom);
  end;

type
  TGetShellWindow = function(): HWND; stdcall;
var
  i: Integer;
  H: HWND;
  Handle: THandle;
  R: TRect;
  GetShellWindow: TGetShellWindow;
begin
  H := GetForegroundWindow;
  @GetShellWindow := nil;
  Handle := GetModuleHandle('user32.dll');
  if (Handle > 0) then
    @GetShellWindow := GetProcAddress(Handle, 'GetShellWindow');

  if ((H <> GetDesktopWindow) and ((@GetShellWindow <> nil) and (H <> GetShellWindow))) then
  begin
    GetWindowRect(H, R);
    for i := 0 to Pred(Screen.MonitorCount) do
      if RectMatches(Screen.Monitors[i].BoundsRect, R) then
        Exit(True);
  end;

  Exit(False);
end;

class function TFunctions.ShellExecute(const Handle: THandle; const Operation, Filename: string; const Parameters: string = ''): Boolean;
begin
  Exit(Windows.ShellExecute(Handle, PChar(Operation), PChar(Filename), PChar(Parameters), nil, SW_NORMAL) > 32);
end;

class function TFunctions.ReadDataObjectFiles(const DataObject: IDataObject; out Files: TStringArray): Boolean;

  procedure ReadHDrop;
  var
    FormatEtc: TFormatEtc;
    Medium: TStgMedium;
    Filename: array[0..MAX_PATH] of Char;
    i, DroppedFileCount, FilenameLength: Integer;
  begin
    FormatEtc.cfFormat := CF_HDROP;
    FormatEtc.ptd := nil;
    FormatEtc.dwAspect := DVASPECT_CONTENT;
    FormatEtc.lindex := -1;
    FormatEtc.tymed := TYMED_HGLOBAL;
    OleCheck(DataObject.GetData(FormatEtc, Medium));
    try
      DroppedFileCount := DragQueryFileA(Medium.hGlobal, $FFFFFFFF, nil, 0);
      for i := 0 to Pred(DroppedFileCount) do
      begin
        FilenameLength := DragQueryFileA(Medium.hGlobal, i, nil, 0) + 1;
        DragQueryFileA(Medium.hGlobal, i, @Filename[0], FilenameLength);

        Files += [StrPas(@Filename[0])];
      end;
    finally
      ReleaseStgMedium(Medium);
    end;
  end;

var
  EnumFormat: IEnumFormatEtc;
  Fetched: LongWord;
  OLEFormat: TFormatEtc;
begin
  Files := [];

  if Failed(DataObject.EnumFormatEtc(DATADIR_GET, EnumFormat)) then
    Exit(False);

  if Failed(EnumFormat.Reset) then
    Exit(False);

  while EnumFormat.Next(1, OLEFormat, @Fetched) = S_OK do
    case OLEFormat.CfFormat of
      CF_HDROP:
      begin
        ReadHDrop;
        Break;
      end;
    end;

  Result := Length(Files) > 0;
end;

class function TFunctions.ReadDataObjectText(const DataObject: IDataObject; out Text: string): Boolean;

  function ReadText(const Unicode: Boolean): string;
  var
    FormatEtc: TFormatEtc;
    Medium: TStgMedium;
    OLEData, Head: Pointer;
    Chars, CharLen: Integer;
    S: string;
  begin
    Result := '';
    CharLen := IfThen<Integer>(Unicode, SizeOf(WideChar), SizeOf(Char));

    FormatEtc.cfFormat := IfThen<Integer>(Unicode, CF_UNICODETEXT, CF_TEXT);
    FormatEtc.ptd := nil;
    FormatEtc.dwAspect := DVASPECT_CONTENT;
    FormatEtc.lindex := -1;
    FormatEtc.tymed := TYMED_HGLOBAL;

    if (DataObject.QueryGetData(FormatEtc) = S_OK) and (DataObject.GetData(FormatEtc, Medium) = S_OK) then
    begin
      OLEData := GlobalLock(Medium.hGlobal);
      if Assigned(OLEData) then
      begin
        Chars := 0;
        Head := OLEData;
        try
          while PChar(Head)^ <> #0 do
          begin
            Head := Pointer(PtrUInt(Head) + CharLen);
            Inc(Chars);
          end;

          if Unicode then
            SetString(S, PWideChar(OLEData), Chars)
          else
            SetString(S, PChar(OLEData), Chars);

          Exit(S);
        finally
          GlobalUnlock(Medium.hGlobal);
        end;
      end;
      ReleaseStgMedium(Medium);
    end;
  end;

var
  EnumFormat: IEnumFormatEtc;
  Fetched: LongWord;
  OLEFormat: TFormatEtc;
begin
  Text := '';

  if Failed(DataObject.EnumFormatEtc(DATADIR_GET, EnumFormat)) then
    Exit(False);

  if Failed(EnumFormat.Reset) then
    Exit(False);

  while EnumFormat.Next(1, OLEFormat, @Fetched) = S_OK do
    case OLEFormat.CfFormat of
      CF_TEXT:
      begin
        Text := ReadText(False);
        Break;
      end;
      CF_UNICODETEXT:
      begin
        Text := ReadText(True);
        Break;
      end;
    end;

  Result := not string.IsNullOrWhiteSpace(Text);
end;

class function TFunctions.ShutdownBlockReasonCreate(hWnd: HWND; pwszReason: string): Boolean;
begin
  if not Assigned(FShutdownBlockReasonCreate) then
    Exit(False);

  Exit(FShutdownBlockReasonCreate(hWnd, PWideChar(UnicodeString(pwszReason))));
end;

class function TFunctions.ShutdownBlockReasonDestroy(hWnd: HWND): Boolean;
begin
  if not Assigned(FShutdownBlockReasonDestroy) then
    Exit(False);

  Exit(FShutdownBlockReasonDestroy(hWnd));
end;

class function TFunctions.LocalToUTC(const DateTime: TDateTime): TDateTime;
var
  ST: SYSTEMTIME;
begin
  DateTimeToSystemTime(DateTime, ST);
  // "TzSpecificLocalTimeToSystemTime takes into account whether daylight saving time (DST) is in effect for the local time to be converted."
  if not FTzSpecificLocalTimeToSystemTime(nil, @ST, @ST) then
    raise Exception.Create('TzSpecificLocalTimeToSystemTime() failed: %d'.Format([GetLastError]));
  Exit(SystemTimeToDateTime(ST));
end;

class function TFunctions.DateTimeToFileTime(const DateTime: TDateTime): FILETIME;
var
  ST: TSystemTime;
  FT: TFileTime;
begin
  DateTimeToSystemTime(DateTime, ST);
  if not SystemTimeToFileTime(ST, FT) then
    raise Exception.Create('SystemTimeToFileTime() failed: %d'.Format([GetLastError]));
  Exit(FT);
end;

var
  LibraryHandle: THandle;

initialization
  LibraryHandle := GetModuleHandle(user32);
  if LibraryHandle <> 0 then
  begin
    TFunctions.FShutdownBlockReasonCreate := GetProcAddress(LibraryHandle, 'ShutdownBlockReasonCreate');
    TFunctions.FShutdownBlockReasonDestroy := GetProcAddress(LibraryHandle, 'ShutdownBlockReasonDestroy');
  end;

  LibraryHandle := GetModuleHandle(kernel32);
  if LibraryHandle <> 0 then
    TFunctions.FTzSpecificLocalTimeToSystemTime := GetProcAddress(LibraryHandle, 'TzSpecificLocalTimeToSystemTime');
end.
