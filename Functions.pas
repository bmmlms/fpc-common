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

unit Functions;

interface

uses
  Windows, ShLwApi, SysUtils, Classes, StrUtils, Graphics, PerlRegEx,
  DateUtils;

type
  TPatternReplace = record
    C: Char;
    Replace: string;
  end;
  TPatternReplaceArray = array of TPatternReplace;

  TAppVersion = record
    Major, Minor, Revision, Build: Byte;
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

function MsgBox(Handle: HWND; Text, Title: string; uType: Cardinal): Integer;
function ValidURL(URL: string): Boolean;
function StripURL(URL: string): string;
function ParseURL(URL: string): TParseURLRes; overload;
function GetSystemDir: string;
function GetTempDir: string;
function ExtractLastDirName(s: string): string;
function RemoveFileExt(const s: string): string;
function StringToMask(s: string): string;
function RPos(SubStr, S: string): Integer;
function Like(AString, APattern: string): Boolean;
function DownCase(ch: Char): Char;
function MakeSize(Size: UInt64): string;
function DiskSpaceOkay(Path: string; MinSpaceGB: Int64): Boolean;
procedure FindFiles(PathPattern: string; Files: TStringList; SubDirs: Boolean = False; TerminateFlag: PBoolean = nil);
function RunProcess(Filename, WorkingDir: string; Timeout: Cardinal; var Output: AnsiString;
  var ExitCode: DWORD; TerminateFlag: PBoolean; KillOnTimeout: Boolean; ReadCallback: TReadCallback = nil): TRunProcessResults; overload;
function RunProcess(Filename: string; var Handle: Cardinal; Hide: Boolean = False): Boolean; overload;
function RunProcess(Filename: string; Hide: Boolean = False): Boolean; overload;
function GetCPUCount: DWord;
function BeautifyFilePath(const s: string; MaxPathChars: Integer): string;
function HashString(Value: string): Cardinal;
function IsAnsi(const s: string): Boolean;
function OccurenceCount(C: Char; Str: string): Integer;
function PatternReplace(S: string; ReplaceList: TPatternReplaceArray): string;
function IsAdmin: LongBool;
function HTML2Color(const HTML: string): Integer;
function GetFileSize(const AFilename: string): Int64;
function CmpInt(const A, B: Int64; R: Boolean = False): Integer;
function CmpUInt64(const A, B: UInt64; R: Boolean = False): Integer;
function ParseVersion(const Version: string): TAppVersion; overload;
function ParseVersion(const Major, Minor, Revision, Build: Cardinal): TAppVersion; overload;
function IsVersionNewer(const Current, Found: TAppVersion): Boolean;
//procedure GetBitmap(const Resname: string; const NumGlyphs: Integer; Bmp: TBitmap);
function BuildPattern(const s: string; var Hash: Cardinal; var NumChars: Integer; AlwaysAlter: Boolean): string;
function CryptStr(const s: string): string;
function TryRelativePath(const s: string; IsFile: Boolean): string;
function TryUnRelativePath(const s: string): string;
function FixPathName(Path: string): string;
function GetFileVersion(Filename: string): TAppVersion;
function ShortenString(Str: string; Len: Integer): string;
procedure Explode(const Separator, S: string; Lst: TStringList);
function RegExReplace(RegEx, ReplaceWith, Data: string): string;
function ContainsRegEx(RegEx, Data: string): Boolean;
function ExistsIconSize(const Name: string; const Size: Integer): Boolean;
function LocalToUTC(DT: TDateTime): TDateTime;

function VerSetConditionMask(dwlConditionMask: LONGLONG; TypeBitMask: DWORD; ConditionMask: Byte): LONGLONG; stdcall;
  external 'kernel32.dll';

implementation

function MsgBox(Handle: HWND; Text, Title: string; uType: Cardinal): Integer;
begin
  Result := MessageBox(Handle, PChar(Text), PChar(Title), uType);
end;

function ValidURL(URL: string): Boolean;
begin
  Result := IsAnsi(URL);
  if Result then
    Result := ParseURL(URL).Success;
end;

function StripURL(URL: string): string;
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

function ParseURL(URL: string): TParseURLRes;
var
  i, n, HostStart: Integer;
  Host2, Port2, Data2: string;
  URL2: string;
begin
  Result.URL := URL;
  Result.Host := '';
  Result.Port := 0;
  Result.Data := '';
  Result.PortDetected := False;
  Result.Secure := False;
  Result.Success := False;

  URL := Trim(URL);
  URL2 := LowerCase(URL);

  if (Copy(URL2, 0, 7) <> 'http://') and (Copy(URL2, 0, 8) <> 'https://') then
  begin
    URL2 := 'http://' + URL2;
    URL := 'http://' + URL;
  end;

  if Copy(URL2, 0, 8) = 'https://' then
    Result.Secure := True;

  HostStart := PosEx('://', URL2) + 3;

  Host2 := ''; Port2 := ''; Data2 := '';
  i := PosEx('/', URL2, HostStart);
  if i = 0 then
  begin
    // No get-data because no further slash
    Host2 := Copy(URL, HostStart, 100);
    n := PosEx(':', Host2, 1);
    if n = 0 then
    begin
      // No port, set default
      if Result.Secure then
        Port2 := '443'
      else
        Port2 := '80';
    end else
    begin
      Port2 := Copy(Host2, n + 1, 10);
      Host2 := Copy(Host2, 1, n - 1);
      Result.PortDetected := True;
      {
      if n > 1 then
      begin
        n := n + 8;
        if URL[n - 1] <> '/' then
          URL := Copy(URL, 1, n - 2) + '/' + Copy(URL, n - 1, Length(URL));
      end;
      }
    end;
    if URL[Length(URL)] <> '/' then
      URL := URL + '/';
    Data2 := '';
  end else
  begin
    Host2 := Copy(URL, HostStart, Length(URL2));
    n := PosEx('/', Host2, 1);
    if n = 0 then
    begin
      // No more slashes => No getdata, check for port..
      n := PosEx(':', URL2, i);
      if n > 0 then
      begin
        // Set the port
        Port2 := Copy(URL2, n + 1, 100);
        Result.PortDetected := True;
        // Remove last slash and stupid stuff
        n := PosEx('/', Port2, 1);
        if n > 0 then
        begin
          Port2 := Copy(Port2, 0, Length(Port2) - n);
          Result.PortDetected := True;
        end;
      end else
      begin
        // Default to 80
        if Result.Secure then
          Port2 := '443'
        else
          Port2 := '80';
      end;
    end else
    begin
      n := PosEx(':', Host2, 1);
      if n > 0 then
      begin
        i := PosEx('/', Host2, 1);
        Data2 := Copy(Host2, i, 1000);
        Port2 := Copy(Host2, n + 1, i - n - 1);
        Result.PortDetected := True;
        Host2 := Copy(Host2, 1, n - 1);
      end else
      begin
        n := PosEx('/', Host2, 1);
        Data2 := Copy(Host2, n, 1000);
        // No port - default to 80
        if Result.Secure then
          Port2 := '443'
        else
          Port2 := '80';
        Host2 := Copy(Host2, 1, n - 1);
      end;
    end;
  end;

  if (StrToIntDef(Port2, -1) <> -1) and (Host2 <> '') then
  begin
    Result.Host := Host2;
    Result.Port := StrToInt(Port2);
    Result.Data := Data2;
    Result.Success := True;
    if Result.Data = '' then
      Result.Data := '/';
  end;
end;

function GetSystemDir: string;
var
  Dir: array [0..MAX_PATH] of Char;
begin
  Result := '';
  if GetSystemDirectory(Dir, MAX_PATH) <> 0 then
  begin
    Result := StrPas(Dir);
    if Result[Length(Result)] <> '\' then
      Result := Result + '\';
  end;
end;

function GetTempDir: string;
var
  Dir: array [0..MAX_PATH] of Char;
begin
  Result := '';
  if GetTempPath(MAX_PATH, Dir) <> 0 then
  begin
    Result := StrPas(Dir);
    if Result[Length(Result)] <> '\' then
      Result := Result + '\';
  end;
end;

function ExtractLastDirName(s: string): string;
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

function RemoveFileExt(const s: string): string;
var
  i: Integer;
begin
  for i := Length(s) downto 1 do
    if s[i] = '.' then
    begin
      Result := Copy(s, 1, Length(s) - (Length(s) - i) - 1);
      Exit;
    end;
  Result := s;
end;

function StringToMask(s: string): string;
begin
  Result := '*' + StringReplace(Trim(s), ' ', '*', [rfReplaceAll]) + '*';
end;

function RPos(SubStr, S: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Length(S) - Length(SubStr) + 1 downto 1 do
  begin
    if Copy(S, i, Length(SubStr)) = SubStr then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function Like(AString, APattern: string): Boolean;
var
  StringPtr, PatternPtr: PChar;
  StringRes, PatternRes: PChar;
begin
  AString := WideLowerCase(AString);
  APattern := WideLowerCase(APattern);
  Result:=false;
  StringPtr:=PChar(AString);
  PatternPtr:=PChar(APattern);
  StringRes:=nil;
  PatternRes:=nil;
  repeat
    repeat // ohne vorangegangenes "*"
      case PatternPtr^ of
        #0: begin
          Result:=StringPtr^=#0;
          if Result or (StringRes=nil) or (PatternRes=nil) then
            Exit;
          StringPtr:=StringRes;
          PatternPtr:=PatternRes;
          Break;
        end;
        '*': begin
          inc(PatternPtr);
          PatternRes:=PatternPtr;
          Break;
        end;
        '?': begin
          if StringPtr^=#0 then
            Exit;
          inc(StringPtr);
          inc(PatternPtr);
        end;
        else begin
          if StringPtr^=#0 then
            Exit;
          if StringPtr^<>PatternPtr^ then begin
            if (StringRes=nil) or (PatternRes=nil) then
              Exit;
            StringPtr:=StringRes;
            PatternPtr:=PatternRes;
            Break;
          end
          else begin
            inc(StringPtr);
            inc(PatternPtr);
          end;
        end;
      end;
    until false;
    repeat
      case PatternPtr^ of
        #0: begin
          Result:=true;
          Exit;
        end;
        '*': begin
          inc(PatternPtr);
          PatternRes:=PatternPtr;
        end;
        '?': begin
          if StringPtr^=#0 then
            Exit;
          inc(StringPtr);
          inc(PatternPtr);
        end;
        else begin
          repeat
            if StringPtr^=#0 then
              Exit;
            if StringPtr^=PatternPtr^ then
              Break;
            inc(StringPtr);
          until false;
          inc(StringPtr);
          StringRes:=StringPtr;
          inc(PatternPtr);
          Break;
        end;
      end;
    until false;
  until false;
end;

function DownCase(ch: Char): Char;
begin
  case ch of
    'Ä': Result := 'ä';
    'Ö': Result := 'ö';
    'Ü': Result := 'ü';
    'A'..'Z': Result := Chr(Ord(ch) or 32);
    else
      Result := ch;
  end;
end;

function MakeSize(Size: UInt64): string;
begin
  if Size < 1048576 then
    Result := Format('%f KB', [Size / (1024)])
  else if Size < 1073741824 then
    Result := Format('%f MB', [Size / (1024 * 1024)])
  else
    Result := Format('%f GB', [Size / (1024 * 1024 * 1024)])
end;

function DiskSpaceOkay(Path: string; MinSpaceGB: Int64): Boolean;
var
  S1, S2, S3: Int64;
  SetMin: TLargeInteger;
begin
  if Path = '' then
    Exit(True);

  Result := False;
  if Pos('\', Path) > 0 then
    Path := Copy(Path, 1, Pos('\', Path));
  if GetDiskFreeSpaceEx(PChar(Path), S1, S2, @S3) then
  begin
    SetMin := MinSpaceGB * 1073741824;
    if S1 > SetMin then
      Result := True;
  end;
end;

procedure FindFiles(PathPattern: string; Files: TStringList; SubDirs: Boolean = False;
  TerminateFlag: PBoolean = nil);
var
  SR: TSearchRec;
  Dir, Pattern: string;
  Dirs: TStringList;
begin
  Files.Clear;

  Dir := IncludeTrailingBackslash(ExtractFilePath(PathPattern));
  Pattern := ExtractFileName(PathPattern);
  Dirs := TStringList.Create;
  try
    repeat
      if (TerminateFlag <> nil) and TerminateFlag^ then
      begin
        Exit;
      end;

      if FindFirst(Dir + Pattern, faAnyFile, SR) = 0 then
      begin
        repeat
          if (SR.Name <> '.') and (SR.Name <> '..') and (not (SR.Attr and faDirectory = faDirectory)) then
          begin
            if SubDirs then
              Files.Add(Dir + SR.Name)
            else
              Files.Add(SR.Name);
          end;
        until FindNext(SR) <> 0;
        FindClose(SR);
      end;

      if SubDirs then
      begin
        if FindFirst(Dir + '*', faDirectory, SR) = 0 then
        begin
          repeat
            if (SR.Name <> '.') and (SR.Name <> '..') and (SR.Attr and faDirectory = faDirectory) then
              Dirs.Add(Dir + SR.Name + '\')
          until FindNext(SR) <> 0;
          FindClose(SR);
        end;
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

function RunProcess(Filename, WorkingDir: string; Timeout: Cardinal; var Output: AnsiString;
  var ExitCode: DWORD; TerminateFlag: PBoolean; KillOnTimeout: Boolean; ReadCallback: TReadCallback = nil): TRunProcessResults; overload;
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
  Started: Cardinal;
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
  OK := CreateProcess(nil, @Filename[1], nil, nil, True,
    CREATE_NEW_PROCESS_GROUP or NORMAL_PRIORITY_CLASS or CREATE_NO_WINDOW,
    nil, @WorkingDir[1], SI, PI);
  try
    if OK then
    begin
      Handle := PI.hProcess;

      Result := rpWin;

      Started := GetTickCount;
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
        except end;

        PeekNamedPipe(ReadPipeOut, nil, 0, nil, @Avail, nil);
        if Avail > 0 then
        begin
          SetLength(Tmp, Avail);
          ReadFile(ReadPipeOut, Tmp[1], Avail, ReadCount, nil);

          if Assigned(ReadCallback) then
            ReadCallback(Tmp);

          Output := Output + Tmp;

          Started := GetTickCount;
        end;

        if Started + Timeout < GetTickCount then
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

function RunProcess(Filename: string; var Handle: Cardinal; Hide: Boolean): Boolean;
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
  OK := CreateProcess(nil, @Filename[1], nil, nil, False,
    CREATE_NEW_PROCESS_GROUP or NORMAL_PRIORITY_CLASS,
    nil, nil, SI, PI);
  Result := OK;
  if OK then
  begin
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
    Handle := PI.hProcess;
  end;
end;

function RunProcess(Filename: string; Hide: Boolean): Boolean;
var
  Dummy: Cardinal;
begin
  Result := RunProcess(Filename, Dummy, Hide);
end;

function GetCPUCount: DWord;
var
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  Result := SysInfo.dwNumberOfProcessors;
end;

function BeautifyFilePath(const s: string; MaxPathChars: Integer): string;
var
  i: Integer;
  CharSep: Integer;
begin
  Result := s;
  CharSep := 0;

  for i := 0 to Length(s) - 1 do
    if s[i] = '\' then
    begin
      CharSep := i;
    end;

  if CharSep > 0 then
    if CharSep - 1 > MaxPathChars then
    begin
      Result := Copy(s, 0, MaxPathChars - 3);
      Result := Result + '...';
      Result := Result + Copy(s, CharSep, Length(s) - CharSep + 1);
    end;
end;

function HashString(Value: string): Cardinal;
var
  i: Integer;
  x: Cardinal;
begin
  Result := 0;
  for i := 1 To Length(Value) do begin
    Result := (Result Shl 4) + Ord(Value[i]);
    x := Result and $F0000000;
    if (x <> 0) then
      Result := Result xor (x shr 24);
    Result := Result and (not x);
  end;
end;

function IsAnsi(const s: string): Boolean;
var
  s2: AnsiString;
  i: Integer;
begin
  Result := True;
  SetLength(s2, Length(s));
  Move(s[1], s2[1], Length(s));
  i := 2;
  while i <= Length(s2) do
  begin
    if s2[i] <> #0 then
    begin
      Result := False;
      Exit;
    end;
    Inc(i, 2);
  end;
end;

function OccurenceCount(C: Char; Str: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Str) do
    if Str[i] = C then
      Inc(Result);
end;

function PatternReplace(S: string; ReplaceList: TPatternReplaceArray): string;
var
  C: Char;
  i, n, j: Integer;
  Replace: string;
  TokenIndices: array of Integer;
begin
  Result := s;

  SetLength(TokenIndices, 0);
  for i := 1 to Length(Result) - 1 do
    if Result[i] = '%' then
    begin
      for j := 0 to High(ReplaceList) do
        if (Length(Result) >= i + 1) and (Result[i + 1] = ReplaceList[j].C) then
        begin
          SetLength(TokenIndices, Length(TokenIndices) + 1);
          TokenIndices[High(TokenIndices)] := i;
        end;
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

function GetAdminSid: PSID;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID: DWORD = $00000020;
  DOMAIN_ALIAS_RID_ADMINS: DWORD = $00000220;
begin
  Result := nil;
  AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
    SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
    0, 0, 0, 0, 0, 0, Result);
end;

function IsAdmin: LongBool;
var
  TokenHandle: THandle;
  ReturnLength: DWORD;
  TokenInformation: PTokenGroups;
  AdminSid: PSID;
  Loop: Integer;
begin
  Result := False;
  TokenHandle := 0;
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
  try
    ReturnLength := 0;
    GetTokenInformation(TokenHandle, TokenGroups, nil, 0, ReturnLength);
    TokenInformation := GetMemory(ReturnLength);
    if Assigned(TokenInformation) then
    try
      if GetTokenInformation(TokenHandle, TokenGroups, TokenInformation,
        ReturnLength, ReturnLength) then
      begin
        AdminSid := GetAdminSid;
        for Loop := 0 to TokenInformation^.GroupCount - 1 do
        begin
          if EqualSid(TokenInformation^.Groups[Loop].Sid, AdminSid) then
          begin
            Result := True;
            Break;
          end;
        end;
        FreeSid(AdminSid);
      end;
    finally
      FreeMemory(TokenInformation);
    end;
  finally
    CloseHandle(TokenHandle);
  end;
end;

function HTML2Color(const HTML: string): Integer;
var
  Offset: Integer;
begin
  if Copy(HTML, 1, 1) = '#' then
    Offset := 1
  else
    Offset := 0;
  Result := Integer(StrToInt('$' + Copy(HTML, Offset + 1, 2))) +
    Integer(StrToInt('$' + Copy(HTML, Offset + 3, 2))) shl 8 +
    Integer(StrToInt('$' + Copy(HTML, Offset + 5, 2))) shl 16;
end;

function GetFileSize(const AFileName: string): Int64;
var
  FileStream: TFileStream;
begin
  Result := -1;
  try
    FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      try
        Result := FileStream.Size;
      except
        Result := 0;
      end;
    finally
      FileStream.Free;
    end;
  except
  end;
end;

function CmpInt(const A, B: Int64; R: Boolean): Integer;
begin
  if not R then
  begin
    if A > B then
      Result := 1
    else if A < B then
      Result := -1
    else
      Result := 0;
  end else
  begin
    if A < B then
      Result := 1
    else if A > B then
      Result := -1
    else
      Result := 0;
  end;
end;

function CmpUInt64(const A, B: UInt64; R: Boolean): Integer;
begin
  if not R then
  begin
    if A > B then
      Result := 1
    else if A < B then
      Result := -1
    else
      Result := 0;
  end else
  begin
    if A < B then
      Result := 1
    else if A > B then
      Result := -1
    else
      Result := 0;
  end;
end;

function ParseVersion(const Version: string): TAppVersion;
var
  Dots: array[0..2] of Integer;
  i: Integer;
begin
  Dots[0] := Pos('.', string(Version));
  Dots[1] := PosEx('.', string(Version), Dots[0] + 1);
  Dots[2] := PosEx('.', string(Version), Dots[1] + 1);
  for i := 0 to Length(Dots) - 1 do
    if Dots[i] < i then
      raise Exception.Create('Error parsing version.');
  Result.Major :=  StrToInt(Copy(string(Version), 0, Dots[0] - 1));
  Result.Minor :=  StrToInt(Copy(string(Version), Dots[0] + 1, Dots[1] - 1 - Dots[0]));
  Result.Revision :=  StrToInt(Copy(string(Version), Dots[1] + 1, Dots[2] - 1 - Dots[1]));
  Result.Build :=  StrToInt(Copy(string(Version), Dots[2] + 1, Length(Version) - Dots[2]));
  Result.AsString := AnsiString(Format('%d.%d.%d.%d', [Result.Major, Result.Minor,
    Result.Revision, Result.Build]));
end;

function ParseVersion(const Major, Minor, Revision, Build: Cardinal): TAppVersion;
begin
  Result.Major := Major;
  Result.Minor := Minor;
  Result.Revision := Revision;
  Result.Build := Build;
  Result.AsString := AnsiString(Format('%d.%d.%d.%d', [Major, Minor, Revision, Build]));
end;

function IsVersionNewer(const Current, Found: TAppVersion): Boolean;
var
  MajorEq, MinorEq, RevisionEq: Boolean;
begin
  MajorEq := Found.Major = Current.Major;
  MinorEq := Found.Minor = Current.Minor;
  RevisionEq := Found.Revision = Current.Revision;

  Result := (Found.Major > Current.Major) or
            (MajorEq and (Found.Minor > Current.Minor)) or
            (MajorEq and MinorEq and (Found.Revision > Current.Revision)) or
            (MajorEq and MinorEq and RevisionEq and (Found.Build > Current.Build));
end;

{
procedure GetBitmap(const ResName: string; const NumGlyphs: Integer; Bmp: TBitmap);
var
  i, j, k: Integer;
  Grayshade, Red, Green, Blue: Byte;
  PixelColor: Longint;
  Icon: TIcon;
  B: TBitmap;
begin
  Icon := TIcon.Create;
  try
    Icon.LoadFromResourceName(HInstance, ResName);

    Bmp.Width := 16 * NumGlyphs;
    Bmp.Height := 16;

    for i := 0 to NumGlyphs - 1 do
    begin
      B := TBitmap.Create;
      B.Width := 32;
      B.Height := 32;

      B.Canvas.Draw(0, 0, Icon);
      B.Canvas.StretchDraw(Rect(0, 0, 16, 16), B);
      B.Width := 16;
      B.Height := 16;

      if i = 1 then
        for j := 0 to B.Width - 1 do
          for k := 0 to B.Height - 1 do
          begin
            PixelColor := ColorToRGB(B.Canvas.Pixels[j, k]);
            Red := PixelColor;
            Green := PixelColor shr 8;
            Blue := PixelColor shr 16;

            Grayshade := Round(0.3 * Red + 0.6 * Green + 0.1 * Blue);
            B.Canvas.Pixels[j, k] := RGB(Grayshade, Grayshade, Grayshade);
          end;

      Bmp.Canvas.Draw(i * 16, 0, B);

      FreeAndNil(B);
    end;

    Bmp.PixelFormat := pf24bit;
  finally
    Icon.Free;
  end;
end;
}

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
      begin

      end else
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

function BuildPattern(const s: string; var Hash: Cardinal; var NumChars: Integer; AlwaysAlter: Boolean): string;
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

function CryptStr(const s: string): string;
var
  i: Integer;
begin
  SetLength(Result, Length(s));
  if Length(s) = 0 then
    Exit;
  for i := 1 to Length(s) do
    Result[i] := Chr(Ord(s[i]) xor $45);
end;

function TryRelativePath(const s: string; IsFile: Boolean): string;
var
  From: string;
  T: Integer;
  OutData: array[0..MAX_PATH - 1] of Char;
begin
  if IsFile then
    T := FILE_ATTRIBUTE_NORMAL
  else
    T := FILE_ATTRIBUTE_DIRECTORY;

  From := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)));

  if PathRelativePathTo(@OutData[0], PChar(From), T, PChar(s), 0) then
    Result := OutData
  else
    Result := s;
end;

function TryUnRelativePath(const s: string): string;
var
  From: string;
  OutData: array[0..MAX_PATH - 1] of Char;
begin
  if (((Length(s) >= 2) and (s[1] = '.') and (s[2] = '\')) or
      ((Length(s) >= 3) and (s[1] = '.') and (s[2] = '.') and (s[3] = '\'))) then
  begin
    From := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)));

    if PathCanonicalize(@OutData[0], PChar(From + s)) then
      Result := OutData
    else
      Result := s;
  end else
    Result := s;
end;

function FixPathName(Path: string): string;
var
  i, LastI: Integer;
  Drive: string;
  Parts: array of string;

  procedure AddPart(S: string);
  begin
    SetLength(Parts, Length(Parts) + 1);
    Parts[High(Parts)] := Trim(S);
  end;
begin
  Result := '';
  Drive := '';
  SetLength(Parts, 0);

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
      AddPart(Copy(Path, LastI, i - LastI));
      LastI := i + 1;
    end;

  // Wenn am Ende noch Rest ist, ist das auch ein Part
  if Length(Path) > LastI then
  begin
    AddPart(Copy(Path, LastI, Length(Path) - LastI + 1));
  end;

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
  for i := 0 to High(Parts) do
    if Parts[i] <> '' then
    begin
      if (Length(Result) > 1) and (Result[Length(Result)] <> '\') then
        Result := Result + '\' + Parts[i]
      else
        Result := Result + Parts[i]
    end;

  // Wenn in der Eingangsvariable am Ende ein '\' war, dann auch anhängen
  if (Length(Path) >= 1) and (Path[Length(Path)] = '\') and (Result <> '') then
    Result := Result + '\';
end;

function GetFileVersion(Filename: string): TAppVersion;
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
      begin
        if VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then
        begin
          with VerValue^ do
          begin
            Result.Major := dwFileVersionMS shr 16;
            Result.Minor := dwFileVersionMS and $FFFF;
            Result.Revision := dwFileVersionLS shr 16;
            Result.Build := dwFileVersionLS and $FFFF;
          end;
          Result.AsString := AnsiString(Format('%d.%d.%d.%d', [Result.Major,
            Result.Minor, Result.Revision, Result.Build]));
        end;
      end;
    finally
      FreeMem(VerInfo,VerInfoSize);
    end;
  end;

  if (Result.Major = 0) and (Result.Minor = 0) and
     (Result.Revision = 0) and (Result.Build = 0) then
  begin
    raise Exception.Create('');
  end;
end;

function ShortenString(Str: string; Len: Integer): string;
begin
  Result := Trim(Str);
  if Length(Result) > Len then
  begin
    Result := Trim(Copy(Result, 1, Len - 3)) + '...';
  end;
end;

procedure Explode(const Separator, S: string; Lst: TStringList);
var
  SepLen: Integer;
  F, P: PChar;
  ALen, Index: Integer;
  Res: array of string;
  i: Integer;
begin
  Lst.Clear;

  try
    SetLength(Res, 0);

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

function RegExReplace(RegEx, ReplaceWith, Data: string): string;
var
  R: TPerlRegEx;
begin
  Result := '';
  R := TPerlRegEx.Create;
  try
    R.Options := R.Options + [preCaseLess];
    R.Subject := Data;
    R.RegEx := RegEx;
    R.Replacement := ReplaceWith;
    try
      // Das muss so. Sonst wird z.B. aus 'ft. ft. ft. ft.' ein 'Feat. ft. Feat. ft.'
      repeat until not R.ReplaceAll;
      Result := R.Subject;
    except end;
  finally
    R.Free;
  end;
end;

function ContainsRegEx(RegEx, Data: string): Boolean;
var
  R: TPerlRegEx;
begin
  Result := False;
  R := TPerlRegEx.Create;
  try
    R.Options := R.Options + [preCaseLess];
    R.Subject := Data;
    R.RegEx := RegEx;
    try
      Result := R.Match;
    except end;
  finally
    R.Free;
  end;
end;

function ExistsIconSize(const Name: string; const Size: Integer): Boolean;
var
  R: TResourceStream;
  Width, Height: Byte;
  IconCount: WORD;
  i: Integer;
begin
  Result := False;

  try
    R := TResourceStream.Create(HInstance, Name, RT_GROUP_ICON);
  except
    Exit(False);
  end;

  try
    R.Seek(SizeOf(WORD) * 2, soFromCurrent);
    R.ReadBuffer(IconCount, SizeOf(IconCount));
    for i := 0 to IconCount - 1 do
    begin
      R.ReadBuffer(Width, SizeOf(Width));
      R.ReadBuffer(Height, SizeOf(Height));

      if (Width = Size) and (Height = Size) then
        Exit(True);

      R.Seek(SizeOf(Byte) * 2 + SizeOf(WORD) * 3 + SizeOf(DWORD), soFromCurrent);
    end;
  finally
    R.Free;
  end;
end;

function LocalToUTC(DT: TDateTime): TDateTime;
var
  Res: Cardinal;
  TZI: TTimeZoneInformation;
const
  Minute = (1 / 24) / 60;
begin
  Result := DT;
  try
    // Exception unter Vista und Wine manchmal... also so gelöst!
    Result := TTimeZone.Local.ToUniversalTime(Now)
  except
    Res := GetTimeZoneInformation(TZI);
    if Res <> TIME_ZONE_ID_INVALID then
      Result := DT + (Minute * TZI.Bias)
  end;
end;

end.
