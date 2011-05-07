unit Logging;

interface

uses
  Windows, SysUtils, Classes, ShlObj, GUIFunctions;

type
  TLogger = class
  private
    class function GetDesktopDir: string;
  public
    class procedure Init;
    class procedure Write(Data: string);
  end;

var
  LoggingFile: string;

implementation

{ TLogger }

class function TLogger.GetDesktopDir: string;
begin
  Result := GetShellFolder(CSIDL_DESKTOP);
  if (Trim(Result) <> '') then
  begin
    Result := IncludeTrailingPathDelimiter(Result) + '\';
  end;
end;

class procedure TLogger.Init;
begin
  LoggingFile := GetDesktopDir + 'streamwriter_log.txt';
end;

class procedure TLogger.Write(Data: string);
const
  FILE_APPEND_DATA = 4;
var
  H: THandle;
  W: Cardinal;
begin
  Exit;

  H := CreateFile(PChar(LoggingFile), FILE_APPEND_DATA, FILE_SHARE_READ or FILE_SHARE_WRITE, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if H = INVALID_HANDLE_VALUE then
  begin
    H := CreateFile(PChar(LoggingFile), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  end;

  if H <> INVALID_HANDLE_VALUE then
  begin
    Data := TimeToStr(Now) + ' - ' + Data + #13#10;
    WriteFile(H, Data[1], Length(Data) * SizeOf(Char), W, nil);

    FileClose(H);
  end;

end;

initialization
  TLogger.Init;

end.
