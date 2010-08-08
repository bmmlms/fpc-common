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
unit UpdateClient;

interface

uses
  Windows, SysUtils, StrUtils, Classes, AppData, AppDataBase,
  HTTPThread, Functions, ShellApi, LanguageObjects;

type
  TUpdateAction = (uaVersion, uaUpdate);

  TUpdateThread = class(THTTPThread)
  private
    FUpdateAction: TUpdateAction;
    FFoundVersion: TAppVersion;
    FUpdateURL: string;
    FChangeLog: string;

    FOnVersionFound: TNotifyEvent;
    FOnUpdateDownloaded: TNotifyEvent;
    FOnError: TNotifyEvent;

    function GetValue(Data, Name: AnsiString): AnsiString;
  protected
    procedure DoReceivedData(Buf: Pointer; Len: Integer); override;
    procedure DoEnded; override;
  public
    constructor Create(Action: TUpdateAction; URL: string);
  end;

  TUpdateClient = class
  private
    FThread: TUpdateThread;

    FAction: TUpdateAction;
    FUpdateLength: Integer;
    FPercent: Integer;
    FFoundVersion: TAppVersion;
    FUpdateURL: string;
    FLanguage: string;
    FUpdateFile: string;
    FChangeLog: string;

    FOnUpdateFound: TNotifyEvent;
    FOnNoUpdateFound: TNotifyEvent;
    FOnDownloadProgress: TNotifyEvent;
    FOnUpdateDownloaded: TNotifyEvent;
    FOnError: TNotifyEvent;

    procedure ThreadVersionFound(Sender: TObject);
    procedure ThreadDownloadPercentProgress(Sender: TObject);
    procedure ThreadUpdateDownloaded(Sender: TObject);
    procedure ThreadError(Sender: TObject);
    procedure ThreadEnded(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(Action: TUpdateAction);
    procedure RunUpdate(Handle: Cardinal = 0);
    procedure SetVersion(Data: string);

    property Action: TUpdateAction read FAction;
    property UpdateLength: Integer read FUpdateLength;
    property Percent: Integer read FPercent;
    property FoundVersion: TAppVersion read FFoundVersion;
    property UpdateFile: string read FUpdateFile;
    property UpdateURL: string read FUpdateURL write FUpdateURL;
    property Language: string read FLanguage write FLanguage;
    property ChangeLog: string read FChangeLog;

    property OnUpdateFound: TNotifyEvent read FOnUpdateFound write FOnUpdateFound;
    property OnNoUpdateFound: TNotifyEvent read FOnNoUpdateFound write FOnNoUpdateFound;
    property OnDownloadProgress: TNotifyEvent read FOnDownloadProgress write FOnDownloadProgress;
    property OnUpdateDownloaded: TNotifyEvent read FOnUpdateDownloaded write FOnUpdateDownloaded;
    property OnError: TNotifyEvent read FOnError write FOnError;
  end;

implementation

{ TUpdateThread }

constructor TUpdateThread.Create(Action: TUpdateAction; URL: string);
begin
  if Action = uaVersion then
    inherited Create(URL)
  else
    inherited Create(URL);
  FUpdateAction := Action;
  FChangeLog := '';
end;

function TUpdateThread.GetValue(Data, Name: AnsiString): AnsiString;
var
  n, n2: Integer;
  Data2: AnsiString;
begin
  Data2 := AnsiString(AnsiLowerCase(string(Data)));
  Name := AnsiString(AnsiLowerCase(string(Name)) + '=');
  Result := '';
  n := Pos(string(Name), string(Data2));
  if n > 0 then
  begin
    n2 := PosEx(#10, string(Data), n);
    if n2 > 0 then
      Result := AnsiString(Copy(string(Data), n + Length(Name), n2 - n - Length(Name)))
    else
      Result := AnsiString(Copy(string(Data), n + Length(Name), Length(Data)));
  end;
  Result := AnsiString(Trim(string(Result)));
end;

procedure TUpdateThread.DoEnded;
var
  Dots: array[0..3] of Integer;
  Version: array[0..3] of Word;
  Data, sVersion: AnsiString;
  CL: AnsiString;
begin
  inherited;
  if FTypedStream.ResponseCode = 200 then
  begin
    case FUpdateAction of
      uaVersion:
        if (FTypedStream.HeaderRemoved) and (RecvDataStream.Size > 0) then
        begin
          try
            Data := RecvDataStream.ToString(0, RecvDataStream.Size);
            sVersion := GetValue(Data, 'version');
            Dots[0] := Pos('.', string(sVersion));
            Dots[1] := PosEx('.', string(sVersion), Dots[0] + 1);
            Dots[2] := PosEx('.', string(sVersion), Dots[1] + 1);
            Dots[3] := PosEx('.', string(sVersion), Dots[2] + 1);
            Version[0] :=  StrToInt(Copy(string(sVersion), 0, Dots[0] - 1));
            Version[1] :=  StrToInt(Copy(string(sVersion), Dots[0] + 1, Dots[1] - 1 - Dots[0]));
            Version[2] :=  StrToInt(Copy(string(sVersion), Dots[1] + 1, Dots[2] - 1 - Dots[1]));
            Version[3] :=  StrToInt(Copy(string(sVersion), Dots[2] + 1, Length(sVersion) - Dots[3] - 1));
            FFoundVersion.Major := Version[0];
            FFoundVersion.Minor := Version[1];
            FFoundVersion.Revision := Version[2];
            FFoundVersion.Build := Version[3];
            FFoundVersion.AsString := IntToStr(FFoundVersion.Major) + '.' +
              IntToStr(FFoundVersion.Minor) + '.' + IntToStr(FFoundVersion.Revision) +
              '.' + IntToStr(FFoundVersion.Build);
            FUpdateURL := string(GetValue(Data, 'updateurl'));
            CL := AnsiString(StringReplace(string(GetValue(Data, 'changelog')), '\r', #13#10, [rfReplaceAll]));
            FChangeLog := UTF8ToUnicodeString(CL);
            if FUpdateURL = '' then
              raise Exception.Create('-');
            Sync(FOnVersionFound);
          except
            Sync(FOnError);
          end;
        end else
        begin
          Sync(FOnError);
        end;
      uaUpdate:
        if (RecvDataStream.Size = FTypedStream.ContentLength) and (RecvDataStream.Size > 1024) then
        begin
          Sync(FOnUpdateDownloaded);
        end else
          Sync(FOnError);
    end;
  end else
    Sync(FOnError);
end;

procedure TUpdateThread.DoReceivedData(Buf: Pointer; Len: Integer);
begin
  inherited;

end;

{ TUpdateClient }

constructor TUpdateClient.Create;
begin
  FUpdateURL := '';
  FLanguage := 'en';
  FChangeLog := '';
  FUpdateFile := AppGlobals.TempDir + LowerCase(AppGlobals.AppName) + 'update.exe';
  FUpdateLength := 0;
  FPercent := 0;
end;

destructor TUpdateClient.Destroy;
begin
  if FThread <> nil then
    FThread.Terminate;
  inherited;
end;

procedure TUpdateClient.RunUpdate(Handle: Cardinal);
var
  osvi: _OSVERSIONINFOW;
  ConditionMask: DWORDLONG;
  op: Integer;
  SEI: TShellExecuteInfo;
const
  VER_GREATER_EQUAL = 3;
begin
  op := VER_GREATER_EQUAL;

  ZeroMemory(@osvi, SizeOf(_OSVERSIONINFOW));
  osvi.dwOSVersionInfoSize := SizeOf(_OSVERSIONINFOW);
  osvi.dwMajorVersion := 6;
  osvi.dwMinorVersion := 0;

  ConditionMask := 0;
  ConditionMask := VerSetConditionMask(ConditionMask, VER_MAJORVERSION, op);
  ConditionMask := VerSetConditionMask(ConditionMask, VER_MINORVERSION, op);

  // Bei >= Vista gehts über das Manifest, ansonsten 'runas'...

  if IsAdmin then
    RunProcess('"' + FUpdateFile + '" /NOICONS /SP /SILENT /UPDATE /RUN /PATH="' + AppGlobals.AppPath + '"')
  else
  begin
    if VerifyVersionInfo(osvi, VER_MAJORVERSION or VER_MINORVERSION, ConditionMask) then
      RunProcess('"' + FUpdateFile + '" /NOICONS /SP /SILENT /UPDATE /PATH="' + AppGlobals.AppPath + '"')
    else
    begin
      MsgBox(Handle, _('You do not have administrative rights.'#13#10'Please enter the credentials of a user with administrative rights now.'), _('Info'), MB_ICONINFORMATION);

      FillChar(SEI, SizeOf(SEI), 0);
      SEI.cbSize := SizeOf(SEI);
      SEI.Wnd := Handle;
      SEI.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
      SEI.lpVerb := 'runas';
      SEI.lpFile := PChar(FUpdateFile);
      SEI.lpParameters := PChar('/NOICONS /SP /SILENT /UPDATE /PATH="' + AppGlobals.AppPath + '"');
      SEI.nShow := SW_SHOWNORMAL;
      ShellExecuteEx(@SEI);
    end;
  end;
end;

procedure TUpdateClient.SetVersion(Data: string);
var
  Dots: array[0..3] of Integer;
  Version: array[0..3] of Word;
begin
  Dots[0] := Pos('.', Data);
  Dots[1] := PosEx('.', Data, Dots[0] + 1);
  Dots[2] := PosEx('.', Data, Dots[1] + 1);
  Dots[3] := PosEx('.', Data, Dots[2] + 1);
  Version[0] :=  StrToInt(Copy(Data, 0, Dots[0] - 1));
  Version[1] :=  StrToInt(Copy(Data, Dots[0] + 1, Dots[1] - 1 - Dots[0]));
  Version[2] :=  StrToInt(Copy(Data, Dots[1] + 1, Dots[2] - 1 - Dots[1]));
  Version[3] :=  StrToInt(Copy(Data, Dots[2] + 1, Length(Data) - Dots[3] - 1));
  FFoundVersion.Major := Version[0];
  FFoundVersion.Minor := Version[1];
  FFoundVersion.Revision := Version[2];
  FFoundVersion.Build := Version[3];
  FFoundVersion.AsString := Format('%d.%d.%d.%d', [FFoundVersion.Major,
    FFoundVersion.Minor, FFoundVersion.Revision, FFoundVersion.Build]);
end;

procedure TUpdateClient.Start(Action: TUpdateAction);
var
  URL: string;
begin
  if FThread <> nil then
    FThread.Terminate;
  FAction := Action;
  if Action = uaVersion then
  begin
    {$IFDEF DEBUG}
    if FLanguage <> '' then
      URL := 'http://mistake.gaia/' + Trim(FLanguage) + '/projekte/update/' + LowerCase(AppGlobals.AppName) + '/'
    else
      URL := 'http://mistake.gaia/en/projekte/update/' + LowerCase(AppGlobals.AppName) + '/';
    {$ELSE}
    if FLanguage <> '' then
      URL := 'http://mistake.ws/' + Trim(FLanguage) + '/projekte/update/' + LowerCase(AppGlobals.AppName) + '/'
    else
      URL := 'http://mistake.ws/en/projekte/update/' + LowerCase(AppGlobals.AppName) + '/';
    {$ENDIF}
    FThread := TUpdateThread.Create(Action, URL)
  end else
    FThread := TUpdateThread.Create(Action, FUpdateURL);
  if AppGlobals.ProxyEnabled then
  begin
    FThread.ProxyEnabled := True;
    FThread.ProxyHost := AppGlobals.ProxyHost;
    FThread.ProxyPort := AppGlobals.ProxyPort;
  end;
  FThread.FUpdateURL := FUpdateURL;
  FThread.FOnVersionFound := ThreadVersionFound;
  FThread.OnDownloadPercentProgress := ThreadDownloadPercentProgress;
  FThread.FOnUpdateDownloaded := ThreadUpdateDownloaded;
  FThread.FOnError := ThreadError;
  FThread.OnEnded := ThreadEnded;
  FThread.Start;
end;

procedure TUpdateClient.ThreadVersionFound(Sender: TObject);
var
  Newer: Boolean;
  MajorEq, MinorEq, RevisionEq: Boolean;
begin
  FFoundVersion := FThread.FFoundVersion;
  FUpdateURL := FThread.FUpdateURL;
  FChangeLog := FThread.FChangeLog;

  MajorEq := FFoundVersion.Major = AppGlobals.AppVersion.Major;
  MinorEq := FFoundVersion.Minor = AppGlobals.AppVersion.Minor;
  RevisionEq := FFoundVersion.Revision = AppGlobals.AppVersion.Revision;

  Newer := (FFoundVersion.Major > AppGlobals.AppVersion.Major) or
           (MajorEq and (FFoundVersion.Minor > AppGlobals.AppVersion.Minor)) or
           (MajorEq and MinorEq and (FFoundVersion.Revision > AppGlobals.AppVersion.Revision)) or
           (MajorEq and MinorEq and RevisionEq and (FFoundVersion.Build > AppGlobals.AppVersion.Build));

  if Newer then
  begin
    if Assigned(FOnUpdateFound) then
      FOnUpdateFound(Self);
  end else
  begin
    if Assigned(FOnNoUpdateFound) then
      FOnNoUpdateFound(Self);
  end;
end;

procedure TUpdateClient.ThreadEnded(Sender: TObject);
begin
  FThread := nil;
end;

procedure TUpdateClient.ThreadDownloadPercentProgress(Sender: TObject);
begin
  if FThread.Received = 0 then
    Exit;
  if FThread.FTypedStream.ResponseCode <> 200 then
    Exit;
  FUpdateLength := FThread.FTypedStream.ContentLength;
  FPercent := FThread.DownloadPercent;
  if Assigned(FOnDownloadProgress) then
    FOnDownloadProgress(Self);
end;

procedure TUpdateClient.ThreadUpdateDownloaded(Sender: TObject);
begin
  try
    FThread.RecvDataStream.SaveToFile(FUpdateFile);
    if Assigned(FOnUpdateDownloaded) then
      FOnUpdateDownloaded(Self);
  except
    if Assigned(FOnError) then
      FOnError(Self);
  end;
end;

procedure TUpdateClient.ThreadError(Sender: TObject);
begin
  if Assigned(FOnError) then
    FOnError(Self);
end;

end.
