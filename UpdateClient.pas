{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2014 Alexander Nottelmann

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
  HTTPThread, Functions, ShellApi, LanguageObjects, Sockets,
  Forms;

type
  TUpdateAction = (uaVersion, uaUpdate);

  TUpdateThread = class(THTTPThread)
  private
    FUpdateAction: TUpdateAction;
    FFoundVersion: TAppVersion;
    FUpdateURL: string;
    FChangeLog: string;

    FOnVersionFound: TSocketEvent;
    FOnUpdateDownloaded: TSocketEvent;
    FOnError: TSocketEvent;

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

    FError: Boolean;
    FURLIndex: Integer;
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
    function FGetActive: Boolean;

    procedure ThreadVersionFound(Sender: TSocketThread);
    procedure ThreadDownloadPercentProgress(Sender: TSocketThread);
    procedure ThreadUpdateDownloaded(Sender: TSocketThread);
    procedure ThreadError(Sender: TSocketThread);
    procedure ThreadEnded(Sender: TSocketThread);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(Action: TUpdateAction; StartOver: Boolean);
    procedure RunUpdate(Handle: Cardinal = 0);
    procedure Kill;

    property Action: TUpdateAction read FAction;
    property UpdateLength: Integer read FUpdateLength;
    property Percent: Integer read FPercent;
    property FoundVersion: TAppVersion read FFoundVersion write FFoundVersion;
    property UpdateFile: string read FUpdateFile;
    property UpdateURL: string read FUpdateURL write FUpdateURL;
    property Language: string read FLanguage write FLanguage;
    property ChangeLog: string read FChangeLog;

    property Active: Boolean read FGetActive;
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
  // Das muss vor das inherited(), weil es dadrin benutzt wird.
  FUserAgent := AnsiString(AppGlobals.AppName) + '/' + AppGlobals.AppVersion.AsString;

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
  Data, Version: AnsiString;
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
            Version := GetValue(Data, 'version');
            FFoundVersion := ParseVersion(Version);
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
        end else if (not Terminated) then                 
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
  Kill;

  inherited;
end;

function TUpdateClient.FGetActive: Boolean;
begin
  Result := FThread <> nil;
end;

procedure TUpdateClient.Kill;
begin
  if FThread <> nil then
  begin
    try
      //TerminateThread(FThread.Handle, 1);
      FThread.Terminate;
      while FThread <> nil do
        Application.ProcessMessages;
      //FThread.Free;
    except
    end;
    FThread := nil;
  end;
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

procedure TUpdateClient.Start(Action: TUpdateAction; StartOver: Boolean);
var
  URL: string;
begin
  if StartOver then
    FURLIndex := 0;

  if FThread <> nil then
    FThread.Terminate;
  FAction := Action;
  if Action = uaVersion then
  begin
    if (FLanguage <> '') and (AppGlobals.WebLanguages.IndexOf(LowerCase(FLanguage)) > -1) then
      URL := AppGlobals.ProjectUpdateLinks[FURLIndex] + Trim(FLanguage) + '/projekte/update/' + LowerCase(AppGlobals.AppName) + '/'
    else
      URL := AppGlobals.ProjectUpdateLinks[FURLIndex] + 'en/projekte/update/' + LowerCase(AppGlobals.AppName) + '/';

    FThread := TUpdateThread.Create(Action, URL)
  end else
    FThread := TUpdateThread.Create(Action, FUpdateURL);
  if AppGlobals.ProxyEnabled then
  begin
    FThread.ProxyEnabled := True;
    FThread.ProxyHost := AppGlobals.ProxyHost;
    FThread.ProxyPort := AppGlobals.ProxyPort;
  end;
  FThread.UseSynchronize := True;
  FThread.FUpdateURL := FUpdateURL;
  FThread.FOnVersionFound := ThreadVersionFound;
  FThread.OnDownloadPercentProgress := ThreadDownloadPercentProgress;
  FThread.FOnUpdateDownloaded := ThreadUpdateDownloaded;
  FThread.FOnError := ThreadError;
  FThread.OnEnded := ThreadEnded;
  FThread.Start;
end;

procedure TUpdateClient.ThreadVersionFound(Sender: TSocketThread);
begin
  FFoundVersion := FThread.FFoundVersion;
  FUpdateURL := FThread.FUpdateURL;
  FChangeLog := FThread.FChangeLog;

  if IsVersionNewer(AppGlobals.AppVersion, FFoundVersion) then
  begin
    if Assigned(FOnUpdateFound) then
      FOnUpdateFound(Self);
  end else
  begin
    if Assigned(FOnNoUpdateFound) then
      FOnNoUpdateFound(Self);
  end;
end;

procedure TUpdateClient.ThreadEnded(Sender: TSocketThread);
begin
  FThread := nil;

  if FError and (FURLIndex < High(AppGlobals.ProjectUpdateLinks)) then
  begin
    FError := False;
    Inc(FURLIndex);
    Start(FAction, False);
  end;
end;

procedure TUpdateClient.ThreadDownloadPercentProgress(Sender: TSocketThread);
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

procedure TUpdateClient.ThreadUpdateDownloaded(Sender: TSocketThread);
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

procedure TUpdateClient.ThreadError(Sender: TSocketThread);
begin
  FError := True;

  if FURLIndex = High(AppGlobals.ProjectUpdateLinks) then
  begin
    if Assigned(FOnError) then
      FOnError(Self);
  end;
end;

end.
