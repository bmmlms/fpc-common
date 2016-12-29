{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2017 Alexander Nottelmann

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

unit DownloadClient;

interface

uses
  Windows, SysUtils, StrUtils, Classes, AppData, AppDataBase, HTTPThread,
  Functions, ShellApi, LanguageObjects, Sockets;

type
  TDownloadThread = class(THTTPThread)
  private
    FOnFileDownloaded: TSocketEvent;
    FOnError: TSocketEvent;
  protected
    procedure DoReceivedData(Buf: Pointer; Len: Integer); override;
    procedure DoEnded; override;
  public
    constructor Create(URL: string); reintroduce;
  end;

  TDownloadClient = class
  private
    FThread: TDownloadThread;

    FURL: string;
    FDownloadLength: Integer;
    FPercent: Integer;

    FOnDownloadProgress: TNotifyEvent;
    FOnDownloaded: TNotifyEvent;
    FOnError: TNotifyEvent;
    function FGetActive: Boolean;

    procedure ThreadDownloadPercentProgress(Sender: TSocketThread);
    procedure ThreadFileDownloaded(Sender: TSocketThread);
    procedure ThreadError(Sender: TSocketThread);
    procedure ThreadEnded(Sender: TSocketThread);
  public
    constructor Create(URL: string);
    destructor Destroy; override;

    procedure Start;
    procedure Kill;

    property DownloadLength: Integer read FDownloadLength;
    property Percent: Integer read FPercent;

    property Thread: TDownloadThread read FThread;
    property Active: Boolean read FGetActive;
    property OnDownloadProgress: TNotifyEvent read FOnDownloadProgress write FOnDownloadProgress;
    property OnDownloaded: TNotifyEvent read FOnDownloaded write FOnDownloaded;
    property OnError: TNotifyEvent read FOnError write FOnError;
  end;

implementation

{ TDownloadThread }

constructor TDownloadThread.Create(URL: string);
begin
  inherited Create(URL, AppGlobals.CheckCertificate);

end;

procedure TDownloadThread.DoEnded;
begin
  inherited;
  if FTypedStream.ResponseCode = 200 then
  begin
    if (RecvDataStream.Size = FTypedStream.ContentLength) and (RecvDataStream.Size > 1024) then
    begin
      Sync(FOnFileDownloaded);
    end else
      Sync(FOnError);
  end else
    Sync(FOnError);
end;

procedure TDownloadThread.DoReceivedData(Buf: Pointer; Len: Integer);
begin
  inherited;

end;

{ TDownloadClient }

constructor TDownloadClient.Create(URL: string);
begin
  FURL := URL;
  FDownloadLength := 0;
  FPercent := 0;
end;

destructor TDownloadClient.Destroy;
begin
  if FThread <> nil then
    FThread.Terminate;
  inherited;
end;

function TDownloadClient.FGetActive: Boolean;
begin
  Result := FThread <> nil;
end;

procedure TDownloadClient.Kill;
begin
  if FThread <> nil then
  begin
    try
      TerminateThread(FThread.Handle, 1);
      FThread.Free;
    except
    end;
    FThread := nil;
  end;
end;

procedure TDownloadClient.Start;
begin
  if FThread <> nil then
    FThread.Terminate;

  FThread := TDownloadThread.Create(FURL);

  AppGlobals.Lock;
  try
    if AppGlobals.ProxyEnabled then
    begin
      FThread.ProxyEnabled := True;
      FThread.ProxyHost := AppGlobals.ProxyHost;
      FThread.ProxyPort := AppGlobals.ProxyPort;
    end;
  finally
    AppGlobals.Unlock;
  end;
  FThread.OnDownloadPercentProgress := ThreadDownloadPercentProgress;
  FThread.FOnFileDownloaded := ThreadFileDownloaded;
  FThread.FOnError := ThreadError;
  FThread.OnEnded := ThreadEnded;
  FThread.Start;
end;

procedure TDownloadClient.ThreadEnded(Sender: TSocketThread);
begin
  FThread := nil;
end;

procedure TDownloadClient.ThreadDownloadPercentProgress(Sender: TSocketThread);
begin
  if FThread = nil then
    Exit;
  if FThread.Received = 0 then
    Exit;
  if FThread.FTypedStream.ResponseCode <> 200 then
    Exit;
  FDownloadLength := FThread.FTypedStream.ContentLength;
  FPercent := FThread.DownloadPercent;
  if Assigned(FOnDownloadProgress) then
    FOnDownloadProgress(Self);
end;

procedure TDownloadClient.ThreadFileDownloaded(Sender: TSocketThread);
begin
  if FThread = nil then
    Exit;
  try
    if Assigned(FOnDownloaded) then
      FOnDownloaded(Self);
  except
    if Assigned(FOnError) then
      FOnError(Self);
  end;
end;

procedure TDownloadClient.ThreadError(Sender: TSocketThread);
begin
  if Assigned(FOnError) then
    FOnError(Self);
end;

end.
