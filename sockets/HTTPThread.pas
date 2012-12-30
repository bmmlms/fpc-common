{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2013 Alexander Nottelmann

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

unit HTTPThread;

interface

uses
  SysUtils, Windows, WinSock, Classes, Sockets, ExtendedStream, HTTPStream,
  Functions;

type
  THTTPThread = class(TSocketThread)
  private
    FSpeed: Integer;
    FSpeedReceived: Integer;
    FLastReceivedUpdate: Cardinal;
    procedure StreamHeaderRemoved(Sender: TObject); virtual;
  protected
    FTypedStream: THTTPStream;
    FDownloadPercent: Integer;

    FOnSpeedChanged: TSocketEvent;
    FOnDownloadProgress: TSocketEvent;
    FOnDownloadPercentProgress: TSocketEvent;
    FURL: string;
    FURLHost: string;
    FPostData: string;
    FProxyEnabled: Boolean;
    FProxyHost: string;
    FProxyPort: Integer;
    FUserAgent: AnsiString;

    procedure SetSendParams;
    procedure FSetPostData(Value: string);
    procedure FSetProxyEnabled(Value: Boolean);
    procedure FSetProxyHost(Value: string);
    procedure FSetProxyPort(Value: Integer);

    function FGetRecvDataStream: TExtendedStream;

    procedure DoReceivedData(Buf: Pointer; Len: Integer); override;
    procedure DoDisconnected; override;

    procedure DoHeaderRemoved; virtual;
    procedure DoSpeedChange; virtual;
    procedure DoDownloadProgress; virtual;
    procedure DoDownloadPercentProgress; virtual;
  public
    constructor Create(URL: string); overload; virtual;
    constructor Create(URL: string; Stream: TSocketStream); overload; virtual;
    destructor Destroy; override;

    property RecvDataStream: TExtendedStream read FGetRecvDataStream;
    property DownloadPercent: Integer read FDownloadPercent;
    property Speed: Integer read FSpeed;
    property PostData: string read FPostData write FSetPostData;
    property ProxyEnabled: Boolean read FProxyEnabled write FSetProxyEnabled;
    property ProxyHost: string read FProxyHost write FSetProxyHost;
    property ProxyPort: Integer read FProxyPort write FSetProxyPort;
    property UserAgent: AnsiString read FUserAgent write FUserAgent;

    property OnSpeedChanged: TSocketEvent read FOnSpeedChanged write FOnSpeedChanged;
    property OnDownloadProgress: TSocketEvent read FOnDownloadProgress write FOnDownloadProgress;
    property OnDownloadPercentProgress: TSocketEvent read FOnDownloadPercentProgress write FOnDownloadPercentProgress;
  end;

implementation

{ THTTPThread }

procedure THTTPThread.SetSendParams;
var
  Host, Data: string;
  SendData: AnsiString;
  Port: Integer;
begin
  ParseURL(FURL, Host, Port, Data);

  FURLHost := Host;

  if FPostData <> '' then
    SendData := 'POST '
  else
    SendData := 'GET ';

  if FProxyEnabled then
  begin
    Self.Host := FProxyHost;
    Self.Port := FProxyPort;
    SendData := SendData + AnsiString(FURL) + ' HTTP/1.1'#13#10
  end else
  begin
    SendData := SendData + AnsiString(Data) + ' HTTP/1.1'#13#10;
  end;
  SendData := SendData + 'Host: ' + AnsiString(Host) + #13#10;
  SendData := SendData + 'Accept: */*'#13#10;
  SendData := SendData + 'User-Agent: mhttplib/' + FUserAgent + #13#10;
  SendData := SendData + 'Connection: close'#13#10;
  if FPostData <> '' then
    SendData := SendData + 'Content-Length: ' + AnsiString(IntToStr(Length(FPostData))) + #13#10;
  SendData := SendData + #13#10;
  if FPostData <> '' then
    SendData := SendData + AnsiString(FPostData);

  FSendStream.SetData(SendData);
end;

procedure THTTPThread.StreamHeaderRemoved(Sender: TObject);
begin
  DoHeaderRemoved;

  // Das muss hier bleiben, wegen Vergleich mit Received
  // in DoDisconnected()..
  FReceived := FTypedStream.Size;
end;

constructor THTTPThread.Create(URL: string);
begin
  Create(URL, THTTPStream.Create);
end;

constructor THTTPThread.Create(URL: string; Stream: TSocketStream);
var
  Host, Data: string;
  Port: Integer;
begin
  FURL := URL;
  ParseURL(URL, Host, Port, Data);

  inherited Create(Host, Port, Stream);
  FTypedStream := THTTPStream(FRecvStream);
  FTypedStream.OnHeaderRemoved := StreamHeaderRemoved;
  FSpeed := 0;
  FSpeedReceived := 0;
  FLastReceivedUpdate := 0;
  FDownloadPercent := 0;
  FPostData := '';
  FProxyEnabled := False;
  FProxyHost := '';
  FProxyPort := 0;
  FUserAgent := '';

  {$IFDEF DEBUG}
  FDataTimeout := 100000;
  {$ELSE}
  FDataTimeout := 20000;
  {$ENDIF}

  SetSendParams;
end;

destructor THTTPThread.Destroy;
begin

  inherited;
end;

procedure THTTPThread.DoDownloadProgress;
begin
  if Assigned(FOnDownloadProgress) then
    Sync(FOnDownloadProgress);
end;

procedure THTTPThread.DoDisconnected;
begin
  inherited;
  if FTypedStream.ContentLength > -1 then
    if FTypedStream.ContentLength <> Received then
    begin
      raise Exception.Create('ContentLength <> Received');
    end;
end;

procedure THTTPThread.DoDownloadPercentProgress;
begin
  if Assigned(FOnDownloadPercentProgress) then
    Sync(FOnDownloadPercentProgress);
end;

procedure THTTPThread.DoHeaderRemoved;
begin

end;

procedure THTTPThread.DoReceivedData(Buf: Pointer; Len: Integer);
var
  P: Integer;
begin
  inherited;

  if (not Terminated) and (FTypedStream.HeaderRemoved) and
     (FLastReceivedUpdate + 1000 < GetTickCount) then
  begin
    FSpeed := Received - FSpeedReceived;
    FLastReceivedUpdate := GetTickCount;
    DoSpeedChange;
    FSpeedReceived := Received;
  end;

  DoDownloadProgress;

  if FTypedStream.ContentLength > 0 then
  begin
    p := Round(Received / FTypedStream.ContentLength * 100);
    if p <> FDownloadPercent then
    begin
      FDownloadPercent := p;
      DoDownloadPercentProgress;
    end;
  end;
end;

procedure THTTPThread.DoSpeedChange;
begin
  Sync(FOnSpeedChanged);
end;

function THTTPThread.FGetRecvDataStream: TExtendedStream;
begin
  Result := FTypedStream.RecvStream as TExtendedStream;
end;

procedure THTTPThread.FSetPostData(Value: string);
begin
  FPostData := Value;
  SetSendParams;
end;

procedure THTTPThread.FSetProxyEnabled(Value: Boolean);
begin
  FProxyEnabled := Value;
  SetSendParams;
end;

procedure THTTPThread.FSetProxyHost(Value: string);
begin
  FProxyHost := Value;
  SetSendParams;
end;

procedure THTTPThread.FSetProxyPort(Value: Integer);
begin
  FProxyPort := Value;
  SetSendParams;
end;

end.
