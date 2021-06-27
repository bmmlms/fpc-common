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
    FLastReceivedUpdate: UInt64;
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

    procedure SetSendParams;
    procedure FSetPostData(Value: string);
    procedure FSetProxyEnabled(Value: Boolean);
    procedure FSetProxyHost(Value: string);
    procedure FSetProxyPort(Value: Integer);

    function FGetRecvDataStream: TExtendedStream;

    procedure DoReceivedData(Buf: Pointer; Len: Integer); override;
    procedure DoDisconnected; override;
    procedure DoException(E: Exception); override;

    procedure DoHeaderRemoved; virtual;
    procedure DoSpeedChange; virtual;
    procedure DoDownloadProgress; virtual;
    procedure DoDownloadPercentProgress; virtual;
  public
    constructor Create(URL: string; CheckCertificate: Boolean); overload; virtual;
    constructor Create(URL: string; Stream: TSocketStream; CheckCertificate: Boolean); overload; virtual;
    destructor Destroy; override;

    property RecvDataStream: TExtendedStream read FGetRecvDataStream;
    property DownloadPercent: Integer read FDownloadPercent;
    property Speed: Integer read FSpeed;
    property PostData: string read FPostData write FSetPostData;
    property ProxyEnabled: Boolean read FProxyEnabled write FSetProxyEnabled;
    property ProxyHost: string read FProxyHost write FSetProxyHost;
    property ProxyPort: Integer read FProxyPort write FSetProxyPort;

    property OnSpeedChanged: TSocketEvent read FOnSpeedChanged write FOnSpeedChanged;
    property OnDownloadProgress: TSocketEvent read FOnDownloadProgress write FOnDownloadProgress;
    property OnDownloadPercentProgress: TSocketEvent read FOnDownloadPercentProgress write FOnDownloadPercentProgress;
  end;

implementation

{ THTTPThread }

procedure THTTPThread.SetSendParams;
var
  SendData: string;
  Res: TParseURLRes;
begin
  Res := ParseURL(FURL);

  FURLHost := Res.Host;

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
    SendData := SendData + AnsiString(Res.Data) + ' HTTP/1.1'#13#10;
  end;
  SendData := SendData + 'Host: ' + AnsiString(Host) + #13#10;
  SendData := SendData + 'Accept: */*'#13#10;
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

constructor THTTPThread.Create(URL: string; CheckCertificate: Boolean);
begin
  Create(URL, THTTPStream.Create, CheckCertificate);
end;

constructor THTTPThread.Create(URL: string; Stream: TSocketStream; CheckCertificate: Boolean);
var
  Res: TParseURLRes;
begin
  FURL := URL;
  Res := ParseURL(URL);

  inherited Create(Res.Host, Res.Port, Stream, Res.Secure, CheckCertificate);
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

procedure THTTPThread.DoException(E: Exception);
begin
  inherited;

  FSpeed := 0;
  DoSpeedChange();
end;

procedure THTTPThread.DoDisconnected;
begin
  inherited;

  FSpeed := 0;
  DoSpeedChange();

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
     (FLastReceivedUpdate + 1000 < GetTickCount64) then
  begin
    FSpeed := Received - FSpeedReceived;
    FLastReceivedUpdate := GetTickCount64;
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
