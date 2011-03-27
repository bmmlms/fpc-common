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
unit ServerSocketThread;

interface

uses
  SysUtils, Windows, WinSock, Classes;

type
  TServerSocketThread = class(TThread)
  private
    FProc: TNotifyEvent;
    FSender: TObject;
    procedure Sync2;
  protected
    procedure Execute; override;

    procedure Sync(Proc: TNotifyEvent); overload;
    procedure Sync(Proc: TNotifyEvent; Sender: TObject); overload;

    procedure DoClientConnected(Handle: Cardinal); virtual;
    procedure DoEnded; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TServerSocketThread }

constructor TServerSocketThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

destructor TServerSocketThread.Destroy;
begin

  inherited;
end;

procedure TServerSocketThread.Sync(Proc: TNotifyEvent);
begin
  Sync(Proc, Self);
end;

procedure TServerSocketThread.Sync(Proc: TNotifyEvent; Sender: TObject);
begin
  if Assigned(Proc) then
  begin
    FProc := Proc;
    FSender := Sender;
    Synchronize(Sync2);
  end;
end;

procedure TServerSocketThread.Sync2;
begin
  FProc(FSender);
end;

procedure TServerSocketThread.DoClientConnected(Handle: Cardinal);
begin

end;

procedure TServerSocketThread.DoEnded;
begin

end;

procedure TServerSocketThread.Execute;
var
  FAcceptHandle: Integer;
  FSocketHandle: Integer;
  Addr: sockaddr_in;
  NonBlock: Integer;
  timeout: TimeVal;
  readfds, exceptfds: TFdSet;
  Res: Integer;
begin
  inherited;

  FAcceptHandle := socket(AF_INET, SOCK_STREAM, 0);
  if FAcceptHandle = SOCKET_ERROR then
    raise Exception.Create('socket() Fehler');

  try
    NonBlock := 1;
    if ioctlsocket(FAcceptHandle, FIONBIO, NonBlock) = SOCKET_ERROR then
      raise Exception.Create('ioctlsocket() Error');

    Addr.sin_family := AF_INET;
    Addr.sin_Port := htons(6000);
    Addr.sin_Addr.S_Addr := inet_addr('127.0.0.1');

    if bind(FAcceptHandle, Addr, sizeof(Addr)) = SOCKET_ERROR then
      raise Exception.Create('bind() error');

    if listen(FAcceptHandle, 5) = SOCKET_ERROR then
      raise Exception.Create('listen() error');

    while True do
    begin
      timeout.tv_sec := 0;
      timeout.tv_usec := 1000;

      FD_ZERO(readfds);
      FD_ZERO(exceptfds);
      FD_SET(FAcceptHandle, readfds);
      FD_SET(FAcceptHandle, exceptfds);

      Res := select(0, @readfds, nil, @exceptfds, @timeout);

      if Res = SOCKET_ERROR then
        raise Exception.Create('select() error: ' + IntToStr(Res));

      if Terminated then
        Exit;

      if (Res > 0) and (FD_ISSET(FAcceptHandle, exceptfds)) then
        raise Exception.Create('select() Socket error');

      if (Res <> SOCKET_ERROR) and (FD_ISSET(FAcceptHandle, readfds)) then
      begin
        FSocketHandle := accept(FAcceptHandle, nil, nil);
        DoClientConnected(FSocketHandle);
      end;
    end;
  finally
    try
      DoEnded;
    except

    end;
    closesocket(FAcceptHandle);
  end;
end;

end.
