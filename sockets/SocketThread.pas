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
unit SocketThread;

interface

uses
  SysUtils, Windows, WinSock, Classes, SyncObjs, SocketStream, ExtendedStream,
  LanguageObjects;

type
  TSocketThread = class(TThread)
  private
    FSocketHandle: TSocket;
    FHost: string;
    FPort: Integer;
    FSendLock: TCriticalSection;

    FDebugMsg: string;
    FDebugData: string;
    FDebugType: Integer;
    FDebugLevel: Integer;

    FOnDebug: TNotifyEvent;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnBeforeEnded: TNotifyEvent;
    FOnEnded: TNotifyEvent;
    FOnException: TNotifyEvent;

    FProc: TNotifyEvent;
    procedure Sync2;

    function HostToAddress(Host: string): Integer;
  protected
    FRecvStream: TSocketStream;
    FSendStream: TExtendedStream;

    FReceived: UInt64;
    FError: Boolean;
    FClosed: Boolean;

    procedure Execute; override;

    procedure WriteDebug(Text, Data: string; T, Level: Integer); overload;
    procedure WriteDebug(Text: string; T, Level: Integer); overload;
    procedure StreamDebug(Sender: TObject);

    procedure Sync(Proc: TNotifyEvent);

    procedure DoStuff; virtual;
    procedure DoDebug(Text, Data: string; T, Level: Integer);
    procedure DoConnecting; virtual;
    procedure DoConnected; virtual;
    procedure DoDisconnected; virtual;
    procedure DoDisconnectedEvent;
    procedure DoEnded; virtual;
    procedure DoBeforeEndedEvent;
    procedure DoEndedEvent;
    procedure DoReceivedData(Buf: Pointer; Len: Integer); virtual;
    procedure DoException(E: Exception); virtual;
  public
    constructor Create(Handle: Cardinal; Stream: TSocketStream); overload;
    constructor Create(Host: string; Port: Integer; Stream: TSocketStream); overload;
    destructor Destroy; override;

    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;

    property DebugMsg: string read FDebugMsg;
    property DebugData: string read FDebugData;
    property DebugType: Integer read FDebugType;
    property DebugLevel: Integer read FDebugLevel;
    property Received: UInt64 read FReceived;
    property Error: Boolean read FError;

    property SendLock: TCriticalSection read FSendLock;
    property SendStream: TExtendedStream read FSendStream;

    property OnDebug: TNotifyEvent read FOnDebug write FOnDebug;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnBeforeEnded: TNotifyEvent read FOnBeforeEnded write FOnBeforeEnded;
    property OnEnded: TNotifyEvent read FOnEnded write FOnEnded;
  end;

implementation

{ TSocketThread }

function TSocketThread.HostToAddress(Host: string): Integer;
type
  PPInAddr = ^PInAddr;
var
  HostInfo: PHostEnt;
  T: PPInAddr;
begin
  T := nil;
  Result := inet_addr(PAnsiChar(AnsiString(Host)));
  if Result = Integer(INADDR_NONE) then
  begin
    Result := 0;
    HostInfo := gethostbyname(PAnsiChar(AnsiString(Host)));
    if HostInfo <> nil then
      T := Pointer(HostInfo^.h_addr_list);
    if (T <> nil) and (T^ <> nil) then
      Result := T^^.S_addr;
  end;
  if Result = 0 then
    raise Exception.Create(Format(_('Host "%s" could not be resolved'), [Host]));
end;

procedure TSocketThread.StreamDebug(Sender: TObject);
begin
  WriteDebug(FRecvStream.DebugMsg, FRecvStream.DebugData, FRecvStream.DebugType, FRecvStream.DebugLevel);
end;

procedure TSocketThread.Sync(Proc: TNotifyEvent);
begin
  if Assigned(Proc) then
  begin
    FProc := Proc;
    Synchronize(Sync2);
  end;
end;

procedure TSocketThread.Sync2;
begin
  FProc(Self);
end;

procedure TSocketThread.Execute;
var
  Addr: sockaddr_in;
  Res, RecvRes, SendRes: Integer;
  readfds, writefds, exceptfds: TFdSet;
  timeout: TimeVal;
  Buf: array[0..8191] of Byte;
  Host: Integer;
  NonBlock: Integer;
  Ticks, StartTime: Cardinal;
  LastTimeReceived, LastTimeSent: Cardinal;
const
  {$IFDEF DEBUG}
  DataTimeout = 100000;
  {$ELSE}
  DataTimeout = 10000;
  {$ENDIF}
begin
  inherited;

  FClosed := False;
  try
    try
      if FSocketHandle = 0 then
      begin
        // Das ist vor dem echten Verbinden, damit Fehler bei HostToAddress() auf
        // mit ins Log reinkommen.
        DoConnecting;

        Host := HostToAddress(FHost);

        FSocketHandle := socket(AF_INET, SOCK_STREAM, 0);
        if FSocketHandle = SOCKET_ERROR then
          raise Exception.Create(_('socket() failed'));

        NonBlock := 1;
        if ioctlsocket(FSocketHandle, FIONBIO, NonBlock) = SOCKET_ERROR then
          raise Exception.Create(_('ioctlsocket() failed'));

        Addr.sin_family := AF_INET;
        Addr.sin_port := htons(FPort);
        Addr.sin_addr.S_addr := Host;

        connect(FSocketHandle, Addr, SizeOf(Addr));

        StartTime := GetTickCount;
        while True do
        begin
          if Terminated then
            Exit;
          Ticks := GetTickCount;
          timeout.tv_sec := 0;
          timeout.tv_usec := 100;
          FD_ZERO(writefds);
          FD_ZERO(exceptfds);
          FD_SET(FSocketHandle, writefds);
          FD_SET(FSocketHandle, exceptfds);
          Res := select(0, nil, @writefds, @exceptfds, @timeout);
          if (Res = SOCKET_ERROR) then
            {$IFDEF DEBUG}
            raise Exception.Create('select() failed while connecting');
            {$ELSE}
            raise Exception.Create(_('Error while connecting'));
            {$ENDIF}
          if (Res > 0) and (FD_ISSET(FSocketHandle, exceptfds)) then
            {$IFDEF DEBUG}
            raise Exception.Create('select() socket error while connecting');
            {$ELSE}
            raise Exception.Create(_('Error while connecting'));
            {$ENDIF}
          if (Res > 0) and (FD_ISSET(FSocketHandle, writefds)) then
            Break;
          if StartTime < Ticks - 5000 then
            raise Exception.Create(_('Timeout while connecting'));
        end;
      end;

      DoConnected;

      LastTimeReceived := GetTickCount;
      LastTimeSent := GetTickCount;

      while True do
      begin
        DoStuff;

        timeout.tv_sec := 0;
        timeout.tv_usec := 200000; // 200ms

        FD_ZERO(readfds);
        FD_ZERO(writefds);
        FD_ZERO(exceptfds);
        FD_SET(FSocketHandle, readfds);
        FSendLock.Enter;
        if FSendStream.Size > 0 then
          FD_SET(FSocketHandle, writefds);
        FSendLock.Leave;
        FD_SET(FSocketHandle, exceptfds);

        if Terminated then
          Exit;

        Res := select(0, @readfds, @writefds, @exceptfds, @timeout);

        if Res = SOCKET_ERROR then
          raise Exception.Create(Format(_('select() error: %d'), [Res]));

        if (Res > 0) and (FD_ISSET(FSocketHandle, exceptfds)) then
          raise Exception.Create(_('select() socket error'));

        if (LastTimeReceived < GetTickCount - DataTimeout) and
           (LastTimeSent < GetTickCount - DataTimeout) then
        begin
          raise Exception.Create(Format(_('No data received/sent for more than %d seconds'), [DataTimeout div 1000]));
        end;

        if FD_ISSET(FSocketHandle, readfds) then
        begin
          RecvRes := recv(FSocketHandle, Buf, 8192, 0);

          if RecvRes = 0 then
          begin
            // Verbindung wurde geschlossen
            FClosed := True;
            Break;
          end else if RecvRes = SOCKET_ERROR then
          begin
            // Fehler
            raise Exception.Create(Format(_('recv() error: %d'), [WSAGetLastError]));
          end else if RecvRes > 0 then
          begin
            // Alles cremig
            FReceived := FReceived + RecvRes;
            LastTimeReceived := GetTickCount;
            FRecvStream.Seek(0, soFromEnd);
            FRecvStream.WriteBuffer(Buf, RecvRes);
            FRecvStream.Process;
            DoReceivedData(@Buf[1], RecvRes);
          end else
          begin

          end;
        end;

        if FD_ISSET(FSocketHandle, writefds) then
        begin
          FSendLock.Enter;
          try
            SendRes := send(FSocketHandle, FSendStream.Memory^, FSendStream.Size, 0);
            if SendRes = SOCKET_ERROR then
              raise Exception.Create(Format(_('send() socket error: %d'), [WSAGetLastError]));
            if SendRes > 0 then
            begin
              LastTimeSent := GetTickCount;
              WriteDebug(Format(_('%d bytes sent'), [SendRes]), string(FSendStream.ToString(0, SendRes)), 0, 1);
              FSendStream.RemoveRange(0, SendRes);
            end;
            if WSAGetLastError <> 0 then
            begin
              raise Exception.Create(Format(_('send() error: %d'), [WSAGetLastError]));
            end;
          finally
            FSendLock.Leave;
          end;
        end;

        if (not FD_ISSET(FSocketHandle, readfds)) and (not FD_ISSET(FSocketHandle, writefds)) then
          Sleep(30);
      end;
      DoDisconnected;
      DoDisconnectedEvent;
    except
      on E: Exception do
      begin
        WriteDebug(E.Message + E.StackTrace, 0, 1);
        DoException(E);
      end;
    end;
  finally
    try
      DoBeforeEndedEvent;
      DoEnded;
      DoEndedEvent;
    except
      on E: Exception do
      begin
        WriteDebug(E.Message + E.StackTrace, '', 0, 1);
        DoException(E);
      end;
    end;
    if FSocketHandle <> INVALID_SOCKET then
      closesocket(FSocketHandle);
  end;
end;

procedure TSocketThread.WriteDebug(Text: string; T, Level: Integer);
begin
  WriteDebug(Text, '', T, Level);
end;

procedure TSocketThread.WriteDebug(Text, Data: string; T, Level: Integer);
begin
  DoDebug(Text, Data, T, Level);
end;

procedure TSocketThread.DoBeforeEndedEvent;
begin
  if Assigned(FOnBeforeEnded) then
    Sync(FOnBeforeEnded);
end;

procedure TSocketThread.DoConnected;
begin
  if Assigned(FOnConnected) then
    Sync(FOnConnected);
end;

procedure TSocketThread.DoConnecting;
begin

end;

procedure TSocketThread.DoStuff;
begin

end;

procedure TSocketThread.DoDebug(Text, Data: string; T, Level: Integer);
begin
  FDebugMsg := Text;
  FDebugData := Data;
  FDebugType := T;
  FDebugLevel := Level;
  if Assigned(FOnDebug) then
    Sync(FOnDebug);
end;

procedure TSocketThread.DoDisconnected;
begin

end;

procedure TSocketThread.DoDisconnectedEvent;
begin
  if Assigned(FOnDisconnected) then
    Sync(FOnDisconnected);
end;

procedure TSocketThread.DoEnded;
begin

end;

procedure TSocketThread.DoEndedEvent;
begin
  if Assigned(FOnEnded) then
    Sync(FOnEnded);
end;

procedure TSocketThread.DoException(E: Exception);
begin
  FError := True;
  if Assigned(FOnException) then
    Sync(FOnException);
end;

procedure TSocketThread.DoReceivedData(Buf: Pointer; Len: Integer);
begin

end;

constructor TSocketThread.Create(Host: string; Port: Integer; Stream: TSocketStream);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FSocketHandle := 0;
  FHost := Host;
  FPort := Port;
  FReceived := 0;
  FRecvStream := Stream;
  FRecvStream.OnDebug := StreamDebug;
  FSendStream := TExtendedStream.Create;
  FSendLock := TCriticalSection.Create;
end;

constructor TSocketThread.Create(Handle: Cardinal; Stream: TSocketStream);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FSocketHandle := Handle;
  FRecvStream := Stream;
  FRecvStream.OnDebug := StreamDebug;
  FSendStream := TExtendedStream.Create;
  FSendLock := TCriticalSection.Create;
end;

destructor TSocketThread.Destroy;
begin
  FRecvStream.Free;
  FSendStream.Free;
  FSendLock.Free;
  inherited;
end;

end.
