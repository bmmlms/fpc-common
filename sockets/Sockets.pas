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

unit Sockets;

interface

uses
  Classes,
  ExtendedStream,
  Generics.Collections,
  Logging,
  mbedTLS,
  SyncObjs,
  SysUtils,
  Windows,
  Winsock2;

type
  TSocketThread = class;
  TSocketServerThread = class;

  TLogEvent = procedure(Sender: TSocketThread; Data: string) of object;
  TSocketEvent = procedure(Sender: TSocketThread) of object;
  TSocketServerEvent = procedure(Sender: TSocketServerThread) of object;

  TSocketLogLevel = (slError, slWarning, slInfo, slDebug);

  TVarRecArray = array of TVarRec;

  EExceptionParams = class(Exception)
  private
    FArgs: TVarRecArray;
  public
    constructor CreateFmt(const Msg: string; const Args: array of const);

    property Args: TVarRecArray read FArgs;
  end;

  ESSLException = class(Exception)
  end;

  TSocketStream = class(TExtendedStream)
  private
    FDebugMsg, FDebugData: string;
    FLogLevel: TSocketLogLevel;
    FOnLog: TNotifyEvent;
  protected
    function FGetRecvDataStream: TExtendedStream; virtual;
    procedure WriteLog(Text, Data: string; Level: TSocketLogLevel); overload;
    procedure WriteLog(Text: string; Level: TSocketLogLevel); overload;
  protected
  public
    procedure Process(Received: Cardinal); virtual;

    procedure Disconnected; virtual;

    property RecvStream: TExtendedStream read FGetRecvDataStream;

    property DebugMsg: string read FDebugMsg;
    property DebugData: string read FDebugData;
    property LogLevel: TSocketLogLevel read FLogLevel;
    property OnLog: TNotifyEvent read FOnLog write FOnLog;
  end;

  TSocketStreamClass = class of TSocketStream;
  TSocketThreadClass = class of TSocketThread;

  TSocketList = TList<TSocketThread>;

  { TSocketBaseThread }

  TSocketBaseThread = class(TThread)
  protected
    FTerminatedEvent: TSimpleEvent;

    class function NetSend(ctx: Pointer; buf: Pointer; len: size_t): Integer; cdecl; static;
    class function NetRecv(ctx: Pointer; buf: Pointer; len: size_t): Integer; cdecl; static;
    class function NetRecvTimeout(ctx: Pointer; buf: Pointer; len: size_t; timeout: uint32_t): Integer; cdecl; static;

    procedure TerminatedSet; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
  end;

  { TSocketThread }

  TSocketThread = class(TSocketBaseThread)
  private
  class var
    FCertChain: mbedtls_x509_crt;
  private
    FSocketHandle: TSocket;
    FHost: string;
    FPort: Integer;
    FSecure: Boolean;
    FCheckCertificate: Boolean;
    FUseSynchronize: Boolean;
    FRaisedException: Exception;

    FLogMsg: string;
    FLogData: string;
    FLogLevel: TSocketLogLevel;

    FOnLog: TSocketEvent;
    FOnConnected: TSocketEvent;
    FOnDisconnected: TSocketEvent;
    FOnBeforeEnded: TSocketEvent;
    FOnEnded: TSocketEvent;
    FOnException: TSocketEvent;
    FOnSecured: TSocketEvent;
    FOnCommunicationEstablished: TSocketEvent;

    function HostToAddress(Host: string): u_long;
  protected
    FRecvStream: TSocketStream;
    FSendStream: TExtendedStream;
    FSendLock: SyncObjs.TCriticalSection;
    FDataTimeout: Cardinal;
    FLastTimeReceived, FLastTimeSent: UInt64;

    FReceived: UInt64;
    FError: Boolean;
    FClosed: Boolean;

    FProc: TSocketEvent;

    procedure Execute; override;

    procedure WriteLog(Text, Data: string; Level: TSocketLogLevel); overload;
    procedure WriteLog(Text: string; Level: TSocketLogLevel); overload;
    procedure StreamLog(Sender: TObject);

    procedure Sync(Proc: TSocketEvent);
    procedure Sync2;

    procedure DoLog(Text, Data: string; Level: TSocketLogLevel); virtual;
    procedure DoStuff; virtual;
    procedure DoConnecting; virtual;
    procedure DoConnected; virtual;
    procedure DoDisconnected; virtual;
    procedure DoBeforeEndedEvent;
    procedure DoDisconnectedEvent;
    procedure DoEnded; virtual;
    procedure DoEndedEvent;
    procedure DoReceivedData(Buf: Pointer; Len: Integer); virtual;
    procedure DoException(E: Exception); virtual;
    procedure DoSecured; virtual;
    procedure DoCommunicationEstablished; virtual;
  public
    constructor Create(SocketHandle: Cardinal; Stream: TSocketStream); overload; virtual;
    constructor Create(Host: string; Port: Integer; Stream: TSocketStream; Secure, CheckCertificate: Boolean); overload; virtual;
    destructor Destroy; override;

    class procedure LoadCertificates(const ResourceName: string);
    class procedure FreeCertificates;

    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property Secure: Boolean read FSecure write FSecure;
    property CheckCertificate: Boolean read FCheckCertificate write FCheckCertificate;
    property UseSynchronize: Boolean read FUseSynchronize write FUseSynchronize;
    property RaisedException: Exception read FRaisedException;

    property LogMsg: string read FLogMsg;
    property LogData: string read FLogData;
    property LogLevel: TSocketLogLevel read FLogLevel;
    property Received: UInt64 read FReceived;
    property Error: Boolean read FError write FError;

    property SendLock: SyncObjs.TCriticalSection read FSendLock;
    property SendStream: TExtendedStream read FSendStream;

    property OnLog: TSocketEvent read FOnLog write FOnLog;
    property OnConnected: TSocketEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TSocketEvent read FOnDisconnected write FOnDisconnected;
    property OnBeforeEnded: TSocketEvent read FOnBeforeEnded write FOnBeforeEnded;
    property OnEnded: TSocketEvent read FOnEnded write FOnEnded;
    property OnException: TSocketEvent read FOnException write FOnException;
    property OnSecured: TSocketEvent read FOnSecured write FOnSecured;
    property OnCommunicationEstablished: TSocketEvent read FOnCommunicationEstablished write FOnCommunicationEstablished;
  end;

  TSocketServerThread = class(TSocketBaseThread)
  private
    FPort: Cardinal;

    FOnClientConnected: TSocketEvent;
    FOnException: TNotifyEvent;

    FThreadType: TSocketThreadClass;
    FStreamType: TSocketStreamClass;

    FProc: TSocketEvent;
    FSender: TSocketThread;
    procedure Sync2;
  protected
    procedure Execute; override;

    procedure Sync(Proc: TSocketEvent; Sender: TSocketThread); overload;

    procedure DoClientConnected(SocketHandle: Cardinal; var SocketThread: TSocketThread); virtual;
    procedure DoClientStart(SocketThread: TSocketThread); virtual;
    procedure DoEnded; virtual;
    procedure DoException; virtual;
  public
    constructor Create(ThreadType: TSocketThreadClass; StreamType: TSocketStreamClass);
    destructor Destroy; override;

    property Port: Cardinal read FPort write FPort;

    property OnClientConnected: TSocketEvent read FOnClientConnected write FOnClientConnected;
    property OnException: TNotifyEvent read FOnException write FOnException;
  end;

implementation

{ TSocketBaseThread }

class function TSocketBaseThread.NetSend(ctx: Pointer; buf: Pointer; len: size_t): Integer; cdecl;
var
  Socket: TSocket absolute ctx;
begin
  Result := send(Socket, buf, len, 0);

  if Result = SOCKET_ERROR then
  begin
    Result := WSAGetLastError;
    if Result = WSAEWOULDBLOCK then
      Result := -MBEDTLS_ERR_SSL_WANT_WRITE
    else
      Result := -MBEDTLS_ERR_NET_SEND_FAILED;
  end;
end;

class function TSocketBaseThread.NetRecv(ctx: Pointer; buf: Pointer; len: size_t): Integer; cdecl;
var
  Socket: TSocket absolute ctx;
begin
  Result := recv(Socket, Buf, len, 0);

  if Result = 0 then
    Result := -MBEDTLS_ERR_NET_CONN_RESET
  else if Result = SOCKET_ERROR then
  begin
    Result := WSAGetLastError;
    if Result = WSAEWOULDBLOCK then
      Result := -MBEDTLS_ERR_SSL_WANT_READ
    else
      Result := -MBEDTLS_ERR_NET_RECV_FAILED;
  end;
end;

class function TSocketBaseThread.NetRecvTimeout(ctx: Pointer; buf: Pointer; len: size_t; timeout: uint32_t): Integer; cdecl;
var
  Socket: TSocket absolute ctx;
  TimeoutVal: TimeVal;
  ReadFds: TFdSet;
  Res: Integer;
begin
  TimeoutVal.tv_sec := Trunc(timeout / 1000);
  TimeoutVal.tv_usec := (timeout div 1000) * 1000;

  FD_ZERO(ReadFds);
  FD_SET(Socket, ReadFds);
  Res := select(0, @readfds, nil, nil, @timeout);
  if Res = 0 then
    Exit(-MBEDTLS_ERR_SSL_TIMEOUT)  // TODO: das "-" vor den errorcodes. sollte direkt zu den konstanten...
  else if Res = SOCKET_ERROR then
    Exit(-MBEDTLS_ERR_NET_RECV_FAILED);

  Result := NetRecv(ctx, buf, len);
end;

procedure TSocketBaseThread.TerminatedSet;
begin
  inherited;

  FTerminatedEvent.SetEvent;
end;

constructor TSocketBaseThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);

  FTerminatedEvent := TSimpleEvent.Create;
end;

destructor TSocketBaseThread.Destroy;
begin
  FTerminatedEvent.Free;

  inherited Destroy;
end;

{ TSocketThread }

constructor TSocketThread.Create(SocketHandle: Cardinal; Stream: TSocketStream);
var
  Len: Integer;
  Addr: TSockAddrIn;
begin
  inherited Create(True);

  FreeOnTerminate := True;
  FSocketHandle := SocketHandle;
  FRecvStream := Stream;
  FRecvStream.OnLog := StreamLog;
  FSendStream := TExtendedStream.Create;
  FSendLock := SyncObjs.TCriticalSection.Create;
  FUseSynchronize := False;

  Len := SizeOf(Addr);
  if getpeername(SocketHandle, Addr, Len) = 0 then
  begin
    FHost := inet_ntoa(Addr.sin_addr);
    FPort := ntohs(Addr.sin_port);
  end else
    raise Exception.Create('Function getpeername() failed');
end;

constructor TSocketThread.Create(Host: string; Port: Integer; Stream: TSocketStream; Secure, CheckCertificate: Boolean);
begin
  inherited Create(True);

  FreeOnTerminate := True;
  FSocketHandle := 0;
  FHost := Host;
  FPort := Port;
  FReceived := 0;
  FRecvStream := Stream;
  FRecvStream.OnLog := StreamLog;
  FSendStream := TExtendedStream.Create;
  FSendLock := SyncObjs.TCriticalSection.Create;
  FUseSynchronize := False;
  FSecure := Secure;
  FCheckCertificate := CheckCertificate;
end;

destructor TSocketThread.Destroy;
begin
  FRecvStream.Free;
  FSendStream.Free;
  FSendLock.Free;
  inherited;
end;

class procedure TSocketThread.LoadCertificates(const ResourceName: string);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(HINSTANCE, ResourceName, RT_RCDATA);
  try
    mbedtls_x509_crt_init(@FCertChain);
    if mbedtls_x509_crt_parse(@FCertChain, Stream.Memory, Stream.Size) < 0 then
      raise Exception.Create('Error loading certificates');
  finally
    Stream.Free;
  end;
end;

class procedure TSocketThread.FreeCertificates;
begin
  mbedtls_x509_crt_free(@FCertChain);
end;

procedure TSocketThread.DoCommunicationEstablished;
begin
  if Assigned(FOnCommunicationEstablished) then
    Sync(FOnCommunicationEstablished);
end;

procedure TSocketThread.DoConnected;
begin
  if Assigned(FOnConnected) then
    Sync(FOnConnected);
end;

procedure TSocketThread.DoConnecting;
begin

end;

procedure TSocketThread.DoDisconnected;
begin
  if Assigned(FOnDisconnected) then
    Sync(FOnDisconnected);
end;

procedure TSocketThread.DoBeforeEndedEvent;
begin
  if Assigned(FOnBeforeEnded) then
    Sync(FOnBeforeEnded);
end;

procedure TSocketThread.DoDisconnectedEvent;
begin

end;

procedure TSocketThread.DoEnded;
begin
  FRecvStream.Disconnected;
end;

procedure TSocketThread.DoEndedEvent;
begin
  if Assigned(FOnEnded) then
    Sync(FOnEnded);
end;

procedure TSocketThread.DoException(E: Exception);
begin
  FRaisedException := E;
  FError := True;
  if Assigned(FOnException) then
    Sync(FOnException);
end;

procedure TSocketThread.DoReceivedData(Buf: Pointer; Len: Integer);
begin

end;

procedure TSocketThread.DoLog(Text, Data: string; Level: TSocketLogLevel);
begin
  FLogMsg := Text;
  FLogData := Data;
  FLogLevel := Level;
  if Assigned(FOnLog) then
    Sync(FOnLog);
end;

procedure TSocketThread.DoSecured;
begin
  if Assigned(FOnSecured) then
    Sync(FOnSecured);
end;

procedure TSocketThread.DoStuff;
begin

end;

procedure TSocketThread.Execute;
const
  BufSize = 65536;
  ConnectTimeout = 5000;
  TLSTimeout = 10000;
var
  Addr: sockaddr_in;
  Res: Integer;
  ReadFds, WriteFds, ExceptFds: TFdSet;
  TimeoutVal: TimeVal;
  Buf: array[0..BufSize - 1] of Byte;
  Ticks, StartTime: UInt64;
  HostAddress, NonBlock: u_long;

  ssl: mbedtls_ssl_context;
  conf: mbedtls_ssl_config;
  entropy: mbedtls_entropy_context;
  ctr_drbg: mbedtls_ctr_drbg_context;
begin
  if FSecure then
  begin
    mbedtls_ssl_init(@ssl);
    mbedtls_ssl_config_init(@conf);
    mbedtls_ctr_drbg_init(@ctr_drbg);
    mbedtls_entropy_init(@entropy);
  end;

  try
    try
      if FSocketHandle = 0 then
      begin
        DoConnecting;

        HostAddress := HostToAddress(FHost);

        FSocketHandle := socket(AF_INET, SOCK_STREAM, 0);
        if FSocketHandle = INVALID_SOCKET then
          raise Exception.Create('Function socket() failed');

        NonBlock := 1;
        if ioctlsocket(FSocketHandle, LongInt(FIONBIO), NonBlock) = SOCKET_ERROR then
          raise Exception.Create('Function ioctlsocket() failed');

        Addr.sin_family := AF_INET;
        Addr.sin_port := htons(FPort);
        Addr.sin_addr.S_addr := HostAddress;

        connect(FSocketHandle, Addr, SizeOf(Addr));

        StartTime := GetTickCount64;
        while True do
        begin
          if Terminated then
            Exit;

          Ticks := GetTickCount64;
          TimeoutVal.tv_sec := 0;
          TimeoutVal.tv_usec := 100000; // 100ms
          FD_ZERO(WriteFds);
          FD_ZERO(ExceptFds);
          FD_SET(FSocketHandle, WriteFds);
          FD_SET(FSocketHandle, ExceptFds);
          Res := select(0, nil, @WriteFds, @ExceptFds, @TimeoutVal);
          if (Res = SOCKET_ERROR) then
            raise Exception.Create('Error while connecting');
          if (Res > 0) and (FD_ISSET(FSocketHandle, ExceptFds)) then
            raise Exception.Create('Error while connecting');
          if (Res > 0) and (FD_ISSET(FSocketHandle, WriteFds)) then
            Break;
          if (Ticks > ConnectTimeout) and (StartTime < Ticks - ConnectTimeout) then
            raise Exception.Create('Timeout while connecting');
        end;
      end;

      DoConnected;

      if FSecure then
      begin
        Res := mbedtls_ssl_config_defaults(@conf, MBEDTLS_SSL_IS_CLIENT, MBEDTLS_SSL_TRANSPORT_STREAM, MBEDTLS_SSL_PRESET_DEFAULT);
        if Res <> 0 then
          raise EExceptionParams.CreateFmt('Function mbedtls_ssl_config_defaults() returned error %d', [WSAGetLastError]);

        res := mbedtls_ctr_drbg_seed(@ctr_drbg, mbedtls_entropy_func, @entropy, nil, 0);
        if Res <> 0 then
          raise EExceptionParams.CreateFmt('Function mbedtls_ctr_drbg_seed() returned error %d', [Res]);

        mbedtls_ssl_conf_ca_chain(@conf, @FCertChain, nil);
        mbedtls_ssl_conf_rng(@conf, mbedtls_ctr_drbg_random, @ctr_drbg);
        mbedtls_ssl_conf_authmode(@conf, MBEDTLS_SSL_VERIFY_OPTIONAL);

        Res := mbedtls_ssl_setup(@ssl, @conf);
        if Res <> 0 then
          raise EExceptionParams.CreateFmt('Function mbedtls_ssl_setup() returned error %d', [Res]);

        Res := mbedtls_ssl_set_hostname(@ssl, PChar(fhost));
        if Res <> 0 then
          raise EExceptionParams.CreateFmt('Function mbedtls_ssl_set_hostname() returned error %d', [Res]);

        mbedtls_ssl_set_bio(@ssl, Pointer(FSocketHandle), @TSocketThread.NetSend, nil, @TSocketThread.NetRecvTimeout);
        mbedtls_ssl_conf_read_timeout(@ssl, TLSTimeout);

        Res := mbedtls_ssl_handshake(@ssl);
        while Res <> 0 do
        begin
          if (Res <> -MBEDTLS_ERR_SSL_WANT_READ) and (Res <> -MBEDTLS_ERR_SSL_WANT_WRITE) and (Res <> -MBEDTLS_ERR_SSL_CRYPTO_IN_PROGRESS) then
            if Res = -MBEDTLS_ERR_SSL_TIMEOUT then
              raise Exception.Create('TLS handshake timed out')
            else
              raise EExceptionParams.CreateFmt('TLS handshake was not successful, error %d', [Res]);

          Res := mbedtls_ssl_handshake(@ssl);
        end;
      end;

      if FSecure then
      begin
        Res := mbedtls_ssl_get_verify_result(@ssl);
        if Res <> 0 then
        begin
          if FCheckCertificate then
            raise ESSLException.Create('TLS handshake was not successful, certificate invalid')
          else
            WriteLog('TLS handshake was not successful, certificate invalid', slWarning);
        end else
          DoSecured;
      end;

      DoCommunicationEstablished;

      FLastTimeReceived := GetTickCount64;
      FLastTimeSent := GetTickCount64;

      while True do
      begin
        DoStuff;

        TimeoutVal.tv_sec := 0;
        TimeoutVal.tv_usec := 100000; // 100ms

        FD_ZERO(ReadFds);
        FD_ZERO(WriteFds);
        FD_ZERO(ExceptFds);
        FD_SET(FSocketHandle, ReadFds);
        FSendLock.Enter;
        if FSendStream.Size > 0 then
          FD_SET(FSocketHandle, WriteFds);
        FSendLock.Leave;
        FD_SET(FSocketHandle, ExceptFds);

        if Terminated then
          Exit;

        Res := select(0, @ReadFds, @WriteFds, @ExceptFds, @TimeoutVal);

        if Res = SOCKET_ERROR then
          raise EExceptionParams.CreateFmt('Function select() returned error %d', [WSAGetLastError]);

        if (Res > 0) and (FD_ISSET(FSocketHandle, ExceptFds)) then
          raise Exception.Create('Function select() failed');

        Ticks := GetTickCount64;

        if (FDataTimeout > 0) and (Ticks > FDataTimeout) and (FLastTimeReceived < Ticks - FDataTimeout) and (FLastTimeSent < Ticks - FDataTimeout) then
          raise EExceptionParams.CreateFmt('No data received/sent for more than %d seconds', [FDataTimeout div 1000]);

        if FD_ISSET(FSocketHandle, ReadFds) then
        begin
          if FSecure then
            Res := mbedtls_ssl_read(@SSL, @Buf[0], BufSize)
          else
            Res := recv(FSocketHandle, Buf, BufSize, 0);

          if (Res = 0) or (FSecure and (Res = -MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY)) then
          begin
            // Verbindung wurde geschlossen
            FClosed := True;
            Break;
          end else if Res < 0 then
          begin
            // Fehler
            if FSecure then
            begin
              if (Res <> -MBEDTLS_ERR_SSL_WANT_READ) and (Res <> -MBEDTLS_ERR_SSL_WANT_READ) then
                raise EExceptionParams.CreateFmt('Function mbedtls_ssl_read() returned error %d', [Res]);
            end else
              raise EExceptionParams.CreateFmt('Function recv() returned error %d', [WSAGetLastError]);
          end else if Res > 0 then
          begin
            // Alles cremig
            FReceived := FReceived + Res;
            FLastTimeReceived := Ticks;
            FRecvStream.Seek(0, soFromEnd);
            FRecvStream.WriteBuffer(Buf, Res);
            FRecvStream.Process(Res);
            DoReceivedData(@Buf[1], Res);
          end;
        end;

        if FD_ISSET(FSocketHandle, WriteFds) then
        begin
          FSendLock.Enter;
          try
            if FSecure then
              Res := mbedtls_ssl_write(@SSL, FSendStream.Memory, FSendStream.Size)
            else
              Res := send(FSocketHandle, FSendStream.Memory^, FSendStream.Size, 0);

            if Res < 0 then
            begin
              if FSecure then
              begin
                if (Res <> -MBEDTLS_ERR_SSL_WANT_READ) and (Res <> -MBEDTLS_ERR_SSL_WANT_READ) then
                  raise EExceptionParams.CreateFmt('Function mbedtls_ssl_write() returned error %d', [Res]);
              end else
                raise EExceptionParams.CreateFmt('Function send() returned error %d', [WSAGetLastError]);
            end else if Res > 0 then
            begin
              FLastTimeSent := Ticks;
              FSendStream.RemoveRange(0, Res);
            end;
          finally
            FSendLock.Leave;
          end;
        end;

        if (not FD_ISSET(FSocketHandle, ReadFds)) and (not FD_ISSET(FSocketHandle, WriteFds)) then
          Sleep(30);
      end;

      DoDisconnected;
      DoDisconnectedEvent;
    except
      on E: Exception do
        DoException(E);
    end;
  finally
    if FSecure then
    begin
      mbedtls_ssl_free(@ssl);
      mbedtls_ssl_config_free(@conf);
      mbedtls_ctr_drbg_free(@ctr_drbg);
      mbedtls_entropy_free(@entropy);
    end;

    try
      DoBeforeEndedEvent;
      DoEnded;
      DoEndedEvent;
    except
      on E: Exception do
      begin
        DoException(E);
      end;
    end;
    if FSocketHandle <> INVALID_SOCKET then
      closesocket(FSocketHandle);
  end;
end;

function TSocketThread.HostToAddress(Host: string): u_long;
type
  PPInAddr = ^PInAddr;
var
  HostInfo: PHostEnt;
  T: PPInAddr;
begin
  T := nil;
  Result := inet_addr(PAnsiChar(AnsiString(Host)));
  if Result = u_long(INADDR_NONE) then
  begin
    Result := 0;
    HostInfo := gethostbyname(PAnsiChar(AnsiString(Host)));
    if HostInfo <> nil then
      T := Pointer(HostInfo^.h_addr_list);
    if (T <> nil) and (T^ <> nil) then
      Result := T^^.S_addr;
  end;

  if Result = 0 then
    raise EExceptionParams.CreateFmt('Host "%s" could not be resolved', [Host]);
end;

procedure TSocketThread.Sync(Proc: TSocketEvent);
begin
  if Assigned(Proc) then
    if FUseSynchronize then
    begin
      FProc := Proc;
      Synchronize(Sync2);
    end else
      Proc(Self);
end;

procedure TSocketThread.Sync2;
begin
  FProc(Self);
end;

procedure TSocketThread.WriteLog(Text: string; Level: TSocketLogLevel);
begin
  WriteLog(Text, '', Level);
end;

procedure TSocketThread.WriteLog(Text, Data: string; Level: TSocketLogLevel);
begin
  {$IFNDEF DEBUG}
  if Level = slDebug then
    Exit;
  {$ENDIF}

  DoLog(Text, Data, Level);
end;

procedure TSocketThread.StreamLog(Sender: TObject);
begin
  WriteLog(FRecvStream.DebugMsg, FRecvStream.DebugData, FRecvStream.LogLevel);
end;

{ TSocketServerThread }

constructor TSocketServerThread.Create(ThreadType: TSocketThreadClass; StreamType: TSocketStreamClass);
begin
  inherited Create(True);
  FThreadType := ThreadType;
  FStreamType := StreamType;
  FreeOnTerminate := True;
end;

destructor TSocketServerThread.Destroy;
begin

  inherited;
end;

procedure TSocketServerThread.DoClientConnected(SocketHandle: Cardinal; var SocketThread: TSocketThread);
begin
  try
    SocketThread := FThreadType.Create(SocketHandle, FStreamType.Create as TSocketStream);
    try
      if Assigned(FOnClientConnected) then
        FOnClientConnected(SocketThread);
    except
      FreeAndNil(SocketThread);
    end;
  except
    FreeAndNil(SocketThread);
  end;
end;

procedure TSocketServerThread.DoClientStart(SocketThread: TSocketThread);
begin
  SocketThread.Start;
end;

procedure TSocketServerThread.DoEnded;
begin

end;

procedure TSocketServerThread.DoException;
begin
  if Assigned(FOnException) then
    FOnException(Self);
end;

procedure TSocketServerThread.Execute;
var
  FAcceptHandle: Integer;
  FSocketHandle: Integer;
  Addr: sockaddr_in;
  NonBlock: u_long;
  timeout: TimeVal;
  readfds, exceptfds: TFdSet;
  Res: Integer;
  SocketThread: TSocketThread;
begin
  FAcceptHandle := socket(AF_INET, SOCK_STREAM, 0);
  if FAcceptHandle = SOCKET_ERROR then
    raise Exception.Create('Function socket() failed');

  try
    try
      NonBlock := 1;
      if ioctlsocket(FAcceptHandle, LongInt(FIONBIO), NonBlock) = SOCKET_ERROR then
        raise Exception.Create('Function ioctlsocket() failed');

      Addr.sin_family := AF_INET;
      Addr.sin_Port := htons(FPort);
      Addr.sin_Addr.S_Addr := inet_addr('0.0.0.0');

      if bind(FAcceptHandle, Addr, sizeof(Addr)) = SOCKET_ERROR then
        raise Exception.Create('Function bind() failed');

      if listen(FAcceptHandle, 5) = SOCKET_ERROR then
        raise Exception.Create('Function listen() failed');

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
          raise EExceptionParams.CreateFmt('Function select() returned error %d', [Res]);

        if Terminated then
          Exit;

        if (Res > 0) and (FD_ISSET(FAcceptHandle, exceptfds)) then
          raise Exception.Create('Function select() failed');

        if (Res <> SOCKET_ERROR) and (FD_ISSET(FAcceptHandle, readfds)) then
        begin
          FSocketHandle := accept(FAcceptHandle, nil, nil);
          DoClientConnected(FSocketHandle, SocketThread);
          if SocketThread <> nil then
            DoClientStart(SocketThread);
        end;
      end;
    except
      try
        DoException;
      except

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

procedure TSocketServerThread.Sync(Proc: TSocketEvent; Sender: TSocketThread);
begin
  if Assigned(Proc) then
  begin
    FProc := Proc;
    FSender := Sender;
    Synchronize(Sync2);
  end;
end;

procedure TSocketServerThread.Sync2;
begin
  FProc(FSender);
end;

{ TSocketStream }

procedure TSocketStream.Disconnected;
begin

end;

procedure TSocketStream.Process(Received: Cardinal);
begin

end;

procedure TSocketStream.WriteLog(Text, Data: string; Level: TSocketLogLevel);
begin
  if Assigned(FOnLog) then
  begin
    FDebugMsg := Text;
    FDebugData := Data;
    FLogLevel := Level;
    FOnLog(Self);
  end;
end;

procedure TSocketStream.WriteLog(Text: string; Level: TSocketLogLevel);
begin
  WriteLog(Text, '', Level);
end;

function TSocketStream.FGetRecvDataStream: TExtendedStream;
begin
  Result := Self;
end;

{ EExceptionParams }

constructor EExceptionParams.CreateFmt(const Msg: string; const Args: array of const);
var
  i: Integer;
begin
  inherited Create(Msg);

  SetLength(FArgs, Length(Args));
  for i := 0 to High(Args) do
    FArgs[i] := Args[i];
end;

end.
