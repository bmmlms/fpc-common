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

unit Sockets;

interface

uses
  Windows, SysUtils, Classes, SyncObjs, Winsock, ExtendedStream,
  Generics.Collections, Logging, IdSSLOpenSSLHeadersCustom, DynOpenSSL;

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

  TSocketThread = class(TThread)
  private
    FSocketHandle: TSocket;
    FHost: string;
    FPort: Integer;
    FSecure: Boolean;
    FCheckCertificate: Boolean;
    FUseSynchronize: Boolean;
    FSSLError: Boolean;
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

    function HostToAddress(Host: string): Integer;
    function SSLErrorToText(Err: Integer): string;
  protected
    FRecvStream: TSocketStream;
    FSendStream: TExtendedStream;
    FSendLock: TCriticalSection;
    FDataTimeout: Cardinal;
    FLastTimeReceived, FLastTimeSent: Cardinal;

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
    procedure DoSSLError(Text: string); virtual;
  public
    constructor Create(SocketHandle: Cardinal; Stream: TSocketStream); overload; virtual;
    constructor Create(Host: string; Port: Integer; Stream: TSocketStream; Secure, CheckCertificate: Boolean); overload; virtual;
    destructor Destroy; override;

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

    property SendLock: TCriticalSection read FSendLock;
    property SendStream: TExtendedStream read FSendStream;

    property OnLog: TSocketEvent read FOnLog write FOnLog;
    property OnConnected: TSocketEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TSocketEvent read FOnDisconnected write FOnDisconnected;
    property OnBeforeEnded: TSocketEvent read FOnBeforeEnded write FOnBeforeEnded;
    property OnEnded: TSocketEvent read FOnEnded write FOnEnded;
    property OnException: TSocketEvent read FOnException write FOnException;
    property OnSecured: TSocketEvent read FOnSecured write FOnSecured;
  end;

  TSocketServerThread = class(TThread)
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
  FSendLock := TCriticalSection.Create;
  FUseSynchronize := False;

  Len := SizeOf(Addr);
  if getpeername(SocketHandle, Addr, Len) = 0 then
  begin
    FHost := inet_ntoa(Addr.sin_addr);
    FPort := ntohs(Addr.sin_port);
  end else
    raise Exception.Create('Function getpeername() failed');
end;

constructor TSocketThread.Create(Host: string; Port: Integer;
  Stream: TSocketStream; Secure, CheckCertificate: Boolean);
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
  FSendLock := TCriticalSection.Create;
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

procedure TSocketThread.DoConnected;
begin
  if Assigned(FOnConnected) then
    Sync(FOnConnected)
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
    Sync(FOnSecured)
end;

procedure TSocketThread.DoSSLError(Text: string);
begin
  FSSLError := True;

  if FCheckCertificate then
    raise ESSLException.Create(Text)
  else
    WriteLog(Text, slError);
end;

procedure TSocketThread.DoStuff;
begin

end;

procedure TSocketThread.Execute;
const
  BufSize = 65536;
var
  Addr: sockaddr_in;
  Res, ErrRes, RecvRes, SendRes, Idx: Integer;
  readfds, writefds, exceptfds: TFdSet;
  timeout: TimeVal;
  Buf: array[0..BufSize - 1] of Byte;
  HostAddress: Integer;
  NonBlock: Integer;
  Ticks, StartTime: Cardinal;

  Method: PSSL_METHOD;
  Ctx: PSSL_CTX;
  SSL: PSSL;
  Cert: PX509;
  CBIO: PBIO;
  SN: PX509_NAME;
  NE: PX509_NAME_ENTRY;
begin
  Ctx := nil;
  SSL := nil;

  try
    try
      if FSocketHandle = 0 then
      begin
        DoConnecting;

        HostAddress := HostToAddress(FHost);

        FSocketHandle := socket(AF_INET, SOCK_STREAM, 0);
        if FSocketHandle = SOCKET_ERROR then
          raise Exception.Create('Function socket() failed');

        NonBlock := 1;
        if ioctlsocket(FSocketHandle, FIONBIO, NonBlock) = SOCKET_ERROR then
          raise Exception.Create('Function ioctlsocket() failed');

        Addr.sin_family := AF_INET;
        Addr.sin_port := htons(FPort);
        Addr.sin_addr.S_addr := HostAddress;

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
            raise Exception.Create('Error while connecting');
          if (Res > 0) and (FD_ISSET(FSocketHandle, exceptfds)) then
            raise Exception.Create('Error while connecting');
          if (Res > 0) and (FD_ISSET(FSocketHandle, writefds)) then
            Break;
          if StartTime < Ticks - 5000 then
            raise Exception.Create('Timeout while connecting');
        end;
      end;

      DoConnected;

      if FSecure then
      begin
        Method := SSLv23_method();
        Ctx := SSL_CTX_new(Method);

        CBIO := BIO_new_mem_buf(@OpenSSL.Cert[1], Length(OpenSSL.Cert));
        Cert := PEM_read_bio_X509(CBIO, nil, nil, nil);
        BIO_free(CBIO);
        X509_STORE_add_cert(Ctx.cert_store, Cert);

        SSL := SSL_new(Ctx);
        SSL_set_fd(SSL, FSocketHandle);

        // SNI
        SSL_set_tlsext_host_name(SSL, Host);

        while True do
        begin
          Res := SSL_connect(SSL);
          if Res = 1 then
            Break
          else if Res = 0 then
          begin
            ErrRes := SSL_get_error(SSL, Res);
            raise EExceptionParams.CreateFmt('TLS handshake was not successful, error %s', [SSLErrorToText(ErrRes)]);
          end else if Res < 0 then
          begin
            ErrRes := SSL_get_error(SSL, Res);
            if (ErrRes <> SSL_ERROR_WANT_READ) and (ErrRes <> SSL_ERROR_WANT_WRITE) and (ErrRes <> SSL_ERROR_WANT_CONNECT) then
              raise EExceptionParams.CreateFmt('TLS handshake was not successful, error %s', [SSLErrorToText(ErrRes)]);
          end;
          if Terminated then
            Exit;
          Sleep(10);
        end;

        //if Self.ClassName = 'TUpdateThread' then
        //if Self.ClassName = 'THomeThread' then
        //if Self.ClassName = 'TDownloadThread' then
        //  DoSSLError('TLS handshake was not successful, no certificate received');

        Cert := SSL_get_peer_certificate(SSL);
        if Cert <> nil then
          X509_free(Cert)
        else
          DoSSLError('TLS handshake was not successful, no certificate received');

        Res := SSL_get_verify_result(SSL);
        if Res <> X509_V_OK then
          DoSSLError('TLS handshake was not successful, certificate invalid');

        SN := X509_get_subject_name(Cert);
        if SN = nil then
          DoSSLError('TLS handshake was not successful, certificate invalid');

        Idx := X509_NAME_get_index_by_NID(SN, NID_commonName, 0);
        NE := X509_NAME_get_entry(SN, Idx);
        if NE = nil then
          DoSSLError('TLS handshake was not successful, certificate invalid');

        if NE.value.data <> FHost then
          DoSSLError('TLS handshake was not successful, certificate invalid');
      end;

      if (not FSSLError) and FSecure then
        DoSecured;

      FLastTimeReceived := GetTickCount;
      FLastTimeSent := GetTickCount;

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
          raise EExceptionParams.CreateFmt('Function select() returned error %d', [WSAGetLastError]);

        if (Res > 0) and (FD_ISSET(FSocketHandle, exceptfds)) then
          raise Exception.Create('Function select() failed');

        if FDataTimeout > 0 then
          if (FLastTimeReceived < GetTickCount - FDataTimeout) and
             (FLastTimeSent < GetTickCount - FDataTimeout) then
          begin
            raise EExceptionParams.CreateFmt('No data received/sent for more than %d seconds', [FDataTimeout div 1000]);
          end;

        if FD_ISSET(FSocketHandle, readfds) then
        begin
          if FSecure then
            RecvRes := SSL_read(SSL, @Buf[0], BufSize)
          else
            RecvRes := recv(FSocketHandle, Buf, BufSize, 0);

          if RecvRes = 0 then
          begin
            // Verbindung wurde geschlossen
            FClosed := True;
            Break;
          end else if RecvRes < 0 then
          begin
            // Fehler
            if FSecure then
            begin
              ErrRes := SSL_get_error(SSL, RecvRes);
              if (ErrRes <> SSL_ERROR_WANT_READ) and (ErrRes <> SSL_ERROR_WANT_WRITE) then
                raise EExceptionParams.CreateFmt('Function SSL_read() returned error %d', [ErrRes]);
            end else
              raise EExceptionParams.CreateFmt('Function recv() returned error %d', [WSAGetLastError]);
          end else if RecvRes > 0 then
          begin
            // Alles cremig
            FReceived := FReceived + RecvRes;
            FLastTimeReceived := GetTickCount;
            FRecvStream.Seek(0, soFromEnd);
            FRecvStream.WriteBuffer(Buf, RecvRes);
            FRecvStream.Process(RecvRes);
            DoReceivedData(@Buf[1], RecvRes);
          end;
        end;

        if FD_ISSET(FSocketHandle, writefds) then
        begin
          FSendLock.Enter;
          try
            if FSecure then
              SendRes := SSL_write(SSL, FSendStream.Memory, FSendStream.Size)
            else
              SendRes := send(FSocketHandle, FSendStream.Memory^, FSendStream.Size, 0);

            if SendRes < 0 then
            begin
              if FSecure then
              begin
                ErrRes := SSL_get_error(SSL, SendRes);
                if (ErrRes <> SSL_ERROR_WANT_READ) and (ErrRes <> SSL_ERROR_WANT_WRITE) then
                  raise EExceptionParams.CreateFmt('Function SSL_write() returned error %d', [ErrRes]);
              end else
                raise EExceptionParams.CreateFmt('Function send() returned error %d', [WSAGetLastError]);
            end else if SendRes > 0 then
            begin
              FLastTimeSent := GetTickCount;
              FSendStream.RemoveRange(0, SendRes);
            end;

            if not FSecure then
              if WSAGetLastError <> 0 then
              begin
                raise EExceptionParams.CreateFmt('Function send() returned error %d', [WSAGetLastError]);
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
        DoException(E);
    end;
  finally
    try
      if SSL <> nil then      
        SSL_free(SSL);
    except
    end;

    try
      if Ctx <> nil then
        SSL_CTX_free(Ctx);    
    except
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
    raise EExceptionParams.CreateFmt('Host "%s" could not be resolved', [Host]);
end;

procedure TSocketThread.Sync(Proc: TSocketEvent);
begin
  if Assigned(Proc) then
  begin
    if FUseSynchronize then
    begin
      FProc := Proc;
      Synchronize(Sync2);
    end else
      Proc(Self);
  end;
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

function TSocketThread.SSLErrorToText(Err: Integer): string;
begin
  Result := 'SSL_ERROR_UNKNOWN';
  case Err of
    1: Result := 'SSL_ERROR_SSL';
    5: Result := 'SSL_ERROR_SYSCALL';
    6: Result := 'SSL_ERROR_ZERO_RETURN';
  end;
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
  NonBlock: Integer;
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
      if ioctlsocket(FAcceptHandle, FIONBIO, NonBlock) = SOCKET_ERROR then
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
          raise Exception.Create('Function select() socket failed');

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

initialization

finalization
  WSACleanup;

end.
