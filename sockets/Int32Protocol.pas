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

unit Int32Protocol;

interface

uses
  Windows, SysUtils, Classes, Sockets, WinSock, XMLLib;

type
  TInt32SocketThread = class(TSocketThread)
  private
  protected
    procedure Write(XML: AnsiString);

    procedure DoReceivedData(Buf: Pointer; Len: Integer); override;
    procedure DoConnected; override;
    procedure DoReceivedString(D: AnsiString); virtual;
  public
    constructor Create(Host: string; Port: Integer);
    destructor Destroy; override;
  end;

implementation

{ TInt32SocketThread }

constructor TInt32SocketThread.Create(Host: string; Port: Integer);
begin
  inherited Create(Host, Port, TSocketStream.Create);

  FDataTimeout := 0;
end;

destructor TInt32SocketThread.Destroy;
begin

  inherited;
end;

procedure TInt32SocketThread.DoConnected;
begin
  inherited;

end;

procedure TInt32SocketThread.DoReceivedData(Buf: Pointer; Len: Integer);
var
  CLen: u_long;
  S: AnsiString;
begin
  inherited;

  while FRecvStream.Size > 4 do
  begin
    CopyMemory(@CLen, FRecvStream.Memory, 4);
    CLen := ntohl(CLen);

    if (CLen > 0) and (FRecvStream.Size >= CLen + 4) then
    begin
      FRecvStream.Seek(4, soFromBeginning);
      SetLength(S, CLen);
      FRecvStream.Read(S[1], CLen);
      FRecvStream.RemoveRange(0, CLen + 4);

      DoReceivedString(S);
    end else
      Break;
  end;
end;

procedure TInt32SocketThread.DoReceivedString(D: AnsiString);
begin

end;

procedure TInt32SocketThread.Write(XML: AnsiString);
var
  L: u_long;
begin
  FSendLock.Enter;
  try
    L := Length(XML);
    L := htonl(L);
    FSendStream.Write(L);
    FSendStream.Write(XML[1], Length(XML));
  finally
    FSendLock.Leave;
  end;
end;

end.
