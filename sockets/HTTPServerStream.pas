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
unit HTTPServerStream;

interface

uses
  SysUtils, Windows, StrUtils, Sockets, Functions;

type
  THTTPServerStream = class(TSocketStream)
  private
    FRequestProcessed: Boolean;
    FRequestedURL: string;

    procedure ProcessGet;
  protected
  public
    constructor Create;
    procedure Process; override;
    property RequestProcessed: Boolean read FRequestProcessed;
    property RequestedURL: string read FRequestedURL;
  end;

implementation

{ THTTPServerStream }

constructor THTTPServerStream.Create;
begin
  inherited;
  FRequestProcessed := False;
end;

procedure THTTPServerStream.Process;
begin
  inherited;
  if not FRequestProcessed then
    ProcessGet;
end;

procedure THTTPServerStream.ProcessGet;
var
  P, P2: Integer;
  Header, Data: string;
begin
  P := PosInStream(#13#10#13#10, 0);
  if P > -1 then
  begin
    Header := string(RecvStream.ToString(0, P));
    if Copy(LowerCase(Header), 1, 3) = 'get' then
    begin
      P := Pos(' ', Header);
      if (P > -1) and (Length(Header) > P + 1) then
      begin
        P2 := PosEx(' ', Header, P + 1);
        if P2 > -1 then
        begin
          Data := Copy(Header, P + 1, P2 - P - 1);
          if not IsAnsi(Data) then
            raise Exception.Create('Client requested invalid URL');
          FRequestedURL := Data;
        end;
      end;
    end;
    FRequestProcessed := True;
  end;
  if Size > 100000 then
    raise Exception.Create('HTTP-Server received too many bytes in request');
end;

end.
