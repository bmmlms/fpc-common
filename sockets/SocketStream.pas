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
unit SocketStream;

interface

uses
  Windows, SysUtils, Classes, ExtendedStream;

type
  TSocketStream = class(TExtendedStream)
  private
    FDebugMsg, FDebugData: string;
    FDebugType: Integer;
    FDebugLevel: Integer;
    FOnDebug: TNotifyEvent;
  protected
    function FGetRecvDataStream: TExtendedStream; virtual;
    procedure WriteDebug(Text, Data: string; T, Level: Integer); overload;
    procedure WriteDebug(Text: string; T, Level: Integer); overload;
  public
    procedure Process; virtual; abstract;

    property RecvStream: TExtendedStream read FGetRecvDataStream;

    property DebugMsg: string read FDebugMsg;
    property DebugData: string read FDebugData;
    property DebugType: Integer read FDebugType;
    property DebugLevel: Integer read FDebugLevel;
    property OnDebug: TNotifyEvent read FOnDebug write FOnDebug;
  end;

implementation

{ TSocketStream }

procedure TSocketStream.WriteDebug(Text, Data: string; T, Level: Integer);
begin
  if Assigned(FOnDebug) then
  begin
    FDebugMsg := Text;
    FDebugData := Data;
    FDebugType := T;
    FDebugLevel := Level;
    FOnDebug(Self);
  end;
end;

procedure TSocketStream.WriteDebug(Text: string; T, Level: Integer);
begin
  WriteDebug(Text, '', T, Level);
end;

function TSocketStream.FGetRecvDataStream: TExtendedStream;
begin
  Result := Self;
end;

end.
