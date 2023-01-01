{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2023 Alexander Nottelmann

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

unit MessageBus;

interface

uses
  Classes,
  Generics.Collections,
  SysUtils;

type
  TMessageBase = class;

  TMsgReceived = procedure(Msg: TMessageBase) of object;

  TMessageBase = class
  end;

  TMessageBus = class
  private
    FSubscribers: TList<TMsgReceived>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddSubscriber(O: TMsgReceived);
    procedure RemoveSubscriber(O: TMsgReceived);
    procedure SendMessage(Msg: TMessageBase); virtual;
  end;

var
  MsgBus: TMessageBus;

implementation

{ TMessageBus }

procedure TMessageBus.AddSubscriber(O: TMsgReceived);
begin
  FSubscribers.Add(O);
end;

constructor TMessageBus.Create;
begin
  FSubscribers := TList<TMsgReceived>.Create;
end;

destructor TMessageBus.Destroy;
begin
  FSubscribers.Free;
  inherited;
end;

procedure TMessageBus.RemoveSubscriber(O: TMsgReceived);
var
  i: Integer;
begin
  for i := 0 to FSubscribers.Count - 1 do
    if @FSubscribers[i] = @O then
    begin
      FSubscribers.Delete(i);
      Exit;
    end;
end;

procedure TMessageBus.SendMessage(Msg: TMessageBase);
var
  i: Integer;
  M: TMsgReceived;
begin
  try
    for i := 0 to FSubscribers.Count - 1 do
    begin
      M := TMsgReceived(FSubScribers[i]);
      M(Msg);
    end;
  finally
    Msg.Free;
  end;
end;

initialization

finalization
  FreeAndNil(MsgBus);

end.
