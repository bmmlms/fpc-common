unit MessageBus;

interface

uses
  Classes, Generics.Collections;

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
    procedure SendMessage(Msg: TMessageBase);
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
  MsgBus := TMessageBus.Create;
finalization
  MsgBus.Free;

end.
