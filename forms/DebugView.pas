{
    ------------------------------------------------------------------------
    streamWriter
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
unit DebugView;

interface

uses
  SysUtils, Windows, Classes, Controls, Graphics, VirtualTrees, Forms,
  ClipBrd, DateUtils, StrUtils;

type
  TDebugView = class(TVirtualStringTree)
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure Resize; override;
    procedure DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode;
      var NodeHeight: Integer); override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode;
      CellRect: TRect; var ContentRect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    function AddData(Time: TDateTime; Text: string; Data: string): PVirtualNode;
    procedure Copy;
  end;

implementation

type
  TNodeData = record
    Time: TDateTime;
    Text: string;
  end;
  PNodeData = ^TNodeData;

procedure TDebugView.Copy;
var
  s: string;
  NodeData: PNodeData;
  Node: PVirtualNode;
begin
  if RootNodeCount > 0 then
  begin
    s := '';

    Node := GetFirst;
    while Node <> nil do
    begin
      NodeData := GetNodeData(Node);
      if GetNodeLevel(Node) = 1 then
        s := s + '    ' + StringReplace(NodeData.Text, #13#10, #13#10'    ', [rfReplaceAll])
      else
        s := s + DateToStr(NodeData.Time) + ' - ' + NodeData.Text;
      s := s + #13#10;
      Node := GetNext(Node);
    end;

    Clipboard.Clear;
    Clipboard.SetTextBuf(PChar(s));
  end;
end;

constructor TDebugView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NodeDataSize := SizeOf(TNodeData);
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toVariableNodeHeight];

  Header.Columns.Add;
  Header.AutoSizeIndex := 0;
end;

procedure TDebugView.DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode;
  var NodeHeight: Integer);
begin
  inherited;
  NodeHeight := ComputeNodeHeight(Canvas, Node, 0);
end;

function TDebugView.AddData(Time: TDateTime; Text: string; Data: string): PVirtualNode;
var
  No, No2: PVirtualNode;
  N: PNodeData;
  OldAdded: Integer;
begin
  No := Self.AddChild(nil, nil);
  MultiLine[No] := True;
  N := GetNodeData(No);
  N.Text := TimeToStr(Time) + ' - ' + Text;
  N.Time := Time;

  if Data <> '' then
  begin
    No2 := Self.AddChild(No, nil);
    MultiLine[No2] := True;
    N := GetNodeData(No2);
    N.Time := Time;
    N.Text := Data;
  end;

  Result := No;
end;

procedure TDebugView.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: UnicodeString);
var
  N: PNodeData;
begin
  N := PNodeData(GetNodeData(Node));
  Text := N.Text;
end;

procedure TDebugView.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  N: PNodeData;
  G: Integer;
begin
  inherited;
  {
  N := GetNodeData(Node);

  G := Trunc(SecondOf(N.Time));
  G := Trunc((G / 60) * 20);
  G := 100 + (G * 4);

  Canvas.Brush.Color := RGB(G, G, G);
  Canvas.FillRect(CellRect);
  }
end;

procedure TDebugView.DoFreeNode(Node: PVirtualNode);
var
  NodeData: PNodeData;
begin
  NodeData := GetNodeData(Node);
  Finalize(NodeData.Text);
  inherited;
end;

procedure TDebugView.Resize;
var
  c: TVirtualTreeColumn;
begin
  inherited;
  c := Header.Columns[0];
  c.Width := width - 30;
end;

end.
