{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2019 Alexander Nottelmann

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
    IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
    OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
    IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
    NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
    THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    ------------------------------------------------------------------------
}

unit LanguageIcons;

interface

uses
  Windows, SysUtils, Classes, Graphics, ImgList, LanguageObjects;

type
  TLanguageIcon = class
  private
    FID: string;
    FIndex: Integer;
  public
    constructor Create(ID: string; Index: Integer);
  end;

  TLanguageIcons = class
  private
    FList: TCustomImageList;
    FLanguageIcons: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateIcon(ID: string): TIcon;
    function GetIconIndex(ID: string): Integer;
    property List: TCustomImageList read FList;
  end;

implementation

{ TLanguageIcons }

constructor TLanguageIcons.Create;
var
  i, Idx: Integer;
  HIco: Cardinal;
  Ico: TIcon;
begin
  FList := TCustomImageList.Create(nil);
  try
    FList.Width := 16;
    FList.Height := 16;
    FLanguageIcons := TList.Create;
    for i := 0 to LanguageList.Count - 1 do
    begin
      try
        Ico := TIcon.Create;
        try
          HIco := LoadIcon(HInstance, PChar(LanguageList[i].ID));
          if HIco > 0 then
          begin
            Ico.Handle := HIco;
            Idx := FList.AddIcon(Ico);
            FLanguageIcons.Add(TLanguageIcon.Create(LanguageList[i].ID, Idx));
          end;
        finally
          Ico.Free;
        end;
      except
      end;
    end;
  except

  end;
end;

destructor TLanguageIcons.Destroy;
var
  i: Integer;
begin
  FList.Free;
  for i := 0 to FLanguageIcons.Count - 1 do
    TLanguageIcon(FLanguageIcons[i]).Free;
  FLanguageIcons.Free;
  inherited;
end;

function TLanguageIcons.CreateIcon(ID: string): TIcon;
var
  i: Integer;
  Icon: TIcon;
begin
  Result := nil;
  for i := 0 to FLanguageIcons.Count - 1 do
    if TLanguageIcon(FLanguageIcons[i]).FID = ID then
    begin
      Icon := TIcon.Create;
      FList.GetIcon(TLanguageIcon(FLanguageIcons[i]).FIndex, Icon);
      Result := Icon;
      Break;
    end;
end;

function TLanguageIcons.GetIconIndex(ID: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FLanguageIcons.Count - 1 do
  begin
    if TLanguageIcon(FLanguageIcons[i]).FID = ID then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{ TLanguageIcon }

constructor TLanguageIcon.Create(ID: string; Index: Integer);
begin
  FID := ID;
  FIndex := Index;
end;


end.
