{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2021 Alexander Nottelmann

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
  Classes,
  Graphics,
  ImgList,
  LanguageObjects,
  SysUtils,
  Windows;

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
    function GetIconIndex(ID: string): Integer;
    property List: TCustomImageList read FList;
  end;

implementation

{ TLanguageIcons }

constructor TLanguageIcons.Create;
var
  i: Integer;
begin
  FList := TCustomImageList.CreateSize(16, 11);
  FLanguageIcons := TList.Create;
  for i := 0 to LanguageList.Count - 1 do
  begin
    if FindResource(HINSTANCE, PChar('FLAG_%s'.Format([LanguageList[i].ID])), Windows.RT_RCDATA) = 0 then
      Continue;

    FLanguageIcons.Add(TLanguageIcon.Create(LanguageList[i].ID, FList.AddResourceName(HINSTANCE, 'FLAG_%s'.Format([LanguageList[i].ID]))));
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

function TLanguageIcons.GetIconIndex(ID: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FLanguageIcons.Count - 1 do
    if TLanguageIcon(FLanguageIcons[i]).FID = ID then
    begin
      Result := i;
      Break;
    end;
end;

{ TLanguageIcon }

constructor TLanguageIcon.Create(ID: string; Index: Integer);
begin
  FID := ID;
  FIndex := Index;
end;

end.
