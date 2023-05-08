unit MStringFunctions;

interface

uses
  ActiveX,
  Classes,
  ComObj,
  Controls,
  DateUtils,
  Dialogs,
  FileUtil,
  Forms,
  LazUTF8,
  Graphics,
  ShellAPI,
  ShlObj,
  shlwapi,
  StrUtils,
  SysUtils,
  Windows,
  ZStream;

type

  TMStringFunctions = class
  public
    class function GetTextSize(Text: string; Font: TFont): TSize; static;
    class function TruncateText(Text: string; MaxWidth: Integer; Font: TFont): string; static;
    class function StringForWidth(const c: string; const Width: Integer; const Font: TFont): string; static;
  end;

const
  MeasureTextHeightString: string = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';

implementation

class function TMStringFunctions.TruncateText(Text: string; MaxWidth: Integer; Font: TFont): string;
var
  Canvas: TCanvas;
  EW: Integer;
begin
  Text := Text.Trim;
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetDC(GetDesktopWindow);
    try
      Canvas.Font.Assign(Font);

      if Canvas.TextWidth(Text) <= MaxWidth then
        Exit(Text);

      EW := Canvas.TextWidth('...');

      Exit(UTF8Copy(Text, 1, Canvas.TextFitInfo(Text, MaxWidth - EW)).Trim + '...');
    finally
      ReleaseDC(GetDesktopWindow, Canvas.Handle);
    end;
  finally
    Canvas.Free;
  end;
end;

class function TMStringFunctions.StringForWidth(const c: string; const Width: Integer; const Font: TFont): string;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetDC(GetDesktopWindow);
    try
      Canvas.Font.Assign(Font);

      Result := c;
      while Canvas.TextWidth(Result) < Width do
        Result += c;
    finally
      ReleaseDC(GetDesktopWindow, Canvas.Handle);
    end;
  finally
    Canvas.Free;
  end;
end;

class function TMStringFunctions.GetTextSize(Text: string; Font: TFont): TSize;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetDC(GetDesktopWindow);
    try
      Canvas.Font.Assign(Font);

      Exit(Canvas.TextExtent(Text));
    finally
      ReleaseDC(GetDesktopWindow, Canvas.Handle);
    end;
  finally
    Canvas.Free;
  end;
end;

end.
