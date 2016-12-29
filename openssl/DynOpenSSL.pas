{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2017 Alexander Nottelmann

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

unit DynOpenSSL;

interface

uses
  Windows, SysUtils, Classes, Generics.Collections, Functions,
  IdSSLOpenSSLHeadersCustom;

type
  TOpenSSLLoader = class
  private
    FLibEayDLLPath: string;
    FSSLEayDLLPath: string;
    FCerts: TStringList;

    FLibEayDLLHandle: Cardinal;
    FSSLEayDLLHandle: Cardinal;

    procedure UninitializeOpenSSL;
  public
    OpenSSLLoaded: Boolean;

    constructor Create;
    destructor Destroy; override;
    function InitializeOpenSSL(TempDir: string): Boolean;
  property
    Certs: TStringList read FCerts;
  end;

var
  OpenSSL: TOpenSSLLoader;

implementation

{ TOpenSSLLoader }

constructor TOpenSSLLoader.Create;
begin
  inherited;

  FCerts := TStringList.Create;
end;

destructor TOpenSSLLoader.Destroy;
begin
  UninitializeOpenSSL;

  FCerts.Free;

  inherited;
end;

function TOpenSSLLoader.InitializeOpenSSL(TempDir: string): Boolean;
var
  C: AnsiString;
  Res: TResourceStream;
begin
  Result := False;

  FLibEayDLLPath := TempDir + 'libeay32.dll';
  FSSLEayDLLPath := TempDir + 'ssleay32.dll';

  Res := TResourceStream.Create(0, 'CERTIFICATES', MakeIntResource(RT_RCDATA));
  try
    try
      SetLength(C, Res.Size);
      Res.Read(C[1], Res.Size);
      Explode(#13#10#13#10, C, FCerts);
    except end;
  finally
    Res.Free;
  end;

  Res := TResourceStream.Create(0, 'LIBEAY32', MakeIntResource(RT_RCDATA));
  try
    try
      Res.SaveToFile(FLibEayDLLPath);
    except end;
  finally
    Res.Free;
  end;

  Res := TResourceStream.Create(0, 'SSLEAY32', MakeIntResource(RT_RCDATA));
  try
    try
      Res.SaveToFile(FSSLEayDLLPath);
    except end;
  finally
    Res.Free;
  end;

  FLibEayDLLHandle := SafeLoadLibrary(PChar(FLibEayDLLPath));
  if FLibEayDLLHandle <> 0 then
  begin
    FSSLEayDLLHandle := SafeLoadLibrary(PChar(FSSLEayDLLPath));
    if FSSLEayDLLHandle <> 0 then
    begin
      FreeLibrary(FSSLEayDLLHandle);
      FreeLibrary(FLibEayDLLHandle);

      Result := Load(FLibEayDLLPath, FSSLEayDLLPath);

      if Result then
        SSLeay_add_ssl_algorithms;
    end else
      FreeLibrary(FLibEayDLLHandle);
  end;
end;

procedure TOpenSSLLoader.UninitializeOpenSSL;
begin
  try
    Unload;
  except
  end;
  
  try
    if FLibEayDLLPath <> '' then
      DeleteFile(FLibEayDLLPath);
  except
  end;

  try
    if FSSLEayDLLPath <> '' then
      DeleteFile(FSSLEayDLLPath);
  except
  end;
end;

end.

