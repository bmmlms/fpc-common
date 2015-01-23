{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2015 Alexander Nottelmann

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
  Windows, SysUtils, Classes, AppData, Generics.Collections,
  IdSSLOpenSSLHeadersCustom;

type
  TOpenSSLLoader = class
  private
    FLibEayDLLPath: string;
    FSSLEayDLLPath: string;
    FCert: AnsiString;

    FLibEayDLLHandle: Cardinal;
    FSSLEayDLLHandle: Cardinal;

    procedure UninitializeOpenSSL;
  public
    OpenSSLLoaded: Boolean;

    constructor Create;
    destructor Destroy; override;
    function InitializeOpenSSL: Boolean;
  property
    Cert: AnsiString read FCert;
  end;

var
  OpenSSL: TOpenSSLLoader;

implementation

{ TBassLoader }

constructor TOpenSSLLoader.Create;
begin
  inherited;

  FCert := 'AddTrust External Root'#13#10 +
           '======================'#13#10 +
           '-----BEGIN CERTIFICATE-----'#13#10 +
           'MIIENjCCAx6gAwIBAgIBATANBgkqhkiG9w0BAQUFADBvMQswCQYDVQQGEwJTRTEUMBIGA1UEChML'#13#10 +
           'QWRkVHJ1c3QgQUIxJjAkBgNVBAsTHUFkZFRydXN0IEV4dGVybmFsIFRUUCBOZXR3b3JrMSIwIAYD'#13#10 +
           'VQQDExlBZGRUcnVzdCBFeHRlcm5hbCBDQSBSb290MB4XDTAwMDUzMDEwNDgzOFoXDTIwMDUzMDEw'#13#10 +
           'NDgzOFowbzELMAkGA1UEBhMCU0UxFDASBgNVBAoTC0FkZFRydXN0IEFCMSYwJAYDVQQLEx1BZGRU'#13#10 +
           'cnVzdCBFeHRlcm5hbCBUVFAgTmV0d29yazEiMCAGA1UEAxMZQWRkVHJ1c3QgRXh0ZXJuYWwgQ0Eg'#13#10 +
           'Um9vdDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALf3GjPm8gAELTngTlvtH7xsD821'#13#10 +
           '+iO2zt6bETOXpClMfZOfvUq8k+0DGuOPz+VtUFrWlymUWoCwSXrbLpX9uMq/NzgtHj6RQa1wVsfw'#13#10 +
           'Tz/oMp50ysiQVOnGXw94nZpAPA6sYapeFI+eh6FqUNzXmk6vBbOmcZSccbNQYArHE504B4YCqOmo'#13#10 +
           'aSYYkKtMsE8jqzpPhNjfzp/haW+710LXa0Tkx63ubUFfclpxCDezeWWkWaCUN/cALw3CknLa0Dhy'#13#10 +
           '2xSoRcRdKn23tNbE7qzNE0S3ySvdQwAl+mG5aWpYIxG3pzOPVnVZ9c0p10a3CitlttNCbxWyuHv7'#13#10 +
           '7+ldU9U0WicCAwEAAaOB3DCB2TAdBgNVHQ4EFgQUrb2YejS0Jvf6xCZU7wO94CTLVBowCwYDVR0P'#13#10 +
           'BAQDAgEGMA8GA1UdEwEB/wQFMAMBAf8wgZkGA1UdIwSBkTCBjoAUrb2YejS0Jvf6xCZU7wO94CTL'#13#10 +
           'VBqhc6RxMG8xCzAJBgNVBAYTAlNFMRQwEgYDVQQKEwtBZGRUcnVzdCBBQjEmMCQGA1UECxMdQWRk'#13#10 +
           'VHJ1c3QgRXh0ZXJuYWwgVFRQIE5ldHdvcmsxIjAgBgNVBAMTGUFkZFRydXN0IEV4dGVybmFsIENB'#13#10 +
           'IFJvb3SCAQEwDQYJKoZIhvcNAQEFBQADggEBALCb4IUlwtYj4g+WBpKdQZic2YR5gdkeWxQHIzZl'#13#10 +
           'j7DYd7usQWxHYINRsPkyPef89iYTx4AWpb9a/IfPeHmJIZriTAcKhjW88t5RxNKWt9x+Tu5w/Rw5'#13#10 +
           '6wwCURQtjr0W4MHfRnXnJK3s9EK0hZNwEGe6nQY1ShjTK3rMUUKhemPR5ruhxSvCNr4TDea9Y355'#13#10 +
           'e6cJDUCrat2PisP29owaQgVR1EX1n6diIWgVIEM8med8vSTYqZEXc4g/VhsxOBi0cQ+azcgOno4u'#13#10 +
           'G+GMmIPLHzHxREzGBHNJdmAPx/i9F4BrLunMTA5amnkPIAou1Z5jJh5VkpTYghdae9C8x49OhgQ='#13#10 +
           '-----END CERTIFICATE-----'#13#10;
end;

destructor TOpenSSLLoader.Destroy;
begin
  UninitializeOpenSSL;

  inherited;
end;

function TOpenSSLLoader.InitializeOpenSSL: Boolean;
var
  Res: TResourceStream;
begin
  Result := False;

  FLibEayDLLPath := AppGlobals.TempDir + 'libeay32.dll';
  FSSLEayDLLPath := AppGlobals.TempDir + 'ssleay32.dll';

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
    if FSSLEayDLLPath <> '' then
      DeleteFile(FSSLEayDLLPath);
  except
  end;
end;

end.

