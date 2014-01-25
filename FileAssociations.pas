{
    ------------------------------------------------------------------------
    mistake.ws common application library
    Copyright (c) 2010-2014 Alexander Nottelmann

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
unit FileAssociations;

interface

uses
  Windows, SysUtils, Registry, ShlObj;

function RegisterAssociation(Ext, FileType, Desc, Filename, Verb: string; Icon: Integer): Boolean;
function UnregisterAssociation(Ext, FileType: string): Boolean;

implementation

function RegisterAssociation(Ext, FileType, Desc, Filename, Verb: string; Icon: Integer): Boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CLASSES_ROOT;
      Result := Reg.OpenKey(Ext, True);
      if not Result then
        raise Exception.Create('');
      Reg.WriteString('', FileType);
      Reg.CloseKey;

      Result := Reg.OpenKey(FileType, True);
      if not Result then
        raise Exception.Create('');
      Reg.WriteString('', Desc);
      Reg.CloseKey;

      Reg.OpenKey(FileType + '\DefaultIcon', True);
      Reg.WriteString('', Filename + ',' + IntToStr(Icon));
      Reg.CloseKey;

      Result := Reg.OpenKey(FileType + '\Shell\Open', True);
      if not Result then
        raise Exception.Create('');
      Reg.WriteString('', Verb);
      Reg.CloseKey;

      Result := Reg.OpenKey(FileType + '\Shell\Open\Command', True);
      if not Result then
        raise Exception.Create('');
      Reg.WriteString('', '"' + Filename + '" "%1"');
      Reg.CloseKey;

      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
    finally
      Reg.Free;
    end;
  except
    UnregisterAssociation(Ext, FileType);
    Result := False;
  end;
end;

function UnregisterAssociation(Ext, FileType: string): Boolean;
var
  Reg: TRegistry;
  R1, R2: Boolean;
begin
  Result := True;
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CLASSES_ROOT;
      R1 := Reg.DeleteKey(Ext);
      R2 := Reg.DeleteKey(FileType);

      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
      if (not R1) and (not R2) then
        Result := False;
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

end.
