(* Delphi-Unit
   IniFile extension
   ==================

   1. Unicode extensions
   2. Read from and write to ini files without using object TIniFiles
      do not raise exceptions, error ist returned as result of function

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND,either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   March 2024 : changed to TMemIniFile
   last modified: June 2024
   *)

unit IniFileUtils;

interface

uses System.IniFiles;

type
{ ------------------------------------------------------------------- }
// Erase all section values, retain empty section
  TExtIniFile = class helper for TMemIniFile
    procedure EraseSectionValues (const Section : string);
  end;

// Unicode extensions to TMemIniFile
  TUnicodeIniFile = class(TMemIniFile)
  private
    FReadOnly : boolean;
  public
    constructor CreateForRead (const FileName: string);
    constructor CreateForWrite (const FileName: string; ClearEntries : boolean = false);
    destructor Destroy; override;
    procedure WriteProgramInfo (const Ident,Value: string);
    function ReadInt64 (const Section, Ident: string; Default: int64): int64;
    procedure WriteInt64 (const Section, Ident: string; Value: int64);
  end;


// Read from ini file
function ReadStringFromIni (const IniName,Section,Ident,Default : string) : string;
function ReadIntegerFromIni (const IniName,Section,Ident : string; Default : integer) : integer;
function ReadBoolFromIni (const IniName,Section,Ident : string; Default : boolean) : boolean;

// Write to ini file
function WriteStringToIniFile(const Filename,Section,Ident,Value: String) : boolean;
function WriteIntegerToIniFile(const Filename,Section,Ident: string; Value: Longint) : boolean;
function WriteBoolToIniFile(const Filename,Section,Ident: string; Value: Boolean) : boolean;
function EraseSectionFromIniFile(const Filename,Section: string) : boolean;
function DeleteKeyFromIniFile(const Filename,Section,Ident: String) : boolean;

{ ---------------------------------------------------------------- }
implementation

uses System.SysUtils, System.Classes, Winapi.Windows, FileConsts;

const
  UcSection = 'Unicode';
  EncId = 'Encoding';
  EncVal = 'UTF-16LE';

{ ---------------------------------------------------------------- }
// Erase all section values, retain empty section
procedure TExtIniFile.EraseSectionValues (const Section: string);
var
  i : integer;
begin
  with self.FSections do begin
    i:=IndexOf(Section);
    if i>=0 then (Objects[i] as TStringList).Clear;
    end;
  end;

{ ---------------------------------------------------------------- }
// Unicode extensions to TMemIniFile
constructor TUnicodeIniFile.CreateForRead (const FileName: string);
begin
  FReadOnly:=true;
  inherited Create(FileName);
  end;

// write Unicode to ini file
constructor TUnicodeIniFile.CreateForWrite (const FileName: string; ClearEntries : boolean);
begin
  FReadOnly:=false;
  inherited Create(FileName);
  if ClearEntries then Clear;
  Encoding:=TEncoding.Unicode;
  if ClearEntries or not SectionExists(UcSection) then begin
    WriteString(UcSection,EncId,EncVal);
    end
  else if not ValueExists(UcSection,EncId) then WriteString(UcSection,EncId,EncVal);
  end;

destructor TUnicodeIniFile.Destroy;
begin
  try
    if not FReadOnly then begin
      try
        UpdateFile;
      except
        on EStreamError do raise EWriteError.Create(Format(rsErrWriting,[FileName]));
      end;
    end;
  finally
    inherited Destroy;
    end;
  end;

// Add strings to Unicode section
procedure TUnicodeIniFile.WriteProgramInfo (const Ident,Value: string);
begin
  WriteString(UcSection,Ident,Value);
  end;

// are missing in System.IniFiles
function TUnicodeIniFile.ReadInt64(const Section, Ident: string; Default: int64): int64;
var
  IntStr: string;
begin
  IntStr:=ReadString(Section,Ident,'');
  if (IntStr.Length>2) and (IntStr.StartsWith('0x',true)) then IntStr:='$'+IntStr.Substring(2);
  Result:=StrToInt64Def(IntStr,Default);
  end;

procedure TUnicodeIniFile.WriteInt64(const Section, Ident: string; Value: int64);
begin
  WriteString(Section,Ident,IntToStr(Value));
  end;

//-----------------------------------------------------------------------------
function ReadStringFromIni (const IniName,Section,Ident,Default : string) : string;
begin
  with TMemIniFile.Create(IniName) do begin
    Result:=ReadString(Section,Ident,Default);
    Free;
    end;
  end;

function ReadIntegerFromIni (const IniName,Section,Ident : string; Default : integer) : integer;
begin
  with TMemIniFile.Create(IniName) do begin
    Result:=ReadInteger(Section,Ident,Default);
    Free;
    end;
  end;

function ReadBoolFromIni (const IniName,Section,Ident : string; Default : boolean) : boolean;
begin
  with TMemIniFile.Create(IniName) do begin
    Result:=ReadBool(Section,Ident,Default);
    Free;
    end;
  end;

//-----------------------------------------------------------------------------
function WriteStringToIniFile(const Filename,Section,Ident,Value: String) : boolean;
begin
  with TMemIniFile.Create(Filename) do begin
    WriteString(Section,Ident,Value);
    UpdateFile;
    Free;
    end;
  end;

function WriteIntegerToIniFile(const Filename,Section,Ident: string; Value: Longint) : boolean;
begin
  with TMemIniFile.Create(Filename) do begin
    WriteInteger(Section,Ident,Value);
    try
      UpdateFile;
    finally
      Free;
      end;
    end;
  end;

function WriteBoolToIniFile(const Filename,Section,Ident: string; Value: Boolean) : boolean;
begin
  with TMemIniFile.Create(Filename) do begin
    WriteBool(Section,Ident,Value);
    UpdateFile;
    try
      UpdateFile;
    finally
      Free;
      end;
    end;
  end;

function EraseSectionFromIniFile(const Filename,Section: string) : boolean;
begin
  with TMemIniFile.Create(Filename) do begin
    EraseSection(Section);
    try
      UpdateFile;
    finally
      Free;
      end;
    end;
  end;

function DeleteKeyFromIniFile(const Filename,Section,Ident: String) : boolean;
begin
  with TMemIniFile.Create(Filename) do begin
    DeleteKey(Section,Ident);
    try
      UpdateFile;
    finally
      Free;
      end;
    end;
  end;

end.
 
