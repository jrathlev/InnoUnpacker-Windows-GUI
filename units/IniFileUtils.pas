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

   last updated: Feb. 2017
   last modified: February 2023
   *)

unit IniFileUtils;

interface

uses System.IniFiles;

const
  UcBom = $FEFF;   //UniCode-Signatur (Little Endian)
  UcBomH = $FFFE;   //UniCode-Signatur (High Endian)
  Utf8BomS : RawByteString = #$EF#$BB#$BF;
  Utf8Bom : array[0..2] of byte = ($EF,$BB,$BF);

type
{ ------------------------------------------------------------------- }
// Unicode extensions to TIniFile
  TUnicodeIniFile = class(TIniFile)
    FFileName : string;
    constructor CreateForRead (const FileName: string);
    constructor CreateForWrite (const FileName: string);
    function ReadString(const Section, Ident, Default: string): string; override;
    function ReadInt64(const Section, Ident: string; Default: int64): int64;
    procedure WriteInt64(const Section, Ident: string; Value: int64);
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
function UpdateIniFile(const Filename) : boolean;

{ ---------------------------------------------------------------- }
implementation

uses System.SysUtils, System.Classes, Winapi.Windows;

{ ---------------------------------------------------------------- }
// Unicode extensions to TIniFile
constructor TUnicodeIniFile.CreateForRead (const FileName: string);
begin
  FFileName:=FileName;
  inherited Create(FileName);
  end;

// force Unicode to ini file
constructor TUnicodeIniFile.CreateForWrite (const FileName: string);
const
  UcSection = '[Unicode]';
var
  fs  : TFileStream;
  uid : word;
  sl  : TStringList;
begin
  FFileName:=FileName;
  if FileExists(FileName) then begin
    fs:=TFileStream.Create(FileName,fmOpenReadWrite);
    try
      fs.Read(uid,2);
      if uid<>UcBom then begin  // in Unicode konvertieren
        fs.Position:=0;
        sl:=TStringList.Create;
        try
          sl.LoadFromStream(fs);
          sl.Insert(0,UcSection);
          fs.Size:=0;    // siehe THandleStream.SetSize
          sl.SaveToStream(fs,TEncoding.Unicode);
        finally
          sl.Free;
          end;
        end
    finally
      fs.Free;
      end;
    end
  else if DirectoryExists(ExtractFilePath(FileName)) then begin
    try
      fs:=TFileStream.Create(FileName,fmCreate);
      sl:=TStringList.Create;
      sl.Add(UcSection);
      try
        sl.SaveToStream(fs,TEncoding.Unicode);
      finally
        sl.Free;
        end;
    finally
      fs.Free;
      end;
    end;
  inherited Create(FileName);
  end;

// replace original function to increase the string length to more than 2047 chars
function TUnicodeIniFile.ReadString(const Section, Ident, Default: string): string;
var
  Buffer: array[0..65535] of Char;
begin
  SetString(Result, Buffer, GetPrivateProfileString(PChar(Section),
    PChar(Ident), PChar(Default), Buffer, Length(Buffer), PChar(FFileName)));
  end;

// are missing in System.IniFiles
function TUnicodeIniFile.ReadInt64(const Section, Ident: string; Default: int64): int64;
var
  IntStr: string;
begin
  IntStr := ReadString(Section,Ident,'');
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
  with TIniFile.Create(IniName) do begin
    Result:=ReadString(Section,Ident,Default);
    Free;
    end;
  end;

function ReadIntegerFromIni (const IniName,Section,Ident : string; Default : integer) : integer;
begin
  with TIniFile.Create(IniName) do begin
    Result:=ReadInteger(Section,Ident,Default);
    Free;
    end;
  end;

function ReadBoolFromIni (const IniName,Section,Ident : string; Default : boolean) : boolean;
begin
  with TIniFile.Create(IniName) do begin
    Result:=ReadBool(Section,Ident,Default);
    Free;
    end;
  end;

//-----------------------------------------------------------------------------
function WriteStringToIniFile(const Filename,Section,Ident,Value: String) : boolean;
begin
  Result:=WritePrivateProfileString(PChar(Section),PChar(Ident),PChar(Value),PChar(FileName));
  end;

function WriteIntegerToIniFile(const Filename,Section,Ident: string; Value: Longint) : boolean;
begin
  Result:= WriteStringToIniFile(Filename,Section,Ident,IntToStr(Value));
  end;

function WriteBoolToIniFile(const Filename,Section,Ident: string; Value: Boolean) : boolean;
const
  Values: array[Boolean] of string = ('0','1');
begin
  Result:=WriteStringToIniFile(Filename,Section,Ident,Values[Value]);
  end;

function EraseSectionFromIniFile(const Filename,Section: string) : boolean;
begin
  Result:=WritePrivateProfileString(PChar(Section),nil,nil,PChar(FileName));
  end;

function DeleteKeyFromIniFile(const Filename,Section,Ident: String) : boolean;
begin
  Result:=WritePrivateProfileString(PChar(Section),PChar(Ident),nil,PChar(FileName));
  end;

function UpdateIniFile(const Filename) : boolean;
begin
  Result:=WritePrivateProfileString(nil,nil,nil,PChar(FileName));
  end;

end.
 
