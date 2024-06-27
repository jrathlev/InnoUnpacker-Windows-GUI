unit CustomVersions;

interface

{ Package info structures }
{ Extracted from SysUtils.pas }

type
  PPkgName = ^TPkgName;
  TPkgName = packed record
    HashCode: Byte;
    Name: array[0..255] of AnsiChar;
  end;

  { PackageUnitFlags:
    bit      meaning
    -----------------------------------------------------------------------------------------
    0      | main unit
    1      | package unit (dpk source)
    2      | $WEAKPACKAGEUNIT unit
    3      | original containment of $WEAKPACKAGEUNIT (package into which it was compiled)
    4      | implicitly imported
    5..7   | reserved
  }
  PUnitName = ^TUnitName;
  TUnitName = packed record
    Flags : Byte;
    HashCode: Byte;
    Name: array[0..255] of AnsiChar;
  end;

  { Package flags:
    bit     meaning
    -----------------------------------------------------------------------------------------
    0     | 1: never-build                  0: always build
    1     | 1: design-time only             0: not design-time only      on => bit 2 = off
    2     | 1: run-time only                0: not run-time only         on => bit 1 = off
    3     | 1: do not check for dup units   0: perform normal dup unit check
    4..25 | reserved
    26..27| (producer) 0: pre-V4, 1: undefined, 2: c++, 3: Pascal
    28..29| reserved
    30..31| 0: EXE, 1: Package DLL, 2: Library DLL, 3: undefined
  }
  PPackageInfoHeader = ^TPackageInfoHeader;
  TPackageInfoHeader = packed record
    Flags: Cardinal;
    RequiresCount: Integer;
    {Requires: array[0..9999] of TPkgName;
    ContainsCount: Integer;
    Contains: array[0..9999] of TUnitName;}
  end;

function CheckResToolsVersion(Filename:string) : boolean;  

implementation

uses Winapi.Windows, System.SysUtils, System.StrUtils;

function PackageInfoTable(Module: HMODULE): PPackageInfoHeader;
var
  ResInfo: HRSRC;
  Data: THandle;
begin
  Result := nil;
  ResInfo := FindResource(Module, 'PACKAGEINFO', RT_RCDATA);
  if ResInfo <> 0 then
  begin
    Data := LoadResource(Module, ResInfo);
    if Data <> 0 then
    try
      Result := LockResource(Data);
      UnlockResource(Data);
    finally
      FreeResource(Data);
    end;
  end;
end;

function CheckResToolsVersion(Filename:string) : boolean;
var
  hMod: HMODULE;
  InfoTable: PPackageInfoHeader;
  i: integer;
  PkgName: PPkgName;
  UName : PUnitName;
  Count: Integer;
begin
  Result := false;
  hMod := LoadLibraryEx(PChar(Filename), 0, LOAD_LIBRARY_AS_DATAFILE);

  // Loading very large files in datafile mode requires a lot of memory
  // And can sometimes fail if system memory is not very large.
  // If this is the case then let's try to load file a regular module
  if (hMod = 0) and (GetLastError() = ERROR_NOT_ENOUGH_MEMORY) then
    hMod := LoadLibraryEx(PChar(Filename), 0, 0);

  if hMod=0 then exit;

  InfoTable := PackageInfoTable(hMod);

  if Assigned(InfoTable) then
  begin
    PkgName := PPkgName(Integer(InfoTable) + SizeOf(InfoTable^));
    { Skip the Requires list }
    for I := 0 to InfoTable.RequiresCount - 1 do Inc(Integer(PkgName), StrLen(PkgName.Name) + 2);
    Count := Integer(Pointer(PkgName)^);
    UName := PUnitName(Integer(PkgName) + 4);
    for I := 0 to Count - 1 do
    begin
      if AnsiStartsText('SetupLdr_D2009', UName.Name) then
      begin
        Result := True;
        break
      end;
      Inc(Integer(UName), StrLen(UName.Name) + 3);
    end;
  end;

  FreeLibrary(hMod);
end;

end.
