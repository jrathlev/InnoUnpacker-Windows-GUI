unit SetupLdr;

interface

uses FileClass, Struct, Compress;

procedure SetupCorruptError;

function GetSetupLdrOffsetTableFromFile(SourceF:TFile; var OffsetTable:TSetupLdrOffsetTable): boolean;
function GetSetupLdrOffsetTableFromResource(Filename:string; SourceF:TFile; var OffsetTable:TSetupLdrOffsetTable): boolean;

procedure HeuristicVersionFinder(Cache: TCacheReader; var Ver: Integer);
function IsVersionSuspicious(AVer: Integer) : Boolean;

procedure RenameFiles(ExtractAllCopies: Boolean);

implementation

uses Winapi.Windows, System.Classes, System.SysUtils, Main, Msgs, MyTypes, SetupEnt, PathFunc;

procedure SetupCorruptError;
begin
  raise EMessage.Create(SSetupFileCorrupt);
end;

function CheckCrc(SourceF:TFile; RawOffsetTable:pointer;
  const OffsetTable:TSetupLdrOffsetTable; OffsetTableSize: Integer):boolean;
begin
  if OffsetTable.TableCRCUsed and
     (GetCRC32(RawOffsetTable^, OffsetTableSize-sizeof(OffsetTable.TableCRC)) <> OffsetTable.TableCRC)
    then SetupCorruptError;
  if (SourceF.CappedSize < longword(OffsetTable.TotalSize)) then SetupCorruptError;
  Result:=true;
end;

function GetSetupLdrOffsetTableFromResource(Filename:string; SourceF:TFile; var OffsetTable:TSetupLdrOffsetTable): boolean;
var
  hMod: HMODULE;
  Rsrc: HRSRC;
  ResData: HGLOBAL;
  p : pbytearray; //pointer;
  id : TIdArray;
  v : cardinal;
//  p : PSetupLdrOffsetTable;  // changes: JR - August 2020
  VerObject: TInnoVer;
begin
  Result:=false;
  hMod := LoadLibraryEx(PChar(Filename), 0, LOAD_LIBRARY_AS_DATAFILE);

  // Loading very large files in datafile mode requires a lot of memory
  // And can sometimes fail if system memory is not very large.
  // If this is the case then let's try to load file a regular module
  if (hMod = 0) and (GetLastError() = ERROR_NOT_ENOUGH_MEMORY) then
    hMod := LoadLibraryEx(PChar(Filename), 0, 0);

  if hMod=0 then exit;
  repeat
    Rsrc := FindResource(hMod, MAKEINTRESOURCE(SetupLdrOffsetTableResID), RT_RCDATA);
    if Rsrc = 0 then break;
    ResData := LoadResource(hMod, Rsrc);
    if ResData = 0 then break;
    p := LockResource(ResData);
    if p = nil then break;
    move(p^[0],id[1],12);
    move(p^[12],v,4);
    Result := GetVersionBySetupId(Id,v,VerObject);
    if not Result then break;
    VerObject.UnifySetupLdrOffsetTable(p^, OffsetTable);
    Result:=CheckCrc(SourceF, p, OffsetTable, VerObject.OfsTabSize);
  until true;
  FreeLibrary(hMod);
end;

function GetSetupLdrOffsetTableFromFile(SourceF:TFile; var OffsetTable:TSetupLdrOffsetTable): boolean;
var
  SetupID: TIdArray;
  SizeOfFile, SizeDif: integer;
  ExeHeader: TSetupLdrExeHeader;
  RawOffsetTable: pointer;
  VerObject: TInnoVer;
begin
  Result:=false;
  SizeOfFile := SourceF.CappedSize;
  SourceF.Seek(SetupLdrExeHeaderOffset);
  SourceF.ReadBuffer(ExeHeader, SizeOf(ExeHeader));
  if (ExeHeader.ID <> SetupLdrExeHeaderID) or
    (ExeHeader.OffsetTableOffset <> not ExeHeader.NotOffsetTableOffset) then exit;
  SizeDif:=(ExeHeader.OffsetTableOffset + SizeOf(TSetupLdrOffsetTable4010) - SizeOfFile);
  if SizeDif>4 then exit; // other info might be appended after the offset table
  // assume that TSetupLdrOffsetTable.ID is the same size (12 bytes) for all versions
  SourceF.Seek(ExeHeader.OffsetTableOffset);
  SourceF.ReadBuffer(SetupID, sizeof(SetupID));
  SourceF.Seek(ExeHeader.OffsetTableOffset);
  if not GetVersionBySetupId(SetupId, 0, VerObject) then SetupCorruptError;
  GetMem(RawOffsetTable, VerObject.OfsTabSize);
  SourceF.ReadBuffer(RawOffsetTable^, VerObject.OfsTabSize);
  VerObject.UnifySetupLdrOffsetTable(RawOffsetTable^, OffsetTable);
  Result:=CheckCrc(SourceF, RawOffsetTable, OffsetTable, VerObject.OfsTabSize);
end;

function IsVersionSuspicious(AVer: Integer) : Boolean;
begin
  Result := (AVer = 3003) or (AVer = 4203);
end;

// if the function returns false, the current position in the input stream
// is undefined
function TryStr(Source:TCacheReader; StringCount,MaxSize:Cardinal):boolean;
var
  i,len:cardinal;
begin
  Result:=false;
  for i:=1 to StringCount do begin
    Source.Read(len,sizeof(len));
    if len>MaxSize then exit;
    Source.Skip(len);
  end;
  Result:=true;
end;

function TryInt(Source:TCacheReader; IntCount,MaxValue:Cardinal):boolean;
var
  i,int:Cardinal;
begin
  Result:=false;
  for i:=1 to IntCount do begin
    Source.Read(int,sizeof(int));
    if int>MaxValue then exit;
  end;
  Result:=true;
end;

procedure HeuristicVersionFinder(Cache:TCacheReader; var Ver: Integer);
const
  OfsPrivs3004=216; SzOpts3004=5; Str3004=21;
  Str4203=23; MoreStr4204=27-23; Entries4203=16;
var
  b:byte;
  bmk,bmk2:Cardinal;
  TryVer:integer;
begin
  with Cache do begin
    CacheEnabled:=true; bmk:=Bookmark;
    if Ver=3003 then begin
      SECompressedBlockSkip(Cache,OfsPrivs3004,0,Str3004);
      Read(b,1);
      if b>2 then Ver:=3003 else begin
        Cache.Skip(SzOpts3004);
        if TryStr(Cache,5,100) then Ver:=3004 else Ver:=3003;
      end;
    end else if Ver=4203 then begin
      SECompressedBlockSkip(Cache,Str4203*4,0,Str4203);
      bmk2:=Bookmark;
      if TryStr(Cache,MoreStr4204,256) then TryVer:=4204
      else begin TryVer:=4203; Seek(bmk2); end;
      Cache.Skip(32);
      if TryInt(Cache,Entries4203,10000) then Ver:=TryVer;
    end;
    Seek(bmk); CacheEnabled:=false;
  end;
end;

// Redistributes file name parameters to how they are in the iss script (DestName => Source,DestDir(,DestName))
// Also handles the duplicate names and/or contents
procedure RenameFiles(ExtractAllCopies: Boolean);
var
  list:TStringList;
  i,j,k,t:integer;
  pfe:PSetupFileEntry;
  loc:PSetupFileLocationEntry;
  fSameFileOnly: boolean;
begin
  list:=TStringList.Create;
  list.Sorted:=true;
  for i:=0 to Entries[seFile].Count-1 do begin
    pfe:=PSetupFileEntry(Entries[seFile][i]);
    with pfe^ do begin
      // Handle files with duplicate contents
      if LocationEntry<>-1 then begin
        loc:=PSetupFileLocationEntry(Entries[seFileLocation][LocationEntry]);
        if loc^.PrimaryFileEntry=-1 then loc^.PrimaryFileEntry:=i;
      end;
      // This is an attempt to fix some bugy compilations
      // with double \\ chars in path
      DestName := StringReplace(DestName, '\\', '\', [rfReplaceAll]);
      DestName := StringReplace(DestName, '{{', '{', [rfReplaceAll]);
      DestName := StringReplace(DestName, '/', '\', [rfReplaceAll]);
      // filter out the inappropriate characters
      if FileType=ftUninstExe then DestName:='embedded\uninstall.exe'
      else if FileType=ftRegSvrExe then DestName:='embedded\regsvr.exe';
      DestDir := PathExtractPath(DestName); SourceFileName:=DestName;
      DestName := PathExtractName(DestName);
      for t:=1 to length(SourceFileName) do                        // '/' and '\' are valid since
      // allow ´',' as valid character - JR June 2025
        if SourceFileName[t] in [':','*','?','"','<','>','|'] then // they work as path delimiters
//        if SourceFileName[t] in [',',':','*','?','"','<','>','|'] then // they work as path delimiters
          SourceFileName[t]:='_';                                  // even inside brace constants
      // count the duplicate file names           // . (period) is reserved for duplicate files - JR June 2025
      j:=list.IndexOf(SourceFileName);
      if j>=0 then list.Objects[j]:=pointer(integer(list.Objects[j])+1)
      else begin
        j:=list.Add(SourceFileName);
        list.Objects[j]:=pointer(1);
      end;
    end;
  end;
  for i:=0 to list.Count-1 do
    if integer(list.Objects[i])=1 then list.Objects[i]:=pointer(0);

  // If we have duplicate copies only and not using -a then
  // we should not append numbers since all dups will be presented only once
  // However if we have different files with same name then numbers should be added
  if (not ExtractAllCopies) then
    for i:=0 to list.Count - 1 do
    begin
      if (Integer(list.Objects[i]) <= 1) then continue; // Ignore unique files

      k := -1;
      fSameFileOnly := true;
      for j := 0 to Entries[seFile].Count - 1 do
      begin
        pfe := PSetupFileEntry(Entries[seFile][j]);
        if (pfe^.SourceFilename = list[i]) then
        begin
          if (pfe^.LocationEntry >= 0) then
          begin
            loc := PSetupFileLocationEntry(Entries[seFileLocation][pfe^.LocationEntry]);
            fSameFileOnly := (loc^.PrimaryFileEntry >= 0) and
              ((loc^.PrimaryFileEntry = k) or (k = -1));
            k := loc^.PrimaryFileEntry;
          end else
            fSameFileOnly := false;

          if not fSameFileOnly then break;
        end;
      end; //for j

      if (fSameFileOnly) then list.Objects[i] := pointer(0);
    end;  // for i

  // append numbers
  for i:=Entries[seFile].Count-1 downto 0 do begin
    pfe:=PSetupFileEntry(Entries[seFile][i]);
    with pfe^ do begin
      j:=list.IndexOf(SourceFileName);
      k:=integer(list.Objects[j]);
      if (k>0) and (FileType<>ftFakeFile) then begin
        SourceFileName:=PathChangeExt(SourceFileName,'')+','+IntToStr(k)+PathExtractExt(SourceFileName);
        // use '.' as separator instead of "," - JR June 2025
//        SourceFileName:=PathChangeExt(SourceFileName,'')+'.'+IntToStr(k)+PathExtractExt(SourceFileName);
        list.Objects[j]:=pointer(k-1);
      end;
      if DestName=PathExtractName(SourceFileName) then DestName:='';  // don't need the DestName if it's the same as Source
    end;
  end;
  list.Free;
end;

end.
