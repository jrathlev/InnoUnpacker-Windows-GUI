unit Main;

interface

uses System.Classes, System.SysUtils, Struct;

type
  TEntryType = (seLanguage, sePermission, seType, seComponent, seTask, seDir,
    seFile, seFileLocation, seIcon, seIni, seRegistry, seInstallDelete,
    seUninstallDelete, seRun, seUninstallRun, seCustomMessage);

  TCommandAction=(caInstallInfo, {caListFiles,} caVerboseList, caExtractFiles,
                  caWriteCode, caVersionList);

  EMessage = class(Exception); // Exception raised by our code, The Message field (string)
                               // holds all necessary information, No need to display
                               // the exception class, ErrorMessage field, Win32 last error,
                               // or (try to) track down error address.

  EFatalError = class(Exception); // Halt program. Only handled by the top-level handler,
                                  // not by local handlers. Typically raised when an error
                                  // has been caught and processed but the execution can not
                                  // continue.
                                 
var
  SetupLdrOffset0: integer;
  SetupLdrOffset1: integer;
  SetupLdrMode: Boolean = true;
  SetupLdrOriginalFilename: String;
  Entries: array[TEntryType] of TList;
  SetupHeader: TSetupHeader;
  TimeStampsInUTC:boolean;
  SetUpMinVersion : TSetupVersionData;
  Ver: integer = 0;
  VerIsUnicode: boolean = false;
  VerIsRT: boolean = false; // For ResTools custom versions
  VerIsISX: Boolean = false;

  StdOutputHandle: THandle;

  WizardImages, WizardSmallImages: TStringList;
  DecompDll:Ansistring;

  UseUtf8 : boolean=false;

procedure InternalError(const Id: String);
function TestPassword(const Password: AnsiString): Boolean;
function GetSliceName(ASlice:integer):string;
function FixPasswordEncoding(const Password: AnsiString) : AnsiString;

function StrToOem(const s:string):AnsiString;
function OemToStr(const s:AnsiString):string;
procedure write(s:string);
procedure writeln(const s:string = '');

function AddFakeFile(const FileName,FileContents : String;
                     RenameNow:boolean=false):integer;

function FileContents(const Filename:string) : string;
function MakeDir(Dir: String) : Boolean;

implementation

uses Winapi.Windows, MD5, SHA1, PathFunc, MyTypes, CmnFunc2, Msgs, MsgIds;

procedure InternalError(const Id: String);
begin
  raise Exception.Create(Id);
end;

function TestPassword_MD5(const Password: AnsiString; const PasswordHash: TSetupHash;
  const PasswordSalt: TSetupSalt): Boolean;
var
  Context: TMD5Context;
  Hash: TMD5Digest;
begin
  MD5Init(Context);
  MD5Update(Context, PAnsiChar('PasswordCheckHash')^, Length('PasswordCheckHash'));
  MD5Update(Context, PasswordSalt, SizeOf(PasswordSalt));
  MD5Update(Context, Pointer(Password)^, Length(Password));
  Hash := MD5Final(Context);
  Result := MD5DigestsEqual(Hash, PasswordHash.MD5);
end;

function TestPassword_SHA1(const Password: AnsiString; const PasswordHash: TSetupHash;
  const PasswordSalt: TSetupSalt): Boolean;
var
  Context: TSHA1Context;
  Hash: TSHA1Digest;
begin
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar('PasswordCheckHash')^, Length('PasswordCheckHash'));
  SHA1Update(Context, PasswordSalt, SizeOf(PasswordSalt));
  SHA1Update(Context, Pointer(Password)^, Length(Password)*SizeOf(Password[1]));
  Hash := SHA1Final(Context);
  Result := SHA1DigestsEqual(Hash, PasswordHash.SHA1);
end;

function TestPassword(const Password: AnsiString): Boolean;
var
  Hash: TSetupHash;
begin
  Hash := SetupHeader.PasswordHash;
  case Hash.HashType of
    htSHA1:
      Result := TestPassword_SHA1(Password, Hash, SetupHeader.PasswordSalt);
    htMD5:
      Result := TestPassword_MD5(Password, Hash, SetupHeader.PasswordSalt);
  else
    Result := False;
  end;
end;

function GetSliceName(ASlice:integer):string;
var
  Major, Minor: Integer;
  Prefix:string;
begin
  if Ver>=4107 then Prefix := PathChangeExt(PathExtractName(SetupLdrOriginalFilename), '')
  else Prefix:=SetupHeader.BaseFilename;
  Major := ASlice div SetupHeader.SlicesPerDisk + 1;
  Minor := ASlice mod SetupHeader.SlicesPerDisk;
  if SetupHeader.SlicesPerDisk = 1 then
    Result := Format('%s-%d.bin', [Prefix, Major])
  else
    Result := Format('%s-%d%s.bin', [Prefix, Major, Chr(Ord('a') + Minor)]);
end;

function StrToOem(const s:string):AnsiString;
begin
  Result:=s; UniqueString(Result);
  CharToOemBuff(PChar(Result),PAnsiChar(Result),length(Result));
end;

function OemToStr(const s:AnsiString):string;
begin
  Result:=s; UniqueString(Result);
  OemToCharBuff(PAnsiChar(Result),PChar(Result),length(Result));
end;

// Delphi RTL does not check GetLastError() after getting GetFileType(hFile)==FILE_TYPE_UNKNOWN
// and incorrectly decides that an error occurred.
// This condition was observed when standard output was redirected by the parent process.
// Here is a workaround. Note that write/writeln now take exactly one argument of type string.
// It's also a good place to convert the characters into the OEM codepage.
// In case standard output is not a disk file, writes are split into 1K chunks to avoid
// out of memory errors.
procedure write(s:string);
var
  WriteOffset,BytesRemaining,BytesToWrite,BytesWritten:dword;
  sa : RawByteString;
begin
// changed: JR - October 2021
//  WriteConsole(GetStdHandle(STD_OUTPUT_HANDLE),PChar(s),Length(s),BytesWritten, nil);
  if UseUtf8 then sa:=Utf8Encode(s) else sa:=StrToOem(s);
  WriteOffset:=0; BytesRemaining:=length(sa);
  while BytesRemaining>0 do begin
    if BytesRemaining>1024 then BytesToWrite:=1024 else BytesToWrite:=BytesRemaining;
    WriteFile(StdOutputHandle,sa[WriteOffset+1],BytesToWrite,BytesWritten,nil);
{    if BytesWritten<>BytesToWrite then begin
      system.writeln('WriteFile failed. Requested=',BytestoWrite,' written=',BytesWritten,' error=',GetLastError);
      halt;
    end;}
    Inc(WriteOffset,BytesToWrite); Dec(BytesRemaining,BytesToWrite);
  end;
end;

procedure writeln(const s:string);
begin
  write(s+#13#10);
end;

function AddFakeFile(const FileName,FileContents : String;
                     RenameNow : boolean = false):integer;
var
  pFileEntry : PSetupFileEntry;
  pFileLocationEntry : PSetupFileLocationEntry;
  SystemTime : TSystemTime;
  i : integer;
begin
  Result := -1;
  if (FileName = '') or (FileContents = '') then Exit;

  pFileLocationEntry:=AllocMem(sizeof(TSetupFileLocationEntry));
  with pFileLocationEntry^ do begin
    Contents:=FileContents;
    OriginalSize:=length(FileContents);
    DateTimeToSystemTime(Now,SystemTime);
    SystemTimeToFileTime(SystemTime,TimeStamp);
    PrimaryFileEntry:=-1;
  end;
  i:=Entries[seFileLocation].Add(pFileLocationEntry);
  pFileEntry:=AllocMem(sizeof(TSetupFileEntry));
  with pFileEntry^ do begin
    FileType:=ftFakeFile;
    DestName:=FileName; // RenameFiles() will split this
    LocationEntry:=i;
    if RenameNow then begin
      SourceFilename:=FileName; DestName:=''; DestDir:='';
    end;
  end;
  Result:=Entries[seFile].Add(pFileEntry);
end;

function FileContents(const Filename : string) : string;
var f:File;
begin
  AssignFile(f, Filename); Result:='';
  Reset(f,1);
  SetLength(Result, FileSize(f));
  BlockRead(f, Result[1], FileSize(f));
  CloseFile(f);
end;

function MakeDir(Dir : String) : Boolean;
{ Returns True if a new directory was created }
var
  ErrorCode: DWORD;
begin
  Result := False;
  Dir := RemoveBackslash(ExpandFilename(Dir));
  if (Dir = '') or (AnsiLastChar(Dir)^ = ':') or (ExtractFilePath(Dir) = Dir) then
    Exit;
  if DirExists(Dir) then
    Exit;

  MakeDir(ExtractFilePath(Dir));
  if not CreateDirectory(PChar(Dir), nil) then begin
    ErrorCode := GetLastError;
    raise Exception.Create(FmtSetupMessage(msgLastErrorMessage,
      [FmtSetupMessage1(msgErrorCreatingDir, Dir), IntToStr(ErrorCode),
       SysErrorMessage(ErrorCode)]));
  end;
  Result := True;
end;

function FixPasswordEncoding(const Password: AnsiString) : AnsiString;
var
  tmpWS: WideString;
  nWideBytesCount: integer;
begin
  Result := Password;
  if (VerIsUnicode) and (Password <> '') then
  begin
    tmpWS := Password;

    nWideBytesCount := Length(Result) * SizeOf(WideChar);
    SetLength(Result, nWideBytesCount);
    CopyMemory(@Result[1], PAnsiChar(@tmpWS[1]), nWideBytesCount);
  end;
end;

end.
