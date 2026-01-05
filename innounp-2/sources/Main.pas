unit Main;

interface

uses System.Classes, System.SysUtils, System.UITypes, MyTypes, Struct;

type
  TEntryType = (seLanguage, sePermission, seType, seComponent, seTask, seDir, seKeySig,
    seFile, seFileLocation, seIcon, seIni, seRegistry, seInstallDelete,
    seUninstallDelete, seRun, seUninstallRun, seCustomMessage);

  TCommandAction=(caInstallInfo, caShortList, caVerboseList, caExtractFiles,
                  caWriteCode, caVersionList, caLanguageList);

  EMessage = class(Exception); // Exception raised by our code, The Message field (string)
                               // holds all necessary information, No need to display
                               // the exception class, ErrorMessage field, Win32 last error,
                               // or (try to) track down error address.

  EFatalError = class(Exception); // Halt program. Only handled by the top-level handler,
                                  // not by local handlers. Typically raised when an error
                                  // has been caught and processed but the execution can not
                                  // continue.

const
  TextAlign = 30;

  clWhite = TColors.White;
  clRed = TColors.Red;
  clGreen = TColors.Green;
  clBlue = TColors.Blue;
  clMaroon = TColors.Maroon;
  clPurple = TColors.Purple;
  clAqua = TColors.Aqua;


var
  SetupLdrOffset0: int64;
  SetupLdrOffset1: int64;
  SetupLdrMode: Boolean = true;
  SetupLdrOriginalFilename: String;
  UpVersion : string;
  Entries: array[TEntryType] of TList;
  SetupEncryptionHeader: TSetupEncryptionHeader;
  SetupHeader: TSetupHeader;
  TimeStampsInUTC:boolean;
  SetUpMinVersion : TSetupVersionData;
  Ver: integer = 0;
  VerIsUnicode: boolean = false;
  VerIsRT: boolean = false; // For ResTools custom versions
  VerIsISX: Boolean = false;

  StdOutputHandle: THandle;
  ConsoleFg : word = 7;
  ConsoleBg : word = 0;

  WizardImages, WizardSmallImages: TStringList;
  DecompDll:Ansistring;

  UseUtf8 : boolean=false;
  ColorMode : integer = 1;

procedure InternalError(const Id: String);
function TestPassword(const Password: AnsiString): Boolean;
function TestPasswordIs64(const EncryptionKey: TSetupEncryptionKey): Boolean;
function GetSliceName(ASlice:integer):string;
function FixPasswordEncoding(const Password: AnsiString) : AnsiString;

function StrToOem(const s:string):AnsiString;
function OemToStr(const s:AnsiString):string;
procedure write(s:string);
procedure writeln(const s:string = '');
procedure WriteColorText (const Text1,Text2 : string; Color1,Color2 : TColor; NewLine : boolean = true);
procedure WriteNormalText (const Text1 : string = ''; const Text2 : string = '');
procedure WriteNormalLine (const Text1 : string = ''; const Text2 : string = '');
procedure WriteHighLightLine (const Text2 : string);
procedure WriteBlueLine (const Text1,Text2 : string);
procedure WriteErrorLine (const Text1 : string; const Text2 : string); overload;
procedure WriteErrorLine (const Text2 : string); overload;
procedure WriteFormatLine (const Text1,Text2 : string; Color1,Color2 : TColor);

function ExtSp (const S : string; len : integer) : string;
function GroupDigits (const s : string; Sep : char = ' ') : string;
function VersionToString (ver : integer) : string;

function AddFakeFile(const FileName,FileContents : String;
                     RenameNow:boolean=false):integer;

function FileContents(const Filename:string) : string;
function MakeDir(Dir: String) : Boolean;

function GetCustomMessage (const AText : string) : string;

procedure GenerateEncryptionKey(const Password: String; const Salt: TSetupKDFSalt;
  const Iterations: Integer; out Key: TSetupEncryptionKey);

implementation

uses Winapi.Windows, System.StrUtils, MD5, SHA1, SHA256, ChaCha20, PathFunc, PBKDF2, CmnFunc2, Msgs, MsgIds;

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

function TestPasswordIs64(const EncryptionKey: TSetupEncryptionKey): Boolean;
var
  Nonce : TSetupEncryptionNonce;
  Context : TChaCha20Context;
  PasswordTest : integer;
begin
  { Do same as compiler did in GeneratePasswordTest and compare results }
  Nonce := SetupHeader.Is64Encryption.EncryptionBaseNonce;
  Nonce.RandomXorFirstSlice := Nonce.RandomXorFirstSlice xor -1;

  XChaCha20Init(Context, EncryptionKey[0], Length(EncryptionKey), Nonce, SizeOf(Nonce), 0);
  PasswordTest := 0;
  XChaCha20Crypt(Context, PasswordTest, PasswordTest, SizeOf(PasswordTest));

  Result := PasswordTest = SetupHeader.Is64Encryption.PasswordTest;
end;

function TestPassword(const Password: AnsiString): Boolean;
var
  Hash: TSetupHash;
begin
  Hash := SetupHeader.PasswordHash;
  case Hash.HashType of
    htSHA256:
      Result := true; // should never happen, see TestPassword
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

function StrToOem(const s:string) : AnsiString;
begin
  SetLength(Result,length(s));
  CharToOem(PChar(s),PAnsiChar(Result));
end;

function OemToStr(const s : AnsiString) : string;
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
  write(s+sLineBreak);
end;

function SetConsoleColor(AColor : TColor) : string;
var
  attr : word;
begin
  Result:='';
  if ColorMode=1 then begin
    case AColor of
      clRed:    attr:=FOREGROUND_RED or FOREGROUND_INTENSITY;
      clGreen:  begin
                if ConsoleBg=0 then attr:=FOREGROUND_GREEN or FOREGROUND_INTENSITY
                else attr:=FOREGROUND_GREEN;
                end;
      clBlue:   begin
                if ConsoleBg=0 then attr:=FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY
                else attr:=FOREGROUND_BLUE or FOREGROUND_INTENSITY;
                end;
      clMaroon: attr:=FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY;
      clPurple: attr:=FOREGROUND_RED or FOREGROUND_BLUE or FOREGROUND_INTENSITY;
      else attr:=ConsoleFg;
      end;
    SetConsoleTextAttribute(StdOutputHandle,ConsoleBg or attr);
    end
  else if ColorMode=2 then begin
    case AColor of
      clRed:    attr:=1;
      clGreen:  attr:=2;
      clBlue:   attr:=3;
      clMaroon: attr:=4;
      clPurple: attr:=5;
      else attr:=0;
      end;
    Result:='<'+IntToStr(attr)+'>';
    end;
  end;

procedure WriteColorText (const Text1,Text2 : string; Color1,Color2 : TColor; NewLine : boolean);
var
  pf : string;
begin
  pf:=SetConsoleColor(Color1);
  if length(Text2)>0 then begin
    if length(Text1)>0 then write(pf+Text1);
    pf:=SetConsoleColor(Color2);
    write(pf+Text2);
    if Newline then writeln;
    end
  else if length(Text1)>0 then begin
    write(pf+Text1);
    if Newline then writeln;
    end;
  end;

procedure WriteNormalText (const Text1,Text2 : string);
begin
  WriteColorText(Text1,Text2,clWhite,clGreen,false);
  end;

procedure WriteNormalLine (const Text1,Text2 : string);
begin
  WriteColorText(Text1,Text2,clWhite,clGreen);
  end;

procedure WriteHighLightLine (const Text2 : string);
begin
  WriteColorText('',Text2,clWhite,clGreen);
  end;

procedure WriteBlueLine (const Text1,Text2 : string);
begin
  WriteColorText(Text1,Text2,clWhite,clBlue);
  end;

procedure WriteErrorLine (const Text1,Text2 : string);
begin
  WriteColorText(Text1,Text2,clWhite,clRed);
  end;

procedure WriteErrorLine (const Text2 : string);
begin
  WriteErrorLine ('',Text2);
  end;

procedure WriteFormatLine (const Text1,Text2 : string; Color1,Color2 : TColor);
var
  n : integer;
begin
  n:=Pos('%',Text1);
  if n>0 then begin
    WriteColorText(copy(Text1,1,n-1),Text2,Color1,Color2,false);
    WriteColorText(copy(Text1,n+1,length(Text1)),'',Color1,Color2);
    end
  else WriteColorText(Text1,Text2,Color1,Color2);
  end;

function ExtSp (const S : string; len : integer) : string;
var
  i  : integer;
begin
  Result:=s;
  for i:=succ(length(Result)) to len do Result:=Result+' ';
  end;

function GroupDigits (const s : string; Sep : char) : string;
var
  i : integer;
const
  Group = 3;
begin
  Result:=s;
  if (Group>0) then begin
    i:=length(Result);
    while (i>Group) do begin
      dec(i,Group); Insert(Sep,Result,i+1);
      end;
    end;
  end;

function VersionToString (ver : integer) : string;
begin
  Result:=IntToStr(ver div 1000)+'.'+IntToStr(ver mod 1000 div 100)+'.'+IntToStr(ver mod 100);
  end;

function DateTimeToFileTime (dt : TDateTime) : TFileTime;
var
  st : TSystemTime;
  ft : TFileTime;
begin
  with st do begin
    DecodeDate(dt,wYear,wMonth,wDay);
    DecodeTime(dt,wHour,wMinute,wSecond,wMilliseconds);
    end;
  SystemTimeToFileTime(st,ft);
  if TimeStampsInUTC then LocalFileTimeToFileTime(ft,Result)
  else Result:=ft;
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
    TimeStamp:=DateTimeToFileTime(Now);
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

function GetCustomMessage (const AText : string) : string;
var
  MsgName,sm : string;
  Args       : array of string;
  n,lo       : integer;

  function ReadNxtStr (var s : string) : string;
  var
    i : integer;
  begin
    if length(s)>0 then begin
      i:=pos (',',s);
      if i=0 then i:=succ(length(s));
      Result:=copy(s,1,pred(i));
      delete(s,1,i);
      end
    else Result:='';
    end;

  function FindCustomMessage (const AName : string; var AText : string) : boolean;
  var
    i : integer;
  begin
    Result:=false;
    if Entries[seCustomMessage].Count>0 then begin
      for i:=0 to Entries[seCustomMessage].Count-1 do with PSetupCustomMessageEntry(Entries[seCustomMessage][i])^ do begin
        if AnsiSameText(Name,AName) and (LangIndex=0) then begin
          AText:=Value; Result:=true;
          Break;
          end;
        end;
      end
    end;

  function CmFormat (const AText : string; Args : array of string) : string;
  var
    n,k,j : integer;
  begin
    n:=1; Result:=AText;
    repeat
      k:=PosEx('%',AText,n);
      if k>0 then begin
        if TryStrToInt(copy(AText,k+1,1),j) and (j>0) and (j<=length(Args)) then
          Result:=AnsiReplaceText(Result,copy(AText,k,2),Args[j-1]);
        n:=k+1;
        end;
      until k=0;
    end;

begin
  if AnsiStartsText('{cm:',AText) then begin
    sm:=copy(AText,5,length(AText)-5);
    MsgName:=ReadNxtStr(sm);
    Args:=nil; n:=0;
    if not sm.IsEmpty then repeat
      SetLength(Args,n+1);
      Args[n]:=ReadNxtStr(sm);
      inc(n);
      until sm.IsEmpty;
    if FindCustomMessage(MsgName,sm) then Result:=CmFormat(sm,Args)
    else Result:=AText;
    end
  else Result:=AText;
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

procedure GenerateEncryptionKey(const Password: String; const Salt: TSetupKDFSalt;
  const Iterations: Integer; out Key: TSetupEncryptionKey);
var
  SaltBytes,
  KeyBytes : TBytes;
  SaltSize,KeyLength : integer;
begin
  SaltSize := SizeOf(Salt);
  SetLength(SaltBytes, SaltSize);
  Move(Salt[0], SaltBytes[0], SaltSize);
  KeyLength := SizeOf(Key);
  KeyBytes := PBKDF2SHA256(Password, SaltBytes, Iterations, KeyLength);
  Move(KeyBytes[0], Key[0], KeyLength);
end;

end.
