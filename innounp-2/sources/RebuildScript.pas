unit RebuildScript;

interface

uses MyTypes;

function GetInstallScript : AnsiString;
function GetLanguageFile(i: Integer) : AnsiString;
function GetVersionText(MinVersion : TMySetupVersionData) : string;
function GetCompressMethodName(Method: TSetupCompressMethod) : String;
procedure AddEmbeddedFiles;

implementation

uses Winapi.Windows, System.SysUtils, System.Classes, Main, PathFunc, MD5, SHA1, Struct;

type
  TScriptBuilder = class
  private
    Res: AnsiString;
    CurSlice: Integer;

    procedure Print(const s: AnsiString);
    procedure PrintLn(const s: AnsiString = '');

    procedure StrParam (const DisplayName,Value : AnsiString; UseQuotes : boolean = true); overload;
    procedure StrParam(const DisplayName : AnsiString; const Value : string; UseQuotes : boolean = true); overload;
    procedure IntParam(const DisplayName : AnsiString; Value : Int64);
    procedure StrConst(const DisplayName : AnsiString; const Value : string; ShowAlways : boolean=false);
    procedure IntConst(const DisplayName : AnsiString; Value : Int64);
    procedure PrintComment(const Text: AnsiString);
    procedure PrintSectionHeader(const SectionName: AnsiString; PrependEmptyLine: boolean = true);
    procedure PrintFlagsParam(const FlagsStr: AnsiString);
    procedure PrintVersions(MinVersion, OnlyBelowVersion: TMySetupVersionData);

    procedure PrintSetupHeader(const sh: TSetupHeader);
    procedure PrintRegistryEntry(const re: TSetupRegistryEntry);
    procedure PrintRunEntry(const re:TSetupRunEntry);
    procedure PrintTaskEntry(const te:TSetupTaskEntry);
    procedure PrintComponentEntry(const ce:TSetupComponentEntry);
    procedure PrintTypeEntry(const te:TSetupTypeEntry);
    procedure PrintFileEntry(const fe:TSetupFileEntry);
    procedure PrintDeleteEntry(const de: TSetupDeleteEntry);
    procedure PrintCustomMessageEntry(const ce:TSetupCustomMessageEntry);
    procedure PrintIconEntry(const ie:TSetupIconEntry);
    procedure PrintDirEntry(const de: TSetupDirEntry);
    procedure PrintIniEntry(const ie: TSetupIniEntry);
    procedure PrintLanguageEntry(const le:TSetupLanguageEntry);

    procedure PrintLangOptions(const le:TSetupLanguageEntry);
  public
    function RebuildScript : AnsiString;
    function GetLanguageFile(i: Integer) : AnsiString;
  end;

function VerToStr(Ver: Cardinal; ServicePack: Word): String;
var
  Digits: Integer;
begin
  with TSetupVersionDataVersion(Ver) do begin
    Digits := 2;
    if Minor mod 10 = 0 then begin
      Dec (Digits);
      Minor := Minor div 10;
    end;
    FmtStr (Result, '%d.%.*d', [Major, Digits, Minor]);
    if Build <> 0 then
      Result := Result + Format('.%d', [Build]);
    if ServicePack <> 0 then begin
      Result := Result + ' Service Pack ' + IntToStr(Hi(ServicePack));
      if Lo(ServicePack) <> 0 then
        Result := Result + Format('.%d', [Lo(ServicePack)]);
    end;
  end;
end;

const
  HKEY_A = 1;  // equals HKLM in administrative install mode, HKCU otherwise

function RegRootToStr(const re:TSetupRegistryEntry):string;
begin
  with re do begin
    if rootkey=HKEY_CLASSES_ROOT then Result:='HKCR'
    else if rootkey=HKEY_CURRENT_USER then Result:='HKCU'
    else if rootkey=HKEY_LOCAL_MACHINE then Result:='HKLM'
    else if rootkey=HKEY_USERS then Result:='HKU'
    else if rootkey=HKEY_CURRENT_CONFIG then Result:='HKCC'
    else if rootkey=HKEY_A then Result:='HKA'
    else Result:='';
    if Result<>'' then
      if ro32bit in Options then Result:=Result+'32'
      else if ro64bit in Options then Result:=Result+'64';
  end;
end;

function DoubleQuotes(s : AnsiString) : AnsiString;
var
  i : integer;
begin
  for i:=length(s) downto 1 do
    if s[i]='"' then Insert('"',s,i);
  Result:=s;
end;

function MakeUtf8(const s : string) : AnsiString;
var
  sr  : RawByteString;
  len : integer;
begin
  if ScriptAsUtf8 then begin
    len:=Length(s);
    SetLength(sr,3*len+1);    // max. length of UTF8 string
    len:=UnicodeToUtf8(PAnsiChar(sr),Length(sr),PWideChar(s),len);
    SetLength(sr,len-1);
    Result:=sr;
    end
  else Result:=s;     // use default code page
  end;

function ApplyCodepage (const s : string; const le : TSetupLanguageEntry) : string;
begin
  with le do if VerIsUnicode or (LanguageCodePage=1252) then Result:=s
  else begin
    try
      Result:=TEncoding.GetEncoding(LanguageCodePage).GetString(TEncoding.ANSI.GetBytes(s));
    except
      Result:=Format('Invalid code page %u for "%s"',[LanguageCodePage,LanguageName]);
      end;
    end;
  end;

function Hash2Str(Hash: TSetupHash) : string;
begin
  case Hash.HashType of
    htMD5:  Result := MD5DigestToString(Hash.MD5);
    htSHA1: Result := SHA1DigestToString(Hash.SHA1);
  else
    Result := '<Unknown>';  
  end;
end;

function SaltToStr(const Salt : array of byte) : string;
var
  i: integer;
begin
  Result := '';
  for i := Low(Salt) to High(Salt) do begin
    Result := Result + IntToHex(Salt[i], 2);
    end;
  Result := LowerCase(Result);
end;

function ProcArcsToStr(Arcs: TMySetupProcessorArchitectures) : String;
  function ArchStr(a: TSetupProcessorArchitecture):string;
  begin
    case a of
      paX86         : Result:='x86';
      paAMD64       : Result:='x64';
      paIA64        : Result:='ia64';
    else Result:='';
    end;
  end;
var
  pa: TSetupProcessorArchitecture;
  t: String;
begin
  Result := '';
  for pa:=Low(pa) to High(pa) do
    if pa in Arcs then begin
      t := ArchStr(pa);
      if t<>'' then Result := Result + t + ' ';
    end;
  Result := Trim(Result);
end;

function GetCompressMethodName(Method: TSetupCompressMethod) : String;
begin
  case Method of
    cmStored: Result:='none';
    cmZip: Result:='zip';
    cmBzip: Result:='bzip2';
    cmLZMA: Result:='lzma';
    cmLZMA2: Result:='lzma2';
  end;
end;

function GetInnoVersionStr : string;
var
  Ver1, Ver2, Ver3: Integer;
begin
  Ver1 := Ver div 1000;
  Ver2 := (Ver div 100) - (Ver1 * 10);
  Ver3 := Ver mod 100; 

  Result := Format('%d.%d.%d', [Ver1, Ver2, Ver3]);
  if (VerIsUnicode) then
    Result := Result + ' (Unicode)';
end;

function GetBaseFileName() : string;
var
  name: string;
begin
  name := SetupHeader.BaseFilename;
  if (name = '') then
  begin
    name := ExtractFileName(SetupLdrOriginalFilename);
	// Remove .exe extenstion if have one
	if (Pos('.exe', name) = Length(name) - 3) then
	  name := Copy(name, 1, Length(name) - 4);
  end;
  Result := name;
end;

function MaybeToRtf(const Name : string; const Contents : AnsiString) : string;
begin
  if copy(Contents, 1, 5)='{\rtf' then Result:=ChangeFileExt(Name, '.rtf')
  else Result:=Name;
end;

procedure AddFakeRtfOrTxtFile(const Name : string; const Contents : AnsiString);
begin
  AddFakeFile(MaybeToRtf(Name, Contents), Contents);
end;

function GetWizardImageFileName(Index: integer; PngImage, IsSmallImage: Boolean) : String;
var
  sn,se : string;
begin
  if PngImage then se:='png' else se:='bmp';
  if IsSmallImage then sn:='WizardSmallImage' else sn:='WizardImage';
  Result := Format('embedded\%s%u.%s', [sn,Index,se])
  end;

function IsPngImage (const Contents : AnsiString) : boolean;
const
  PngHeader : Array[0..7] of AnsiChar = (#137, #80, #78, #71, #13, #10, #26, #10);
begin
  Result := (Length(Contents)>= 8) and
            CompareMem(@Contents[1], @PngHeader[0], 8);
  end;

procedure AddEmbeddedFiles;
var i:integer;
begin
  with SetupHeader do begin      // mirror these filenames in procedures above
    AddFakeRtfOrTxtFile('embedded\License.txt', LicenseText);
    AddFakeRtfOrTxtFile('embedded\InfoBefore.txt', InfoBeforeText);
    AddFakeRtfOrTxtFile('embedded\InfoAfter.txt', InfoAfterText);
    AddFakeFile('embedded\CompiledCode.bin', CompiledCodeText);
  end;
  for i:= 0 to WizardImages.Count - 1 do
    AddFakeFile(GetWizardImageFileName(i,IsPngImage(WizardImages[i]),false), WizardImages[i]);
  for i:= 0 to WizardSmallImages.Count - 1 do
    AddFakeFile(GetWizardImageFileName(i,IsPngImage(WizardSmallImages[i]),true), WizardSmallImages[i]);
  AddFakeFile('embedded\decompressor.dll', DecompDll);

  for i:=0 to SetupHeader.NumLanguageEntries-1 do
    with PSetupLanguageEntry(Entries[SeLanguage][i])^ do begin
      AddFakeFile('embedded\'+Name+'.isl', GetLanguageFile(i));
      AddFakeRtfOrTxtFile('embedded\'+Name+'License.txt', LicenseText);
      AddFakeRtfOrTxtFile('embedded\'+Name+'InfoBefore.txt', InfoBeforeText);
      AddFakeRtfOrTxtFile('embedded\'+Name+'InfoAfter.txt', InfoAfterText);
    end;
end;

function GetInstallScript : AnsiString;
var
  Builder: TScriptBuilder;
begin
  Builder := TScriptBuilder.Create;
  try
    Result := Builder.RebuildScript;
  finally
    Builder.Free;
  end;    
end;

function GetLanguageFile(i: Integer) : AnsiString;
var
  Builder: TScriptBuilder;
begin
  Builder := TScriptBuilder.Create;
  try
    Result := Builder.GetLanguageFile(i);
  finally
    Builder.Free;
  end;
end;

// Version descriptor in plain text
function GetVersionText(MinVersion : TMySetupVersionData) : string;
const
  sWindows = 'Windows';
var
  sv : string;
  nv : integer;
begin
  with MinVersion do begin
    if WinVersion<>0 then Result:=VerToStr(WinVersion,0)
    else begin   // version 6 and up
      sv:=VerToStr(NTVersion,NTServicePack);
      if TSetupVersionDataVersion(NtVersion).Major=5 then begin
        case TSetupVersionDataVersion(NtVersion).Minor of
        0 : Result:='Windows 2000';
        1 : Result:='Windows XP';
        2 : Result:='Windows Server 2003';
        else Result:='Unkown version';
          end;
        end
      else if TSetupVersionDataVersion(NtVersion).Major=6 then begin  // Windows 7 and 8
        case TSetupVersionDataVersion(NtVersion).Minor of
        1 : Result:='Windows 7';
        2 : Result:='Windows 8';
        3 : Result:='Windows 8.1';
        else Result:='Unkown version';
          end;
        end
      else begin // Windows 10 and 11
        if TSetupVersionDataVersion(NtVersion).Build<22000 then Result:='Windows 10'
        else Result:='Windows 11';
        end;
      Result:=Result+' ('+sv+')';
      end;
    end;
  end;

{ TScriptBuilder }

procedure TScriptBuilder.Print(const s : AnsiString);
begin
  Res:=Res+s;
end;

procedure TScriptBuilder.PrintLn(const s: AnsiString);
begin
  Res:=Res+s+sLineBreak;
end;

procedure TScriptBuilder.StrParam (const DisplayName,Value : AnsiString; UseQuotes : boolean = true);
begin
  if Value<>'' then begin
    if UseQuotes then
      Print(DisplayName + ': "' + DoubleQuotes(Value) + '"; ')
    else
      Print(DisplayName + ': ' + Value + '; ');
  end;
end;

procedure TScriptBuilder.StrParam (const DisplayName : AnsiString; const Value : string;
                                   UseQuotes :  boolean = true);
begin
  StrParam(DisplayName,MakeUtf8(Value),UseQuotes);
  end;

procedure TScriptBuilder.IntParam (const DisplayName : AnsiString; Value:Int64);
begin
  if Value <> 0 then StrParam(DisplayName, IntToStr(Value), True);
end;

procedure TScriptBuilder.StrConst (const DisplayName : AnsiString; const Value : string;
                                   ShowAlways : boolean = false);
begin
  if ShowAlways or (Value<>'') then PrintLn(DisplayName+'='+MakeUtf8(Value));
end;

procedure TScriptBuilder.IntConst (const DisplayName : AnsiString; Value:Int64);
begin
  StrConst(DisplayName, IntToStr(Value));
end;

procedure TScriptBuilder.PrintComment(const Text: AnsiString);
begin
  PrintLn('; ' + Text);
end;

procedure TScriptBuilder.PrintSectionHeader(const SectionName: AnsiString; PrependEmptyLine: boolean = true);
begin
  if (PrependEmptyLine) then PrintLn;
  PrintLn('[' + SectionName + ']');
end;

procedure TScriptBuilder.PrintFlagsParam(const FlagsStr: AnsiString);
begin
  if (FlagsStr <> '') then
    Print('Flags: ' + FlagsStr);
  PrintLn;
end;

procedure TScriptBuilder.PrintVersions(MinVersion, OnlyBelowVersion: TMySetupVersionData);
begin
  if (MinVersion.WinVersion<>SetUpMinVersion.WinVersion)
      or (MinVersion.NTVersion<>SetUpMinVersion.NTVersion)
      or (MinVersion.NTServicePack<>SetUpMinVersion.NTServicePack) then begin
    with MinVersion do
      if (WinVersion>$04000000) or (NTVersion>$04000000) then
        StrParam('MinVersion', VerToStr(WinVersion,0) + ',' + VerToStr(NTVersion,NTServicePack), False);
    with OnlyBelowVersion do
      if (WinVersion<>0) or (NTVersion<>0) then
        StrParam('OnlyBelowVersion', VerToStr(WinVersion,0) + ',' + VerToStr(NTVersion,NTServicePack), False);
  end;
end;

function TScriptBuilder.RebuildScript: AnsiString;
var
  i:integer;
begin
//  Res.Init;
  Res:='';

// Add UTF-8 BOM to script start for Unicode versions.
  if ScriptAsUtf8 then Print(#$EF#$BB#$BF);

  PrintSetupHeader(SetupHeader);

  CurSlice:=0;
  if Entries[seFile].Count>0 then begin
    PrintSectionHeader('Files');
    for i:=0 to Entries[seFile].Count-1 do
      PrintFileEntry(PSetupFileEntry(Entries[seFile][i])^);
  end;
  if Entries[seDir].Count>0 then begin
    PrintSectionHeader('Dirs');
    for i:=0 to Entries[seDir].Count-1 do
      PrintDirEntry(PSetupDirEntry(Entries[seDir][i])^);
  end;
  if Entries[seRegistry].Count>0 then begin
    PrintSectionHeader('Registry');
    for i:=0 to Entries[seRegistry].Count-1 do
      PrintRegistryEntry(PSetupRegistryEntry(Entries[seRegistry][i])^);
  end;
  if Entries[seIni].Count>0 then begin
    PrintSectionHeader('INI');
    for i:=0 to Entries[seIni].Count-1 do
      PrintIniEntry(PSetupIniEntry(Entries[seIni][i])^);
  end;
  if Entries[seRun].Count>0 then begin
    PrintSectionHeader('Run');
    for i:=0 to Entries[seRun].Count-1 do
      PrintRunEntry(PSetupRunEntry(Entries[seRun][i])^);
  end;
  if Entries[seUninstallRun].Count>0 then begin
    PrintSectionHeader('UninstallRun');
    for i:=0 to Entries[seUninstallRun].Count-1 do
      PrintRunEntry(PSetupRunEntry(Entries[seUninstallRun][i])^);
  end;
  if Entries[seIcon].Count>0 then begin
    PrintSectionHeader('Icons');
    for i:=0 to Entries[seIcon].Count-1 do
      PrintIconEntry(PSetupIconEntry(Entries[seIcon][i])^);
  end;
  if Entries[seTask].Count>0 then begin
    PrintSectionHeader('Tasks');
    for i:=0 to Entries[seTask].Count-1 do
      PrintTaskEntry(PSetupTaskEntry(Entries[seTask][i])^);
  end;
  if Entries[seComponent].Count>0 then begin
    PrintSectionHeader('Components');
    for i:=0 to Entries[seComponent].Count-1 do
      PrintComponentEntry(PSetupComponentEntry(Entries[seComponent][i])^);
  end;
  if Entries[seInstallDelete].Count>0 then begin
    PrintSectionHeader('InstallDelete');
    for i:=0 to Entries[seInstallDelete].Count-1 do
      PrintDeleteEntry(PSetupDeleteEntry(Entries[seInstallDelete][i])^);
  end;
  if Entries[seUninstallDelete].Count>0 then begin
    PrintSectionHeader('UninstallDelete');
    for i:=0 to Entries[seUninstallDelete].Count-1 do
      PrintDeleteEntry(PSetupDeleteEntry(Entries[seUninstallDelete][i])^);
  end;
  if Entries[seType].Count>0 then begin
    PrintSectionHeader('Types');
    for i:=0 to Entries[seType].Count-1 do
      PrintTypeEntry(PSetupTypeEntry(Entries[seType][i])^);
  end;
  if Entries[seCustomMessage].Count>0 then begin
    PrintSectionHeader('CustomMessages');
    for i:=0 to Entries[seCustomMessage].Count-1 do
      PrintCustomMessageEntry(PSetupCustomMessageEntry(Entries[seCustomMessage][i])^);
  end;
  if Entries[seLanguage].Count>0 then begin
    PrintSectionHeader('Languages');
    PrintComment('These files are stubs');
    PrintComment('To achieve better results after recompilation, use the real language files');
    for i:=0 to Entries[seLanguage].Count-1 do
      PrintLanguageEntry(PSetupLanguageEntry(Entries[seLanguage][i])^);
  end;
  
  Result := Res;
  end;

procedure TScriptBuilder.PrintSetupHeader(const sh: TSetupHeader);
var
  s : string;

  function Priv2Str (Priv : TMySetupPrivileges) : string;
  begin
    case Priv of
      prNone        : Result:='none';
      prPowerUser   : Result:='poweruser';
      prLowest      : Result:='lowest';
    else Result:='';
    end;
  end;

  function DisPage2Str (a : TMySetupDisablePage) : string;
  begin
    case a of
      dpAuto   : Result:='auto';
      dpNo     : Result:='no';
      dpYes    : Result:='yes';
    else Result:='';
    end;
  end;

  function LangDlg2Str (a : TMySetupShowLanguageDialog) : string;
  begin
    case a of
    slYes    : Result:='yes';
    slNo     : Result:='no';
    slAuto   : Result:='auto';
    else Result:='';
    end;
  end;

  function GetImageFileList(Images : TAnsiStringList; IsSmallImage: boolean) : String;
  var
    i: Integer;
    NameList: TStrings;
  begin
    if (Images.Count = 0) then Result:=''
    else begin
      NameList := TStringList.Create;
      NameList.Delimiter := ',';
      for i := 0 to Images.Count - 1 do NameList.Add(GetWizardImageFileName(i,IsPngImage(Images[i]),IsSmallImage));
      Result := NameList.DelimitedText;
      NameList.Free;
      end;
  end;

begin
  PrintComment('Created by "innounp" version '+UpVersion);
  PrintComment('Setup file: '+MakeUtf8(ExtractFilename(SetupLdrOriginalFilename)));
  PrintComment('Inno Setup Version: '+GetInnoVersionStr);
//  StrConst(';InnoSetupVersion',GetInnoVersionStr());

  PrintSectionHeader('Setup');
  StrConst('AppName', GetCustomMessage(sh.AppName), True);
  StrConst('AppVerName', GetCustomMessage(sh.AppVerName));
  StrConst('AppId', sh.AppId, True);
  StrConst('AppVersion', sh.AppVersion);
  StrConst('AppPublisher', sh.AppPublisher);
  StrConst('AppPublisherURL', sh.AppPublisherURL);
  StrConst('AppSupportPhone', sh.AppSupportPhone);
  StrConst('AppCopyright', sh.AppCopyright);
  StrConst('AppSupportURL', sh.AppSupportURL);
  StrConst('AppUpdatesURL', sh.AppUpdatesURL);
  StrConst('AppMutex', sh.AppMutex);
  StrConst('AppComments', sh.AppComments);
  StrConst('AppModifyPath', sh.AppModifyPath);
  if (shCreateAppDir in sh.Options) then
    StrConst('DefaultDirName', sh.DefaultDirName)
  else
    StrConst('CreateAppDir', 'no');
  if (sh.DefaultGroupName <> '(Default)') then
    StrConst('DefaultGroupName', sh.DefaultGroupName);
  StrConst('OutputBaseFilename',  GetBaseFilename());
  StrConst('UninstallDisplayIcon', sh.UninstallDisplayIcon);
  StrConst('UninstallDisplayName', sh.UninstallDisplayName);
  if (sh.UninstallFilesDir <> '{app}') then
    StrConst('UninstallFilesDir', sh.UninstallFilesDir);
  StrConst('Compression', GetCompressMethodName(sh.CompressMethod));
  if sh.EncryptionUsed then StrConst('; Encryption', 'yes');
  if (Ver > 4202) and (sh.EncryptionUsed or (shPassword in sh.Options)) then begin
    if (Ver < 6400) then begin
      StrConst('; PasswordHash', Hash2Str(sh.PasswordHash));
      StrConst('; PasswordSalt', SaltToStr(sh.PasswordSalt));
      end
    else with sh.Is64Encryption do begin
      StrConst('; PasswordTest', IntToStr(PasswordTest));
      StrConst('; EncryptionKDFSalt', SaltToStr(EncryptionKDFSalt));
      StrConst('; EncryptionKDFIterations', IntToStr(EncryptionKDFIterations));
//      StrConst('; ', EncryptionBaseNonce: TSetupEncryptionNonce;
      end;
    end;
  StrConst('ArchitecturesAllowed', sh.ArchitecturesAllowed);
  StrConst('ArchitecturesInstallIn64BitMode', sh.ArchitecturesInstallIn64BitMode);
  if (Ver<5310) and not(shUninstallable in sh.Options) then
    StrConst('Uninstallable', 'no')
  else if not SameText(sh.Uninstallable, 'yes') then
    StrConst('Uninstallable', sh.Uninstallable);
  if (sh.PrivilegesRequired <> prAdmin) then
    StrConst('PrivilegesRequired', Priv2Str(sh.PrivilegesRequired));
  if (Ver>=6000) then begin
    s:='';
    if proDialog in sh.PrivilegesRequiredOverridesAllowed then s:=s+'dialog'
    else if (proCommandLine in sh.PrivilegesRequiredOverridesAllowed) then s:='commandline';
    StrConst('PrivilegesRequiredOverridesAllowed',s);
    if not (shUsePreviousPrivileges in sh.Options) then StrConst('UsePreviousPrivileges','no');
    end;
  if (sh.ExtraDiskSpaceRequired > 0) then
    IntConst('ExtraDiskSpaceRequired', sh.ExtraDiskSpaceRequired);
  if (sh.DisableDirPage <> dpNo) then
    StrConst('DisableDirPage', DisPage2Str(sh.DisableDirPage));
  if (sh.DisableProgramGroupPage<> dpNo) then
    StrConst('DisableProgramGroupPage', DisPage2Str(sh.DisableProgramGroupPage));
//  if (shChangesAssociations in sh.Options) then
//    StrConst('ChangesAssociations', 'yes');
  StrConst('ChangesAssociations', sh.ChangesAssociations);
  if (Ver>=4000) then
    StrConst('ShowLanguageDialog', LangDlg2Str(sh.ShowLanguageDialog));
  if (shAllowNoIcons in sh.Options) then
    StrConst('AllowNoIcons', 'yes');
  // mirror these filenames in AddEmbeddedFiles()
  with sh do begin
    if LicenseText<>'' then StrConst('LicenseFile', MaybeToRtf('embedded\License.txt', LicenseText));
    if InfoBeforeText<>'' then StrConst('InfoBeforeFile', MaybeToRtf('embedded\InfoBefore.txt', InfoBeforeText));
    if InfoAfterText<>'' then StrConst('InfoAfterFile', MaybeToRtf('embedded\InfoAfter.txt', InfoAfterText));
  end;
  if (Ver >= 6000) then begin
    if (Ver >= 6600) then begin // support of WizardStyle
      if (shWizardModern in sh.Options) then s:='modern' else s:='classic';
      case sh.WizardDarkStyle of
      wdsDark    : s:=s+' dark';
      wdsDynamic : s:=s+' dynamic';
      else s:=s+' light'
        end;
      end
    else if sh.WizardStyle=wsModern then s:='modern' else s:='classic';
    StrConst('WizardStyle',s);
    end;
  StrConst('WizardImageFile', GetImageFileList(WizardImages, false));
  StrConst('WizardSmallImageFile', GetImageFileList(WizardSmallImages, true));
  if TimeStampsInUTC then StrConst(';TimeStampsInUTC','yes');
end;

procedure TScriptBuilder.PrintRegistryEntry(const re: TSetupRegistryEntry);

  function OptStr(Opt: TSetupRegistryOption) : AnsiString;
  begin
    case Opt of
      roCreateValueIfDoesntExist         : Result:='createvalueifdoesntexist';
      roUninsDeleteValue                 : Result:='uninsdeletevalue';
      roUninsClearValue                  : Result:='uninsclearvalue';
      roUninsDeleteEntireKey             : Result:='uninsdeletekey';
      roUninsDeleteEntireKeyIfEmpty      : Result:='uninsdeletekeyifempty';
      roPreserveStringType               : Result:='preservestringtype';
      roDeleteKey                        : Result:='deletekey';
      roDeleteValue                      : Result:='deletevalue';
      roNoError                          : Result:='noerror';
      roDontCreateKey                    : Result:='dontcreatekey';
    else Result:='';
    end;
  end;

var
  t : string;
  vType,sa,ta : AnsiString;
  i  :dword;
  o  :TSetupRegistryOption;
begin
  with re do begin
    StrParam('Root', RegRootToStr(re), false);
    StrParam('Subkey', Subkey);
    StrParam('ValueName', ValueName);
    t:='';
    if Typ<>rtNone then begin
      case Typ of
        rtString: begin vType:='String'; t:=ValueData; end;
        rtExpandString: begin vType:='ExpandSZ'; t:=ValueData; end;
        rtDword: begin vType:='Dword';
          if Ver>=4104 then t:=ValueData else begin
            Move(ValueData[1],i,4); t:=Format('$%x', [i]);
          end;
        end;
        rtQWord: begin vType:='Qword'; t := ValueData; end;
        rtBinary: begin vType:='Binary';
          t:='';
          for i:=1 to length(ValueData) do t:=t+IntToHex(byte(ValueData[i]),2)+' ';
          if length(t)>0 then SetLength(t,length(t)-1);
        end;
        rtMultiString: begin vType:='MultiSZ'; t:=ValueData;
          repeat
            i:=pos(#0,t);
            if i=0 then break;
            t[i]:='{'; insert('break}',t,i+1);
          until false;
        end;
        else vType:='Unknown';
      end;
    end;
    StrParam('ValueType', vType, False);
    StrParam('ValueData',t);
    StrParam('Components',Components, false);
    StrParam('Tasks',Tasks, false);
    StrParam('Languages',Languages);
    StrParam('Check',Check);
    StrParam('BeforeInstall',BeforeInstall);
    StrParam('AfterInstall',AfterInstall);

    PrintVersions(MinVersion, OnlyBelowVersion);

    sa:='';
    for o:=Low(o) to High(o) do if o in Options then begin
      ta:=OptStr(o);
      if ta<>'' then sa:=sa+ta+' ';
    end;
    PrintFlagsParam(sa);
  end;
end;

procedure TScriptBuilder.PrintRunEntry(const re:TSetupRunEntry);

  function OptStr(Opt: TSetupRunOption) : AnsiString;
  begin
    case Opt of
      roShellExec           : Result:='shellexec';
      roSkipIfDoesntExist   : Result:='skipifdoesntexist';
      roPostInstall         : Result:='postinstall';
      roUnchecked           : Result:='unchecked';
      roSkipIfSilent        : Result:='skipifsilent';
      roSkipIfNotSilent     : Result:='skipifnotsilent';
      roHideWizard          : Result:='hidewizard';
      roRun32Bit            : Result:='32bit';
      roRun64Bit            : Result:='64bit';
      roRunAsOriginalUser   : Result:='';
    else Result:='';
    end;
  end;

var
  sa,ta : string;
  o : TSetupRunOption;
begin
  with re do begin
    StrParam('Filename',Name);
    StrParam('Parameters',Parameters);
    StrParam('WorkingDir',WorkingDir);
    StrParam('RunOnceId',RunOnceId);
    StrParam('StatusMsg',StatusMsg);
    StrParam('Description',Description);
    StrParam('Components',Components, false);
    StrParam('Tasks',Tasks, false);
    StrParam('Languages',Languages);
    StrParam('Check',Check);
    StrParam('BeforeInstall',BeforeInstall);
    StrParam('AfterInstall',AfterInstall);

    PrintVersions(MinVersion, OnlyBelowVersion);

    sa:='';
    for o:=Low(o) to High(o) do if o in Options then begin
      ta:=OptStr(o);
      if ta<>'' then sa:=sa+ta+' ';
    end;
    case Wait of
      rwNoWait: sa:=sa+'nowait';
      rwWaitUntilIdle: sa:=sa+'waituntilidle';
    end;
    PrintFlagsParam(sa);
  end;
end;

procedure TScriptBuilder.PrintTaskEntry(const te:TSetupTaskEntry);
begin
  with te do begin
    StrParam('Name',Name);
    StrParam('Description',Description);
    StrParam('GroupDescription',GroupDescription);
    StrParam('Components',Components);
    StrParam('Languages',Languages);
    StrParam('Check',Check);

    PrintVersions(MinVersion, OnlyBelowVersion);

    PrintLn;
  end;
end;

procedure TScriptBuilder.PrintComponentEntry(const ce:TSetupComponentEntry);
var
  s:string;
  i:integer;
begin
  with ce do begin
    StrParam('Name',Name);
    StrParam('Description',Description);
    s:=Types;
    for i:=1 to length(s) do if s[i]=',' then s[i]:=' ';
    StrParam('Types',s);
    IntParam('ExtraDiskSpaceRequired', ExtraDiskSpaceRequired);
    StrParam('Languages',Languages);
    StrParam('Check',Check);

    PrintVersions(MinVersion, OnlyBelowVersion);

    PrintLn;
  end;
end;

procedure TScriptBuilder.PrintTypeEntry(const te:TSetupTypeEntry);
begin
  with te do begin
    StrParam('Name',Name);
    StrParam('Description',Description);
    StrParam('Languages',Languages);
    StrParam('Check',Check);

    PrintVersions(MinVersion, OnlyBelowVersion);

    PrintLn;
  end;
end;

procedure TScriptBuilder.PrintFileEntry(const fe:TSetupFileEntry);

  function OptStr(Opt: TSetupFileOption) : AnsiString;
  begin
    case Opt of
      foConfirmOverwrite         : Result:='confirmoverwrite';
      foUninsNeverUninstall      : Result:='uninsneveruninstall';
      foRestartReplace           : Result:='restartreplace';
      foDeleteAfterInstall       : Result:='deleteafterinstall';
      foRegisterServer           : Result:='regserver';
      foRegisterTypeLib          : Result:='regtypelib';
      foSharedFile               : Result:='sharedfile';
      foCompareTimeStamp         : Result:='comparetimestamp';
      foFontIsntTrueType         : Result:='fontisnttruetype';
      foSkipIfSourceDoesntExist  : Result:='skipifsourcedoesntexist';
      foOverwriteReadOnly        : Result:='overwritereadonly';
      foOverwriteSameVersion     : Result:='';
      foCustomDestName           : Result:='';
      foOnlyIfDestFileExists     : Result:='onlyifdestfileexists';
      foNoRegError               : Result:='noregerror';
      foUninsRestartDelete       : Result:='uninsrestartdelete';
      foOnlyIfDoesntExist        : Result:='onlyifdoesntexist';
      foIgnoreVersion            : Result:='ignoreversion';
      foPromptIfOlder            : Result:='promptifolder';
      foDontCopy                 : Result:='dontcopy';
      foUninsRemoveReadOnly      : Result:='uninsremovereadonly';
      foRecurseSubDirsExternal   : Result:='';
      fo32bit                    : Result:='32bit';
      fo64bit                    : Result:='64bit';
      foExternalSizePreset       : Result:='';
      foSetNTFSCompression       : Result:='setntfscomptression';
      foUnsetNTFSCompression     : Result:='unsetntfscomptression';
      foGacInstall               : Result:='gacinstall';
    else Result:='';
    end;
  end;

  function SignStr(Sign: TSetupFileLocationSign) : AnsiString;
  begin
    case Sign of
      fsYes   : Result:='sign';
      fsOnce  : Result:='signonce';
      fsCheck : Result:='signcheck';
    else Result:='';
    end;
  end;

var
  sa,ta,ss : AnsiString;
  o : TSetupFileOption;
  Opts : TMySetupFileOptions;
begin
  if (fe.FileType <> ftUserFile) then Exit;

  with fe do begin
    if LocationEntry<>-1 then
      with PSetupFileLocationEntry(Entries[seFileLocation][LocationEntry])^ do begin
        if FirstSlice<>LastSlice then
          PrintComment('the following file spans ' + GetSliceName(FirstSlice) + ' to ' + GetSliceName(LastSlice))
        else if FirstSlice<>CurSlice then
          PrintComment('the following file starts on ' + GetSliceName(FirstSlice));
        CurSlice:=LastSlice;
        ss:=SignStr(Sign);;
      end
    else ss:='';
    if (LocationEntry=-1) then Print('; ');
    StrParam('Source',SourceFilename);
    StrParam('DestDir',RemoveBackslashUnlessRoot(DestDir));
    StrParam('DestName',DestName);
    StrParam('FontInstall',InstallFontName);
    StrParam('Components',Components, false);
    StrParam('Tasks',Tasks, false);
    StrParam('Languages',Languages);
    StrParam('Check',Check);
    StrParam('BeforeInstall',BeforeInstall);
    StrParam('AfterInstall',AfterInstall);

    PrintVersions(MinVersion, OnlyBelowVersion);

    sa:='';
    Opts:=Options;
    if foDeleteAfterInstall in Opts then Exclude(Opts, foUninsNeverUninstall);
    for o:=Low(o) to High(o) do if o in Opts then begin
      ta:=OptStr(o);
      if ta<>'' then sa:=sa+ta+' ';
    end;
    if ss<>'' then sa:=sa+ss+' ';
    PrintFlagsParam(sa);
  end;
end;

procedure TScriptBuilder.PrintDeleteEntry(const de: TSetupDeleteEntry);

  function TypeStr(Typ: TSetupDeleteType) : AnsiString;
  begin
    case Typ of
      dfFiles             : Result:='files';
      dfFilesAndOrSubdirs : Result:='filesandordirs';
      dfDirIfEmpty        : Result:='dirifempty';
    end;
  end;

begin
  with de do begin
    StrParam('Type',TypeStr(DeleteType), false);
    StrParam('Name',Name);
    StrParam('Components',Components, false);
    StrParam('Tasks',Tasks, false);
    StrParam('Languages',Languages);
    StrParam('Check',Check, false);
    StrParam('BeforeInstall',BeforeInstall);
    StrParam('AfterInstall',AfterInstall);
    PrintLn;
  end;
end;

procedure TScriptBuilder.PrintCustomMessageEntry(const ce:TSetupCustomMessageEntry);
var
  s,v : string;
begin
  with ce do begin
    s := Name;
    if (LangIndex >= 0) then
      s:=PSetupLanguageEntry(Entries[seLanguage][LangIndex])^.Name+'.'+s;
    v:=StringReplace(Value, #13#10, '%n', [rfReplaceAll]);
    if ScriptAsUtf8 then begin
      if (LangIndex >= 0) then
        StrConst(s,ApplyCodepage(v,PSetupLanguageEntry(Entries[seLanguage][LangIndex])^))
      else StrConst(s,v);
      end
    else StrConst(s,v)
    end;
  end;

procedure TScriptBuilder.PrintIconEntry(const ie:TSetupIconEntry);
var
  sa : AnsiString;
begin
  with ie do begin
    StrParam('Name',IconName);
    StrParam('Filename',Filename);
    StrParam('Parameters',Parameters);
    StrParam('WorkingDir',WorkingDir);
    StrParam('IconFilename',IconFilename);
    if IconIndex<>0 then StrParam('IconIndex', IntToStr(IconIndex), false);
    StrParam('Comment',Comment);
    StrParam('Components',Components, false);
    StrParam('Tasks',Tasks, false);
    StrParam('Languages',Languages);
    StrParam('Check',Check);
    StrParam('BeforeInstall',BeforeInstall);
    StrParam('AfterInstall',AfterInstall);

    PrintVersions(MinVersion, OnlyBelowVersion);

    sa:='';
    case CloseOnExit of
      icYes: sa:=sa+'closeonexit ';
      icNo: sa:=sa+'dontcloseonexit ';
    end;
    case ShowCmd of
      SW_SHOWMAXIMIZED: sa:=sa+'runmaximized ';
      SW_SHOWMINNOACTIVE: sa:=sa+'runminimized ';
    end;
    PrintFlagsParam(sa);
    end;
  end;

procedure TScriptBuilder.PrintDirEntry(const de: TSetupDirEntry);

  function OptStr(Opt: TSetupDirOption): AnsiString;
  begin
    case Opt of
      doUninsNeverUninstall      : Result:='uninsneveruninstall';
      doDeleteAfterInstall       : Result:='deleteafterinstall';
      doUninsAlwaysUninstall     : Result:='uninsalwaysuninstall';
      doSetNTFSCompression       : Result:='setntfscomptression';
      doUnsetNTFSCompression     : Result:='unsetntfscomptression';
    else Result:='';
    end;
  end;

var
  sa,ta:string;
  Opts: TMySetupDirOptions;
  o: TSetupDirOption;
begin
  StrParam('Name', de.DirName);
  Opts := de.Options;
  sa:='';
  for o:=Low(o) to High(o) do if o in Opts then begin
    ta:=OptStr(o);
    if ta<>'' then sa:=sa+ta+' ';
  end;
  PrintFlagsParam(sa);
  end;

procedure TScriptBuilder.PrintIniEntry(const ie: TSetupIniEntry);

  function OptStr(Opt: TSetupIniOption) : AnsiString;
  begin
    case Opt of
      ioCreateKeyIfDoesntExist      : Result:='createkeyifdoesntexist';
      ioUninsDeleteEntry            : Result:='uninsdeleteentry';
      ioUninsDeleteEntireSection    : Result:='uninsdeletesection';
      ioUninsDeleteSectionIfEmpty   : Result:='uninsdeletesectionifempty';
    else Result:='';
    end;
  end;

var
  sa,ta : string;
  Opts : TMySetupIniOptions;
  o : TSetupIniOption;
begin
  StrParam('FileName', ie.Filename);
  StrParam('Section', ie.Section);
  StrParam('Key', ie.Entry);
  StrParam('String', ie.Value);

  Opts := ie.Options;
  sa:='';
  for o:=Low(o) to High(o) do if o in Opts then begin
    ta:=OptStr(o);
    if ta<>'' then sa:=sa+ta+' ';
    end;
  PrintFlagsParam(sa);
  end;

procedure TScriptBuilder.PrintLanguageEntry(const le : TSetupLanguageEntry);
begin
  with le do begin                                      // these filenames must be mirrored in AddEmbeddedFiles()
    StrParam('Name', Name);
    StrParam('MessagesFile', 'embedded\'+Name+'.isl');
    if LicenseText<>'' then StrParam('LicenseFile', MaybeToRtf('embedded\'+Name+'License.txt', LicenseText));
    if InfoBeforeText<>'' then StrParam('InfoBeforeFile', MaybeToRtf('embedded\'+Name+'InfoBefore.txt', InfoBeforeText));
    if InfoAfterText<>'' then StrParam('InfoAfterFile', MaybeToRtf('embedded\'+Name+'InfoAfter.txt', InfoAfterText));
//    StrParam('Codepage', IntToStr(LanguageCodePage));
  end;
  PrintLn;
end;

procedure TScriptBuilder.PrintLangOptions(const le:TSetupLanguageEntry);
begin
  with le do begin
    StrConst('LanguageName', LanguageName);
    StrConst('LanguageID', '$'+IntToHex(LanguageID,4));
    IntConst('LanguageCodePage', LanguageCodePage);
    StrConst('DialogFontName', DialogFontName);
    StrConst('TitleFontName', TitleFontName);
    StrConst('WelcomeFontName', WelcomeFontName);
    StrConst('CopyrightFontName', CopyrightFontName);
    IntConst('DialogFontSize', DialogFontSize);
    IntConst('TitleFontSize', TitleFontSize);
    IntConst('WelcomeFontSize', WelcomeFontSize);
    IntConst('CopyrightFontSize', CopyrightFontSize);
    if (RightToLeft) then StrConst('RightToLeft', 'yes');
  end;
end;

function TScriptBuilder.GetLanguageFile(i: Integer) : AnsiString;
begin
  Res:='';
// Add UTF-8 BOM to script start for Unicode versions.
  if ScriptAsUtf8 then Print(#$EF#$BB#$BF);
  PrintSectionHeader('LangOptions', False);
  PrintLangOptions(PSetupLanguageEntry(Entries[seLanguage][i])^);
  Result := Res;
//  Res.Destroy;
end;

end.
