unit MyTypes;

interface

uses Winapi.Windows, MD5, SHA1, SHA256;

////// in general extraction units these types are given their original
////// names as aliases, e.g. type TSetupHeader = TMySetupHeader
////// in verXXXX units this is not done to avoid conflicts
type
  TMySetupVersionData = packed record // same as in all IS versions
    WinVersion, NTVersion: Cardinal;
    NTServicePack: Word;
  end;

  TMySetupLdrOffsetTable = record // in-memory only
    ID: AnsiString; //array[1..12] of Char;
    Version: LongWord;
    TotalSize, OffsetEXE : int64;
    CompressedSizeEXE, UncompressedSizeEXE : UInt32;
    CRCEXE : Int32;
    Offset0, Offset1: int64;
    TableCRC: Int32;  { CRC of all prior fields in this record }
    TableCRCUsed: boolean;
  end;

  TSetupCompressMethod = (cmStored, cmZip, cmBzip, cmLZMA, cmLZMA2);
  TSetupSalt = array[0..7] of Byte;
  TSetupKDFSalt = array[0..15] of Byte;
  TSetupEncryptionKey = array[0..31] of Byte;
  TSetupEncryptionNonce = record
    RandomXorStartOffset: Int64;
    RandomXorFirstSlice: Int32;
    RemainingRandom: array[0..2] of Int32;
  end;
  TMySetupProcessorArchitecture = (paUnknown, paX86, paAMD64, paIA64);
  TMySetupProcessorArchitectures = set of TMySetupProcessorArchitecture;
  TMySetupPrivileges = (prNone, prPowerUser, prAdmin, prLowest);
  TMySetupPrivilegesRequiredOverride = (proCommandLine, proDialog);
  TMySetupPrivilegesRequiredOverrides = set of TMySetupPrivilegesRequiredOverride;
  TMySetupDisablePage = (dpAuto, dpNo, dpYes);
  TMySetupLanguageDetectionMethod = (ldUILanguage, ldLocale, ldNone);

  TFileHashType = (htAdler, htCRC32, htMD5, htSHA1, htSHA256);

  TSetupHash = record
    HashType: TFileHashType;
    case TFileHashType of
      htMD5:  (MD5: TMD5Digest);
      htSHA1: (SHA1: TSHA1Digest);
      htSHA256: (SHA256: TSHA256Digest); // not used
  end;

  TIs64Encryption = record
    PasswordTest: Integer;
    EncryptionKDFSalt: TSetupKDFSalt;
    EncryptionKDFIterations: Integer;
    EncryptionBaseNonce: TSetupEncryptionNonce;
    end;

  { Should not contain strings }
  TMySetupEncryptionHeader = packed record
    EncryptionUse: (euNone, euFiles, euFull);
    KDFSalt: TSetupKDFSalt;
    KDFIterations: Integer;
    BaseNonce: TSetupEncryptionNonce;
    PasswordTest: Integer;
  end;

  TMySetupHeaderOption = (shDisableStartupPrompt, shUninstallable, shCreateAppDir,
    shAllowNoIcons, shAlwaysRestart, shAlwaysUsePersonalGroup,
    shWindowVisible, shWindowShowCaption, shWindowResizable,
    shWindowStartMaximized, shEnableDirDoesntExistWarning,
    shPassword, shAllowRootDirectory, shDisableFinishedPage,
    shChangesAssociations, shUsePreviousAppDir,
    shBackColorHorizontal, shUsePreviousGroup, shUpdateUninstallLogAppName,
    shUsePreviousSetupType, shDisableReadyMemo, shAlwaysShowComponentsList,
    shFlatComponentsList, shShowComponentSizes, shUsePreviousTasks,
    shDisableReadyPage, shAlwaysShowDirOnReadyPage, shAlwaysShowGroupOnReadyPage,
    shAllowUNCPath, shUserInfoPage, shUsePreviousUserInfo,
    shUninstallRestartComputer, shRestartIfNeededByRun, shShowTasksTreeLines,
    shAllowCancelDuringInstall, shWizardImageStretch, shAppendDefaultDirName,
    shAppendDefaultGroupName, shEncryptionUsed, shChangesEnvironment,
    shShowUndisplayableLanguages,shSetupLogging,
    shSignedUninstaller, shUsePreviousLanguage, shDisableWelcomePage,
    shCloseApplications, shRestartApplications, shAllowNetworkDrive,
    shForceCloseApplications, shAppNameHasConsts, shUsePreviousPrivileges,
    shWizardResizable, shUninstallLogging, shWizardModern, shWizardBorderStyled,
    shWizardKeepAspectRatio, shWizardLightButtonsUnstyled,
    shRedirectionGuard, shWizardBevelsHidden,shUnusedPadding);
  const MySetupHeaderOptionLast = ord(High(TMySetupHeaderOption));
  type TMySetupHeaderOptions = set of TMySetupHeaderOption;

  TMySetupWizardStyle = (wsClassic, wsModern);
  TMySetupWizardDarkStyle = (wdsLight, wdsDark, wdsDynamic);

  TMySetupHeader = record // in-memory only
    AppName, AppVerName, AppId, AppCopyright, AppPublisher, AppPublisherURL,
      AppSupportPhone, AppSupportURL, AppUpdatesURL, AppVersion, DefaultDirName,
      DefaultGroupName, BaseFilename, UninstallFilesDir, UninstallDisplayName,
      UninstallDisplayIcon, AppMutex, DefaultUserInfoName, DefaultUserInfoOrg,
      DefaultUserInfoSerial, AppReadmeFile, AppContact, AppComments,
      AppModifyPath, CreateUninstallRegKey, Uninstallable, CloseApplicationsFilter,
      SetupMutex, ChangesEnvironment, ChangesAssociations,
      ArchitecturesAllowed, ArchitecturesInstallIn64BitMode: String;
    LicenseText, InfoBeforeText, InfoAfterText, CompiledCodeText: AnsiString;
    NumLanguageEntries, NumCustomMessageEntries, NumPermissionEntries,
      NumTypeEntries, NumComponentEntries, NumTaskEntries, NumDirEntries,
      NumISSigKeyEntries, NumFileEntries, NumFileLocationEntries, NumIconEntries, NumIniEntries,
      NumRegistryEntries, NumInstallDeleteEntries, NumUninstallDeleteEntries,
      NumRunEntries, NumUninstallRunEntries: Integer;
    MinVersion, OnlyBelowVersion: TMySetupVersionData;
    BackColor, BackColor2: Longint;
    WizardStyle: TMySetupWizardStyle;
    WizardDarkStyle: TMySetupWizardDarkStyle;
    EncryptionUsed: Boolean;
    PasswordHash: TSetupHash;
    PasswordSalt: TSetupSalt;
    Is64Encryption : TIs64Encryption;
    ExtraDiskSpaceRequired: int64;
    SlicesPerDisk: Integer;
    UninstallLogMode: (lmAppend, lmNew, lmOverwrite);
    DirExistsWarning: (ddAuto, ddNo, ddYes);
    PrivilegesRequired: TMySetupPrivileges;
    PrivilegesRequiredOverridesAllowed: TMySetupPrivilegesRequiredOverrides;
    ShowLanguageDialog: (slYes, slNo, slAuto);
    LanguageDetectionMethod: TMySetupLanguageDetectionMethod;
    CompressMethod: TSetupCompressMethod;
    DisableDirPage, DisableProgramGroupPage: TMySetupDisablePage;
    UninstallDisplaySize: Int64;
    Options: TMySetupHeaderOptions;
  end;

  ////////// Warning: changes made here must be reflected in StructTemplate.pas !
  TMySetupFileOption = (foConfirmOverwrite, foUninsNeverUninstall, foRestartReplace,
    foDeleteAfterInstall, foRegisterServer, foRegisterTypeLib, foSharedFile,
    foCompareTimeStamp, foFontIsntTrueType,
    foSkipIfSourceDoesntExist, foOverwriteReadOnly, foOverwriteSameVersion,
    foCustomDestName, foOnlyIfDestFileExists, foNoRegError,
    foUninsRestartDelete, foOnlyIfDoesntExist, foIgnoreVersion,
    foPromptIfOlder, foDontCopy, foUninsRemoveReadOnly,
    foRecurseSubDirsExternal, foReplaceSameVersionIfContentsDiffer,
    foDontVerifyChecksum, foUninsNoSharedFilePrompt, foCreateAllSubDirs,
    fo32bit, fo64bit, foExternalSizePreset, foSetNTFSCompression,
    foUnsetNTFSCompression, foGacInstall, foDownload, foExtractArchive);
  const MySetupFileOptionLast = ord(High(TMySetupFileOption));
  type TMySetupFileOptions = set of TMySetupFileOption;

  TMySetupISSigKeyEntry = record
    PublicX, PublicY, RuntimeID: String;
    end;

  TMySetupFileEntry = record // in-memory only
    SourceFilename, DestName, InstallFontName, StrongAssemblyName: String;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    MinVersion, OnlyBelowVersion: TMySetupVersionData;
    LocationEntry: Integer;
    Attribs: Integer;
    ExternalSize: Int64;
    PermissionsEntry: Smallint;
    Options: TMySetupFileOptions;
    FileType: (ftUserFile, ftUninstExe, ftRegSvrExe, ftFakeFile);
    // Custom fields
    DestDir: String;
  end;

  TMySetupFileLocationFlag = (foVersionInfoValid, foVersionInfoNotValid, foTimeStampInUTC,
      foIsUninstExe, foCallInstructionOptimized, foTouch, foChunkEncrypted,
      foChunkCompressed, foSolidBreak, foSign, foSignOnce);
  TMySetupFileLocationSign = (fsNoSetting, fsYes, fsOnce, fsCheck);
  const MySetupFileLocationFlagLast = Ord(High(TMySetupFileLocationFlag));
  type TMySetupFileLocationFlags = set of TMySetupFileLocationFlag;

  TMySetupFileLocationEntry = record // in-memory only
    FirstSlice, LastSlice: Integer;
    StartOffset: int64;
    ChunkSuboffset: Int64;
    OriginalSize: Int64;
    ChunkCompressedSize: Int64;
    HashType: TFileHashType;
    CRC: Longint;
    MD5Sum: TMD5Digest;
    SHA1Sum: TSHA1Digest;	// From version 5309
    SHA256Sum: TSHA256Digest;	// From version 6400
    TimeStamp: TFileTime;
    FileVersionMS, FileVersionLS: DWORD;
    Flags: TMySetupFileLocationFlags;
    Sign : TMySetupFileLocationSign;
    Contents: String; // for fake files
    PrimaryFileEntry:integer; // for duplicate files
  end;

  ////////// Warning: changes made here must be reflected in StructTemplate.pas !
  TMySetupRegistryOption = (roCreateValueIfDoesntExist, roUninsDeleteValue,
      roUninsClearValue, roUninsDeleteEntireKey, roUninsDeleteEntireKeyIfEmpty,
      roPreserveStringType, roDeleteKey, roDeleteValue, roNoError,
      roDontCreateKey, ro32Bit, ro64Bit);
  const MySetupRegistryOptionLast = ord(High(TMySetupRegistryOption));
  type TMySetupRegistryOptions = set of TMySetupRegistryOption;

  TMySetupRegistryEntry = record // in-memory only
    Subkey, ValueName, ValueData: String;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    MinVersion, OnlyBelowVersion: TMySetupVersionData;
    RootKey: HKEY;
//    PermissionsEntry: Smallint;
    Typ: (rtNone, rtString, rtExpandString, rtDWord, rtBinary, rtMultiString, rtQWord);
    Options: TMySetupRegistryOptions;
  end;

  TMySetupRunOption = (roShellExec, roSkipIfDoesntExist,
      roPostInstall, roUnchecked, roSkipIfSilent, roSkipIfNotSilent,
      roHideWizard, roRun32Bit, roRun64Bit, roRunAsOriginalUser,
      roDontLogParameters, roLogOutput);
  const MySetupRunOptionLast = ord(High(TMySetupRunOption));
  type TMySetupRunOptions = set of TMySetupRunOption;

  TMySetupRunEntry = record // in-memory only
    Name, Parameters, WorkingDir, RunOnceId, StatusMsg, Verb: String;
    Description, Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    MinVersion, OnlyBelowVersion: TMySetupVersionData;
    ShowCmd: Integer;
    Wait: (rwWaitUntilTerminated, rwNoWait, rwWaitUntilIdle);
    Options: TMySetupRunOptions;
  end;

  TMySetupIconCloseOnExit = (icNoSetting, icYes, icNo);
  TMySetupIconEntry = record // in-memory only
    IconName, Filename, Parameters, WorkingDir, IconFilename, Comment: String;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    MinVersion, OnlyBelowVersion: TMySetupVersionData;
    IconIndex, ShowCmd: Integer;
    CloseOnExit: TMySetupIconCloseOnExit;
    HotKey: Word;
    Options: set of (ioUninsNeverUninstall, ioCreateOnlyIfFileExists,
      ioUseAppPaths);
  end;

  TMySetupTaskEntry = record // in-memory only
    Name, Description, GroupDescription, Components, Languages, Check: String;
    Level: Integer;
    Used: Boolean;
    MinVersion, OnlyBelowVersion: TMySetupVersionData;
    Options: set of (toExclusive, toUnchecked, toRestart, toCheckedOnce,
      toDontInheritCheck);
  end;

  TMySetupComponentEntry = record // in-memory only
    Name, Description, Types, Languages, Check: String;
    ExtraDiskSpaceRequired: Int64;
    Level: Integer;
    Used: Boolean;
    MinVersion, OnlyBelowVersion: TMySetupVersionData;
    Options: set of (coFixed, coRestart, coDisableNoUninstallWarning,
      coExclusive, coDontInheritCheck);
  end;

  TMySetupTypeOption = (toIsCustom);
  TMySetupTypeOptions = set of TMySetupTypeOption;
  TMySetupTypeType = (ttUser, ttDefaultFull, ttDefaultCompact, ttDefaultCustom);
  TMySetupTypeEntry = record
    Name, Description, Languages, Check: String;
    MinVersion, OnlyBelowVersion: TMySetupVersionData;
    Options: TMySetupTypeOptions;
    Typ: TMySetupTypeType;
  end;

  TMySetupCustomMessageEntry = record
    Name, Value: String;
    LangIndex: Integer;
  end;

  TMySetupLanguageEntry = record
    { Note: LanguageName is Unicode }
    Name, LanguageName, DialogFontName, TitleFontName, WelcomeFontName,
      CopyrightFontName : String;
    Data, LicenseText, InfoBeforeText, InfoAfterText: AnsiString;
    LanguageID, LanguageCodePage: Cardinal;
    DialogFontSize,
    DialogFontBaseScaleHeight,
    DialogFontBaseScaleWidth : Integer;
    TitleFontSize: Integer;
    WelcomeFontSize: Integer;
    CopyrightFontSize: Integer;
    RightToLeft: Boolean;
  end;

   ////////// Warning: changes made here must be reflected in StructTemplate.pas !
  TMySetupDirOption = (doUninsNeverUninstall, doDeleteAfterInstall,
    doUninsAlwaysUninstall, doSetNTFSCompression, doUnsetNTFSCompression);
  const MySetupDirOptionLast = ord(High(TMySetupDirOption));
  type TMySetupDirOptions = set of TMySetupDirOption;

  TMySetupDirEntry = record
    DirName: String;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    Attribs: Integer;
    MinVersion, OnlyBelowVersion: TMySetupVersionData;
    Options: TMySetupDirOptions;
  end;

  TMySetupIniOption = (ioCreateKeyIfDoesntExist, ioUninsDeleteEntry,
    ioUninsDeleteEntireSection, ioUninsDeleteSectionIfEmpty,
    { internally used: }
    ioHasValue);
  const MySetupIniOptionLast = ord(High(TMySetupIniOption));
  type TMySetupIniOptions = set of TMySetupIniOption;

  TMySetupIniEntry = record
    Filename, Section, Entry, Value: String;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    MinVersion, OnlyBelowVersion: TMySetupVersionData;
    Options: TMySetupIniOptions;
  end;

  TMySetupDeleteType = (dfFiles, dfFilesAndOrSubdirs, dfDirIfEmpty);
  TMySetupDeleteEntry = record
    Name: String;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: String;
    MinVersion, OnlyBelowVersion: TMySetupVersionData;
    DeleteType: TMySetupDeleteType;
  end;

////////// variables get filled in verXXXX units based on information
////////// in structXXXX
var
  SetupHeaderSize, SetupHeaderStrings, SetupHeaderAnsiStrings: Integer;
  SetupLanguageEntrySize, SetupLanguageEntryStrings, SetupLanguageEntryAnsiStrings: Integer;
  SetupCustomMessageEntrySize, SetupCustomMessageEntryStrings, SetupCustomMessageEntryAnsiStrings: Integer;
  SetupPermissionEntrySize, SetupPermissionEntryStrings, SetupPermissionEntryAnsiStrings: Integer;
  SetupTypeEntrySize, SetupTypeEntryStrings, SetupTypeEntryAnsiStrings: Integer;
  SetupComponentEntrySize, SetupComponentEntryStrings, SetupComponentEntryAnsiStrings: Integer;
  SetupTaskEntrySize, SetupTaskEntryStrings, SetupTaskEntryAnsiStrings: Integer;
  SetupDirEntrySize, SetupDirEntryStrings, SetupDirEntryAnsiStrings: Integer;
  SetupFileEntrySize, SetupFileEntryStrings, SetupFileEntryAnsiStrings: Integer;
  SetupIconEntrySize, SetupIconEntryStrings, SetupIconEntryAnsiStrings: Integer;
  SetupIniEntrySize, SetupIniEntryStrings, SetupIniEntryAnsiStrings: Integer;
  SetupRegistryEntrySize, SetupRegistryEntryStrings, SetupRegistryEntryAnsiStrings: Integer;
  SetupDeleteEntrySize, SetupDeleteEntryStrings, SetupDeleteEntryAnsiStrings: Integer;
  SetupRunEntrySize, SetupRunEntryStrings, SetupRunEntryAnsiStrings: Integer;
  SetupFileLocationEntrySize, SetupFileLocationEntryStrings, SetupFileLocationEntryAnsiStrings: Integer;
  SetupISSigKeyEntrySize, SetupISSigKeyEntryStrings , SetupISSigKeyEntryAnsiStrings : integer;

const
  UNI_FIRST = 5205; // First Inno Setup version that had Unicode support

//////////  encapsulates the version-specific stuff
type
  TInnoVer = class
  public
    VerSupported:integer;
    IsUnicode:boolean;
    IsRT:boolean;
    SetupID:array[1..12] of AnsiChar; // other units have no access to global data in structXXXX.pas
    OfsTabVers:UInt32;   // same reason
    OfsTabSize:integer;  // same reason
    constructor Create; virtual; abstract;
    procedure SetupSizes; virtual; abstract;
    procedure UnifySetupLdrOffsetTable(const p; var OffsetTable:TMySetupLdrOffsetTable); virtual; abstract;
    procedure UnifySetupHeader(const p; var SetupHeader:TMySetupHeader); virtual; abstract;
    procedure UnifySetupISSigKeyEntry(const p; var SetupISSigKeyEntry:TMySetupISSigKeyEntry); virtual; abstract;
    procedure UnifyFileEntry(const p; var FileEntry:TMySetupFileEntry); virtual; abstract;
    procedure UnifyFileLocationEntry(const p; var FileLocationEntry:TMySetupFileLocationEntry); virtual; abstract;
    procedure UnifyRegistryEntry(const p; var RegistryEntry:TMySetupRegistryEntry); virtual; abstract;
    procedure UnifyRunEntry(const p; var RunEntry:TMySetupRunEntry); virtual; abstract;
    procedure UnifyIconEntry(const p; var IconEntry:TMySetupIconEntry); virtual; abstract;
    procedure UnifyTaskEntry(const p; var TaskEntry:TMySetupTaskEntry); virtual; abstract;
    procedure UnifyComponentEntry(const p; var ComponentEntry:TMySetupComponentEntry); virtual; abstract;
    procedure UnifyTypeEntry(const p; var TypeEntry:TMySetupTypeEntry); virtual; abstract;
    procedure UnifyCustomMessageEntry(const p; var CustomMessageEntry:TMySetupCustomMessageEntry); virtual; abstract;
    procedure UnifyLanguageEntry(const p; var LanguageEntry:TMySetupLanguageEntry); virtual; abstract;
    procedure UnifyDirEntry(const p; var DirEntry: TMySetupDirEntry); virtual; abstract;
    procedure UnifyIniEntry(const p; var IniEntry: TMySetupIniEntry); virtual; abstract;
    procedure UnifyDeleteEntry(const p; var DeleteEntry: TMySetupDeleteEntry); virtual; abstract;
  end;

  TByteArray = array [byte] of byte;
  PByteArray = ^TByteArray;
  TIdArray = array [1..12] of AnsiChar;

///////// all objects representing supported versions are stored here
var
  VerList:array of TInnoVer;

procedure TranslateSet(const SourceSet; var DestSet; const XlatTab: TByteArray; MaxElement: integer);

function NormalizeStringVal(const Input: String) : String; overload;
function NormalizeStringVal(const Input: AnsiString) : String; overload;
function CopyStringVal(const Input: String) : String; overload;
function CopyStringVal(const Input: AnsiString) : String; overload;

function GetVersionBySetupId(const ASetupId : TIdArray; ASetupVersion : UInt32; var VerObject: TInnoVer):boolean;

implementation

procedure TranslateSet(const SourceSet; var DestSet; const XlatTab: TByteArray; MaxElement: integer);
var
  SourceArray: TByteArray absolute SourceSet;
  DestArray: TByteArray absolute DestSet;
  i:integer;
begin
  for i:=0 to MaxElement do
    if XlatTab[i]<>255 then
      if (SourceArray[XlatTab[i] shr 3] and (1 shl (XlatTab[i] and 7))) <> 0 then //XlatTab[i] in SourceSet then
        DestArray[i shr 3]:=DestArray[i shr 3] or (1 shl (i and 7));  //Include(DestSet,i);
end;

function NormalizeStringVal(const Input: String) : String;
begin
  Result := Input;
end;

function NormalizeStringVal(const Input: AnsiString) : String;
begin
  Result := Input;
end;

function CopyStringVal(const Input: AnsiString) : String;
var
  len: integer;
begin
// required because "LanguageName" string is Unicode
  len := Length(Input);
  SetLength(Result, len div sizeof(WideChar));
  CopyMemory(@Result[1], @Input[1], len);
end;

function CopyStringVal(const Input: String) : String;
begin
  Result := Input;    // dummy
end;

function GetVersionBySetupId(const ASetupId : TIdArray; ASetupVersion : UInt32; var VerObject: TInnoVer):boolean;
var
  i:integer;
//  aSetupId: array [1..12] of AnsiChar absolute pSetupId;
begin
  VerObject := nil;
  for i:=0 to High(VerList) do with VerList[i] do
    if (SetupID=ASetupID) and (OfsTabVers=ASetupVersion) then begin VerObject:=VerList[i]; break end;
  Result := VerObject<>nil;
end;

end.
