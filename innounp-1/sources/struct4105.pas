unit Struct4105;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Various records and other types that are shared by the ISCmplr, Setup,
  SetupLdr, and Uninst projects

  $jrsoftware: issrc/Projects/Struct.pas,v 1.111 2004/02/12 00:38:50 jr Exp $
}

interface

uses
  Windows, Int64Em;

const
  SetupTitle = 'Inno Setup';
  SetupVersion = '4.1.5'  {$IFDEF USEBUILDTAG} {$I BuildTag.inc} {$ENDIF};
  SetupBinVersion = (4 shl 24) + (1 shl 16) + (5 shl 8) + 0;

type
  TSetupID = array[0..63] of AnsiChar;
  TUninstallLogID = array[0..63] of AnsiChar;
  TMessagesHdrID = array[0..63] of AnsiChar;
  TUninstLangOptionsID = array[1..8] of AnsiChar;
  TCompID = array[1..4] of AnsiChar;
  TDiskSliceID = array[1..8] of AnsiChar;
const
  { SetupID is used by the Setup program to check if the SETUP.0 file is
    compatible with with it. If you make any modifications to the records in
    this file it's recommended you change SetupID. Any change will do (like
    changing the letters or numbers), as long as your format is
    unrecognizable by the standard Inno Setup. }
  SetupID: TSetupID = 'Inno Setup Setup Data (4.1.5)';
  UninstallLogID: TUninstallLogID = 'Inno Setup Uninstall Log (b)';
  MessagesHdrID: TMessagesHdrID = 'Inno Setup Messages (4.1.4)';
  UninstLangOptionsID: TUninstLangOptionsID = '!ulo!000';
  ZLIBID: TCompID = 'zlb'#26;
  DiskSliceID: TDiskSliceID = 'idska32'#26;
type
  TSetupVersionDataVersion = packed record
    Build: Word;
    Minor, Major: Byte;
  end;
  TSetupVersionData = packed record
    WinVersion, NTVersion: Cardinal;
    NTServicePack: Word;
  end;
  TSetupHeaderOption = (shDisableStartupPrompt, shUninstallable, shCreateAppDir,
    shDisableDirPage, shDisableProgramGroupPage,
    shAllowNoIcons, shAlwaysRestart, shAlwaysUsePersonalGroup,
    shWindowVisible, shWindowShowCaption, shWindowResizable,
    shWindowStartMaximized, shEnableDirDoesntExistWarning,
    shPassword, shAllowRootDirectory, shDisableFinishedPage,
    shChangesAssociations, shCreateUninstallRegKey, shUsePreviousAppDir,
    shBackColorHorizontal, shUsePreviousGroup, shUpdateUninstallLogAppName,
    shUsePreviousSetupType, shDisableReadyMemo, shAlwaysShowComponentsList,
    shFlatComponentsList, shShowComponentSizes, shUsePreviousTasks,
    shDisableReadyPage, shAlwaysShowDirOnReadyPage, shAlwaysShowGroupOnReadyPage,
    shAllowUNCPath, shUserInfoPage, shUsePreviousUserInfo,
    shUninstallRestartComputer, shRestartIfNeededByRun, shShowTasksTreeLines,
    shAllowCancelDuringInstall, shWizardImageStretch);
  TSetupCompressMethod = (cmZip, cmBzip, cmLZMA);

const
  SetupHeaderStrings = 23;
type
  TSetupHeader = packed record
    AppName, AppVerName, AppId, AppCopyright, AppPublisher, AppPublisherURL,
      AppSupportURL, AppUpdatesURL, AppVersion, DefaultDirName,
      DefaultGroupName, BaseFilename, LicenseText,
      InfoBeforeText, InfoAfterText, UninstallFilesDir, UninstallDisplayName,
      UninstallDisplayIcon, AppMutex, DefaultUserInfoName,
      DefaultUserInfoOrg, DefaultUserInfoSerial, CompiledCodeText: AnsiString;
    LeadBytes: set of AnsiChar;
    NumLanguageEntries, NumPermissionEntries, NumTypeEntries,
      NumComponentEntries, NumTaskEntries, NumDirEntries, NumFileEntries,
      NumFileLocationEntries, NumIconEntries, NumIniEntries,
      NumRegistryEntries, NumInstallDeleteEntries, NumUninstallDeleteEntries,
      NumRunEntries, NumUninstallRunEntries: Integer;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    BackColor, BackColor2, WizardImageBackColor: Longint;
    WizardSmallImageBackColor: Longint;
    Password: Longint;
    ExtraDiskSpaceRequired: Integer64;
    SlicesPerDisk: Integer;
    InstallMode: (imNormal, imSilent, imVerySilent);
    UninstallLogMode: (lmAppend, lmNew, lmOverwrite);
    UninstallStyle: (usClassic, usModern);
    DirExistsWarning: (ddAuto, ddNo, ddYes);
    PrivilegesRequired: (prNone, prPowerUser, prAdmin);
    ShowLanguageDialog: (slYes, slNo, slAuto);
    LanguageDetectionMethod: (ldUILanguage, ldLocale, ldNone);
    CompressMethod: TSetupCompressMethod;
    Options: set of TSetupHeaderOption;
  end;
const
  SetupPermissionEntryStrings = 1;
type
  PSetupPermissionEntry = ^TSetupPermissionEntry;
  TSetupPermissionEntry = packed record
    Permissions: AnsiString;  { an array of TGrantPermissionEntry's }
  end;
const
  SetupLanguageEntryStrings = 10;
type
  PSetupLanguageEntry = ^TSetupLanguageEntry;
  TSetupLanguageEntry = packed record
    Name, LanguageName, DialogFontName, TitleFontName, WelcomeFontName,
      CopyrightFontName, Data, LicenseText, InfoBeforeText,
      InfoAfterText: AnsiString;
    LanguageID: Cardinal;
    DialogFontSize: Integer;
    TitleFontSize: Integer;
    WelcomeFontSize: Integer;
    CopyrightFontSize: Integer;
  end;
const
  SetupTypeEntryStrings = 4;
type
  TSetupTypeOption = (toIsCustom);
  TSetupTypeOptions = set of TSetupTypeOption;
  TSetupTypeType = (ttUser, ttDefaultFull, ttDefaultCompact, ttDefaultCustom);
  PSetupTypeEntry = ^TSetupTypeEntry;
  TSetupTypeEntry = packed record
    Name, Description, Languages, Check: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: TSetupTypeOptions;
    Typ: TSetupTypeType;
    { internally used: }
    Size: Integer64;
  end;
const
  SetupComponentEntryStrings = 5;
type
  PSetupComponentEntry = ^TSetupComponentEntry;
  TSetupComponentEntry = packed record
    Name, Description, Types, Languages, Check: AnsiString;
    ExtraDiskSpaceRequired: Integer64;
    Level: Integer;
    Used: Boolean;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: set of (coFixed, coRestart, coDisableNoUninstallWarning, coExclusive);
    { internally used: }
    Size: Integer64;
  end;
const
  SetupTaskEntryStrings = 6;
type
  PSetupTaskEntry = ^TSetupTaskEntry;
  TSetupTaskEntry = packed record
    Name, Description, GroupDescription, Components, Languages, Check: AnsiString;
    Level: Integer;
    Used: Boolean;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: set of (toExclusive, toUnchecked, toRestart, toCheckedOnce);
  end;
const
  SetupDirEntryStrings = 7;
type
  PSetupDirEntry = ^TSetupDirEntry;
  TSetupDirEntry = packed record
    DirName: AnsiString;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: AnsiString;
    Attribs: Integer;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    PermissionsEntry: Smallint;
    Options: set of (doUninsNeverUninstall, doDeleteAfterInstall,
      doUninsAlwaysUninstall);
  end;
const
  SetupFileEntryStrings = 9;
type
  PSetupFileEntry = ^TSetupFileEntry;
  TSetupFileEntry = packed record
    SourceFilename, DestName, InstallFontName: AnsiString;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    LocationEntry: Integer;
    Attribs: Integer;
    ExternalSize: Integer64;
    PermissionsEntry: Smallint;
    Options: set of (foConfirmOverwrite, foUninsNeverUninstall, foRestartReplace,
      foDeleteAfterInstall, foRegisterServer, foRegisterTypeLib, foSharedFile,
      foCompareTimeStamp, foFontIsntTrueType,
      foSkipIfSourceDoesntExist, foOverwriteReadOnly, foOverwriteSameVersion,
      foCustomDestName, foOnlyIfDestFileExists, foNoRegError,
      foUninsRestartDelete, foOnlyIfDoesntExist, foIgnoreVersion,
      foPromptIfOlder, foDontCopy, foUninsRemoveReadOnly);
    FileType: (ftUserFile, ftUninstExe, ftRegSvrExe);
  end;
const
  SetupFileLocationEntryStrings = 0;
type
  PSetupFileLocationEntry = ^TSetupFileLocationEntry;
  TSetupFileLocationEntry = packed record
    FirstSlice, LastSlice: Integer;
    StartOffset: Longint;
    ChunkSuboffset: Integer64;
    OriginalSize: Integer64;
    ChunkCompressedSize: Integer64;
    CRC: Longint;
    TimeStamp: TFileTime;
    FileVersionMS, FileVersionLS: DWORD;
    Flags: set of (foVersionInfoValid, foVersionInfoNotValid, foTimeStampInUTC,
      foIsUninstExe);
  end;
  TSetupIconCloseOnExit = (icNoSetting, icYes, icNo);
const
  SetupIconEntryStrings = 12;
type
  PSetupIconEntry = ^TSetupIconEntry;
  TSetupIconEntry = packed record
    IconName, Filename, Parameters, WorkingDir, IconFilename, Comment: AnsiString;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    IconIndex, ShowCmd: Integer;
    CloseOnExit: TSetupIconCloseOnExit;
    HotKey: Word;
    Options: set of (ioUninsNeverUninstall, ioCreateOnlyIfFileExists,
      ioUseAppPaths);
  end;
const
  SetupIniEntryStrings = 10;
type
  PSetupIniEntry = ^TSetupIniEntry;
  TSetupIniEntry = packed record
    Filename, Section, Entry, Value: AnsiString;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: set of (ioCreateKeyIfDoesntExist, ioUninsDeleteEntry,
      ioUninsDeleteEntireSection, ioUninsDeleteSectionIfEmpty,
      { internally used: }
      ioHasValue);
  end;
const
  SetupRegistryEntryStrings = 9;
type
  PSetupRegistryEntry = ^TSetupRegistryEntry;
  TSetupRegistryEntry = packed record
    Subkey, ValueName, ValueData: AnsiString;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    RootKey: HKEY;
    PermissionsEntry: Smallint;
    Typ: (rtNone, rtString, rtExpandString, rtDWord, rtBinary, rtMultiString);
    Options: set of (roCreateValueIfDoesntExist, roUninsDeleteValue,
      roUninsClearValue, roUninsDeleteEntireKey, roUninsDeleteEntireKeyIfEmpty,
      roPreserveStringType, roDeleteKey, roDeleteValue, roNoError,
      roDontCreateKey);
  end;
const
  SetupDeleteEntryStrings = 7;
type
  TSetupDeleteType = (dfFiles, dfFilesAndOrSubdirs, dfDirIfEmpty);
  PSetupDeleteEntry = ^TSetupDeleteEntry;
  TSetupDeleteEntry = packed record
    Name: AnsiString;
    Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    DeleteType: TSetupDeleteType;
  end;
const
  SetupRunEntryStrings = 12;
type
  PSetupRunEntry = ^TSetupRunEntry;
  TSetupRunEntry = packed record
    Name, Parameters, WorkingDir, RunOnceId, StatusMsg: AnsiString;
    Description, Components, Tasks, Languages, Check, AfterInstall, BeforeInstall: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    ShowCmd: Integer;
    Wait: (rwWaitUntilTerminated, rwNoWait, rwWaitUntilIdle);
    Options: set of (roShellExec, roSkipIfDoesntExist,
      roPostInstall, roUnchecked, roSkipIfSilent, roSkipIfNotSilent,
      roHideWizard);
  end;

  { TGrantPermissionEntry is stored inside string fields named 'Permissions' }
  TGrantPermissionSid = record
    Authority: TSIDIdentifierAuthority;
    SubAuthCount: Byte;
    SubAuth: array[0..1] of DWORD;
  end;
  TGrantPermissionEntry = record
    Sid: TGrantPermissionSid;
    AccessMask: DWORD;
  end;

  { A TDiskSliceHeader record follows DiskSliceID in a SETUP-*.BIN file }
  TDiskSliceHeader = packed record
    TotalSize: Longint;
  end;

  { A TMessageHeader record follows MessagesHdrID in a SETUP.MSG file }
  TMessagesHeader = packed record
    NumMessages, TotalSize, NotTotalSize, CRCMessages: Longint;
  end;

  TSetupLdrExeHeader = packed record
    ID: Longint;
    OffsetTableOffset, NotOffsetTableOffset: Longint;
  end;

  TSetupLdrOffsetTable = packed record
    ID: array[1..12] of AnsiChar;
    TotalSize,
    OffsetEXE, CompressedSizeEXE, UncompressedSizeEXE, CRCEXE,
    Offset0, Offset1: Longint;
    TableCRC: Longint;  { CRC of all prior fields in this record }
  end;

  { TUninstLangOptions is a simplified version of TSetupLangOptions that is
    used by the uninstaller }
  TUninstLangOptions = packed record
    ID: TUninstLangOptionsID;
    DialogFontName: String[31];
    DialogFontSize: Integer;
  end;

  TUninstallerMsgTail = packed record
    ID: Longint;
    Offset: Longint;
  end;
const
  SetupLdrExeHeaderOffset = $30;
  SetupLdrExeHeaderID = $6F6E6E49;
  SetupLdrOffsetTableID = 'rDlPtS06'#$87#$65#$56#$78;
  UninstallerMsgTailID = $67734D49;

implementation

end.
