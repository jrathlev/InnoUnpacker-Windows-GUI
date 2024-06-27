unit Struct3003;

{
  Inno Setup
  Copyright (C) 1998-2002 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Various records and other types that are shared by the ISCmplr, Setup,
  SetupLdr, and Uninst projects

  $Id: Struct.pas,v 1.53 2002/08/27 04:26:51 jr Exp $
}

interface

uses
  Windows;

const
  SetupTitle = 'Inno Setup';
  SetupVersion = '3.0.3-beta'  {$IFDEF USEBUILDTAG} {$I BuildTag.inc} {$ENDIF};
  SetupBinVersion = (3 shl 24) + (0 shl 16) + (3 shl 8) + 0;

type
  TSetupID = array[0..63] of AnsiChar;
  TUninstallLogID = array[0..63] of AnsiChar;
  TMessagesHdrID = array[0..63] of AnsiChar;
  TUninstLangOptionsID = array[1..8] of AnsiChar;
  TCompID = array[1..4] of AnsiChar;
  TDiskID = array[1..8] of AnsiChar;
const
  { SetupID is used by the Setup program to check if the SETUP.0 file is
    compatible with with it. If you make any modifications to the records in
    this file it's recommended you change SetupID. Any change will do (like
    changing the letters or numbers), as long as your format is
    unrecognizable by the standard Inno Setup. }
  SetupID: TSetupID = 'Inno Setup Setup Data (3.0.3)';
  UninstallLogID: TUninstallLogID = 'Inno Setup Uninstall Log (b)';
  MessagesHdrID: TMessagesHdrID = 'Inno Setup Messages (3.0.3)';
  UninstLangOptionsID: TUninstLangOptionsID = '!ulo!000';
  ZLIBID: TCompID = 'zlb'#26;
  DiskID: TDiskID = 'idska32'#26;
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
    shDisableAppendDir, shPassword, shAllowRootDirectory,
    shDisableFinishedPage, shAdminPrivilegesRequired,
    shChangesAssociations, shCreateUninstallRegKey, shUsePreviousAppDir,
    shBackColorHorizontal, shUsePreviousGroup, shUpdateUninstallLogAppName,
    shUsePreviousSetupType, shDisableReadyMemo, shAlwaysShowComponentsList,
    shFlatComponentsList, shShowComponentSizes, shUsePreviousTasks,
    shDisableReadyPage, shAlwaysShowDirOnReadyPage, shAlwaysShowGroupOnReadyPage,
    shBzipUsed, shAllowUNCPath, shUserInfoPage, shUsePreviousUserInfo,
    shUninstallRestartComputer, shRestartIfNeededByRun);

const
  SetupHeaderStrings = 21;
type
  TSetupHeader = packed record
    AppName, AppVerName, AppId, AppCopyright, AppPublisher, AppPublisherURL,
      AppSupportURL, AppUpdatesURL, AppVersion, DefaultDirName,
      DefaultGroupName, BaseFilename, LicenseText,
      InfoBeforeText, InfoAfterText, UninstallFilesDir, UninstallDisplayName,
      UninstallDisplayIcon, AppMutex, DefaultUserInfoName,
      DefaultUserInfoOrg: AnsiString;
    LeadBytes: set of AnsiChar;
    NumTypeEntries, NumComponentEntries, NumTaskEntries: Integer;
    NumDirEntries, NumFileEntries, NumFileLocationEntries, NumIconEntries,
      NumIniEntries, NumRegistryEntries, NumInstallDeleteEntries,
      NumUninstallDeleteEntries, NumRunEntries, NumUninstallRunEntries: Integer;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    BackColor, BackColor2, WizardImageBackColor: Longint;
    WizardSmallImageBackColor: Longint;
    Password: Longint;
    ExtraDiskSpaceRequired: Longint;
    InstallMode: (imNormal, imSilent, imVerySilent);
    UninstallLogMode: (lmAppend, lmNew, lmOverwrite);
    UninstallStyle: (usClassic, usModern);
    DirExistsWarning: (ddAuto, ddNo, ddYes);
    Options: set of TSetupHeaderOption;
  end;
const
  SetupLangOptionsStrings = 5;
type
  TSetupLangOptions = packed record
    LanguageName, DialogFontName, TitleFontName, WelcomeFontName,
      CopyrightFontName: AnsiString;
    LanguageID: Cardinal;
    DialogFontSize, DialogFontStandardHeight: Integer;
    TitleFontSize: Integer;
    WelcomeFontSize: Integer;
    CopyrightFontSize: Integer;
  end;
const
  SetupTypeEntryStrings = 2;
type
  TSetupTypeOption = (toIsCustom);
  TSetupTypeOptions = set of TSetupTypeOption;
  PSetupTypeEntry = ^TSetupTypeEntry;
  TSetupTypeEntry = packed record
    Name, Description: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: TSetupTypeOptions;
    { internally used: }
    Size: LongInt;
  end;
const
  SetupComponentEntryStrings = 3;
type
  PSetupComponentEntry = ^TSetupComponentEntry;
  TSetupComponentEntry = packed record
    Name, Description, Types: AnsiString;
    ExtraDiskSpaceRequired: Longint;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: set of (coFixed, coRestart, coDisableNoUninstallWarning);
    { internally used: }
    Size: LongInt;
  end;
const
  SetupTaskEntryStrings = 4;
type
  PSetupTaskEntry = ^TSetupTaskEntry;
  TSetupTaskEntry = packed record
    Name, Description, GroupDescription, Components: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: set of (toExclusive, toUnchecked, toRestart, toCheckedOnce);
  end;
const
  SetupDirEntryStrings = 3;
type
  PSetupDirEntry = ^TSetupDirEntry;
  TSetupDirEntry = packed record
    DirName: AnsiString;
    Components, Tasks: AnsiString;
    Attribs: Integer;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: set of (doUninsNeverUninstall, doDeleteAfterInstall,
      doUninsAlwaysUninstall);
  end;
  TSetupFileCopyMode = (cmNormal, cmIfDoesntExist, cmAlwaysOverwrite,
    cmAlwaysSkipIfSameOrOlder);
const
  SetupFileEntryStrings = 5;
type
  PSetupFileEntry = ^TSetupFileEntry;
  TSetupFileEntry = packed record
    SourceFilename, DestName, InstallFontName: AnsiString;
    Components, Tasks: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    LocationEntry: Integer;
    Attribs: Integer;
    ExternalSize: Longint;
    CopyMode: TSetupFileCopyMode;
    Options: set of (foConfirmOverwrite, foUninsNeverUninstall, foRestartReplace,
      foDeleteAfterInstall, foRegisterServer, foRegisterTypeLib, foSharedFile,
      foCompareTimeStampAlso, foFontIsntTrueType,
      foSkipIfSourceDoesntExist, foOverwriteReadOnly, foOverwriteSameVersion,
      foCustomDestName, foOnlyIfDestFileExists, foNoRegError,
      foUninsRestartDelete);
    FileType: (ftUserFile, ftUninstExe, ftRegSvrExe);
  end;
const
  SetupFileLocationEntryStrings = 0;
type
  PSetupFileLocationEntry = ^TSetupFileLocationEntry;
  TSetupFileLocationEntry = packed record
    FirstDisk, LastDisk: Integer;
    StartOffset, OriginalSize, CompressedSize: Longint;
    Adler: Longint;
    Date: TFileTime;
    FileVersionMS, FileVersionLS: DWORD;
    Flags: set of (foVersionInfoValid, foVersionInfoNotValid, foBzipped);
  end;
  TSetupIconCloseOnExit = (icNoSetting, icYes, icNo);
const
  SetupIconEntryStrings = 8;
type
  PSetupIconEntry = ^TSetupIconEntry;
  TSetupIconEntry = packed record
    IconName, Filename, Parameters, WorkingDir, IconFilename, Comment: AnsiString;
    Components, Tasks: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    IconIndex, ShowCmd: Integer;
    CloseOnExit: TSetupIconCloseOnExit;
    HotKey: Word;
    Options: set of (ioUninsNeverUninstall, ioCreateOnlyIfFileExists,
      ioUseAppPaths);
  end;
const
  SetupIniEntryStrings = 6;
type
  PSetupIniEntry = ^TSetupIniEntry;
  TSetupIniEntry = packed record
    Filename, Section, Entry, Value: AnsiString;
    Components, Tasks: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: set of (ioCreateKeyIfDoesntExist, ioUninsDeleteEntry,
      ioUninsDeleteEntireSection, ioUninsDeleteSectionIfEmpty,
      { internally used: }
      ioHasValue);
  end;
const
  SetupRegistryEntryStrings = 5;
type
  PSetupRegistryEntry = ^TSetupRegistryEntry;
  TSetupRegistryEntry = packed record
    Subkey, ValueName, ValueData: AnsiString;
    Components, Tasks: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    RootKey: HKEY;
    Typ: (rtNone, rtString, rtExpandString, rtDWord, rtBinary, rtMultiString);
    Options: set of (roCreateValueIfDoesntExist, roUninsDeleteValue,
      roUninsClearValue, roUninsDeleteEntireKey, roUninsDeleteEntireKeyIfEmpty,
      roPreserveStringType, roDeleteKey, roDeleteValue, roNoError,
      roDontCreateKey);
  end;
const
  SetupDeleteEntryStrings = 3;
type
  TSetupDeleteType = (dfFiles, dfFilesAndOrSubdirs, dfDirIfEmpty);
  PSetupDeleteEntry = ^TSetupDeleteEntry;
  TSetupDeleteEntry = packed record
    Name: AnsiString;
    Components, Tasks: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    DeleteType: TSetupDeleteType;
  end;
const
  SetupRunEntryStrings = 8;
type
  PSetupRunEntry = ^TSetupRunEntry;
  TSetupRunEntry = packed record
    Name, Parameters, WorkingDir, RunOnceId, StatusMsg: AnsiString;
    Description, Components, Tasks: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    ShowCmd: Integer;
    Wait: (rwWaitUntilTerminated, rwNoWait, rwWaitUntilIdle);
    Options: set of (roShellExec, roSkipIfDoesntExist,
      roPostInstall, roUnchecked, roSkipIfSilent, roSkipIfNotSilent,
      roHideWizard);
  end;

  { A TDiskHeader record follows DiskID in a SETUP.x file }
  TDiskHeader = packed record
    TotalSize: Longint;
  end;

  { A TMessageHeader record follows MessagesHdrID in a SETUP.MSG file }
  TMessagesHeader = packed record
    NumMessages, TotalSize, NotTotalSize, Padding: Longint;
    CRCLengths, CRCMessages: Longint;
  end;

  TSetupLdrExeHeader = packed record
    ID: Longint;
    OffsetTableOffset, NotOffsetTableOffset: Longint;
  end;

  TSetupLdrOffsetTable = packed record
    ID: array[1..12] of AnsiChar;
    TotalSize,
    OffsetEXE, CompressedSizeEXE, UncompressedSizeEXE, AdlerEXE,
    OffsetMsg, Offset0, Offset1: Longint;
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
  SetupLdrOffsetTableID = 'rDlPtS02'#$87#$65#$56#$78;
  UninstallerMsgTailID = $67734D49;

implementation

end.
