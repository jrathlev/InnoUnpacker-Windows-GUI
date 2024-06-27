unit Struct1325;

{
  Inno Setup
  Copyright (C) 1998-2000 Jordan Russell
  For conditions of distribution and use, see LICENSE.TXT.

  Various records and other types that are shared by the ISCmplr, Setup,
  SetupLdr, and Uninst projects

  $Id: Struct.pas,v 1.7 2001/01/21 00:03:21 jr Exp $
}

interface

uses
  Windows;

const
  SetupTitle = 'Inno Setup';
  SetupVersion = '1.3.26'  {$IFDEF USEBUILDTAG} {$I BuildTag.inc} {$ENDIF};
  SetupBinVersion = (1 shl 24) + (3 shl 16) + (26 shl 8) + 0;

type
  TSetupID = array[0..63] of AnsiChar;
  TMessagesHdrID = array[0..63] of AnsiChar;
  TUninstallLogID = array[0..63] of AnsiChar;
  TCompID = array[1..4] of AnsiChar;
  TDiskID = array[1..8] of AnsiChar;
const
  { SetupID is used by the Setup program to check if the SETUP.0 file is
    compatible with with it. If you make any modifications to the records in
    this file it's recommended you change SetupID. Any change will do (like
    changing the letters or numbers), as long as your format is
    unrecognizable by the standard Inno Setup. }
  SetupID: TSetupID = 'Inno Setup Setup Data (1.3.25)';
  UninstallLogID: TUninstallLogID = 'Inno Setup Uninstall Log (b)';
  MessagesHdrID: TMessagesHdrID = 'Inno Setup Messages (1.3.15)';
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
    shAlwaysCreateUninstallIcon,
    shChangesAssociations, shCreateUninstallRegKey, shUsePreviousAppDir,
    shBackColorHorizontal, shUsePreviousGroup, shUpdateUninstallLogAppName);
const
  SetupHeaderStrings = 20;
type
  TSetupHeader = packed record
    AppName, AppVerName, AppId, AppCopyright, AppPublisher, AppPublisherURL,
      AppSupportURL, AppUpdatesURL, AppVersion, DefaultDirName,
      DefaultGroupName, UninstallIconName, BaseFilename, LicenseText,
      InfoBeforeText, InfoAfterText, UninstallFilesDir, UninstallDisplayName,
      UninstallDisplayIcon, AppMutex: AnsiString;
    NumDirEntries, NumFileEntries, NumFileLocationEntries, NumIconEntries,
      NumIniEntries, NumRegistryEntries, NumInstallDeleteEntries,
      NumUninstallDeleteEntries, NumRunEntries, NumUninstallRunEntries: Integer;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    BackColor, BackColor2, WizardImageBackColor: Longint;
    Password: Longint;
    ExtraDiskSpaceRequired: Longint;
    UninstallLogMode: (lmAppend, lmNew, lmOverwrite);
    DirExistsWarning: (ddAuto, ddNo, ddYes);
    Options: set of TSetupHeaderOption;
  end;
const
  SetupDirEntryStrings = 1;
type
  PSetupDirEntry = ^TSetupDirEntry;
  TSetupDirEntry = packed record
    DirName: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: set of (doUninsNeverUninstall, doDeleteAfterInstall,
      doUninsAlwaysUninstall);
  end;
  TSetupFileCopyMode = (cmNormal, cmIfDoesntExist, cmAlwaysOverwrite,
    cmAlwaysSkipIfSameOrOlder);
const
  SetupFileEntryStrings = 3;
type
  PSetupFileEntry = ^TSetupFileEntry;
  TSetupFileEntry = packed record
    SourceFilename, DestName, InstallFontName: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    LocationEntry: Integer;
    Attribs: Integer;
    ExternalSize: Longint;
    CopyMode: TSetupFileCopyMode;
    Options: set of (foConfirmOverwrite, foUninsNeverUninstall, foRestartReplace,
      foDeleteAfterInstall, foRegisterServer, foRegisterTypeLib, foSharedFile,
      foIsReadmeFile, foCompareTimeStampAlso, foFontIsntTrueType,
      foSkipIfSourceDoesntExist, foOverwriteReadOnly, foOverwriteSameVersion,
      foCustomDestName, foOnlyIfDestFileExists);
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
    Flags: set of (foVersionInfoValid, foVersionInfoNotValid);
  end;
  TSetupIconCloseOnExit = (icNoSetting, icYes, icNo);
const
  SetupIconEntryStrings = 6;
type
  PSetupIconEntry = ^TSetupIconEntry;
  TSetupIconEntry = packed record
    IconName, Filename, Parameters, WorkingDir, IconFilename, Comment: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    IconIndex, ShowCmd: Integer;
    CloseOnExit: TSetupIconCloseOnExit;
    Options: set of (ioUninsNeverUninstall, ioCreateOnlyIfFileExists,
      ioUseAppPaths);
  end;
const
  SetupIniEntryStrings = 4;
type
  PSetupIniEntry = ^TSetupIniEntry;
  TSetupIniEntry = packed record
    Filename, Section, Entry, Value: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    Options: set of (ioCreateKeyIfDoesntExist, ioUninsDeleteEntry,
      ioUninsDeleteEntireSection, ioUninsDeleteSectionIfEmpty,
      { internally used: }
      ioHasValue);
  end;
const
  SetupRegistryEntryStrings = 3;
type
  PSetupRegistryEntry = ^TSetupRegistryEntry;
  TSetupRegistryEntry = packed record
    Subkey, ValueName, ValueData: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    RootKey: HKEY;
    Typ: (rtNone, rtString, rtExpandString, rtDWord, rtBinary, rtMultiString);
    Options: set of (roCreateValueIfDoesntExist, roUninsDeleteValue,
      roUninsClearValue, roUninsDeleteEntireKey, roUninsDeleteEntireKeyIfEmpty,
      roPreserveStringType, roDeleteKey, roDeleteValue, roNoError,
      roDontCreateKey);
  end;
const
  SetupDeleteEntryStrings = 1;
type
  TSetupDeleteType = (dfFiles, dfFilesAndOrSubdirs, dfDirIfEmpty);
  PSetupDeleteEntry = ^TSetupDeleteEntry;
  TSetupDeleteEntry = packed record
    Name: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    DeleteType: TSetupDeleteType;
  end;
const
  SetupRunEntryStrings = 4;
type
  PSetupRunEntry = ^TSetupRunEntry;
  TSetupRunEntry = packed record
    Name, Parameters, WorkingDir, RunOnceId: AnsiString;
    MinVersion, OnlyBelowVersion: TSetupVersionData;
    ShowCmd: Integer;
    Wait: (rwWaitUntilTerminated, rwNoWait, rwWaitUntilIdle);
    Options: set of (roShellExec, roSkipIfDoesntExist);
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
