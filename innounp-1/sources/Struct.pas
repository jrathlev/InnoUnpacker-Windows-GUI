unit Struct;

//////// only version-independent types and constants
//////// a separate file is used to keep version-specific definitions off the project namespace

interface

uses Winapi.Windows, MyTypes;

type
  TSetupLdrOffsetTable = TMySetupLdrOffsetTable;
  PSetupLdrOffsetTable = ^TSetupLdrOffsetTable;
  TSetupHeader = TMySetupHeader;
  PSetupHeader = ^TSetupHeader;
  TSetupFileEntry = TMySetupFileEntry;
  PSetupFileEntry = ^TSetupFileEntry;
  TSetupFileLocationEntry = TMySetupFileLocationEntry;
  PSetupFileLocationEntry = ^TSetupFileLocationEntry;
  TSetupRegistryEntry = TMySetupRegistryEntry;
  PSetupRegistryEntry = ^TSetupRegistryEntry;
  TSetupRunEntry = TMySetupRunEntry;
  PSetupRunEntry = ^TSetupRunEntry;
  TSetupIconEntry = TMySetupIconEntry;
  PSetupIconEntry = ^TSetupIconEntry;
  TSetupTaskEntry = TMySetupTaskEntry;
  PSetupTaskEntry = ^TSetupTaskEntry;
  TSetupComponentEntry = TMySetupComponentEntry;
  PSetupComponentEntry = ^TSetupComponentEntry;
  TSetupTypeEntry = TMySetupTypeEntry;
  PSetupTypeEntry = ^TMySetupTypeEntry;
  TSetupCustomMessageEntry = TMySetupCustomMessageEntry;
  PSetupCustomMessageEntry = ^TSetupCustomMessageEntry;
  TSetupLanguageEntry = TMySetupLanguageEntry;
  PSetupLanguageEntry = ^TMySetupLanguageEntry;
  TSetupDirEntry = TMySetupDirEntry;
  PSetupDirEntry = ^TMySetupDirEntry;
  TSetupIniEntry = TMySetupIniEntry;
  PSetupIniEntry = ^TMySetupIniEntry;
  TSetupDeleteEntry = TMySetupDeleteEntry;
  PSetupDeleteEntry = ^TMySetupDeleteEntry;

  TSetupFileOption = TMySetupFileOption;
  TSetupFileLocationSign = TMySetupFileLocationSign;
  TSetupRegistryOption = TMySetupRegistryOption;
  TSetupDirOption = TMySetupDirOption;
  TSetupIniOption = TMySetupIniOption;
  TSetupRunOption = TMySetupRunOption;
  TSetupFileLocationFlag = TMySetupFileLocationFlag;

  TSetupProcessorArchitecture = TMySetupProcessorArchitecture;
  TSetupProcessorArchitectures = TMySetupProcessorArchitectures;
  TSetupDeleteType = TMySetupDeleteType;


  TSetupID = array[0..63] of AnsiChar;
  TCompID = array[1..4] of AnsiChar;
  TDiskSliceID = array[1..8] of AnsiChar;
const
  { SetupID is used by the Setup program to check if the SETUP.0 file is
    compatible with with it. If you make any modifications to the records in
    this file it's recommended you change SetupID. Any change will do (like
    changing the letters or numbers), as long as your format is
    unrecognizable by the standard Inno Setup. }
//  SetupID: TSetupID = 'Inno Setup Setup Data (4.1.5)';
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

  { A TDiskSliceHeader record follows DiskSliceID in a SETUP-*.BIN file }
  TDiskSliceHeader = packed record
    TotalSize: Cardinal;
  end;

  { A TMessageHeader record follows MessagesHdrID in a SETUP.MSG file }
  TMessagesHeader = packed record
    NumMessages: Cardinal;
    TotalSize: Cardinal;
    NotTotalSize: Cardinal;
    CRCMessages: Longint;
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

  TSetupLdrExeHeader = packed record
    ID: Longint;
    OffsetTableOffset, NotOffsetTableOffset: Longint;
  end;

  TSetupLdrOffsetTable4010 = packed record // valid since v4.0.10
    ID: array[1..12] of AnsiChar;
    TotalSize,
    OffsetEXE, CompressedSizeEXE, UncompressedSizeEXE, CRCEXE,
    Offset0, Offset1: Longint;
    TableCRC: Longint;  { CRC of all prior fields in this record }
  end;

const
  SetupLdrExeHeaderOffset = $30;
  SetupLdrExeHeaderID = $6F6E6E49;
  SetupLdrOffsetTableResID = 11111;

implementation

end.
