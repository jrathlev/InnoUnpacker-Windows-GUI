(* Delphi Unit
   collection of subroutines for Windows drives and devices
   ========================================================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1 - Dec. 2016
   last modified: December 2023
   *)

unit WinDevUtils;

interface

{$Z4}        // use DWORD (4 bytes for enumerations)
{$Align on}  // align records

uses WinApi.Windows, System.Classes, System.SysUtils;

type
  TDriveType = (dtUnknown,dtNoRoot,dtRemovable,dtFixed,dtRemote,dtCdRom,dtRamDisk);
  TDriveTypes = set of TDriveType;
  TPathType =  (ptNotAvailable,ptFixed,ptRelative,ptRemovable,ptRemote);

  TDriveProperties = class(TObject)
    Number    : integer;
    DriveType : TDriveType;
    DriveName,
    VolName   : string
    end;

  STORAGE_PROPERTY_ID = (
    StorageDeviceProperty,
    StorageAdapterProperty,
    StorageDeviceIdProperty,
    StorageDeviceUniqueIdProperty,              // See storduid.h for details
    StorageDeviceWriteCacheProperty,
    StorageMiniportProperty,
    StorageAccessAlignmentProperty,
    StorageDeviceSeekPenaltyProperty,
    StorageDeviceTrimProperty,
    StorageDeviceWriteAggregationProperty);

  STORAGE_QUERY_TYPE = (
    PropertyStandardQuery,              // Retrieves the descriptor
    PropertyExistsQuery,                // Used to test whether the descriptor is supported
    PropertyMaskQuery,                  // Used to retrieve a mask of writeable fields in the descriptor
    PropertyQueryMaxDefined);           // use to validate the value

  PStoragePropertyQuery = ^TStoragePropertyQuery;
  _STORAGE_PROPERTY_QUERY = record
    PropertyId : STORAGE_PROPERTY_ID;
    QueryType : STORAGE_QUERY_TYPE;
    AdditionalParameters : byte;
    end;
  TStoragePropertyQuery = _STORAGE_PROPERTY_QUERY;

  STORAGE_BUS_TYPE = (
    BusTypeUnknown = 0,
    BusTypeScsi,
    BusTypeAtapi,
    BusTypeAta,
    BusType1394,
    BusTypeSsa,
    BusTypeFibre,
    BusTypeUsb,
    BusTypeRAID,
    BusTypeiScsi,
    BusTypeSas,
    BusTypeSata,
    BusTypeSd,
    BusTypeMmc,
    BusTypeVirtual,
    BusTypeFileBackedVirtual,
    BusTypeSpaces,
    BusTypeNvme,
    BusTypeSCM,
    BusTypeUfs,
    BusTypeMax,
    BusTypeMaxReserved);
  TBusType = STORAGE_BUS_TYPE;

  PStorageDeviceDescriptor = ^TStorageDeviceDescriptor;
  STORAGE_DEVICE_DESCRIPTOR = record
    Version, Size : DWORD;
    DeviceType, DeviceTypeModifier : byte;
    RemovableMedia, CommandQueueing : boolean;
    VendorIdOffset, ProductIdOffset,
    ProductRevisionOffset, SerialNumberOffset : DWORD;
    BusType : STORAGE_BUS_TYPE;
    RawPropertiesLength : DWORD;
    RawDeviceProperties : byte;
    end;
  TStorageDeviceDescriptor = STORAGE_DEVICE_DESCRIPTOR;

  PStorageReadCapacity = ^TStorageReadCapacity;
  STORAGE_READ_CAPACITY = record
    Version, Size, BlockLength : DWORD;
    NumberOfBlocks, DiskLength : LONGLONG;
    end;
  TStorageReadCapacity = STORAGE_READ_CAPACITY;

  TFileSystemFlag =
   (
    fsCaseSensitive,            // The file system supports case-sensitive file names.
    fsCasePreservedNames,       // The file system preserves the case of file names when it places a name on disk.
    fsSupportsUnicodeOnDisk,    // The file system supports Unicode in file names as they appear on disk.
    fsPersistentACLs,           // The file system preserves and enforces ACLs. For example, NTFS preserves and enforces ACLs, and FAT does not.
    fsSupportsFileCompression,  // The file system supports file-based compression.
    fsSupportsVolumeQuotas,     // The file system supports disk quotas.
    fsSupportsSparseFiles,      // The file system supports sparse files.
    fsSupportsReparsePoints,    // The file system supports reparse points.
    fsSupportsRemoteStorage,    // The file system supports remote storage.
    fsReturnsCleanupInfo,       // The file system returns additional cleanup information.
    fsPosixStyleOperation,      // The file system supports POSIX-style operations.
    fsVolumeIsCompressed,       // The specified volume is a compressed volume; for example, a DoubleSpace volume.
    fsSupportsObjectIds,        // The file system supports object identifiers.
    fsSupportsEncryption,       // The file system supports the Encrypted File System (EFS).
    fsSupportsNamedStreams,     // The file system supports named streams.
    fsVolumeIsReadOnly,         // The specified volume is read-only.
    fsSequentialWriteOnce,      // The specified volume can be written to one time only.
    fsSupportsTransactions,     // The file system supports transactions.
    fsSupportsHardlinks,        // The file system supports hard links.
    fsExtendedAttributes,       // The file system supports extended attributes.
    fsOpenByFileId,             // The file system supports open by file ID.
    fsSupportsUSNJournal,       // The file system supports update sequence number (USN) journals.
    fsIntegrityStreams,         // The file system supports integrity streams.
    fsBlockCloning,             // The file system supports block cloning.
    fsSparseVDL,                // The file system uses sparse valid data length (VDL)
    fsDirectAccessVolume,       // The specified volume is a direct access (DAX) volume.
    fsGhosting                  // The file system supports ghosting.
   );


  TFileSystemFlags = set of TFileSystemFlag;

const
  DriveTypeNames : array [TDriveType] of string =
    ('Unknown','Not mounted','Removable','Fixed','Remote','CD/DVD','Ramdisk');
  BusNames : array [TBusType] of string =
    ('Unknown','SCSI','Atapi', 'ATA','IEEE1394','SSA','Fiber channel','USB','RAID',
     'iSCSI','SCSI (SAS)','SATA','SD','MMC','Virtual','File-backed virtual',
     'Spaces','Nvme','SCM','Ufs','Unknown','Unknown');
  FileSystemFlagDesc : array [TFileSystemFlag] of string =
    ('Supports case-sensitive file names',
     'Preserves the case of file names',
     'Supports Unicode in file names',
     'Preserves and enforces ACLs',
     'Supports file-based compression',
     'Supports disk quotas',
     'Supports sparse files',
     'Supports reparse points',
     'Supports remote storage',
     'Returns additional cleanup information',
     'Supports POSIX-style operations',
     'Is a compressed volume',
     'Supports object identifiers',
     'Supports the Encrypted File System (EFS)',
     'Supports named streams',
     'Is read-only',
     'Can be written to one time only',
     'Supports transactions',
     'Supports hard links',
     'Supports extended attributes',
     'Supports open by file ID',
     'Supports USN journals',
     'Supports integrity streams',
     'Supports block cloning',
     'Uses sparse valid data length (VDL)',
     'Is a direct access (DAX) volume',
     'Supports ghosting'
    );
{ Unterstützt die Groß-/Kleinschreibung von Dateinamen
  Behält die Groß-/Kleinschreibung von Dateinamen
  Unterstützt Unicode-Dateinamen
  Behält und erzwingt Zugriffsteuerungslisten (ACL)
  Unterstützt Datei-basierte Komprimierung
  Unterstützt Datenträgerkontingente
  Unterstützt Dateien mit geringer Datendichte
  Unterstützt Analysepunkte
  Unterstützt Remotespeicher
  Gibt zusätzliche Ergebnisinformationen beim Aufräumen zurück
  Unterstützt das Trennen und Umbenennen im POSIX-Stil
  Ist ein komprimiertes Volume
  Unterstützt Objektkennungen
  Unterstützt EFS (verschlüsseltes Dateisystem)
  Unterstützt benannte Streams
  Nur Lesezugriff
  Kann nur einmal beschrieben werden
  Unterstützt Transaktionen
  Unterstützt feste Links
  Unterstützt erweiterte Attribute
  Unterstützt das Öffnen nach Datei-ID.
  Mit USN-Journal-Unterstützung
  Unterstützt Integritätsdatenströme
  Unterstützt das Klonen von Blöcken
  Benutzt sparse valid data length (VDL)
  Unterstützt direkten Zugriff (DAX)
  Unterstützt Ghosting
}

// Typ eines Laufwerkes ermitteln
function DriveType (const Path : string) : TDriveType;

// Typ eines Pfades ermitteln (siehe TPathType)
function CheckPath (const Path : string) : TPathType;

// Removable oder Fixed
function IsLocalDrive (const Path : string) : boolean;

// Fixed
function IsFixedDrive (const Path : string) : boolean;

// check if system drive
function IsSystemDrive (const Path : string) : boolean;

// Prüfe Laufwerk auf Verfügbarkeit
function CheckForDriveAvailable (const Path : string; var VolumeID : string) : boolean; overload;
function CheckForDriveAvailable (const Path : string) : boolean; overload;

// Liste aller Laufwerke der Typen "UseTypes" aufbauen
procedure BuildDriveList(DriveList : TStrings; UseTypes : TDriveTypes);

// Zu einem Datenträgernamen gehörendes Laufwerk ermitteln
function GetDriveLetterForVolume (const Vol : string; FirstDrive : integer) : string;

function GetDriveForVolume (const VolName : string; var DriveName : string;
  OnlyMounted : boolean = false) : integer;
function DriveForVolume (const VolName : string; OnlyMounted : boolean = false) : string;

function GetStorageProperty (const Drive : string; var StorageProperty : TStorageDeviceDescriptor) : boolean;
function GetBusType (const Drive : string) : TBusType;
function IsRemovableDrive (const Drive : string) : boolean;

{ ---------------------------------------------------------------- }
(* Disk size and free space *)
function GetDiskFree (const Path : string) : int64;
function GetDiskTotal (const Path : string) : int64;

{ ---------------------------------------------------------------- }
// drive and file system info
function GetVolumeName(const Drive: string): string;
function GetVolumeSerialNumber(const Drive: string): string;
function GetVolumeFileSystem(const Drive: string): string;
function GetVolumeComponentLength(const Drive: string): string;
function IsNtfs (const Drive : string) : boolean;
function IsExFat (const Drive : string) : boolean;
function IsLtfs (const Drive : string) : boolean;
function IsExtFs (const Drive : string) : boolean; // file systems capable to keep files > 4GB

function GetFileSystemAttributes(Flags: Cardinal) : TFileSystemFlags;
function GetVolumeFileSystemFlags(const Volume: string): TFileSystemFlags;
function GetVolumeUniqueName(const Drive : string) : string;

{ ---------------------------------------------------------------- }
// The following definitions are erroneous in Winapi.Windows (Delphi 10):
// LPCWSTR/LPWSTR should be used instead of LPCSTR/LPSTR
{$EXTERNALSYM GetVolumePathName}
function GetVolumePathName(lpszFileName: LPCWSTR; lpszVolumePathName: LPWSTR;
         cchBufferLength: DWORD): BOOL; stdcall;

{$EXTERNALSYM GetVolumePathNamesForVolumeName}
function GetVolumePathNamesForVolumeName(lpszVolumeName: LPCWSTR; lpszVolumePathNames: LPWSTR;
         cchBufferLength: DWORD; var lpcchReturnLength: DWORD): BOOL; stdcall;

{$EXTERNALSYM GetVolumeNameForVolumeMountPoint}
function GetVolumeNameForVolumeMountPoint (lpszVolumeMountPoint : LPCWSTR;
  lpszVolumeName : LPWSTR; cchBufferLength : DWORD) : BOOL; stdcall;

{$EXTERNALSYM FindFirstVolume}
function FindFirstVolume(lpszVolumeName: LPWSTR; cchBufferLength: DWORD): THandle; stdcall;

{$EXTERNALSYM FindNextVolume}
function FindNextVolume(hFindVolume: THandle; lpszVolumeName: LPWSTR;
  cchBufferLength: DWORD): BOOL; stdcall;

{$EXTERNALSYM FindFirstVolumeMountPoint}
function FindFirstVolumeMountPoint(lpszRootPathName: LPCWSTR;
  lpszVolumeMountPoint: LPWSTR; cchBufferLength: DWORD): THandle; stdcall;

{$EXTERNALSYM FindNextVolumeMountPoint}
function FindNextVolumeMountPoint(hFindVolumeMountPoint: THandle;
  lpszVolumeMountPoint: LPWSTR; cchBufferLength: DWORD): BOOL; stdcall;

implementation

uses UnitConsts, WinApiUtils;

const
  FILE_RETURNS_CLEANUP_RESULT_INFO   = $00000200;
  FILE_SUPPORTS_POSIX_UNLINK_RENAME  = $00000400;
  FILE_SUPPORTS_INTEGRITY_STREAMS    = $04000000;
  FILE_SUPPORTS_BLOCK_REFCOUNTING    = $08000000;
  FILE_SUPPORTS_SPARSE_VDL           = $10000000;
  FILE_DAX_VOLUME                    = $20000000;
  FILE_SUPPORTS_GHOSTING             = $40000000;

// Typ eines Laufwerkes ermitteln
function DriveType (const Path : string) : TDriveType;
var
  Drive : string;
begin
  Drive:=ExtractFileDrive(Path)+'\';
  case GetDriveType(pchar(Drive)) of
  DRIVE_NO_ROOT_DIR : Result:=dtNoRoot;
  DRIVE_REMOVABLE   : Result:=dtRemovable;
  DRIVE_FIXED       : Result:=dtFixed;
  DRIVE_REMOTE      : Result:=dtRemote;
  DRIVE_CDROM       : Result:=dtCdRom;
  DRIVE_RAMDISK     : Result:=dtRamDisk;
  else Result:=dtUnknown;
    end;
// some Windows 10 systems returns DRIVE_FIXED also for USB connected storage media
  if (Result=dtFixed) and IsRemovableDrive(Drive) then Result:=dtRemovable;
  end;

// Typ eines Pfades ermitteln (siehe TPathType)
function CheckPath (const Path : string) : TPathType;
var
  dr : string;
  dt : TDriveType;
begin
  dr:=ExtractFileDrive(IncludeTrailingPathDelimiter(Path));
  if length(dr)>0 then begin
    if AnsiSameText(copy(dr,1,11),'\\?\Volume{') then Result:=ptFixed
    else if (copy(dr,1,2)='\\') then Result:=ptRemote    // Netzwerkumgebung
    else begin                                      // Pfad mit Laufwerksangabe
      dt:=DriveType(dr);
      case dt of
      dtRemote : Result:=ptRemote;          // Netzlaufwerk
      dtUnknown,
      dtNoRoot : Result:=ptNotAvailable;    // nicht verfügbar
      dtCdRom,
      dtRemovable : Result:=ptRemovable;    // Laufwerk mit Wechselmedium
      else Result:=ptFixed;
        end;
      end;
    end
  else Result:=ptRelative;
  end;

function IsLocalDrive (const Path : string) : boolean;
var
  pt : TPathType;
begin
  pt:=CheckPath(Path);
  Result:=(pt=ptFixed) or (pt=ptRemovable);
  end;

function IsFixedDrive (const Path : string) : boolean;
begin
  Result:=CheckPath(Path)=ptFixed;
  end;

function IsSystemDrive (const Path : string) : boolean;
var
  p : pchar;
begin
  p:=StrAlloc(MAX_PATH+1);
  GetSystemDirectory (p,MAX_PATH+1);
  Result:=AnsiSameText(ExtractFileDrive(Path),ExtractFileDrive(p));
  StrDispose(p);
  end;

// Prüfe Laufwerk auf Verfügbarkeit
function CheckForDriveAvailable (const Path : string; var VolumeID : string) : boolean;
var
  v : pchar;
  d : string;
  n,cl,sf : dword;
begin
  d:=IncludeTrailingPathDelimiter(ExtractFileDrive(Path));
  n:=255; v:=StrAlloc(n);
  Result:=GetVolumeInformation(pchar(d),v,n,nil,cl,sf,nil,0);
  if Result then VolumeID:=Trim(v)  // remove leading and trailing spaces
  else VolumeID:=rsNotAvail;
  StrDispose(v);
  end;

function CheckForDriveAvailable (const Path : string) : boolean;
var
  s : string;
begin
  Result:=CheckForDriveAvailable(Path,s);
  end;

// Liste aller Laufwerke der Typen "UseTypes" aufbauen
procedure BuildDriveList (DriveList : TStrings; UseTypes : TDriveTypes);
var
  i         : integer;
  DriveBits : set of 0..25;
  dp        : TDriveProperties;
begin
  DriveList.Clear;
  Integer(DriveBits):=GetLogicalDrives;
  for i:=0 to 25 do begin
    if not (i in DriveBits) then Continue;
    dp:=TDriveProperties.Create;
    with dp do begin
      Number:=i;
      DriveName:=Char(i+Ord('A'))+':\';
      DriveType:=TDriveType(GetDriveType(PChar(DriveName)));
      CheckForDriveAvailable(DriveName,VolName);
      if length(VolName)=0 then VolName:=rsNoName;
      end;
    if dp.DriveType in UseTypes then DriveList.AddObject(dp.VolName,dp)
    else dp.Free;;
    end;
  end;

// Zu einem Datenträgernamen gehörendes Laufwerk ermitteln
function GetDriveLetterForVolume (const Vol : string; FirstDrive : integer) : string;
var
  i         : integer;
  DriveBits : set of 0..25;
  sd,sv     : string;
begin
  Result:='';
  Integer(DriveBits):=GetLogicalDrives;
  for i:=FirstDrive to 25 do begin
    if not (i in DriveBits) then Continue;
    sd:=Char(i+Ord('A'))+':\';
    if CheckForDriveAvailable(sd,sv) and AnsiSameText(sv,Vol) then begin
      Result:=sd; Exit;
      end;
    end;
  end;

// Get the drive name associated with a volume name
// if not mounted, return volume GUID
function GetDriveForVolume (const VolName : string; var DriveName : string;
  OnlyMounted : boolean = false) : integer;
var
  VolHandle   : THandle;
  Buf         : array [0..MAX_PATH+1] of Char;
  VolumeId,
  VName       : string;
  n,cl,sf     : cardinal;
begin
  Result:=NO_ERROR;
  VolHandle:=FindFirstVolume(Buf,length(Buf));
  if VolHandle=INVALID_HANDLE_VALUE then Result:=GetLastError
  else begin
    repeat
      VolumeId:=Buf;
      if GetVolumePathNamesForVolumeName(PChar(VolumeId),Buf,length(Buf),n) then begin
        DriveName:=Buf;
        if GetVolumeInformation(pchar(VolumeId),Buf,length(Buf),nil,cl,sf,nil,0) then begin
          VName:=Buf;
          if AnsiSameText(VolName,VName) then begin
            if (length(DriveName)=0) then begin
              if OnlyMounted then DriveName:=''
              else DriveName:=VolumeId;
              end;
            Break;
            end
          else DriveName:='';
          end
        else begin
          Result:=GetLastError;
          if Result=ERROR_NOT_READY then DriveName:=''
          else Break;
          end;
        end
      else begin
        Result:=GetLastError; Break;
        end;
      until not FindNextVolume(VolHandle,Buf,length(Buf));
    end;
  FindVolumeClose(VolHandle);
  if length(DriveName)=0 then Result:=ERROR_NO_VOLUME_LABEL;
  end;

function DriveForVolume (const VolName : string; OnlyMounted : boolean = false) : string;
begin
  GetDriveForVolume(VolName,Result,OnlyMounted);
  end;

function GetStorageProperty (const Drive : string; var StorageProperty : TStorageDeviceDescriptor) : boolean;
var
  Handle : THandle;
  query  : TStoragePropertyQuery;
  bytes  : DWORD;
  sp     : string;
begin
  Result:=false;
  ZeroMemory(@StorageProperty, SizeOf(TStorageDeviceDescriptor));
  if Pos('\\',Drive)=1 then sp:=copy(Drive,1,length(Drive)-1) else sp:='\\.\'+copy(Drive,1,2);
  Handle:=CreateFile(PChar(sp),0,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil,OPEN_EXISTING,0,0);
  if Handle <> INVALID_HANDLE_VALUE then begin
    with query do begin
      PropertyId:=StorageDeviceProperty; QueryType:=PropertyStandardQuery;
      AdditionalParameters:=0;
      end;
    Result:=DeviceIoControl(Handle,IOCTL_STORAGE_QUERY_PROPERTY,
        @query,sizeof(query),@StorageProperty,sizeof(TStorageDeviceDescriptor),bytes,nil);
    CloseHandle(Handle);
    end;
  end;

function GetBusType (const Drive : string) : TBusType;
var
  StgProp : TStorageDeviceDescriptor;
begin
  if GetStorageProperty (Drive,StgProp) then Result:=StgProp.BusType
  else Result:=BusTypeUnknown;
  end;

function IsRemovableDrive (const Drive : string) : boolean;
var
  StgProp : TStorageDeviceDescriptor;
begin
  if GetStorageProperty (Drive,StgProp) then Result:=StgProp.BusType=BusTypeUsb
  else Result:=false;
  end;

{ ---------------------------------------------------------------- }
(* Disk size and free space *)
function GetDiskFree (const Path : string) : int64 ;
var
  n : Int64;
begin
  GetDiskFreeSpaceEx(pchar(IncludeTrailingPathDelimiter(Path)),Result,n,nil);
  end;

function GetDiskTotal (const Path : string) : int64;
var
  n : Int64;
begin
  GetDiskFreeSpaceEx(pchar(IncludeTrailingPathDelimiter(Path)),n,Result,nil);
  end;

{ ------------------------------------------------------------------- }
// following part from JclSysInfo (Project JEDI Code Library)
type
  TVolumeInfoKind = (vikName, vikSerial, vikFileSystem, vikComponentLength);

function GetVolumeInfoHelper(const Drive: string; InfoKind: TVolumeInfoKind): string;
var
  VolumeSerialNumber: DWORD;
  MaximumComponentLength: DWORD;
  Flags: DWORD;
  Name: array [0..MAX_PATH] of Char;
  FileSystem: array [0..MAX_PATH] of Char;
  ErrorMode: Cardinal;
  DriveStr: string;
begin
  { TODO : Change to RootPath }
  { TODO : Perform better checking of Drive param or document that no checking
    is performed. RM Suggested:
    DriveStr := Drive;
    if (Length(Drive) < 2) or (Drive[2] <> ':') then
      DriveStr := GetCurrentFolder;
    DriveStr  := DriveStr[1] + ':\'; }
  Result:='';
  if length(Drive)=0 then Exit;
  if Pos('\\',Drive)=1 then DriveStr:=copy(Drive,1,length(Drive)-1)
  else if pos(':',Drive)=0 then DriveStr:=Drive + ':'
  else DriveStr:=Drive;
  DriveStr:=IncludeTrailingPathDelimiter(DriveStr);
  ErrorMode:=SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if GetVolumeInformation(PChar(DriveStr), Name, SizeOf(Name), @VolumeSerialNumber,
      MaximumComponentLength, Flags, FileSystem, SizeOf(FileSystem)) then
    case InfoKind of
      vikName:
        Result:=StrPas(Name);
      vikSerial:
        begin
          Result:=IntToHex(HiWord(VolumeSerialNumber), 4) + '-' +
          IntToHex(LoWord(VolumeSerialNumber), 4);
        end;
      vikFileSystem:
        Result:=StrPas(FileSystem);
      vikComponentLength:
        Result:=IntToStr(MaximumComponentLength);
    end;
  finally
    SetErrorMode(ErrorMode);
  end;
end;

function GetVolumeName(const Drive: string): string;
begin
  Result:=GetVolumeInfoHelper(Drive, vikName);
end;

function GetVolumeSerialNumber(const Drive: string): string;
begin
  Result:=GetVolumeInfoHelper(Drive, vikSerial);
end;

function GetVolumeFileSystem(const Drive: string): string;
begin
  Result:=GetVolumeInfoHelper(Drive, vikFileSystem);
end;

function GetVolumeComponentLength(const Drive: string): string;
begin
  Result:=GetVolumeInfoHelper(Drive, vikComponentLength);
end;

function IsNtfs (const Drive : string) : boolean;
begin
  Result:=AnsiSameText('NTFS',GetVolumeFileSystem(Drive));
  end;

function IsExFat (const Drive : string) : boolean;
begin
  Result:=AnsiSameText('exFAT',GetVolumeFileSystem(Drive));
  end;

function IsLtfs (const Drive : string) : boolean;
begin
  Result:=AnsiSameText('LTFS',GetVolumeFileSystem(Drive));
  end;

// file systems capable to keep files > 4GB
function IsExtFs (const Drive : string) : boolean;
var
  s : string;
begin
  s:=GetVolumeFileSystem(Drive);
  Result:=AnsiSameText('NTFS',s) or AnsiSameText('exFAT',s) or AnsiSameText('LTFS',s);
  end;

function GetFileSystemAttributes (Flags: Cardinal) : TFileSystemFlags;
var
  Flag: TFileSystemFlag;
const
  FileSystemFlags: array [TFileSystemFlag] of DWORD =
    ( FILE_CASE_SENSITIVE_SEARCH,        // fsCaseSensitive
      FILE_CASE_PRESERVED_NAMES,         // fsCasePreservedNames
      FILE_UNICODE_ON_DISK,              // fsSupportsUnicodeOnDisk
      FILE_PERSISTENT_ACLS,              // fsPersistentACLs
      FILE_FILE_COMPRESSION,             // fsSupportsFileCompression
      FILE_VOLUME_QUOTAS,                // fsSupportsVolumeQuotas
      FILE_SUPPORTS_SPARSE_FILES,        // fsSupportsSparseFiles
      FILE_SUPPORTS_REPARSE_POINTS,      // fsSupportsReparsePoints
      FILE_SUPPORTS_REMOTE_STORAGE,      // fsSupportsRemoteStorage
      FILE_RETURNS_CLEANUP_RESULT_INFO,  // fsReturnsCleanupInfo
      FILE_SUPPORTS_POSIX_UNLINK_RENAME, // fsPosixStyleOperation
      FILE_VOLUME_IS_COMPRESSED,         // fsVolumeIsCompressed
      FILE_SUPPORTS_OBJECT_IDS,          // fsSupportsObjectIds
      FILE_SUPPORTS_ENCRYPTION,          // fsSupportsEncryption
      FILE_NAMED_STREAMS,                // fsSupportsNamedStreams
      FILE_READ_ONLY_VOLUME,             // fsVolumeIsReadOnly
      FILE_SEQUENTIAL_WRITE_ONCE,        // fsSequentialWriteOnce
      FILE_SUPPORTS_TRANSACTIONS,        // fsSupportsTransactions
      FILE_SUPPORTS_HARD_LINKS,          // fsSupportsHardlinks
      FILE_SUPPORTS_EXTENDED_ATTRIBUTES, // fsExtendedAttributes
      FILE_SUPPORTS_OPEN_BY_FILE_ID,     // fsOpenByFileId
      FILE_SUPPORTS_USN_JOURNAL,         // fsSupportsUSNJournal
      FILE_SUPPORTS_INTEGRITY_STREAMS,   // fsIntegrityStreams
      FILE_SUPPORTS_BLOCK_REFCOUNTING,   // fsBlockCloning
      FILE_SUPPORTS_SPARSE_VDL,          // fsSparseVDL
      FILE_DAX_VOLUME,                   // fsDirectAccessVolume
      FILE_SUPPORTS_GHOSTING             // fsGhosting
    );
begin
  Result:=[];
  for Flag:=Low(TFileSystemFlag) to High(TFileSystemFlag) do
    if (Flags and FileSystemFlags[Flag]) <> 0 then
      Include(Result, Flag);
  end;

function GetVolumeFileSystemFlags(const Volume: string) : TFileSystemFlags;
var
  MaximumComponentLength, Flags: Cardinal;
begin
  if not GetVolumeInformation(PChar(IncludeTrailingPathDelimiter(Volume)), nil, 0, nil,
    MaximumComponentLength, Flags, nil, 0) then
    RaiseLastOSError;
  Result:=GetFileSystemAttributes(Flags);
  end;

function GetVolumeUniqueName(const Drive : string) : string;
var
  uname : array [0..MAX_PATH] of Char;
begin
  if (length(Drive)>0) and GetVolumeNameForVolumeMountPoint(PChar(Drive),uname,MAX_PATH+1) then
    Result:=uname
  else Result:='';
  end;

{ ------------------------------------------------------------------- }
// The following definitions are erroneous in Winapi.Windows (Delphi 10):
// LPCWSTR/LPWSTR should be used instead of LPCSTR/LPSTR
function GetVolumePathName; external kernel32 name 'GetVolumePathNameW';
function GetVolumePathNamesForVolumeName; external kernel32 name 'GetVolumePathNamesForVolumeNameW';
function GetVolumeNameForVolumeMountPoint; external kernel32 name 'GetVolumeNameForVolumeMountPointW';
function FindFirstVolume; external kernel32 name 'FindFirstVolumeW';
function FindNextVolume; external kernel32 name 'FindNextVolumeW';
//function FindVolumeClose; external kernel32 name 'FindVolumeClose';
function FindFirstVolumeMountPoint; external kernel32 name 'FindFirstVolumeMountPointW';
function FindNextVolumeMountPoint; external kernel32 name 'FindNextVolumeMountPointW';

end.
