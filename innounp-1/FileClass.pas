unit FileClass;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TFile class
  Better than File and TFileStream in that does more extensive error checking
  and uses descriptive, localized system error messages.

  TTextFileReader and TTextFileWriter support ANSI and UTF8 textfiles only.

  $jrsoftware: issrc/Projects/FileClass.pas,v 1.31 2010/01/26 06:26:18 jr Exp $
}

{$I VERSION.INC}

{$IFDEF IS_D6}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
  Winapi.Windows, System.SysUtils, Int64Em;

type
  TFileCreateDisposition = (fdCreateAlways, fdCreateNew, fdOpenExisting,
    fdOpenAlways, fdTruncateExisting);
  TFileAccess = (faRead, faWrite, faReadWrite);
  TFileSharing = (fsNone, fsRead, fsWrite, fsReadWrite);

  TCustomFile = class
  private
    function GetCappedSize: Cardinal;
  protected
    function GetPosition: Integer64; virtual; abstract;
    function GetSize: Integer64; virtual; abstract;
  public
    class procedure RaiseError(ErrorCode: DWORD);
    class procedure RaiseLastError;
    function Read(var Buffer; Count: Cardinal): Cardinal; virtual; abstract;
    procedure ReadBuffer(var Buffer; Count: Cardinal);
    procedure Seek(Offset: Cardinal);
    procedure Seek64(Offset: Integer64); virtual; abstract;
    procedure WriteAnsiString(const S: AnsiString);
    procedure WriteBuffer(const Buffer; Count: Cardinal); virtual; abstract;
    property CappedSize: Cardinal read GetCappedSize;
    property Position: Integer64 read GetPosition;
    property Size: Integer64 read GetSize;
  end;

  TFile = class(TCustomFile)
  private
    FHandle: THandle;
    FHandleCreated: Boolean;
  protected
    function CreateHandle(const AFilename: String;
      ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
      ASharing: TFileSharing): THandle; virtual;
    function GetPosition: Integer64; override;
    function GetSize: Integer64; override;
  public
    constructor Create(const AFilename: String;
      ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
      ASharing: TFileSharing);
    destructor Destroy; override;
    function Read(var Buffer; Count: Cardinal): Cardinal; override;
    procedure Seek64(Offset: Integer64); override;
    procedure SeekToEnd;
    procedure WriteBuffer(const Buffer; Count: Cardinal); override;
    property Handle: THandle read FHandle;
  end;

  EFileError = class(Exception)
  private
    FErrorCode: DWORD;
  public
    property ErrorCode: DWORD read FErrorCode;
  end;

implementation

uses
  CmnFunc2;

const
  SGenericIOError = 'File I/O error %d';

{ TCustomFile }

function TCustomFile.GetCappedSize: Cardinal;
{ Like GetSize, but capped at $7FFFFFFF }
var
  S: Integer64;
begin
  S := GetSize;
  if (S.Hi = 0) and (S.Lo and $80000000 = 0) then
    Result := S.Lo
  else
    Result := $7FFFFFFF;
end;

class procedure TCustomFile.RaiseError(ErrorCode: DWORD);
var
  S: String;
  E: EFileError;
begin
  S := Win32ErrorString(ErrorCode);
  if S = '' then begin
    { In case there was no text for the error code. Shouldn't get here under
      normal circumstances. }
    S := Format(SGenericIOError, [ErrorCode]);
  end;
  E := EFileError.Create(S);
  E.FErrorCode := ErrorCode;
  raise E;
end;

class procedure TCustomFile.RaiseLastError;
begin
  RaiseError(GetLastError);
end;

procedure TCustomFile.ReadBuffer(var Buffer; Count: Cardinal);
begin
  if Read(Buffer, Count) <> Count then begin
    { Raise localized "Reached end of file" error }
    RaiseError(ERROR_HANDLE_EOF);
  end;
end;

procedure TCustomFile.Seek(Offset: Cardinal);
var
  I: Integer64;
begin
  I.Hi := 0;
  I.Lo := Offset;
  Seek64(I);
end;

procedure TCustomFile.WriteAnsiString(const S: AnsiString);
begin
  WriteBuffer(S[1], Length(S));
end;

{ TFile }

constructor TFile.Create(const AFilename: String;
  ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
  ASharing: TFileSharing);
begin
  inherited Create;
  FHandle := CreateHandle(AFilename, ACreateDisposition, AAccess, ASharing);
  if (FHandle = 0) or (FHandle = INVALID_HANDLE_VALUE) then
    RaiseLastError;
  FHandleCreated := True;
end;

destructor TFile.Destroy;
begin
  if FHandleCreated then
    CloseHandle(FHandle);
  inherited;
end;

function TFile.CreateHandle(const AFilename: String;
  ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
  ASharing: TFileSharing): THandle;
const
  AccessFlags: array[TFileAccess] of DWORD =
    (GENERIC_READ, GENERIC_WRITE, GENERIC_READ or GENERIC_WRITE);
  SharingFlags: array[TFileSharing] of DWORD =
    (0, FILE_SHARE_READ, FILE_SHARE_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE);
  Disps: array[TFileCreateDisposition] of DWORD =
    (CREATE_ALWAYS, CREATE_NEW, OPEN_EXISTING, OPEN_ALWAYS, TRUNCATE_EXISTING);
begin
  Result := CreateFile(PChar(AFilename), AccessFlags[AAccess],
    SharingFlags[ASharing], nil, Disps[ACreateDisposition],
    FILE_ATTRIBUTE_NORMAL, 0);
end;

function TFile.GetPosition: Integer64;
begin
  Result.Hi := 0;
  Result.Lo := SetFilePointer(FHandle, 0, @Result.Hi, FILE_CURRENT);
  if (Result.Lo = $FFFFFFFF) and (GetLastError <> 0) then
    RaiseLastError;
end;

function TFile.GetSize: Integer64;
begin
  Result.Lo := GetFileSize(FHandle, @Result.Hi);
  if (Result.Lo = $FFFFFFFF) and (GetLastError <> 0) then
    RaiseLastError;
end;

function TFile.Read(var Buffer; Count: Cardinal): Cardinal;
begin
  if not ReadFile(FHandle, Buffer, Count, DWORD(Result), nil) then
    if FHandleCreated or (GetLastError <> ERROR_BROKEN_PIPE) then
      RaiseLastError;
end;

procedure TFile.Seek64(Offset: Integer64);
begin
  if (SetFilePointer(FHandle, Integer(Offset.Lo), @Offset.Hi,
      FILE_BEGIN) = $FFFFFFFF) and (GetLastError <> 0) then
    RaiseLastError;
end;

procedure TFile.SeekToEnd;
var
  DistanceHigh: Integer;
begin
  DistanceHigh := 0;
  if (SetFilePointer(FHandle, 0, @DistanceHigh, FILE_END) = $FFFFFFFF) and
     (GetLastError <> 0) then
    RaiseLastError;
end;

procedure TFile.WriteBuffer(const Buffer; Count: Cardinal);
var
  BytesWritten: DWORD;
begin
  if not WriteFile(FHandle, Buffer, Count, BytesWritten, nil) then
    RaiseLastError;
  if BytesWritten <> Count then begin
    { I'm not aware of any case where WriteFile will return True but a short
      BytesWritten count. (An out-of-disk-space condition causes False to be
      returned.) But if that does happen, raise a generic-sounding localized
      "The system cannot write to the specified device" error. }
    RaiseError(ERROR_WRITE_FAULT);
  end;
end;

end.
