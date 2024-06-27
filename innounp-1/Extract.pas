unit Extract;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TFileExtractor class

  $jrsoftware: issrc/Projects/Extract.pas,v 1.12 2004/03/16 20:16:28 jr Exp $
        manually updatable
}

interface

uses
  Winapi.Windows, System.SysUtils, FileClass, Compress, Struct, ArcFour;

type
  TExtractorProgressProc = procedure(Bytes: Cardinal);
  TExtractorNotificationProc = procedure(MessageText: String);

  TFileExtractor = class
  private
    FDecompressor: array[Boolean] of TCustomDecompressor;
    FChunkFirstSlice, FChunkLastSlice: Integer;
    FChunkStartOffset: Longint;
    FChunkBytesLeft, FChunkDecompressedBytesRead: Int64;
    FNeedReset: Boolean;
    FChunkCompressed, FChunkEncrypted: Boolean;
    FCryptContext: TArcFourContext;
    FCryptKey: AnsiString;
    procedure DecompressBytes(var Buffer; Count: Cardinal);
    procedure OpenSlice(const ASlice: Integer);
    function ReadProc(var Buf; Count: Longint): Longint;
  protected
    FSourceF: TFile;
    FLastSourceDir: String;
    FOriginSourceDir: String;
    FOpenedSlice: Integer;
    FInteractive: Boolean;
    FOnNotification : TExtractorNotificationProc;
    function FindSliceFilename(const ASlice: Integer): String;
    procedure Notify(MessageText: String);
  public
    constructor Create(ADecompressorClass: TCustomDecompressorClass); virtual;
    destructor Destroy; override;
    procedure DecompressFile(const FL: TSetupFileLocationEntry; const DestF: TFile;
      const ProgressProc: TExtractorProgressProc); virtual;
    procedure SeekTo(const FL: TSetupFileLocationEntry); virtual;
    property CryptKey: AnsiString read FCryptKey write FCryptKey;
    property OnNotification : TExtractorNotificationProc read FOnNotification write FOnNotification;
    property Interactive : Boolean read FInteractive write FInteractive;
  end;

function OpenFileDlg(FileName, StartDir:string):string; // empty on errors

implementation

uses
  Winapi.CommDlg, PathFunc, CmnFunc2, Main, Msgs, MsgIDs, zlib, bzlib, LZMADecomp, CallOptimizer,
  MD5, SHA1, MyTypes {NewDisk};

procedure SourceIsCorrupted;
begin
  raise Exception.Create(SetupMessages[msgSourceIsCorrupted]);
end;

{ TFileExtractor }

constructor TFileExtractor.Create(ADecompressorClass: TCustomDecompressorClass);
begin
  inherited Create;
  FSourceF := nil;
  FOpenedSlice := -1;
  FChunkFirstSlice := -1;
  FLastSourceDir := '';
  FOriginSourceDir := PathExtractDir(SetupLdrOriginalFilename);
  FInteractive := True;
  { Create one 'decompressor' for use with uncompressed chunks, and another
    for use with compressed chunks }
  FDecompressor[False] := TStoredDecompressor.Create(ReadProc);
  FDecompressor[True] := ADecompressorClass.Create(ReadProc);
end;

destructor TFileExtractor.Destroy;
begin
  FSourceF.Free;
  FDecompressor[True].Free;
  FDecompressor[False].Free;
  inherited;
end;

function OpenFileDlg(FileName, StartDir:string):string; // empty on errors
var
  ofn:TOpenFilename;
  filterbuf,outbuf,savedir:array[0..1023] of char;
  p:PChar;
  tmp:bool;
begin
  Result:='';
  FillChar(ofn,sizeof(ofn),0);
  with ofn do begin
    lStructSize:=sizeof(ofn);
    p:=StrCopy(filterbuf,PChar(FileName))+length(FileName)+1;
    p:=StrCopy(p,PChar(FileName))+length(FileName)+1;
    p^:=#0;
    lpstrFilter:=filterbuf;
    StrCopy(outbuf,PChar(FileName));
    lpstrFile:=outbuf;
    nMaxFile:=sizeof(outbuf);
    lpstrInitialDir:=PChar(StartDir);
    lpstrTitle:='Specify location of or insert the disk with the required file';
    Flags := OFN_FILEMUSTEXIST or OFN_PATHMUSTEXIST or OFN_NOCHANGEDIR or OFN_DONTADDTORECENT;
  end;
  savedir[0]:=#0; GetCurrentDirectory(sizeof(savedir),savedir);
  tmp:=GetOpenFileName(ofn);
  if savedir[0]<>#0 then SetCurrentDirectory(savedir);
  if not tmp then exit;
  Result:=outbuf;
end;

function TFileExtractor.FindSliceFilename(const ASlice: Integer): String;
var
  Major: Integer;
  F1, F2: String;
begin
  Major := ASlice div SetupHeader.SlicesPerDisk + 1;
  F1 := GetSliceName(ASlice);
  F2 := Format('..\DISK%d\', [Major]) + F1;

  if FLastSourceDir <> '' then begin
    Result := AddBackslash(FLastSourceDir) + F1;
    if NewFileExists(Result) then Exit;
  end;
  Result := AddBackslash(FOriginSourceDir) + F1;
  if NewFileExists(Result) then Exit;
  if FLastSourceDir <> '' then begin
    Result := ExpandFilename(AddBackslash(FLastSourceDir) + F2);
    if NewFileExists(Result) then Exit;
  end;
  Result := ExpandFilename(AddBackslash(FOriginSourceDir) + F2);
  if NewFileExists(Result) then Exit;
  if not FInteractive then raise Exception.Create('Could not find a necessary file: '+F1)
  else begin
    Result := OpenFileDlg(F1, FOriginSourceDir);
    if Result='' then raise Exception.Create('Could not find a necessary file: '+F1);
    FLastSourceDir := PathExtractDir(Result);
  end;
end;

procedure TFileExtractor.OpenSlice(const ASlice: Integer);
var
  Filename: String;
  TestDiskSliceID: TDiskSliceID;
  DiskSliceHeader: TDiskSliceHeader;
begin
  if FOpenedSlice = ASlice then
    Exit;

  FOpenedSlice := -1;
  FreeAndNil(FSourceF);

  if SetupLdrOffset1 = 0 then
    Filename := FindSliceFilename(ASlice)
  else
    Filename := SetupLdrOriginalFilename;
  Notify('Reading slice ' + Filename);
  FSourceF := TFile.Create(Filename, fdOpenExisting, faRead, fsRead);
  if SetupLdrOffset1 = 0 then begin
    if FSourceF.Read(TestDiskSliceID, SizeOf(TestDiskSliceID)) <> SizeOf(TestDiskSliceID) then
      SourceIsCorrupted;
    if TestDiskSliceID <> DiskSliceID then
      SourceIsCorrupted;
    if FSourceF.Read(DiskSliceHeader, SizeOf(DiskSliceHeader)) <> SizeOf(DiskSliceHeader) then
      SourceIsCorrupted;
    if FSourceF.Size.Lo <> DiskSliceHeader.TotalSize then
      SourceIsCorrupted;
  end;
  FOpenedSlice := ASlice;
end;

procedure TFileExtractor.DecompressBytes(var Buffer; Count: Cardinal);
begin
  try
    FDecompressor[FChunkCompressed].DecompressInto(Buffer, Count);
  except
    { If DecompressInto raises an exception, force a decompressor reset &
      re-seek the next time SeekTo is called by setting FNeedReset to True.
      We don't want to get stuck in an endless loop with the decompressor
      in e.g. a data error state. Also, we have no way of knowing if
      DecompressInto successfully decompressed some of the requested bytes
      before the exception was raised. }
    FNeedReset := True;
    raise;
  end;
  Inc(FChunkDecompressedBytesRead, Count);
end;

procedure TFileExtractor.SeekTo(const FL: TSetupFileLocationEntry);

  procedure InitDecryption;
  var
    Salt: TSetupSalt;
    MD5Context: TMD5Context;
    MD5Hash: TMD5Digest;
    SHA1Context: TSHA1Context;
    SHA1Hash: TSHA1Digest;
  begin
    { Read the salt }
    if FSourceF.Read(Salt, SizeOf(Salt)) <> SizeOf(Salt) then
      SourceIsCorrupted;

    if (FL.HashType = htSHA1) then
    begin
      { Initialize the key, which is the SHA1 hash of the salt plus FCryptKey }
      SHA1Init(SHA1Context);
      SHA1Update(SHA1Context, Salt, SizeOf(Salt));
      SHA1Update(SHA1Context, Pointer(FCryptKey)^, Length(FCryptKey));
      SHA1Hash := SHA1Final(SHA1Context);
      ArcFourInit(FCryptContext, SHA1Hash, SizeOf(SHA1Hash));
    end else
    begin
      { Initialize the key, which is the MD5 hash of the salt plus FCryptKey }
      MD5Init(MD5Context);
      MD5Update(MD5Context, Salt, SizeOf(Salt));
      MD5Update(MD5Context, Pointer(FCryptKey)^, Length(FCryptKey));
      MD5Hash := MD5Final(MD5Context);
      ArcFourInit(FCryptContext, MD5Hash, SizeOf(MD5Hash));
    end;

    { The compiler discards the first 1000 bytes for extra security,
      so we must as well }
    ArcFourDiscard(FCryptContext, 1000);
  end;

  procedure Discard(Count: Int64);
  var
    Buf: array[0..65535] of Byte;
    BufSize: Cardinal;
  begin
    try
      while True do begin
        BufSize := SizeOf(Buf);
        if (Count < BufSize) then
          BufSize := Count;
        if BufSize = 0 then
          Break;
        DecompressBytes(Buf, BufSize);
        Dec(Count, BufSize);
      end;
    except
      on ECompressDataError do
        SourceIsCorrupted;
    end;
  end;

var
  TestCompID: TCompID;
  Diff: Int64;
begin
  if (foChunkEncrypted in FL.Flags) and (FCryptKey = '') then
    InternalError('A file is encrypted but no password has been specified');

  { Is the file in a different chunk than the current one?
    Or, is the file in a part of the current chunk that we've already passed?
    Or, did a previous decompression operation fail, necessitating a reset? }
  if (FChunkFirstSlice <> FL.FirstSlice) or
     (FChunkStartOffset <> FL.StartOffset) or
     (FL.ChunkSuboffset < FChunkDecompressedBytesRead) or
     FNeedReset then
  begin
    FChunkFirstSlice := -1;
    FDecompressor[foChunkCompressed in FL.Flags].Reset;
    FNeedReset := False;

    OpenSlice(FL.FirstSlice);

    FSourceF.Seek(SetupLdrOffset1 + FL.StartOffset);
    if FSourceF.Read(TestCompID, SizeOf(TestCompID)) <> SizeOf(TestCompID) then
      SourceIsCorrupted;
    if Longint(TestCompID) <> Longint(ZLIBID) then
      SourceIsCorrupted;
    if foChunkEncrypted in FL.Flags then
      InitDecryption;

    FChunkFirstSlice := FL.FirstSlice;
    FChunkLastSlice := FL.LastSlice;
    FChunkStartOffset := FL.StartOffset;
    FChunkBytesLeft := FL.ChunkCompressedSize;
    FChunkDecompressedBytesRead := 0;
    FChunkCompressed := foChunkCompressed in FL.Flags;
    FChunkEncrypted := foChunkEncrypted in FL.Flags;
  end;

  { Need to seek forward in the chunk? }
  if (FL.ChunkSuboffset > FChunkDecompressedBytesRead) then
  begin
    Diff := FL.ChunkSuboffset - FChunkDecompressedBytesRead;
    Discard(Diff);
  end;
end;

function TFileExtractor.ReadProc(var Buf; Count: Longint): Longint;
var
  Buffer: Pointer;
  Left, Res: Cardinal;
begin
  Buffer := @Buf;
  Left := Count;
  if (FChunkBytesLeft < Left) then
    Left := FChunkBytesLeft;
  Result := Left;
  while Left <> 0 do begin
    Res := FSourceF.Read(Buffer^, Left);
    Dec(FChunkBytesLeft, Res);

    { Decrypt the data after reading from the file }
    if FChunkEncrypted then
      ArcFourCrypt(FCryptContext, Buffer^, Buffer^, Res);

    if Left = Res then
      Break
    else begin
      Dec(Left, Res);
      Inc(Longint(Buffer), Res);
      { Go to next disk }
      if FOpenedSlice >= FChunkLastSlice then
        { Already on the last slice, so the file must be corrupted... }
        SourceIsCorrupted;
      OpenSlice(FOpenedSlice + 1);
    end;
  end;
end;

procedure TransformCallInstructions(var Buf; Size: Integer;
  const Encode: Boolean; const AddrOffset: LongWord);
{ Transforms addresses in relative CALL or JMP instructions to absolute ones
  if Encode is True, or the inverse if Encode is False.
  This transformation can lead to a higher compression ratio when compressing
  32-bit x86 code. }
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..$7FFFFFFE] of Byte;
var
  P: PByteArray;
  I, X: Integer;
  Addr: LongWord;
begin
  if Size < 5 then
    Exit;
  Dec(Size, 4);
  P := @Buf;
  I := 0;
  while I < Size do begin
    { Does it appear to be a CALL or JMP instruction with a relative 32-bit
      address? }
    if (P[I] = $E8) or (P[I] = $E9) then begin
      Inc(I);
      { Verify that the high byte of the address is $00 or $FF. If it isn't,
        then what we've encountered most likely isn't a CALL or JMP. }
      if (P[I+3] = $00) or (P[I+3] = $FF) then begin
        { Change the lower 3 bytes of the address to be relative to the
          beginning of the buffer, instead of to the next instruction. If
          decoding, do the opposite. }
        Addr := AddrOffset + LongWord(I) + 4;  { may wrap, but OK }
        if not Encode then
          Addr := -Addr;
        for X := 0 to 2 do begin
          Inc(Addr, P[I+X]);
          P[I+X] := Byte(Addr);
          Addr := Addr shr 8;
        end;
      end;
      Inc(I, 4);
    end
    else
      Inc(I);
  end;
end;

procedure TransformCallInstructions5309(var Buf; Size: Integer;
  const Encode: Boolean; const AddrOffset: LongWord);
{ [Version 3] Converts relative addresses in x86/x64 CALL and JMP instructions
  to absolute addresses if Encode is True, or the inverse if Encode is False. }
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..$7FFFFFFE] of Byte;
var
  P: PByteArray;
  I: Integer;
  Addr, Rel: LongWord;
begin
  if Size < 5 then
    Exit;
  Dec(Size, 4);
  P := @Buf;
  I := 0;
  while I < Size do begin
    { Does it appear to be a CALL or JMP instruction with a relative 32-bit
      address? }
    if (P[I] = $E8) or (P[I] = $E9) then begin
      Inc(I);
      { Verify that the high byte of the address is $00 or $FF. If it isn't,
        then what we've encountered most likely isn't a CALL or JMP. }
      if (P[I+3] = $00) or (P[I+3] = $FF) then begin
        { Change the lower 3 bytes of the address to be relative to the
          beginning of the buffer, instead of to the next instruction. If
          decoding, do the opposite. }
        Addr := (AddrOffset + LongWord(I) + 4) and $FFFFFF;  { may wrap, but OK }
        Rel := P[I] or (P[I+1] shl 8) or (P[I+2] shl 16);
        if not Encode then
          Dec(Rel, Addr);
        { For a slightly higher compression ratio, we want the resulting high
          byte to be $00 for both forward and backward jumps. The high byte
          of the original relative address is likely to be the sign extension
          of bit 23, so if bit 23 is set, toggle all bits in the high byte. }
        if Rel and $800000 <> 0 then
          P[I+3] := not P[I+3];
        if Encode then
          Inc(Rel, Addr);
        P[I] := Byte(Rel);
        P[I+1] := Byte(Rel shr 8);
        P[I+2] := Byte(Rel shr 16);
      end;
      Inc(I, 4);
    end
    else
      Inc(I);
  end;
end;

procedure TFileExtractor.DecompressFile(const FL: TSetupFileLocationEntry;
  const DestF: TFile; const ProgressProc: TExtractorProgressProc);
var
  BytesLeft: Int64;
  MD5Context: TMD5Context;
  SHA1Context: TSHA1Context;
  CRC: Longint;
  CallDecoder: TCallInstructionOptimizer;
  BufSize: Cardinal;
  Buf: array[0..65535] of Byte;
  AddrOffset: LongWord;
  IsHashMatch: boolean;
begin
  BytesLeft := FL.OriginalSize;

  CRC := Longint($FFFFFFFF);
  MD5Init(MD5Context);
  SHA1Init(SHA1Context);

  if foCallInstructionOptimized in FL.Flags then
    CallDecoder := TCallInstructionOptimizer.Create(False)
  else
    CallDecoder := nil;
  try
    try
      AddrOffset:=0;
      while True do begin
        BufSize := SizeOf(Buf);
        if (BytesLeft < BufSize) then
          BufSize := BytesLeft;
        if BufSize = 0 then
          Break;

        DecompressBytes(Buf, BufSize);
        if Assigned(CallDecoder) then
          if Ver>=5200 then begin
            if (Ver >= 5309) then
              TransformCallInstructions5309(Buf, BufSize, false, AddrOffset)
            else
              TransformCallInstructions(Buf, BufSize, false, AddrOffset);
            Inc(AddrOffset, BufSize);  { may wrap, but OK }
          end else CallDecoder.Code(Buf, BufSize);

        case FL.HashType of
          htCRC32:  CRC := UpdateCRC32(CRC, Buf, BufSize);
          htMD5:    MD5Update(MD5Context, Buf, BufSize);
          htSHA1:   SHA1Update(SHA1Context, Buf, BufSize);
        end;

        DestF.WriteBuffer(Buf, BufSize);
        Dec(BytesLeft, BufSize);

        if Assigned(ProgressProc) then
          ProgressProc(BufSize);
      end;
    except
      on ECompressDataError do
        SourceIsCorrupted;
    end;
  finally
    CallDecoder.Free;
  end;

  CRC := CRC xor Longint($FFFFFFFF);

  IsHashMatch := false;
  case FL.HashType of
    htCRC32:  IsHashMatch := (CRC = FL.CRC);
    htMD5:    IsHashMatch := MD5DigestsEqual(MD5Final(MD5Context), FL.MD5Sum);
    htSHA1:   IsHashMatch := SHA1DigestsEqual(SHA1Final(SHA1Context), FL.SHA1Sum);
  end;

  if (not IsHashMatch) then SourceIsCorrupted();
end;

procedure TFileExtractor.Notify(MessageText: String);
begin
  if Assigned(FOnNotification) then FOnNotification(MessageText);
end;

end.
