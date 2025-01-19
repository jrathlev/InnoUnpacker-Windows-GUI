unit Extract6400;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TFileExtractor class to extract (=decrypt, decompress, and/or verify) Setup files

  modified for "innounp" and compilation with Delphi 10 by J. Rathlev, January 2025
}

interface

uses
  Windows, SysUtils, Int64Em, FileClass, Compress, Struct, ChaCha20, Extract, MyTypes;

type
  TFileExtractor6400 = class (TFileExtractor)
  private
    FCryptContext: TChaCha20Context;
    FCryptKey: TSetupEncryptionKey;
    FCryptKeySet: Boolean;
    FEntered: Integer;
  protected
    function FindSliceFilename(const ASlice: Integer): String;
    function ReadProc(var Buf; Count: Longint): Longint; override;
    procedure SetCryptKey(const Value: TSetupEncryptionKey);
  public
    constructor Create(ADecompressorClass: TCustomDecompressorClass);
    procedure DecompressFile(const FL: TSetupFileLocationEntry; const DestF: TFile;
      const ProgressProc: TExtractorProgressProc; const VerifyChecksum: Boolean); override;
    procedure SeekTo(const FL: TSetupFileLocationEntry); override;
    property Is64CryptKey: TSetupEncryptionKey write SetCryptKey;
  end;

implementation

uses
  PathFunc, CmnFunc2, Main, Msgs, MsgIDs, Zlib, bzlib,
  Compression.LZMADecompressor, SHA256;

{ TFileExtractor }

constructor TFileExtractor6400.Create(ADecompressorClass: TCustomDecompressorClass);
begin
  inherited Create (ADecompressorClass);
end;

procedure TFileExtractor6400.SetCryptKey(const Value: TSetupEncryptionKey);
begin
  FCryptKey := Value;
  FCryptKeySet := True;
end;

function TFileExtractor6400.FindSliceFilename(const ASlice: Integer): String;
var
  Major, Minor: Integer;
  Prefix, F1: String;
begin
  Prefix := PathChangeExt(PathExtractName(SetupLdrOriginalFilename), '');
  Major := ASlice div SetupHeader.SlicesPerDisk + 1;
  Minor := ASlice mod SetupHeader.SlicesPerDisk;
  if SetupHeader.SlicesPerDisk = 1 then
    F1 := Format('%s-%d.bin', [Prefix, Major])
  else
    F1 := Format('%s-%d%s.bin', [Prefix, Major, Chr(Ord('a') + Minor)]);
  if FLastSourceDir <> '' then begin
    Result := AddBackslash(FLastSourceDir) + F1;
    if NewFileExists(Result) then Exit;
  end;
  Result := AddBackslash(FOriginSourceDir) + F1;
  if NewFileExists(Result) then Exit;
  if not FInteractive then raise Exception.Create('Could not find a necessary file: '+F1)
  else begin
    Result := OpenFileDlg(F1, FOriginSourceDir);
    if Result='' then raise Exception.Create('Could not find a necessary file: '+F1);
    FLastSourceDir := PathExtractDir(Result);
    end;
//  Path := FOriginSourceDir;
//  if SelectDisk(Major, F1, Path) then begin
//    LastSourceDir := Path;
//    Result := AddBackslash(Path) + F1;
//  end
//  else
//    Abort;
end;

procedure TFileExtractor6400.SeekTo(const FL: TSetupFileLocationEntry);

  procedure InitDecryption;
  var
    Nonce : TSetupEncryptionNonce;
  begin
    { Recreate the unique nonce from the base nonce }
    Nonce := SetupHeader.Is64Encryption.EncryptionBaseNonce;
    Nonce.RandomXorStartOffset := Nonce.RandomXorStartOffset xor FChunkStartOffset;
    Nonce.RandomXorFirstSlice := Nonce.RandomXorFirstSlice xor FChunkFirstSlice;

    XChaCha20Init(FCryptContext, FCryptKey[0], Length(FCryptKey), Nonce, SizeOf(Nonce), 0);
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
//        if (Count.Hi = 0) and (Count.Lo < BufSize) then
          BufSize := Count;
        if BufSize = 0 then
          Break;
        DecompressBytes(Buf, BufSize);
        Dec(Count, BufSize);
//        if Assigned(ProgressProc) then
//          ProgressProc(0);
      end;
    except
      on E: ECompressDataError do
        SourceIsCorrupted(E.Message);
    end;
  end;

var
  TestCompID: TCompID;
  Diff: Int64;
begin
  if FEntered <> 0 then
    InternalError('Cannot call file extractor recursively');
  Inc(FEntered);
  try
    if (foChunkEncrypted in FL.Flags) and not FCryptKeySet then
      InternalError('Cannot read an encrypted file before the key has been set');

    { Is the file in a different chunk than the current one?
      Or, is the file in a part of the current chunk that we've already passed?
      Or, did a previous decompression operation fail, necessitating a reset? }
    if (FChunkFirstSlice <> FL.FirstSlice) or
       (FChunkStartOffset <> FL.StartOffset) or
       (FL.ChunkSuboffset < FChunkDecompressedBytesRead) or
//       (FL.ChunkSuboffset, FChunkDecompressedBytesRead) < 0) or
       FNeedReset then begin
      FChunkFirstSlice := -1;
      FDecompressor[foChunkCompressed in FL.Flags].Reset;
      FNeedReset := False;

      OpenSlice(FL.FirstSlice);

      FSourceF.Seek(SetupLdrOffset1 + FL.StartOffset);
      if FSourceF.Read(TestCompID, SizeOf(TestCompID)) <> SizeOf(TestCompID) then
        SourceIsCorrupted('Failed to read CompID');
      if Longint(TestCompID) <> Longint(ZLIBID) then
        SourceIsCorrupted('Invalid CompID');

      FChunkFirstSlice := FL.FirstSlice;
      FChunkLastSlice := FL.LastSlice;
      FChunkStartOffset := FL.StartOffset;
      FChunkBytesLeft := FL.ChunkCompressedSize;
      FChunkDecompressedBytesRead := 0;
//      FChunkDecompressedBytesRead.Hi := 0;
//      FChunkDecompressedBytesRead.Lo := 0;
      FChunkCompressed := foChunkCompressed in FL.Flags;
      FChunkEncrypted := foChunkEncrypted in FL.Flags;

      if foChunkEncrypted in FL.Flags then
        InitDecryption;
    end;

    { Need to seek forward in the chunk? }
    if (FL.ChunkSuboffset > FChunkDecompressedBytesRead) then begin
//    if Compare64(FL.ChunkSuboffset, FChunkDecompressedBytesRead) > 0 then begin
      Diff := FL.ChunkSuboffset - FChunkDecompressedBytesRead;
//      Diff := FL.ChunkSuboffset;
//      Dec6464(Diff, FChunkDecompressedBytesRead);
      Discard(Diff);
    end;
  finally
    Dec(FEntered);
  end;
end;

function TFileExtractor6400.ReadProc(var Buf; Count: Longint): Longint;
var
  Buffer: Pointer;
  Left, Res: Cardinal;
begin
  Buffer := @Buf;
  Left := Count;
  if (FChunkBytesLeft < Left) then
//  if (FChunkBytesLeft.Hi = 0) and (FChunkBytesLeft.Lo < Left) then
    Left := FChunkBytesLeft;
  Result := Left;
  while Left <> 0 do begin
    Res := FSourceF.Read(Buffer^, Left);
    Dec(FChunkBytesLeft, Res);

    { Decrypt the data after reading from the file }
    if FChunkEncrypted then
      XChaCha20Crypt(FCryptContext, Buffer^, Buffer^, Res);

    if Left = Res then
      Break
    else begin
      Dec(Left, Res);
      Inc(Longint(Buffer), Res);
      { Go to next disk }
      if FOpenedSlice >= FChunkLastSlice then
        { Already on the last slice, so the file must be corrupted... }
        SourceIsCorrupted('Already on last slice');
      OpenSlice(FOpenedSlice + 1);
    end;
  end;
end;

procedure TransformCallInstructions(var Buf; Size: Integer;
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

procedure TFileExtractor6400.DecompressFile(const FL: TSetupFileLocationEntry;
  const DestF: TFile; const ProgressProc: TExtractorProgressProc;
  const VerifyChecksum: Boolean);
var
  BytesLeft: Integer64;
  Context: TSHA256Context;
  AddrOffset: LongWord;
  BufSize: Cardinal;
  Buf: array[0..65535] of Byte;
  { ^ *must* be the same buffer size used by the compiler (TCompressionHandler),
    otherwise the TransformCallInstructions call will break }
begin
  if FEntered <> 0 then
    InternalError('Cannot call file extractor recursively');
  Inc(FEntered);
  try
    BytesLeft := Integer64(FL.OriginalSize);

    { To avoid file system fragmentation, preallocate all of the bytes in the
      destination file }
    DestF.Seek64(BytesLeft);
    DestF.Truncate;
    DestF.Seek(0);

    SHA256Init(Context);

    try
      AddrOffset := 0;
      while True do begin
        BufSize := SizeOf(Buf);
        if (BytesLeft.Hi = 0) and (BytesLeft.Lo < BufSize) then
          BufSize := BytesLeft.Lo;
        if BufSize = 0 then
          Break;

        DecompressBytes(Buf, BufSize);
        if foCallInstructionOptimized in FL.Flags then begin
          TransformCallInstructions(Buf, BufSize, False, AddrOffset);
          Inc(AddrOffset, BufSize);  { may wrap, but OK }
        end;
        Dec64(BytesLeft, BufSize);
        SHA256Update(Context, Buf, BufSize);
        DestF.WriteBuffer(Buf, BufSize);

        if Assigned(ProgressProc) then
          ProgressProc(BufSize);
      end;
    except
      on E: ECompressDataError do
        SourceIsCorrupted(E.Message);
    end;

    if VerifyChecksum and not SHA256DigestsEqual(SHA256Final(Context), FL.SHA256Sum) then
      SourceIsCorrupted('SHA-256 hash mismatch');
  finally
    Dec(FEntered);
  end;
end;

end.
