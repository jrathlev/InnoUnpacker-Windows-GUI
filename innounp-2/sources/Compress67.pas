unit Compress67;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Abstract compression classes, and some generic compression-related functions

  $jrsoftware: issrc/Projects/Compress.pas,v 1.6 2004/02/28 02:16:16 jr Exp $

  modified for innounp, J. Rathlev
}

interface

uses
  System.SysUtils, Int64Em, FileClass, Compress;

type
  TCompressedBlockReader = class(TAbstractBlockReader)
  private
    FDecompressor: TCustomDecompressor;
    FFile: TFile;
    FInBytesLeft: int64;
    FInitialized: Boolean;
    FInBufferNext: Cardinal;
    FInBufferAvail: Cardinal;
    FInBuffer: array[0..4095] of Byte;
    function DecompressorReadProc(var Buffer; Count: Longint): Longint;
    procedure ReadChunk;
  public
    constructor Create(AFile: TFile; ADecompressorClass: TCustomDecompressorClass); override;
    destructor Destroy; override;
    procedure Read(var Buffer; Count: Cardinal); override;
  end;

implementation

type
  TCompressedBlockHeader = packed record
    StoredSize: int64;   { Total bytes written, including the CRCs }
    Compressed: Boolean;    { True if data is compressed, False if not }
  end;

{ TCompressedBlockReader }

constructor TCompressedBlockReader.Create(AFile: TFile;
  ADecompressorClass: TCustomDecompressorClass);
var
  HdrCRC: Integer;
  Hdr: TCompressedBlockHeader;
  P: Integer64;
begin
  inherited;

  FFile := AFile;

  if (AFile.Read(HdrCRC, SizeOf(HdrCRC)) <> SizeOf(HdrCRC)) or
     (AFile.Read(Hdr, SizeOf(Hdr)) <> SizeOf(Hdr)) then
    raise ECompressDataError.Create(SCompressedBlockDataError);
  if HdrCRC <> GetCRC32(Hdr, SizeOf(Hdr)) then
    raise ECompressDataError.Create(SCompressedBlockDataError);
  P := AFile.Position;
  Inc6464(P, Hdr.StoredSize);
  if (Hdr.StoredSize < 0) or (Compare64(P, AFile.Size) > 0) then
//  if (Hdr.StoredSize < 0) or (AFile.Position > AFile.Size - Hdr.StoredSize) then
    raise ECompressDataError.Create(SCompressedBlockDataError);
  if Hdr.Compressed then
    FDecompressor := ADecompressorClass.Create(DecompressorReadProc);
  FInBytesLeft := Hdr.StoredSize;
  FInitialized := True;
end;

destructor TCompressedBlockReader.Destroy;
var
  P: Integer64;
begin
  FDecompressor.Free;
  if FInitialized then begin
    { Must seek ahead if the caller didn't read everything that was originally
      compressed, or if it did read everything but zlib is in a "CHECK" state
      (i.e. it didn't read and verify the trailing adler32 yet due to lack of
      input bytes). }
    P := FFile.Position;
    Inc64(P, FInBytesLeft);
    FFile.Seek64(P);
  end;
  inherited;
end;

procedure TCompressedBlockReader.ReadChunk;
var
  CRC: Integer;
  Len: Cardinal;
begin
  { Read chunk CRC }
  if FInBytesLeft < SizeOf(CRC) + 1 then
    raise ECompressDataError.Create(SCompressedBlockDataError);
  FFile.ReadBuffer(CRC, SizeOf(CRC));
  Dec(FInBytesLeft, SizeOf(CRC));

  { Read chunk data }
  if FInBytesLeft > SizeOf(FInBuffer) then
    Len := SizeOf(FInBuffer)
  else
    Len := Cardinal(FInBytesLeft);
  FFile.ReadBuffer(FInBuffer, Len);
  Dec(FInBytesLeft, Len);
  FInBufferNext := 0;
  FInBufferAvail := Len;
  if CRC <> GetCRC32(FInBuffer, Len) then
    raise ECompressDataError.Create(SCompressedBlockDataError);
end;

function TCompressedBlockReader.DecompressorReadProc(var Buffer;
  Count: Longint): Longint;
var
  P: ^Byte;
  Bytes: Cardinal;
begin
  Result := 0;
  P := @Buffer;
  while Count > 0 do begin
    if FInBufferAvail = 0 then begin
      if FInBytesLeft = 0 then
        Break;
      ReadChunk;
    end;
    Bytes := Count;
    if Bytes > FInBufferAvail then
      Bytes := FInBufferAvail;
    Move(FInBuffer[FInBufferNext], P^, Bytes);
    Inc(FInBufferNext, Bytes);
    Dec(FInBufferAvail, Bytes);
    Inc(P, Bytes);
    Dec(Count, Bytes);
    Inc(Result, Bytes);
  end;
end;

procedure TCompressedBlockReader.Read(var Buffer; Count: Cardinal);
begin
  if Assigned(FDecompressor) then
    FDecompressor.DecompressInto(Buffer, Count)
  else begin
    { Not compressed -- call DecompressorReadProc directly }
    if Cardinal(DecompressorReadProc(Buffer, Count)) <> Count then
      raise ECompressDataError.Create(SCompressedBlockDataError);
  end;
end;

end.
