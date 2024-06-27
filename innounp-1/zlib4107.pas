unit zlib4107;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Declarations for zlib, plus various 'helper' functions

  $jrsoftware: issrc/Projects/zlib.pas,v 1.13 2004/02/17 09:29:49 jr Exp $
}

interface

uses
  SysUtils, FileClass;

const
  { Some common compression levels. You may use one of these constants for
    the CompressionLevel parameters of the functions, or any value between 0
    and 9 (0 being no compression, 9 being maximum compression) }
  Z_NO_COMPRESSION      =  0;
  Z_BEST_SPEED          =  1;
  Z_BEST_COMPRESSION    =  9;
  Z_DEFAULT_COMPRESSION = -1;
type
  TCompressionLevel = Z_DEFAULT_COMPRESSION..9;

{ *** High-level compression functions *** }

type
  TZAlloc = function(AppData: Pointer; Items, Size: Cardinal): Pointer;
  TZFree = procedure(AppData, Block: Pointer);

  TZStreamRec = packed record
    next_in: PChar;       { next input byte }
    avail_in: Cardinal;   { number of bytes available at next_in }
    total_in: Longint;    { total nb of input bytes read so far }

    next_out: PChar;      { next output byte should be put here }
    avail_out: Cardinal;  { remaining free space at next_out }
    total_out: Longint;   { total nb of bytes output so far }

    msg: PChar;           { last error message, NULL if no error }
    internal: Pointer;    { not visible by applications }

    zalloc: TZAlloc;      { used to allocate the internal state }
    zfree: TZFree;        { used to free the internal state }
    AppData: Pointer;     { private data object passed to zalloc and zfree }

    data_type: Integer;   { best guess about the data type: ascii or binary }
    adler: Longint;       { adler32 value of the uncompressed data }
    reserved: Longint;    { reserved for future use }
  end;

  TNewBlockDeflateHeader = packed record
    StoredSize: Longint;    { Total bytes written, including the CRCs }
    Compressed: Boolean;    { True if data is zlib-compressed, False if not }
  end;
{  PDeflateBlockWriteData = ^TDeflateBlockWriteData;
  TDeflateBlockWriteData = record
    F: ^File;
    StartPos: Longint;
    InBuffer, OutBuffer: array[0..4095] of Byte;
    InBufferCount: Cardinal;
    OutBufferCount: Integer;
    strm: TZStreamRec;
    Compressed: Boolean;
    TotalBytesStored: Cardinal;
  end;}

  PDeflateBlockReadData = ^TDeflateBlockReadData;
  TDeflateBlockReadData = record
    F: TFile;
    StartPos, InBytesLeft: Longint;
    InBuffer, OutBuffer: array[0..4095] of Byte;
    InBufferCount, OutBufferStart, OutBufferCount: Integer;
    strm: TZStreamRec;
    Compressed, NoMoreData: Boolean;
  end;

{  TZCompressor = class(TCustomCompressor)
  private
    FInitialized: Boolean;
    FStrm: TZStreamRec;
    FBuffer: array[0..65535] of Byte;
    procedure FlushBuffer;
  public
    constructor Create(AWriteProc: TCompressorWriteProc;
      AProgressProc: TCompressorProgressProc; CompressionLevel: Integer); override;
    destructor Destroy; override;
    procedure Compress(const Buffer; Count: Longint); override;
    procedure Finish; override;
  end;

  TZDecompressor = class(TCustomDecompressor)
  private
    FInitialized: Boolean;
    FStrm: TZStreamRec;
    FReachedEnd: Boolean;
    FBuffer: array[0..65535] of Byte;
  public
    constructor Create(AReadProc: TDecompressorReadProc); override;
    destructor Destroy; override;
    procedure DecompressInto(var Buffer; Count: Longint); override;
    procedure Reset; override;
  end;

procedure DeflateBlockWriteBegin(var F: File; const CompressionLevel: TCompressionLevel;
  var Data: TDeflateBlockWriteData);
procedure DeflateBlockWrite(var Data: TDeflateBlockWriteData; var Buf;
  Count: Cardinal);
procedure DeflateBlockWriteEnd(var Data: TDeflateBlockWriteData);
procedure DeflateBlockWriteCancel(var Data: TDeflateBlockWriteData);
}
procedure InflateBlockReadBegin(F: TFile; var Data: TDeflateBlockReadData);
procedure InflateBlockRead(var Data: TDeflateBlockReadData; var Buf;
  const Count: Cardinal);
procedure InflateBlockReadEnd(var Data: TDeflateBlockReadData);

(*
function DeflateData(var ASource; const ASourceSize: Longint; const ASourceIsFile: Boolean;
  var ADest; const ADestSize: Longint; const ADestIsFile: Boolean;
  const CompressionLevel: TCompressionLevel; var BytesWritten, Adler: Longint): Boolean;
{ Compresses ASource and stores the compressed data in ADest. The input or
  output data can either be stored in memory or a disk file.

  ASource is the input data. ASourceIsFile determines whether ASource is a
  block of memory or a File variable. ASourceSize specifies the maximum number
  of bytes to read from ASource.

  ADest is the output data. ADestIsFile determines whether ADest is a block
  of memory or a File variable. ADestSize specifies the maximum number of bytes
  to write to ADest. If ADestSize is -1, it sets no limit on the maximum number
  of bytes to write. It's not recommended that use -1 when ADestIsFile is
  False, since this could lead to problems if the compressed data turns out to
  be larger than what you allocated for your buffer.

  This function returns True if successful, or False if the destination buffer
  was too small to hold all of the data. Exceptions are raised for other
  errors. Upon return, BytesWritten specifies the total number of bytes
  written to ADest, and Adler specifies the Adler-32 checksum of the input
  data. But, these two variables are only accurate if the function returns
  True.
}

function InflateData(var ASource; const ASourceSize: Longint; const ASourceIsFile: Boolean;
  var ADest; const ADestSize: Longint; const ADestIsFile: Boolean;
  var Adler: Longint): Boolean;
{ Decompresses ASource and stores the uncompressed data in ADest. The input or
  output data can either be stored in memory or a disk file.

  ASource is the input data. ASourceIsFile determines whether ASource is a
  block of memory or a File variable. ASourceSize specifies the maximum number
  of bytes to read from ASource.

  ADest is the output data. ADestIsFile determines whether ADest is a block
  of memory or a File variable. ADestSize specifies the maximum number of bytes
  to write to ADest. Unlike DeflateData, -1 can NOT be specified for the
  ADestSize parameter.

  This function returns True if successful, or False if the destination buffer
  was too small to hold all of the data. Exceptions are raised for other
  errors. Upon return, BytesWritten specifies the total number of bytes
  written to ADest, and Adler specifies the Adler-32 checksum of the output
  data. But, these two variables are only accurate if the function returns
  True.
}


{ *** Low-level compression functions *** }

type
  TZReadProc = function(var Buf; MaxBytes: Cardinal; ExtraData: Longint): Cardinal;
  { A custom callback function used by CustomDeflateData and CustomInflateData.
    This is called whenever data needs to be read. The data read should be
    stored in Buf, up to a maximum of MaxBytes. ExtraData is the same as the
    ExtraData originally passed to CustomDeflateData or CustomInflateData.
    This function must return the number of bytes read, up to a maximum of
    MaxBytes. CustomDeflateData or CustomInflateData will continue calling this
    function until it returns zero.
    NOTE: This function must be declared as 'far' when compiling under Delphi 1 }
  TZWriteProc = function(var Buf; BufSize: Cardinal; ExtraData: Longint): Cardinal;
  { A custom callback function used by CustomDeflateData and CustomInflateData.
    This is called whenever data needs to be written. Buf specifies the data,
    BufSize specifies the size of Buf, and ExtraData is the same as the
    ExtraData originally passed to CustomDeflateData or CustomInflateData.
    This function must return the number of bytes written, normally equal to
    BufSize. If this function returns a value less than BufSize, CustomDeflateData
    or CustomInflateData aborts and returns False.
    NOTE: This function must be declared as 'far' when compiling under Delphi 1 }

function CustomDeflateData(const ReadProc: TZReadProc; const WriteProc: TZWriteProc;
  const ExtraData: Longint; const CompressionLevel: TCompressionLevel;
  var BytesWritten, Adler: Longint): Boolean;
{ Just like DeflateData, but reads and writes data by use of callback
  functions specified by the ReadProc and WriteProc parameters. ExtraData is
  application-defined, and not used by this unit in any way, except to pass
  it ReadProc or WriteProc.

  This function returns True if successful, or False if a call to WriteProc
  returned a value less than the BufSize parameter passed to WriteProc.
  Exceptions are raised for other errors. }

function CustomInflateData(const ReadProc: TZReadProc; const WriteProc: TZWriteProc;
  const ExtraData: Longint; var Adler: Longint): Boolean;
{ Just like InflateData, but reads and writes data by use of callback
  functions specified by the ReadProc and WriteProc parameters. ExtraData is
  application-defined, and not used by this unit in any way, except to pass
  it ReadProc or WriteProc.

  This function returns True if successful, or False if a call to WriteProc
  returned a value less than the BufSize parameter passed to WriteProc.
  Exceptions are raised for other errors. }

function CalcAdler32(CurAdler: Longint; const Buf; const BufSize: Cardinal): Longint;
*)

implementation

uses Compress, zlib;

const
  SZlibDataError = 'zlib: Compressed data is corrupted';
  SZlibInternalError = 'zlib: Internal error. Code %d';

  zlib_Version = '1.1.3';  { Do not change this! }

  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;

  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = -1;
  Z_STREAM_ERROR  = -2;
  Z_DATA_ERROR    = -3;
  Z_MEM_ERROR     = -4;
  Z_BUF_ERROR     = -5;
  Z_VERSION_ERROR = -6;

  Z_FILTERED         = 1;
  Z_HUFFMAN_ONLY     = 2;
  Z_DEFAULT_STRATEGY = 0;

  Z_BINARY  = 0;
  Z_ASCII   = 1;
  Z_UNKNOWN = 2;

  Z_DEFLATED = 8;

(*
{$IFDEF WIN32}
{$L zlib32\deflate.obj}
{$L zlib32\inflate.obj}
{$L zlib32\inftrees.obj}
{$L zlib32\trees.obj}
{$L zlib32\adler32.obj}
{$L zlib32\infblock.obj}
{$L zlib32\infcodes.obj}
{$L zlib32\infutil.obj}
{$L zlib32\inffast.obj}
{$ELSE}
{$L zlib16\deflate.obj}
{$L zlib16\inflate.obj}
{$L zlib16\inftrees.obj}
{$L zlib16\trees.obj}
{$L zlib16\adler32.obj}
{$L zlib16\infblock.obj}
{$L zlib16\infcodes.obj}
{$L zlib16\infutil.obj}
{$L zlib16\inffast.obj}
{$L zlib16\h_scopy.obj}
{$L zlib16\h_llsh.obj}
{$L zlib16\h_lursh.obj}
{$L zlib16\f_lxmul.obj}
{$L zlib16\h_spush.obj}
{$L zlib16\h_ldiv.obj}
{$ENDIF}

{$IFDEF WIN32}
procedure _tr_init; external;
procedure _tr_flush_block; external;
procedure _tr_align; external;
procedure _tr_stored_block; external;
{$ENDIF}
function adler32(adler: Longint; const buf; len: Cardinal): Longint; far; external;
procedure inflate_blocks_new; external;
procedure inflate_blocks; external;
procedure inflate_blocks_reset; external;
procedure inflate_blocks_free; external;
procedure inflate_set_dictionary; external;
procedure inflate_trees_bits; external;
procedure inflate_trees_dynamic; external;
procedure inflate_trees_fixed; external;
procedure inflate_codes_new; external;
procedure inflate_codes; external;
procedure inflate_codes_free; external;
procedure inflate_flush; external;
procedure inflate_fast; external;

procedure _memset(P: Pointer; B: Byte; Count: Cardinal); cdecl; far;
begin
  FillChar(P^, Count, B);
end;

procedure _memcpy(Dest, Source: Pointer; Count: Cardinal); cdecl; far;
begin
  Move(Source^, Dest^, Count);
end;

function deflateInit_ (var strm: TZStreamRec; level: Integer; version: PChar;
  recsize: Integer): Integer; far; external;
function deflate(var strm: TZStreamRec; flush: Integer): Integer; far; external;
function deflateEnd(var strm: TZStreamRec): Integer; far; external;
function inflateInit_ (var strm: TZStreamRec; version: PChar;
  recsize: Integer): Integer; far; external;
function inflate(var strm: TZStreamRec; flush: Integer): Integer; far; external;
function inflateEnd(var strm: TZStreamRec): Integer; far; external;
function inflateReset(var strm: TZStreamRec): Integer; far; external;
*)

function zlibAllocMem(AppData: Pointer; Items, Size: Cardinal): Pointer; far;
begin
  {$IFDEF WIN32}
  try
    GetMem(Result, Items * Size);
  except
    { trap any exception, because zlib expects a NULL result if it's out
      of memory }
    Result := nil;
  end;
  {$ELSE}
  { GlobalAlloc is used instead of GetMem or LocalAlloc since it allows
    allocating objects >65535 bytes. zlib requires an allocator that is able
    to allocate up to 65536 bytes.
    Note: The Longint casts below are to ensure the multiply operation is 32
    bits. (If they aren't there, it will clip to 16 bits.) }
  Result := GlobalAllocPtr(GMEM_MOVEABLE, Longint(Items) * Longint(Size));
  {$ENDIF}
end;

procedure zlibFreeMem(AppData, Block: Pointer); far;
begin
  {$IFDEF WIN32}
  FreeMem(Block);
  {$ELSE}
  if Assigned(Block) then
    GlobalFreePtr(Block);
  {$ENDIF}
end;

function MemCheck(const Code: Integer): Integer;
begin
  if Code = Z_MEM_ERROR then
    OutOfMemoryError;
  Result := Code;
end;

function Check(const Code: Integer; const ValidCodes: array of Integer): Integer;
var
  I: Integer;
begin
  if Code = Z_MEM_ERROR then
    OutOfMemoryError;
  Result := Code;
  for I := Low(ValidCodes) to High(ValidCodes) do
    if ValidCodes[I] = Code then
      Exit;
  raise ECompressInternalError.CreateFmt(SZlibInternalError, [Code]);
end;

const
  BufferSize = {$IFDEF WIN32} 65536 {$ELSE} 32000 {$ENDIF};

procedure InitStream(var strm: TZStreamRec);
begin
  FillChar(strm, SizeOf(strm), 0);
  with strm do begin
    zalloc := zlibAllocMem;
    zfree := zlibFreeMem;
  end;
end;

(*
const
  Actions: array[Boolean] of Integer = (Z_NO_FLUSH, Z_FINISH);

function CustomDeflateData(const ReadProc: TZReadProc; const WriteProc: TZWriteProc;
  const ExtraData: Longint; const CompressionLevel: TCompressionLevel;
  var BytesWritten, Adler: Longint): Boolean;
var
  strm: TZStreamRec;
  I, O: Pointer;
  Finished: Boolean;
  Res: Integer;
  Written: Longint;
  Successful: Boolean;
begin
  Result := False;
  I := nil;
  O := nil;
  try
    GetMem(I, BufferSize);
    GetMem(O, BufferSize);
    BytesWritten := 0;
    InitStream(strm);
    Res := MemCheck(deflateInit_(strm, CompressionLevel, zlib_version, SizeOf(strm)));
    if Res <> Z_OK then
      raise ECompressInternalError.CreateFmt(SZlibInternalError, [Res]);
    Successful := False;
    try
      Finished := False;
      repeat
        if strm.avail_in = 0 then begin
          strm.next_in := I;
          strm.total_in := 0;
          if not Finished then begin
            strm.avail_in := ReadProc(I^, BufferSize, ExtraData);
            if strm.avail_in = 0 then
              Finished := True;
          end;
        end;
        strm.next_out := O;
        strm.avail_out := BufferSize;
        strm.total_out := 0;
        Res := MemCheck(deflate(strm, Actions[Finished]));
        if (Res <> Z_OK) and (Res <> Z_STREAM_END) then
          raise ECompressInternalError.CreateFmt(SZlibInternalError, [Res]);
        if strm.total_out <> 0 then begin
          Written := WriteProc(O^, strm.total_out, ExtraData);
          Inc(BytesWritten, Written);
          if Written <> strm.total_out then
            Exit;
        end;
      until Res = Z_STREAM_END;
      Adler := strm.adler;
      Successful := True;
    finally
      Res := MemCheck(deflateEnd(strm));
      { The only time Successful will be False if another exception
        was already raised. In that case, don't raise a new one }
      if Successful and (Res <> Z_OK) then
        raise ECompressInternalError.CreateFmt(SZlibInternalError, [Res]);
    end;
  finally
    if Assigned(O) then FreeMem(O {$IFNDEF WIN32}, BufferSize{$ENDIF});
    if Assigned(I) then FreeMem(I {$IFNDEF WIN32}, BufferSize{$ENDIF});
  end;
  Result := True;
end;

function CustomInflateData(const ReadProc: TZReadProc; const WriteProc: TZWriteProc;
  const ExtraData: Longint; var Adler: Longint): Boolean;
var
  strm: TZStreamRec;
  I, O: Pointer;
  Finished: Boolean;
  A: Longint;
  Res: Integer;
begin
  Result := False;
  GetMem(I, BufferSize);
  try
    GetMem(O, BufferSize);
    try
      InitStream(strm);
      A := 1;
      Res := MemCheck(inflateInit_(strm, zlib_version, SizeOf(strm)));
      if Res <> Z_OK then
        raise ECompressInternalError.CreateFmt(SZlibInternalError, [Res]);
      try
        Finished := False;
        repeat
          if strm.avail_in = 0 then begin
            strm.next_in := I;
            strm.total_in := 0;
            if not Finished then begin
              strm.avail_in := ReadProc(I^, BufferSize, ExtraData);
              if strm.avail_in = 0 then
                Finished := True;
            end;
          end;
          strm.next_out := O;
          strm.avail_out := BufferSize;
          strm.total_out := 0;
          Res := MemCheck(inflate(strm, Z_NO_FLUSH));
          case Res of
            Z_OK, Z_STREAM_END: ;
            Z_DATA_ERROR: raise ECompressDataError.Create(SZlibDataError);
          else
            raise ECompressInternalError.CreateFmt(SZlibInternalError, [Res]);
          end;
          if strm.total_out <> 0 then begin
            if {$IFDEF WIN32}Integer({$ENDIF} WriteProc(O^, strm.total_out, ExtraData)
               {$IFDEF WIN32}){$ENDIF} <> strm.total_out then
              Exit;
            { For some reason, the adler field of strm gets reset to 1 after the last
              block of data has been decompressed, making it unusable. So it has to
              calculate the Adler-32 itself }
            A := adler32(A, O^, strm.total_out);
          end;
        until Res = Z_STREAM_END;
        Adler := A;
      finally
        Res := MemCheck(inflateEnd(strm));
        if Res <> Z_OK then
          raise ECompressInternalError.CreateFmt(SZlibInternalError, [Res]);
      end;
    finally
      FreeMem(O {$IFNDEF WIN32}, BufferSize{$ENDIF});
    end;
  finally
    FreeMem(I {$IFNDEF WIN32}, BufferSize{$ENDIF});
  end;
  Result := True;
end;

type
  PExtraData = ^TExtraData;
  TExtraData = record
    Source, Dest: Pointer;
    SourceSize, DestSize: Longint;
    SourceIsFile, DestIsFile: Boolean;
  end;

function DataReadProc(var Buf; MaxBytes: Cardinal; ExtraData: Longint): Cardinal; far;
begin
  with PExtraData(ExtraData)^ do begin
    if {$IFDEF WIN32}Integer({$ENDIF} MaxBytes {$IFDEF WIN32}){$ENDIF} > SourceSize then
      MaxBytes := SourceSize;
    Result := MaxBytes;
    if SourceIsFile then
      BlockRead(File(Source^), Buf, MaxBytes)
    else begin
      Move(Source^, Buf, MaxBytes);
      {$IFDEF WIN32}
      Inc(Cardinal(Source), MaxBytes);
      {$ELSE}
      Inc(LongRec(Source).Lo, MaxBytes);
      {$ENDIF}
    end;
    Dec(SourceSize, MaxBytes);
  end;
end;

function DataWriteProc(var Buf; BufSize: Cardinal; ExtraData: Longint): Cardinal; far;
begin
  with PExtraData(ExtraData)^ do begin
    if (DestSize <> -1) and
       ({$IFDEF WIN32}Integer({$ENDIF} BufSize {$IFDEF WIN32}){$ENDIF} > DestSize) then
      BufSize := DestSize;
    Result := BufSize;
    if DestIsFile then
      BlockWrite(File(Dest^), Buf, BufSize)
    else begin
      Move(Buf, Dest^, BufSize);
      {$IFDEF WIN32}
      Inc(Cardinal(Dest), BufSize);
      {$ELSE}
      Inc(LongRec(Dest).Lo, BufSize);
      {$ENDIF}
    end;
    if DestSize <> -1 then
      Dec(DestSize, BufSize);
  end;
end;

function DeflateData(var ASource; const ASourceSize: Longint; const ASourceIsFile: Boolean;
  var ADest; const ADestSize: Longint; const ADestIsFile: Boolean;
  const CompressionLevel: TCompressionLevel;
  var BytesWritten, Adler: Longint): Boolean;
var
  Extra: TExtraData;
begin
  with Extra do begin
    Source := @ASource;
    Dest := @ADest;
    SourceSize := ASourceSize;
    DestSize := ADestSize;
    SourceIsFile := ASourceIsFile;
    DestIsFile := ADestIsFile;
  end;
  Result := CustomDeflateData(DataReadProc, DataWriteProc, Longint(@Extra),
    CompressionLevel, BytesWritten, Adler);
end;

function InflateData(var ASource; const ASourceSize: Longint; const ASourceIsFile: Boolean;
  var ADest; const ADestSize: Longint; const ADestIsFile: Boolean;
  var Adler: Longint): Boolean;
var
  Extra: TExtraData;
begin
  with Extra do begin
    Source := @ASource;
    Dest := @ADest;
    SourceSize := ASourceSize;
    DestSize := ADestSize;
    SourceIsFile := ASourceIsFile;
    DestIsFile := ADestIsFile;
  end;
  Result := CustomInflateData(DataReadProc, DataWriteProc, Longint(@Extra),
    Adler);
end;

{ DeflateBlockWrite* & InflateBlockRead* }

procedure DeflateBlockWriteBegin(var F: File; const CompressionLevel: TCompressionLevel;
  var Data: TDeflateBlockWriteData);
var
  Res: Integer;
  HdrCRC: Longint;
  Hdr: TNewBlockDeflateHeader;
begin
  FillChar(Data, SizeOf(Data), 0);
  Data.Compressed := CompressionLevel <> Z_NO_COMPRESSION;
  Data.F := @F;
  Data.StartPos := FilePos(F);

  HdrCRC := 0;
  BlockWrite(F, HdrCRC, SizeOf(HdrCRC));
  Hdr.StoredSize := 0;
  Hdr.Compressed := False;
  BlockWrite(F, Hdr, SizeOf(Hdr));

  with Data do begin
    InitStream(strm);
    if Compressed then begin
      Res := MemCheck(deflateInit_(strm, CompressionLevel, zlib_version, SizeOf(strm)));
      if Res <> Z_OK then
        raise ECompressInternalError.CreateFmt(SZlibInternalError, [Res]);
    end;
    strm.next_out := @OutBuffer;
    strm.avail_out := SizeOf(OutBuffer);
  end;
end;

procedure WriteOutputBuffer(var Data: TDeflateBlockWriteData;
  const OnlyIfFull: Boolean);
var
  Bytes: Cardinal;
  CRC: Longint;
begin
  with Data do
    if (OnlyIfFull and (strm.avail_out = 0)) or
       (not OnlyIfFull and (strm.avail_out <> SizeOf(OutBuffer))) then begin
      Bytes := SizeOf(OutBuffer) - strm.avail_out;
      CRC := GetCRC32(OutBuffer, Bytes);
      BlockWrite(F^, CRC, SizeOf(CRC));
      Inc(TotalBytesStored, SizeOf(CRC));
      BlockWrite(F^, OutBuffer, Bytes);
      Inc(TotalBytesStored, Bytes);
      strm.next_out := @OutBuffer;
      strm.avail_out := SizeOf(OutBuffer);
    end;
end;

procedure DeflateBuf(var Data: TDeflateBlockWriteData; var Buf;
  var Count: Cardinal);
var
  Res: Integer;
  Bytes: Cardinal;
begin
  with Data do begin
    if Count = 0 then
      Exit;
    strm.avail_in := Count;
    strm.next_in := @Buf;
    repeat
      if Compressed then begin
        Res := MemCheck(deflate(strm, Z_NO_FLUSH));
        if Res <> Z_OK then
          raise ECompressInternalError.CreateFmt(SZlibInternalError, [Res]);
      end
      else begin
        Bytes := strm.avail_out;
        if Bytes > strm.avail_in then
          Bytes := strm.avail_in;
        Move(strm.next_in^, strm.next_out^, Bytes);
        Dec(strm.avail_in, Bytes);
        Inc(strm.next_in, Bytes);
        Inc(strm.total_in, Bytes);
        Dec(strm.avail_out, Bytes);
        Inc(strm.next_out, Bytes);
        Inc(strm.total_out, Bytes);
      end;
      WriteOutputBuffer(Data, True);
    until strm.avail_in = 0;
    Count := 0;
  end;
end;

procedure DeflateBlockWrite(var Data: TDeflateBlockWriteData; var Buf;
  Count: Cardinal);
var
  P: Pointer;
  Bytes: Cardinal;
begin
  with Data do begin
    if Count < SizeOf(InBuffer) then begin
      { for blocks < 4 KB, use a fast buffering routine instead of passing the
        small blocks to 'deflate' }
      P := @Buf;
      while Count <> 0 do begin
        Bytes := Count;
        if Bytes > SizeOf(InBuffer) - InBufferCount then
          Bytes := SizeOf(InBuffer) - InBufferCount;
        Move(P^, InBuffer[InBufferCount], Bytes);
        Inc(InBufferCount, Bytes);
        if InBufferCount = SizeOf(InBuffer) then
          DeflateBuf(Data, InBuffer, InBufferCount);
        Dec(Count, Bytes);
        Inc(Cardinal(P), Bytes);
      end;
    end
    else begin
      { have to flush out whatever's in InBuffer first }
      DeflateBuf(Data, InBuffer, InBufferCount);
      DeflateBuf(Data, Buf, Count);
    end;
  end;
end;

procedure DeflateBlockWriteEnd(var Data: TDeflateBlockWriteData);
var
  Res: Integer;
  Pos: Longint;
  HdrCRC: Longint;
  Hdr: TNewBlockDeflateHeader;
begin
  with Data do begin
    DeflateBuf(Data, InBuffer, InBufferCount);
    if Compressed then begin
      repeat
        Res := MemCheck(deflate(strm, Z_FINISH));
        if (Res <> Z_OK) and (Res <> Z_STREAM_END) then
          raise ECompressInternalError.CreateFmt(SZlibInternalError, [Res]);
        WriteOutputBuffer(Data, True);
      until Res = Z_STREAM_END;
    end;
    WriteOutputBuffer(Data, False);
    if Compressed then begin
      Res := MemCheck(deflateEnd(strm));
      if Res <> Z_OK then
        raise ECompressInternalError.CreateFmt(SZlibInternalError, [Res]);
    end;

    Pos := FilePos(F^);
    Seek(F^, StartPos);
    Hdr.StoredSize := TotalBytesStored;
    Hdr.Compressed := Compressed;
    HdrCRC := GetCRC32(Hdr, SizeOf(Hdr));
    BlockWrite(F^, HdrCRC, SizeOf(HdrCRC));
    BlockWrite(F^, Hdr, SizeOf(Hdr));
    Seek(F^, Pos);
  end;
end;

procedure DeflateBlockWriteCancel(var Data: TDeflateBlockWriteData);
begin
  if Data.Compressed then
    deflateEnd(Data.strm);
end;
*)

procedure InflateBlockReadBegin(F: TFile; var Data: TDeflateBlockReadData);
var
  Res: Integer;
  HdrCRC: Longint;
  Hdr: TNewBlockDeflateHeader;
begin
  FillChar(Data, SizeOf(Data), 0);
  Data.F := F;
  Data.StartPos := F.Position.Lo; //FilePos(F);

  if F.Position.Lo + SizeOf(HdrCRC) + SizeOf(Hdr) > F.CappedSize then
    raise ECompressDataError.Create(SZlibDataError);
  F.ReadBuffer(HdrCRC, SizeOf(HdrCRC));
  F.ReadBuffer(Hdr, SizeOf(Hdr));
  if HdrCRC <> GetCRC32(Hdr, SizeOf(Hdr)) then
    raise ECompressDataError.Create(SZlibDataError);
  if F.Position.Lo + Cardinal(Hdr.StoredSize) > F.CappedSize then
    raise ECompressDataError.Create(SZlibDataError);
  Data.Compressed := Hdr.Compressed;
  Data.InBytesLeft := Hdr.StoredSize;

  with Data do begin
    InitStream(strm);
    if Compressed then begin
      Res := MemCheck(inflateInit_(zlib.TZStreamRec(strm), zlib_version, SizeOf(strm)));
      if Res <> Z_OK then
        raise ECompressInternalError.CreateFmt(SZlibInternalError, [Res]);
    end;
    strm.next_out := @OutBuffer;
    strm.avail_out := SizeOf(OutBuffer);
  end;
end;

procedure InflateBlockRead(var Data: TDeflateBlockReadData; var Buf;
  const Count: Cardinal);
var
  B: Pointer;
  Left, OutCount: Cardinal;
  Len, CRC: Longint;
  Res: Integer;
begin
  with Data do begin
    B := @Buf;
    Left := Count;
    while Left <> 0 do begin
      if (strm.avail_in = 0) and (InBytesLeft <> 0) then begin
        { Read chunk CRC }
        if InBytesLeft < SizeOf(CRC) + 1 then
          raise ECompressDataError.Create(SZlibDataError);  { just in case... }
        F.ReadBuffer(CRC, SizeOf(CRC));
        Dec(InBytesLeft, SizeOf(CRC));
        { Read chunk data }
        Len := InBytesLeft;
        if Len > SizeOf(InBuffer) then
          Len := SizeOf(InBuffer);
        F.ReadBuffer(InBuffer, Len);
        Dec(Data.InBytesLeft, Len);
        strm.next_in := @InBuffer;
        strm.avail_in := Len;
        if CRC <> GetCRC32(InBuffer, Len) then
          raise ECompressDataError.Create(SZlibDataError);
      end;
      if (strm.avail_out <> 0) and
         (Cardinal(strm.next_out) - Cardinal(@OutBuffer[OutBufferStart]) < Left) then begin
        if NoMoreData then
          raise ECompressDataError.Create(SZlibDataError);
        if Compressed then begin
          Res := MemCheck(inflate(zlib.TZStreamRec(strm), Z_NO_FLUSH));
          case Res of
            Z_OK: ;
            Z_STREAM_END: NoMoreData := True;
            Z_DATA_ERROR: raise ECompressDataError.Create(SZlibDataError);
          else
            raise ECompressInternalError.CreateFmt(SZlibInternalError, [Res]);
          end;
        end
        else begin
          OutCount := strm.avail_out;
          if OutCount > strm.avail_in then
            OutCount := strm.avail_in;
          Move(strm.next_in^, strm.next_out^, OutCount);
          Dec(strm.avail_in, OutCount);
          Inc(strm.next_in, OutCount);
          Inc(strm.total_in, OutCount);
          Dec(strm.avail_out, OutCount);
          Inc(strm.next_out, OutCount);
          Inc(strm.total_out, OutCount);
        end;
      end;
      if strm.next_out > @OutBuffer[OutBufferStart] then begin
        OutCount := strm.next_out - @OutBuffer[OutBufferStart];
        if OutCount > Left then OutCount := Left;
        Move(OutBuffer[OutBufferStart], B^, OutCount);
        Dec(Left, OutCount);
        Inc(Longint(B), OutCount);
        Inc(OutBufferStart, OutCount);
        if OutBufferStart = SizeOf(OutBuffer) then begin
          strm.next_out := @OutBuffer;
          strm.avail_out := SizeOf(OutBuffer);
          OutBufferStart := 0;
        end;
      end;
    end;
  end;
end;

procedure InflateBlockReadEnd(var Data: TDeflateBlockReadData);
begin
  with Data do begin
    if Compressed then
      inflateEnd(zlib.TZStreamRec(strm));
    { Must seek ahead if the caller didn't read everything that was originally
      compressed, or if it did read everything but zlib is in a "CHECK" state
      (i.e. it didn't read and verify the trailing adler32 yet due to lack of
      input bytes). }
    F.Seek(F.Position.Lo + Cardinal(InBytesLeft));
  end;
end;

(*
function CalcAdler32(CurAdler: Longint; const Buf; const BufSize: Cardinal): Longint;
begin
  Result := adler32(CurAdler, Buf, BufSize);
end;

{ TZCompressor }

constructor TZCompressor.Create(AWriteProc: TCompressorWriteProc;
  AProgressProc: TCompressorProgressProc; CompressionLevel: Integer);
begin
  inherited;
  InitStream(FStrm);
  FStrm.next_out := @FBuffer;
  FStrm.avail_out := SizeOf(FBuffer);
  Check(deflateInit_(FStrm, CompressionLevel, zlib_version, SizeOf(FStrm)), [Z_OK]);
  FInitialized := True;
end;

destructor TZCompressor.Destroy;
begin
  if FInitialized then
    deflateEnd(FStrm);
  inherited;
end;

procedure TZCompressor.FlushBuffer;
begin
  if FStrm.avail_out < SizeOf(FBuffer) then begin
    WriteProc(FBuffer, SizeOf(FBuffer) - FStrm.avail_out);
    FStrm.next_out := @FBuffer;
    FStrm.avail_out := SizeOf(FBuffer);
  end;
end;

procedure TZCompressor.Compress(const Buffer; Count: Longint);
begin
  FStrm.next_in := @Buffer;
  FStrm.avail_in := Count;
  while FStrm.avail_in > 0 do begin
    Check(deflate(FStrm, Z_NO_FLUSH), [Z_OK]);
    if FStrm.avail_out = 0 then
      FlushBuffer;
  end;
  if Assigned(ProgressProc) then
    ProgressProc(Count);
end;

procedure TZCompressor.Finish;
begin
  FStrm.next_in := nil;
  FStrm.avail_in := 0;
  { Note: This assumes FStrm.avail_out > 0. This shouldn't be a problem since
    Compress always flushes when FStrm.avail_out reaches 0. }
  while Check(deflate(FStrm, Z_FINISH), [Z_OK, Z_STREAM_END]) <> Z_STREAM_END do
    FlushBuffer;
  FlushBuffer;
end;

{ TZDecompressor }

constructor TZDecompressor.Create(AReadProc: TDecompressorReadProc);
begin
  inherited Create(AReadProc);
  InitStream(FStrm);
  FStrm.next_in := @FBuffer;
  FStrm.avail_in := 0;
  Check(inflateInit_(FStrm, zlib_version, SizeOf(FStrm)), [Z_OK]);
  FInitialized := True;
end;

destructor TZDecompressor.Destroy;
begin
  if FInitialized then
    inflateEnd(FStrm);
  inherited Destroy;
end;

procedure TZDecompressor.DecompressInto(var Buffer; Count: Longint);
begin
  FStrm.next_out := @Buffer;
  FStrm.avail_out := Count;
  while FStrm.avail_out > 0 do begin
    if FReachedEnd then  { unexpected EOF }
      raise ECompressDataError.Create(SZlibDataError);
    if FStrm.avail_in = 0 then begin
      FStrm.next_in := @FBuffer;
      FStrm.avail_in := ReadProc(FBuffer, SizeOf(FBuffer));
      { Note: If avail_in is zero while zlib still needs input, inflate() will
        return Z_BUF_ERROR. We interpret that as a data error (see below). }
    end;
    case Check(inflate(FStrm, Z_NO_FLUSH), [Z_OK, Z_STREAM_END, Z_DATA_ERROR, Z_BUF_ERROR]) of
      Z_STREAM_END: FReachedEnd := True;
      Z_DATA_ERROR, Z_BUF_ERROR: raise ECompressDataError.Create(SZlibDataError);
    end;
  end;
end;

procedure TZDecompressor.Reset;
begin
  FStrm.next_in := @FBuffer;
  FStrm.avail_in := 0;
  Check(inflateReset(FStrm), [Z_OK]);
  FReachedEnd := False;
end;
*)
end.
