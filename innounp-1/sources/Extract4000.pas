unit Extract4000;

interface
uses
  Compress, Struct, Extract, FileClass;

type
  TFileExtractor4000 = class;

  PDecompressExtraData = ^TDecompressExtraData;
  TDecompressExtraData = record
    CurSlice, LastSlice: Integer;
    BytesLeft, Size, BytesWritten: Int64;
    Extractor: TFileExtractor4000;
    DestF: TFile;
  end;

  TFileExtractor4000 = class(TFileExtractor)
  private
    procedure OpenNextSlice(const ASlice: Integer);
  public
    constructor Create(ADecompressorClass: TCustomDecompressorClass); override;
    destructor Destroy; override;
    procedure DecompressFile(const FL: TSetupFileLocationEntry; const DestFile: TFile;
      const ProgressProc: TExtractorProgressProc); override;
    procedure SeekTo(const FL: TSetupFileLocationEntry); override;
  end;

implementation
uses
  System.SysUtils, zlib, bzlib, Msgs, CmnFunc2, PathFunc, Main, MyTypes, Winapi.Windows;

function BZCustomInflateData(const ReadProc: TZReadProc; const WriteProc: TZWriteProc;
  const ExtraData: Longint; var Adler: Longint): Boolean; forward;

procedure SourceIsCorrupted;
begin
  raise Exception.Create(SSetupFileCorrupt);
end;

procedure ReadDiskSliceHeader(SourceF: TFile);
var
  TestDiskSliceID: TDiskSliceID;
  DiskSliceHeader: TDiskSliceHeader;
begin
  if SourceF.CappedSize < SizeOf(TestDiskSliceID) + SizeOf(DiskSliceHeader) then
    SourceIsCorrupted;
  SourceF.ReadBuffer(TestDiskSliceID, SizeOf(TestDiskSliceID));
  if TestDiskSliceID <> DiskSliceID then
    SourceIsCorrupted;
  SourceF.ReadBuffer(DiskSliceHeader, SizeOf(DiskSliceHeader));
  if SourceF.CappedSize <> DiskSliceHeader.TotalSize then
    SourceIsCorrupted;
end;

function zlibReadProc(var Buf; MaxBytes: Cardinal; ExtraData: Longint): Cardinal; far;
var
  Buffer: Pointer;
  Left, Res: Cardinal;
begin
  Buffer := @Buf;
  with PDecompressExtraData(ExtraData)^ do begin
    Left := MaxBytes;
    if (BytesLeft < Left) then
      Left := BytesLeft;
    Result := Left;
    while Left <> 0 do begin
      Res := Extractor.FSourceF.Read(Buffer^, Left);
      Dec(BytesLeft, Res);
      if Left = Res then
        Break
      else begin
        Dec(Left, Res);
        Inc(Longint(Buffer), Res);
        { Go to next disk }
        if CurSlice >= LastSlice then
          { Already on the last slice, so the file must be corrupted... }
          SourceIsCorrupted
        else begin
          Inc(CurSlice);
          Extractor.OpenNextSlice(CurSlice);
        end;
      end;
    end;
  end;
(*  if zlibSetProgress then begin
    { Increment progress meter }
    IncProgress(Result);
    ProcessEvents;
  end;*)
end;

function zlibWriteProc(var Buf; BufSize: Cardinal; ExtraData: Longint): Cardinal; far;
begin
  with PDecompressExtraData(ExtraData)^ do begin
    Inc(BytesWritten, BufSize);
    if (BytesWritten > Size) then
      SourceIsCorrupted;
    DestF.WriteBuffer(Buf, BufSize);
  end;
  Result := BufSize;
end;

///////////////////////       TFileExtractor4000        ///////////////////////////////

constructor TFileExtractor4000.Create(ADecompressorClass: TCustomDecompressorClass);
begin
  inherited;
end;

destructor TFileExtractor4000.Destroy;
begin
  inherited;
end;

procedure TFileExtractor4000.SeekTo(const FL: TSetupFileLocationEntry);
var
  SourceFile: String;
  TestCompID: TCompID;
begin
  if SetupLdrOffset1 = 0 then
    SourceFile := FindSliceFilename(FL.FirstSlice)
  else
    SourceFile := SetupLdrOriginalFilename;

  { Open source file }
  FSourceF := TFile.Create(SourceFile,fdOpenExisting,faRead,fsRead);
  FOpenedSlice := FL.FirstSlice;

  if SetupLdrOffset1 = 0 then
    ReadDiskSliceHeader(FSourceF);
  FSourceF.Seek(SetupLdrOffset1 + FL.StartOffset);
  FSourceF.ReadBuffer(TestCompID, SizeOf(TestCompID));
  if TestCompID <> ZLIBID then
    SourceIsCorrupted;
end;

procedure TFileExtractor4000.DecompressFile(const FL: TSetupFileLocationEntry; const DestFile: TFile;
  const ProgressProc: TExtractorProgressProc);
var
  CurFileDate, CurFileDate2: TFileTime;
  DecompressData: TDecompressExtraData;
  Adler32: Longint;
begin
  with DecompressData do begin
    CurSlice := FL.FirstSlice;
    LastSlice := FL.LastSlice;
    BytesLeft := FL.ChunkCompressedSize;
    Size := FL.OriginalSize;
    BytesWritten := 0;
    Extractor := Self;
    DestF := DestFile;
  end;

  try
    case SetupHeader.CompressMethod of
      cmZip: CustomInflateData(zlibReadProc, zlibWriteProc, Longint(@DecompressData), Adler32);
      cmBzip: BZCustomInflateData(zlibReadProc, zlibWriteProc, Longint(@DecompressData), Adler32);
    end;
  except
    on ECompressDataError do
      SourceIsCorrupted;
  end;
  if (DecompressData.BytesWritten <> FL.OriginalSize) or (Adler32 <> FL.CRC) then
    SourceIsCorrupted;

  { Set time/date stamp }
  CurFileDate := FL.TimeStamp;
  LocalFileTimeToFileTime(CurFileDate, CurFileDate2);
  SetFileTime(DestFile.Handle, @CurFileDate2, nil, @CurFileDate2);
  FreeAndNil(FSourceF);
end;

procedure TFileExtractor4000.OpenNextSlice(const ASlice: Integer);
var
  NewSliceFilename: String;
begin
  if Assigned(FSourceF) and (FOpenedSlice = ASlice) then Exit;

  FreeAndNil(FSourceF);
  NewSliceFilename := FindSliceFilename(ASlice);
  Notify('Reading slice ' + NewSliceFilename);
  FSourceF := TFile.Create(NewSliceFilename, fdOpenExisting, faRead, fsRead);
  ReadDiskSliceHeader(FSourceF);
  FOpenedSlice := ASlice;
end;

/////////////////////////////         old (3.x - 4.0.0) bzip2 stuff    ////////////////////////

const
  BZ_RUN              = 0;
  BZ_FLUSH            = 1;
  BZ_FINISH           = 2;

  BZ_OK               = 0;
  BZ_RUN_OK           = 1;
  BZ_FLUSH_OK         = 2;
  BZ_FINISH_OK        = 3;
  BZ_STREAM_END       = 4;
  BZ_SEQUENCE_ERROR   = (-1);
  BZ_PARAM_ERROR      = (-2);
  BZ_MEM_ERROR        = (-3);
  BZ_DATA_ERROR       = (-4);
  BZ_DATA_ERROR_MAGIC = (-5);
  BZ_IO_ERROR         = (-6);
  BZ_UNEXPECTED_EOF   = (-7);
  BZ_OUTBUFF_FULL     = (-8);
  BZ_CONFIG_ERROR     = (-9);

  SBzlibDataError = 'bzlib: Compressed data is corrupted';
  SBzlibInternalError = 'bzlib: Internal error. Code %d';


function BZAllocMem(AppData: Pointer; Items, Size: Cardinal): Pointer; stdcall;
begin
  try
    GetMem(Result, Items * Size);
  except
    { trap any exception, because zlib expects a NULL result if it's out
      of memory }
    Result := nil;
  end;
end;

procedure BZFreeMem(AppData, Block: Pointer); stdcall;
begin
  FreeMem(Block);
end;

function MemCheck(const Code: Integer): Integer;
begin
  if Code = BZ_MEM_ERROR then
    OutOfMemoryError;
  Result := Code;
end;

procedure InitStream(var strm: TBZStreamRec);
begin
  FillChar(strm, SizeOf(strm), 0);
  with strm do begin
    zalloc := BZAllocMem;
    zfree := BZFreeMem;
  end;
end;

const
  BufferSize = 65536;

function BZCustomInflateData(const ReadProc: TZReadProc; const WriteProc: TZWriteProc;
  const ExtraData: Longint; var Adler: Longint): Boolean;
var
  strm: TBZStreamRec;
  I, O: Pointer;
  Finished: Boolean;
  A: Longint;
  Res: Integer;
begin
  Result := False;
  I := nil;
  O := nil;
  try
    GetMem(I, BufferSize);
    GetMem(O, BufferSize);
    InitStream(strm);
    A := 1;
    Res := MemCheck(BZ2_bzDecompressInit(strm, 0, 0));
    if Res <> BZ_OK then
      raise ECompressInternalError.CreateFmt(SBzlibInternalError, [Res]);
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
        Res := MemCheck(BZ2_bzDecompress(strm));
        case Res of
          BZ_OK, BZ_STREAM_END: ;
          BZ_DATA_ERROR, BZ_DATA_ERROR_MAGIC: raise ECompressDataError.Create(SBzlibDataError);
        else
          raise ECompressInternalError.CreateFmt(SBzlibInternalError, [Res]);
        end;
        if strm.total_out <> 0 then begin
          if Integer(WriteProc(O^, strm.total_out, ExtraData)) <> strm.total_out then
            Exit;
          A := CalcAdler32(A, O^, strm.total_out);
        end;
      until Res = BZ_STREAM_END;
      Adler := A;
    finally
      Res := MemCheck(BZ2_bzDecompressEnd(strm));
      if Res <> BZ_OK then
        raise ECompressInternalError.CreateFmt(SBzlibInternalError, [Res]);
    end;
  finally
    if Assigned(O) then FreeMem(O);
    if Assigned(I) then FreeMem(I);
  end;
  Result := True;
end;

end.
