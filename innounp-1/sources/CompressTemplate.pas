unit CompressVerDEFVER;

interface
uses zlibDEFVER, Compress, FileClass;

type
  TZlibBlockReaderDEFVER = class (TAbstractBlockReader)
  protected
    Data: TDeflateBlockReadData;
  public
    constructor Create(AFile: TFile; ADecompressorClass: TCustomDecompressorClass); override;
    destructor Destroy; override;
    procedure Read(var Buffer; Count: Cardinal); override;
  end;

implementation

constructor TZlibBlockReaderDEFVER.Create(AFile: TFile; ADecompressorClass: TCustomDecompressorClass);
begin
  InflateBlockReadBegin(AFile, Data);
end;

destructor TZlibBlockReaderDEFVER.Destroy;
begin
  InflateBlockReadEnd(Data);
end;

procedure TZlibBlockReaderDEFVER.Read(var Buffer; Count: Cardinal);
begin
  InflateBlockRead(Data,Buffer,Count);
end;

end.
