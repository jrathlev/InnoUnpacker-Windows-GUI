unit CompressVer4008;

interface
uses zlib4008, Compress, FileClass;

type
  TZlibBlockReader4008 = class (TAbstractBlockReader)
  protected
    Data: TDeflateBlockReadData;
  public
    constructor Create(AFile: TFile; ADecompressorClass: TCustomDecompressorClass); override;
    destructor Destroy; override;
    procedure Read(var Buffer; Count: Cardinal); override;
  end;

implementation

constructor TZlibBlockReader4008.Create(AFile: TFile; ADecompressorClass: TCustomDecompressorClass);
begin
  InflateBlockReadBegin(AFile, Data);
end;

destructor TZlibBlockReader4008.Destroy;
begin
  InflateBlockReadEnd(Data);
end;

procedure TZlibBlockReader4008.Read(var Buffer; Count: Cardinal);
begin
  InflateBlockRead(Data,Buffer,Count);
end;

end.
