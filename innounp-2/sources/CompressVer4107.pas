unit CompressVer4107;

interface
uses zlib4107, Compress, FileClass;

type
  TZlibBlockReader4107 = class (TAbstractBlockReader)
  protected
    Data: TDeflateBlockReadData;
  public
    constructor Create(AFile: TFile; ADecompressorClass: TCustomDecompressorClass); override;
    destructor Destroy; override;
    procedure Read(var Buffer; Count: Cardinal); override;
  end;

implementation

constructor TZlibBlockReader4107.Create(AFile: TFile; ADecompressorClass: TCustomDecompressorClass);
begin
  InflateBlockReadBegin(AFile, Data);
end;

destructor TZlibBlockReader4107.Destroy;
begin
  InflateBlockReadEnd(Data);
end;

procedure TZlibBlockReader4107.Read(var Buffer; Count: Cardinal);
begin
  InflateBlockRead(Data,Buffer,Count);
end;

end.
