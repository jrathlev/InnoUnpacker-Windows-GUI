unit FileNull;

interface

uses Int64Em, FileClass;

type
  TNullFile = class(TFile)
  protected
    function GetPosition: Integer64; override;
    function GetSize: Integer64; override;
  public
    constructor Create();

    function Read(var Buffer; Count: Cardinal): Cardinal; override;
    procedure Seek64(Offset: Integer64); override;
    procedure WriteBuffer(const Buffer; Count: Cardinal); override;
  end;

implementation

uses System.SysUtils;

{ TNullFile }

constructor TNullFile.Create;
begin
  //
end;

function TNullFile.GetPosition: Integer64;
begin
  Result.Lo := 0;
  Result.Hi := 0;
end;

function TNullFile.GetSize: Integer64;
begin
  Result.Lo := 0;
  Result.Hi := 0;
end;

function TNullFile.Read(var Buffer; Count: Cardinal): Cardinal;
begin
  raise Exception.Create('Can not read from Null file');
end;

procedure TNullFile.Seek64(Offset: Integer64);
begin
  // Do nothing
end;

procedure TNullFile.WriteBuffer(const Buffer; Count: Cardinal);
begin
  // Do nothing
end;

end.
