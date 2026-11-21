unit GogGalaxy;

interface

uses
  System.Classes, System.SysUtils, Struct, FileClass, Extract, MD5, FileTable;

type
  EGogGalaxyError = class(Exception);

  TGogGalaxyPart = record
    Location: Integer;
    CompressedSize, Size: Int64;
    Checksum: TMD5Digest;
  end;

  TGogGalaxyFile = record
    Name: String;
    Size: Int64;
    Checksum: TMD5Digest;
    Parts: array of TGogGalaxyPart;
    Hidden: Boolean;
  end;

  { Rebuilds the files of a GOG Galaxy installer, which are stored as
    compressed, MD5-hashed parts described by before_install/after_install
    callbacks on the file entries. }
  TGogGalaxyArchive = class(TFileEntryProvider)
  private
    FFiles: array of TGogGalaxyFile;
    FLocations: TList;
    constructor Create(Files, Locations: TList);
    function ParseGalaxyFiles(Files: TList): Boolean;
    function IsGalaxyFile(Index: Integer): Boolean;
    procedure ResolveNames(ExtractAllCopies: Boolean);
    procedure Publish(Table: TFileTable);
  public
    { Detects a GOG Galaxy installer from the structure of its file entries
      and, if found, registers the rebuilt files in Table. Returns False and
      leaves Table unchanged otherwise; Warning then says why entries that
      looked like Galaxy parts were rejected, or is empty if there were none. }
    class function Detect(Table: TFileTable; Files, Locations: TList;
      ExtractAllCopies: Boolean; out Warning: String): Boolean;
    procedure ExtractEntry(Index: Integer; Extractor: TFileExtractor;
      Destination: TFile; Progress: TExtractorProgressProc); override;
  end;

implementation

uses
  zlib;

type
  PMD5Context = ^TMD5Context;

  { Receives an Inno file's bytes and inflates the inner Galaxy zlib stream.
    Neither the compressed part nor the assembled file is buffered in memory. }
  TGogGalaxyPartWriter = class(TFile)
  private
    FStream: TZStreamRec;
    FInitialized, FFinished: Boolean;
    FCompressedLeft, FBytesLeft: Int64;
    FDestination: TFile;
    FFileHash: PMD5Context;
    FPartHash: TMD5Context;
    FChecksum: TMD5Digest;
  public
    constructor Create(Destination: TFile; const Part: TGogGalaxyPart;
      var FileHash: TMD5Context);
    destructor Destroy; override;
    procedure WriteBuffer(const Buffer; Count: Cardinal); override;
    procedure Finish;
  end;

procedure InvalidGalaxy(const Detail: String);
begin
  raise EGogGalaxyError.Create('Invalid GOG Galaxy data: ' + Detail);
end;

function ParseCall(const Code, Name: String; out Arguments: TArray<String>): Boolean;
var
  Text, Value: String;
  P, Start: Integer;
  Closed: Boolean;

  procedure SkipSpace;
  begin
    while (P <= Length(Text)) and (Text[P] <= ' ') do Inc(P);
  end;

begin
  Result := False;
  SetLength(Arguments, 0);
  Text := Trim(Code);
  P := Pos('(', Text);
  if (P = 0) or not SameText(Trim(Copy(Text, 1, P - 1)), Name) then Exit;
  Inc(P);
  repeat
    SkipSpace;
    if P > Length(Text) then InvalidGalaxy('incomplete ' + Name + ' callback');
    Value := '';
    if Text[P] = '''' then begin
      Inc(P);
      Closed := False;
      while P <= Length(Text) do begin
        if Text[P] = '''' then begin
          Inc(P);
          if (P > Length(Text)) or (Text[P] <> '''') then begin
            Closed := True;
            Break;
          end;
        end;
        Value := Value + Text[P];
        Inc(P);
      end;
      if not Closed then InvalidGalaxy('unterminated string in ' + Name);
    end
    else begin
      Start := P;
      while (P <= Length(Text)) and not CharInSet(Text[P], [',', ')']) do Inc(P);
      Value := Trim(Copy(Text, Start, P - Start));
      if Value = '' then InvalidGalaxy('empty argument in ' + Name);
    end;
    SetLength(Arguments, Length(Arguments) + 1);
    Arguments[High(Arguments)] := Value;
    SkipSpace;
    if P > Length(Text) then InvalidGalaxy('incomplete ' + Name + ' callback');
    if Text[P] = ')' then Break;
    if Text[P] <> ',' then InvalidGalaxy('unexpected argument in ' + Name);
    Inc(P);
  until False;
  Inc(P);
  SkipSpace;
  if (P <= Length(Text)) and (Text[P] = ';') then Inc(P);
  SkipSpace;
  if (P <= Length(Text)) or (Length(Arguments) <> 3) then
    InvalidGalaxy('unexpected arguments in ' + Name);
  Result := True;
end;

function ParseSize(const Value: String): Int64;
var
  I: Integer;
begin
  if Value = '' then InvalidGalaxy('missing size or part count');
  for I := 1 to Length(Value) do
    if not CharInSet(Value[I], ['0'..'9']) then
      InvalidGalaxy('invalid size or part count');
  if not TryStrToInt64(Value, Result) then InvalidGalaxy('size or part count overflow');
end;

function ParseChecksum(const Value: String): TMD5Digest;
var
  I, N: Integer;
begin
  if Length(Value) <> 32 then InvalidGalaxy('invalid MD5 checksum');
  for I := 0 to High(Result) do begin
    if not TryStrToInt('$' + Copy(Value, I * 2 + 1, 2), N) then
      InvalidGalaxy('invalid MD5 checksum');
    Result[I] := N;
  end;
end;

function SafeFileName(const Value: String): String;
var
  Component: String;
  I, Start: Integer;
begin
  Result := StringReplace(Value, '/', '\', [rfReplaceAll]);
  if (Result = '') or (Result[1] = '\') then InvalidGalaxy('absolute or empty filename');
  Start := 1;
  for I := 1 to Length(Result) + 1 do begin
    if I <= Length(Result) then begin
      if (Ord(Result[I]) < 32) or CharInSet(Result[I], [':', '*', '?', '"', '<', '>', '|']) then
        InvalidGalaxy('invalid filename');
      if Result[I] <> '\' then Continue;
    end;
    Component := Copy(Result, Start, I - Start);
    if (Component = '') or (Component = '.') or (Component = '..') then
      InvalidGalaxy('invalid path component');
    if CharInSet(Component[Length(Component)], [' ', '.']) then
      InvalidGalaxy('invalid path component');
    Start := I + 1;
  end;
end;

class function TGogGalaxyArchive.Detect(Table: TFileTable; Files, Locations: TList;
  ExtractAllCopies: Boolean; out Warning: String): Boolean;
var
  Archive: TGogGalaxyArchive;
begin
  Warning := '';
  Archive := TGogGalaxyArchive.Create(Files, Locations);
  try
    try
      Result := Archive.ParseGalaxyFiles(Files);
    except
      on E: EGogGalaxyError do begin
        Warning := E.Message;
        Result := False;
      end;
    end;
    if Result then begin
      Archive.ResolveNames(ExtractAllCopies);
      Archive.Publish(Table);
      Table.AddProvider(Archive);
      Archive := nil;
    end;
  finally
    Archive.Free;
  end;
end;

constructor TGogGalaxyArchive.Create(Files, Locations: TList);
begin
  inherited Create;
  FLocations := Locations;
  SetLength(FFiles, Files.Count);
end;

procedure TGogGalaxyArchive.Publish(Table: TFileTable);
var
  I: Integer;
begin
  for I := 0 to High(FFiles) do begin
    if IsGalaxyFile(I) then Table.Reconstruct(I, Self, FFiles[I].Name, FFiles[I].Size);
    if FFiles[I].Hidden then Table.Hide(I);
  end;
end;

{ Returns True if at least one Galaxy multipart file was found. Raises
  EGogGalaxyError if Galaxy callbacks are present but inconsistent: every part
  must follow its starting entry, share its conditions, and the compressed size
  in its metadata must match the size Inno Setup stored for it. }
function TGogGalaxyArchive.ParseGalaxyFiles(Files: TList): Boolean;
var
  I, J, PartCount: Integer;
  Count: Int64;
  FileEntry, PartEntry: PSetupFileEntry;
  StartInfo, PartInfo: TArray<String>;
  Dependency: Boolean;
  Locations: TList;
begin
  Result := False;
  Locations := FLocations;
  I := 0;
  while I < Files.Count do begin
    FileEntry := PSetupFileEntry(Files[I]);
    Dependency := False;
    if not ParseCall(FileEntry^.BeforeInstall, 'before_install', StartInfo) then begin
      Dependency := True;
      if not ParseCall(FileEntry^.BeforeInstall, 'before_install_dependency', StartInfo) then begin
        if ParseCall(FileEntry^.AfterInstall, 'after_install', PartInfo) or
          ParseCall(FileEntry^.AfterInstall, 'after_install_dependency', PartInfo) then
          InvalidGalaxy('file part without a starting entry');
        Inc(I);
        Continue;
      end;
    end;

    Count := ParseSize(StartInfo[2]);
    if (Count < 1) or (Count > Files.Count - I) then InvalidGalaxy('incomplete file');
    PartCount := Integer(Count);
    FFiles[I].Name := SafeFileName(StartInfo[1]);
    FFiles[I].Checksum := ParseChecksum(StartInfo[0]);
    FFiles[I].Size := 0;
    SetLength(FFiles[I].Parts, PartCount);
    for J := 0 to PartCount - 1 do begin
      PartEntry := PSetupFileEntry(Files[I + J]);
      if (J > 0) and (Trim(PartEntry^.BeforeInstall) <> '') then
        InvalidGalaxy('unexpected file start inside a multipart file');
      if (PartEntry^.Check <> FileEntry^.Check) or
        (PartEntry^.Languages <> FileEntry^.Languages) or
        (PartEntry^.Components <> FileEntry^.Components) or
        (PartEntry^.Tasks <> FileEntry^.Tasks) then
        InvalidGalaxy('mismatched multipart file conditions');
      if Dependency then begin
        if not ParseCall(PartEntry^.AfterInstall, 'after_install_dependency', PartInfo) then
          InvalidGalaxy('missing dependency part metadata');
      end
      else if not ParseCall(PartEntry^.AfterInstall, 'after_install', PartInfo) then
        InvalidGalaxy('missing file part metadata');
      if (PartEntry^.LocationEntry < 0) or (PartEntry^.LocationEntry >= Locations.Count) then
        InvalidGalaxy('invalid file part location');
      with FFiles[I].Parts[J] do begin
        Location := PartEntry^.LocationEntry;
        CompressedSize := ParseSize(PartInfo[1]);
        Size := ParseSize(PartInfo[2]);
        Checksum := ParseChecksum(PartInfo[0]);
        if CompressedSize <> PSetupFileLocationEntry(Locations[Location])^.OriginalSize then
          InvalidGalaxy('compressed part size mismatch');
        if Size > High(Int64) - FFiles[I].Size then InvalidGalaxy('file size overflow');
        Inc(FFiles[I].Size, Size);
      end;
      if J > 0 then FFiles[I + J].Hidden := True;
    end;
    Result := True;
    Inc(I, PartCount);
  end;
end;

procedure TGogGalaxyArchive.ResolveNames(ExtractAllCopies: Boolean);
var
  Names, Seen: TStringList;
  I, N, Count: Integer;
  Key: String;
begin
  Names := TStringList.Create;
  Seen := TStringList.Create;
  try
    Names.Sorted := True;
    Seen.Sorted := True;
    for I := 0 to High(FFiles) do begin
      if not IsGalaxyFile(I) then Continue;
      Key := FFiles[I].Name + #1 + MD5DigestToString(FFiles[I].Checksum);
      if not ExtractAllCopies and (Seen.IndexOf(Key) >= 0) then begin
        FFiles[I].Hidden := True;
        Continue;
      end;
      Seen.Add(Key);
      N := Names.IndexOf(FFiles[I].Name);
      if N < 0 then Names.AddObject(FFiles[I].Name, TObject(NativeInt(1)))
      else Names.Objects[N] := TObject(NativeInt(Names.Objects[N]) + 1);
    end;
    Seen.Clear;
    for N := 0 to Names.Count - 1 do
      if NativeInt(Names.Objects[N]) > 1 then Seen.Add(Names[N]);
    { Match innounp's numbering of distinct files with the same destination. }
    for I := High(FFiles) downto 0 do begin
      if not IsGalaxyFile(I) or FFiles[I].Hidden then Continue;
      N := Names.IndexOf(FFiles[I].Name);
      Count := NativeInt(Names.Objects[N]);
      if Seen.IndexOf(FFiles[I].Name) >= 0 then begin
        Key := ChangeFileExt(FFiles[I].Name, '') + ',' + IntToStr(Count) +
          ExtractFileExt(FFiles[I].Name);
        Names.Objects[N] := TObject(NativeInt(Count - 1));
        FFiles[I].Name := Key;
      end;
    end;
  finally
    Seen.Free;
    Names.Free;
  end;
end;

function TGogGalaxyArchive.IsGalaxyFile(Index: Integer): Boolean;
begin
  Result := Length(FFiles[Index].Parts) <> 0;
end;

procedure TGogGalaxyArchive.ExtractEntry(Index: Integer; Extractor: TFileExtractor;
  Destination: TFile; Progress: TExtractorProgressProc);
var
  Part: TGogGalaxyPart;
  Writer: TGogGalaxyPartWriter;
  Hash: TMD5Context;
begin
  MD5Init(Hash);
  for Part in FFiles[Index].Parts do begin
    Writer := TGogGalaxyPartWriter.Create(Destination, Part, Hash);
    try
      Extractor.SeekTo(PSetupFileLocationEntry(FLocations[Part.Location])^);
      Extractor.DecompressFile(PSetupFileLocationEntry(FLocations[Part.Location])^, Writer, Progress);
      Writer.Finish;
    finally
      Writer.Free;
    end;
  end;
  if not MD5DigestsEqual(MD5Final(Hash), FFiles[Index].Checksum) then
    InvalidGalaxy('file checksum mismatch for ' + FFiles[Index].Name);
end;

constructor TGogGalaxyPartWriter.Create(Destination: TFile; const Part: TGogGalaxyPart;
  var FileHash: TMD5Context);
begin
  inherited CreateWithExistingHandle(0);
  FDestination := Destination;
  FFileHash := @FileHash;
  FCompressedLeft := Part.CompressedSize;
  FBytesLeft := Part.Size;
  FChecksum := Part.Checksum;
  MD5Init(FPartHash);
  InitStream(FStream);
  if inflateInit_(FStream, zlib_Version, SizeOf(FStream)) <> 0 then
    InvalidGalaxy('cannot initialize zlib');
  FInitialized := True;
end;

destructor TGogGalaxyPartWriter.Destroy;
begin
  if FInitialized then inflateEnd(FStream);
  inherited;
end;

procedure TGogGalaxyPartWriter.WriteBuffer(const Buffer; Count: Cardinal);
var
  Output: array[0..65535] of Byte;
  Produced, PreviousInput: Cardinal;
  Status: Integer;
begin
  if Count = 0 then Exit;
  if FFinished or (Count > FCompressedLeft) then InvalidGalaxy('trailing file part data');
  Dec(FCompressedLeft, Count);
  FStream.next_in := @Buffer;
  FStream.avail_in := Count;
  repeat
    PreviousInput := FStream.avail_in;
    FStream.next_out := @Output;
    FStream.avail_out := SizeOf(Output);
    Status := inflate(FStream, 0);
    if (Status <> 0) and (Status <> 1) and (Status <> -5) then
      InvalidGalaxy('corrupt zlib file part');
    Produced := SizeOf(Output) - FStream.avail_out;
    if Produced > FBytesLeft then InvalidGalaxy('uncompressed part size mismatch');
    if Produced > 0 then begin
      FDestination.WriteBuffer(Output, Produced);
      MD5Update(FFileHash^, Output, Produced);
      MD5Update(FPartHash, Output, Produced);
      Dec(FBytesLeft, Produced);
    end;
    if Status = 1 then begin
      FFinished := True;
      if FStream.avail_in <> 0 then InvalidGalaxy('trailing zlib data');
      Break;
    end;
    if (PreviousInput = FStream.avail_in) and (Produced = 0) then begin
      if FStream.avail_in <> 0 then InvalidGalaxy('invalid zlib file part');
      Break;
    end;
  until (FStream.avail_in = 0) and (FStream.avail_out <> 0);
end;

procedure TGogGalaxyPartWriter.Finish;
begin
  if not FFinished or (FCompressedLeft <> 0) or (FBytesLeft <> 0) then
    InvalidGalaxy('incomplete zlib file part');
  if not MD5DigestsEqual(MD5Final(FPartHash), FChecksum) then
    InvalidGalaxy('file part checksum mismatch');
end;

end.
