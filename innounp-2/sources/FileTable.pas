unit FileTable;

{ The names, sizes and visibility of the stored file entries as innounp
  presents them. By default every entry is shown as Inno Setup stored it.
  A TFileEntryProvider (for example the GOG Galaxy support) can take over
  entries whose real content is rebuilt from one or more stored entries:
  it gives them their real name and size, hides the entries that are only
  parts of them, and extracts them. }

interface

uses
  System.Classes, Struct, FileClass, Extract;

type
  TFileEntryProvider = class
  public
    procedure ExtractEntry(Index: Integer; Extractor: TFileExtractor;
      Destination: TFile; Progress: TExtractorProgressProc); virtual; abstract;
  end;

  TFileTable = class
  private
    type
      TEntry = record
        Name: String;
        Size: Int64;
        Hidden: Boolean;
        Provider: TFileEntryProvider;
      end;
    var
      FEntries: array of TEntry;
      FProviders: TList;
  public
    constructor Create(Files, Locations: TList);
    destructor Destroy; override;
    { The table owns Provider and frees it. }
    procedure AddProvider(Provider: TFileEntryProvider);
    { Lets Provider extract the entry, presented with Name and Size. }
    procedure Reconstruct(Index: Integer; Provider: TFileEntryProvider;
      const Name: String; Size: Int64);
    procedure Hide(Index: Integer);
    function Name(Index: Integer): String;
    function Size(Index: Integer): Int64;
    function IsHidden(Index: Integer): Boolean;
    { True if the entry is extracted by a provider rather than stored as is.
      Such entries are never skipped as duplicate copies of a stored file. }
    function IsReconstructed(Index: Integer): Boolean;
    procedure ExtractEntry(Index: Integer; Extractor: TFileExtractor;
      Destination: TFile; Progress: TExtractorProgressProc);
  end;

implementation

constructor TFileTable.Create(Files, Locations: TList);
var
  I: Integer;
  FileEntry: PSetupFileEntry;
begin
  inherited Create;
  FProviders := TList.Create;
  SetLength(FEntries, Files.Count);
  for I := 0 to Files.Count - 1 do begin
    FileEntry := PSetupFileEntry(Files[I]);
    FEntries[I].Name := FileEntry^.SourceFilename;
    if (FileEntry^.LocationEntry >= 0) and (FileEntry^.LocationEntry < Locations.Count) then
      FEntries[I].Size := PSetupFileLocationEntry(Locations[FileEntry^.LocationEntry])^.OriginalSize;
  end;
end;

destructor TFileTable.Destroy;
var
  I: Integer;
begin
  if FProviders <> nil then
    for I := 0 to FProviders.Count - 1 do
      TFileEntryProvider(FProviders[I]).Free;
  FProviders.Free;
  inherited;
end;

procedure TFileTable.AddProvider(Provider: TFileEntryProvider);
begin
  FProviders.Add(Provider);
end;

procedure TFileTable.Reconstruct(Index: Integer; Provider: TFileEntryProvider;
  const Name: String; Size: Int64);
begin
  FEntries[Index].Provider := Provider;
  FEntries[Index].Name := Name;
  FEntries[Index].Size := Size;
end;

procedure TFileTable.Hide(Index: Integer);
begin
  FEntries[Index].Hidden := True;
end;

function TFileTable.Name(Index: Integer): String;
begin
  Result := FEntries[Index].Name;
end;

function TFileTable.Size(Index: Integer): Int64;
begin
  Result := FEntries[Index].Size;
end;

function TFileTable.IsHidden(Index: Integer): Boolean;
begin
  Result := FEntries[Index].Hidden;
end;

function TFileTable.IsReconstructed(Index: Integer): Boolean;
begin
  Result := FEntries[Index].Provider <> nil;
end;

procedure TFileTable.ExtractEntry(Index: Integer; Extractor: TFileExtractor;
  Destination: TFile; Progress: TExtractorProgressProc);
begin
  FEntries[Index].Provider.ExtractEntry(Index, Extractor, Destination, Progress);
end;

end.
