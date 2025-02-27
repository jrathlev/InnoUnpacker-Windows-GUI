(* innounp, the Inno Setup Unpacker
   Version 2.64
     Supports Inno Setup versions 2.0.7 through 6.4

   based on:
     Version 0.50
     from
     https://sourceforge.net/projects/innounp/

   J. Rathlev (kontakt(a)rathlev-home.de) - December 2020

   changes required to compile innounp under Delphi 10 (Version 1.63 - August 2022):
   - Compiler conditional symbol UNICODE renamed to ISUNICODE
     reason: Delphi 10 uses a predefined global conditional symbol called UNICODE
     concerned: all "Struct*.pas" files
   - Unit Main: AnsiString in procedure "write"
   - Unit MyTypes: AnsiString in function "GetVersionBySetupId(const...)"
                   Many type declarations for "Unify" functions changed from
                   AnsiString to String
   - Unit PathFunc: function "PathLowercase"
   - Unit SetupLdr: pointer to PSetupLdrOffsetTable redifined
   - Unit SetupEntity: string conversion in SECompressedBlockRead
   - Unit RebuildScript: TBigString replaced by standard string
   - Units zlib40xx: PChar replaced by PAnsiChar
   - Encoding of fake file output

   further changes:
   - Command line option "-b" does no longer assume "-y" (overwrite) by default
     Use "-y" as additional switch if you want to overwrite
   - Fixed: Option oaSkip was not handled
   - new command "-i" to show a list of all supported Inno Setup versions
   - new option "-u" for UTF-8 output to console
   - Exit codes: 0 no errors
                 1 version not supported
                 2 corrupt or incompatible setup file
                 3 other error

   Version 1.64 - November 2022 : command line filelist supports Unicode and Utf8
           1.66 - August 2023   : bug fixes
           1.67 - January 2024  : bug fix for encrypted setups
           1.70 - June 2024     : support for InnoSetup 6.3
           1.71 - June 2024     : MyTypes, RebuildScript adapted to Inno Setup 6.3
                                  zlib4008 and zlib4017 fixed
           1.72 - July 2024     : Fixes issue on extracting zip compressed setups (2.0 - 4.1)
           1.73 - October 2024  : UTF-8 encoding of install_script.iss fixed [files]
           1.74 - October 2024  : UTF-8 encoding of install_script.iss fixed [setup]
           1.75 - November 2024 : new command line option to list languages (-l)
           1.76 - November 2024 : colored console output, optional suppressing header
           2.64 - January 2025  : new version numbering, support for Inno Setup 6.4
                                  EurekaLog directives removed
*)

program innounp;
{$APPTYPE CONSOLE}

{$IFDEF RELEASE}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.StrUtils, System.Math,
  MyTypes in 'MyTypes.pas',
  StructJoin in 'StructJoin.pas',
  Struct in 'Struct.pas',
  Msgs in 'Msgs.pas',
  MsgIds in 'MsgIds.pas',
  PathFunc in 'PathFunc.pas',
  CmnFunc2 in 'CmnFunc2.pas',
  SetupEnt in 'SetupEnt.pas',
  Extract in 'Extract.pas',
  Extract4000 in 'Extract4000.pas',
  Extract6400 in 'Extract6400.pas',
  Main in 'Main.pas',
  Int64Em in 'Int64Em.pas',
  InstFunc in 'InstFunc.pas',
  ZLib in 'zlib.pas',
  BZlib in 'bzlib.pas',
  Compression.LZMADecompressor in 'Compression.LZMADecompressor.pas',
  Compression.LZMA1SmallDecompressor in 'Compression.LZMA1SmallDecompressor.pas',
  Compress in 'Compress.pas',
  RebuildScript in 'RebuildScript.pas',
  FileClass in 'FileClass.pas',
  CallOptimizer in 'CallOptimizer.pas',
  FileNull in 'FileNull.pas',
  SetupLdr in 'SetupLdr.pas',
  CustomVersions in 'CustomVersions.pas',
{$I CompressList.inc}
  ;

{$R innounp.res}
{$R version.res}

const
{$I VersionInfo.inc}

type
  TCompressedBlockReaderClass = class of TAbstractBlockReader;
  TOverwriteAction = (oaOverwrite, oaSkip, oaAsk, oaAbort);
var
  WarnOnMod:boolean=false;
  IsUnknownVersion:boolean=false;
  OverwriteAction: TOverwriteAction;

// Run parameters
  Filelist,
  SetupFileName: string;
  FileMasks: array of string;
  Password: string;
  CommandAction: TCommandAction;
  StripPaths:boolean=false;
  OutDir:string='';
  BaseDirToStrip:string='';
  AttemptUnpackUnknown:boolean=true;
  ExtractAllCopies:boolean=false;
  AutoYes: Boolean = false;
  ExtractTestOnly: Boolean = false;
  QuietExtract: Boolean = false;
  ExtractEmbedded: Boolean = false;
  NoHeader: Boolean = false;
  InteractiveMode: Boolean = true;
  ExitCode : integer;

function FindVerObject(var IsUnknown: boolean) : TInnoVer;
var
  tmpVer: integer;
  i: integer;
begin
  Result := nil;
  IsUnknown := true;

  // ISX versions currently are not supported
  if (VerIsISX) then Exit;

  // First try to find direct match
  for i := 0 to High(VerList) do
    if (VerList[i].VerSupported = Ver) and (VerList[i].IsUnicode = VerIsUnicode) and (VerList[i].IsRT = VerIsRT) then
    begin
      Result := VerList[i];
      IsUnknown := false;
      break;
    end;

  if (Result = nil) and AttemptUnpackUnknown then
  begin
    // Try to find closest match (version higher or equal)
    tmpVer := 0;
    for i:=0 to High(VerList) do with VerList[i] do
      if (Ver >= VerSupported) and (VerSupported >= tmpVer) and (IsUnicode=VerIsUnicode) then
      begin
        Result := VerList[i];
        tmpVer := VerSupported;
      end;
  end;
end;

function IsCompatibleVersion(const TestID: TSetupID; var VerObject: TInnoVer):boolean;
const
  SignBegin='Inno Setup Setup Data (';
  SignBeginIsx='My Inno Setup Extensions Setup Data (';
  Digits=['0'..'9'];
var
  s:string;
  i,j:integer;
  Ver1,Ver2,Ver3:integer;
  FileVersion: String;
begin
  s:=TestID; Result:=false;
  if pos(SignBegin,s)=1 then i:=length(SignBegin)+1
  else if pos(SignBeginIsx,s)=1 then i:=length(SignBeginIsx)+1
  else exit;
  if not (s[i] in ['1','2','3','4','5','6']) or (s[i+1]<>'.') then exit;
  j:=i; while (j<=sizeof(TSetupID)) and (s[j]<>')') do inc(j);
  if (j>sizeof(TSetupID)) then exit;
  FileVersion:=copy(s,i,j-i);
  if FileVersion='3.0.6.1' then FileVersion:='3.0.8'; // ugly hack to support ISX 3.0.6.1 at low cost
  if not ((FileVersion[1] in Digits) and (FileVersion[2]='.')) then exit;
  Val(copy(FileVersion,1,1),Ver1,i); if i<>0 then exit;
  Val(copy(FileVersion,3,1),Ver2,i); if i<>0 then exit;
  if FileVersion[4]<>'.' then exit;
  j:=5; while (j<=length(FileVersion)) and (FileVersion[j] in Digits) do inc(j);
  Val(copy(FileVersion,5,j-5),Ver3,i); if i<>0 then exit;
  if (j<=length(FileVersion)) then WarnOnMod:=true;
  Ver:=Ver1*1000+Ver2*100+Ver3;

  if Ver>=6300 then VerIsUnicode := true    // always unicode
  else VerIsUnicode := (PosEx('(u)', s, Length(SignBegin)) > 0) or (PosEx('(U)', s, Length(SignBegin)) > 0);
  VerIsRT := CheckResToolsVersion(SetupFileName);
  VerIsISX := PosEx('with ISX', s, Length(SignBegin)) > 0;

  VerObject := FindVerObject(IsUnknownVersion);
  Result := VerObject <> nil;
end;

function GetBlockReaderClass(InnoVer: integer) : TCompressedBlockReaderClass;
begin
  Result := nil;

  if (InnoVer <= 4008) then Result := TZlibBlockReader4008
  else if (InnoVer >= 4009) and (InnoVer <= 4105) then Result := TZlibBlockReader4107
  // version mismatch (4105 vs. 4107) is ok
  else if (InnoVer > 4105) then Result := Compress.TCompressedBlockReader;
end;

procedure CreateEntryLists;
var
  I: TEntryType;
begin
  for I := Low(I) to High(I) do
    Entries[I] := TList.Create;

  WizardImages := TStringList.Create;
  WizardSmallImages := TStringList.Create;
end;

procedure ReleaseEntryList;
var
  I: TEntryType;
begin
  for I := Low(I) to High(I) do Entries[I].Free;
  WizardImages.Free;
  WizardSmallImages.Free;
  end;

procedure SetupLdr;
var
  SourceF: TFile;
  OffsetTable: TSetupLdrOffsetTable;
  IsCompatible: boolean;
  TestID: TSetupID;
  VerObject: TInnoVer;
  s : string;
begin
  SetupLdrOriginalFilename:=SetupFileName;
  try
    SourceF := TFile.Create(SetupFileName, fdOpenExisting, faRead, fsRead);
    try
      if not (GetSetupLdrOffsetTableFromFile(SourceF, OffsetTable) or
        GetSetupLdrOffsetTableFromResource(SetupFileName, SourceF, OffsetTable)) then
      begin
        if SourceF.CappedSize<sizeof(TestID) then SetupCorruptError;
        OffsetTable.Offset0:=0;
        OffsetTable.Offset1:=0;
        SetupLdrMode:=false;
      end;
      SourceF.Seek(OffsetTable.Offset0);
      SourceF.ReadBuffer(TestID, SizeOf(TestID));

      IsCompatible:=IsCompatibleVersion(TestID, VerObject);
      if AttemptUnpackUnknown and IsUnknownVersion and IsCompatible then begin
        WriteNormalLine('=> Signature detected: ',TestID);
        WriteNormalLine('This is not directly supported, but i''ll try to unpack it as version ',IntToStr(VerObject.VerSupported));
      end;
      if not IsCompatible then begin
        if not SetupLdrMode then SetupCorruptError;
        WriteNormalText('=> Signature detected: ',TestID);
        WriteErrorLine(' - This is not a supported version');
        raise EFatalError.Create('1');
      end else if IsVersionSuspicious(Ver) then
        WriteNormalLine(ExtSp('Inno Setup Version specified:',TextAlign),VersionToString(Ver))
      else begin
        s:=VersionToString(Ver);
        if (VerIsUnicode) then s:=s+' (Unicode)';
        if (VerIsRT) then s:=s+' (Custom)';
        WriteNormalLine(ExtSp('Inno Setup version detected: ',TextAlign),s);
      end;
      if WarnOnMod then WriteNormalLine('=> Signature: '+TestID);

// Extract the embedded setup exe      // causes problems. disabled for now.
      {if ExtractEmbedded then begin
        SourceF.Seek(OffsetTable.OffsetEXE);
        P := nil;

        try
          GetMem(P, OffsetTable.UncompressedSizeEXE);
          FillChar(P^, OffsetTable.UncompressedSizeEXE, 0);
          try
            Reader := TCompressedBlockReader.Create(SourceF, TLZMADecompressor);
            try
              Reader.Read(P^, OffsetTable.UncompressedSizeEXE);
            finally
              Reader.Free;
            end;
          except
            on ECompressDataError do
              SetupCorruptError;
          end;
          TransformCallInstructions(P^, OffsetTable.UncompressedSizeEXE, False);
          if GetCRC32(P^, OffsetTable.UncompressedSizeEXE) <> OffsetTable.CRCEXE then
            SetupCorruptError;
        finally
          SetLength(s,OffsetTable.UncompressedSizeEXE);
          Move(p^,s[1],OffsetTable.UncompressedSizeEXE);
          FreeMem(P);
          AddFakeFile('embedded\setup.exe',s);
        end;
      end;}
    finally
      FreeAndNil(SourceF);
    end;
    SetupLdrOffset0 := OffsetTable.Offset0;
    SetupLdrOffset1 := OffsetTable.Offset1;
  except
    on E:Exception do begin
      if E is EMessage then raise;
      if E is EFatalError then raise;

      WriteFormatLine('Cannot open or read the specified file: ','"'+SetupFileName+'"',clRed,clBlue);
      if (E is EInOutError) then
        WriteErrorLine('  ==> '+SysErrorMessage(EInOutError(E).ErrorCode))
      else if (E is EFileError) then
        WriteErrorLine('  ==> '+SysErrorMessage(EFileError(E).ErrorCode))
      else begin
        WriteNormalLine('');
        WriteErrorLine('Exception class '+E.ClassName+' with message: ',E.Message);
      end;  
      raise EFatalError.Create('1');
    end;
  end;
end;

procedure AbortInit(const Msg: TSetupMessageID);
begin
  WriteFormatLine('Critical error: ',SetupMessages[Msg],clRed,clBlue);
//  MsgBox(SetupMessages[Msg], '', mbCriticalError, MB_OK);
  Abort;
end;

procedure InitializeSetup;
var
  SetupFile: TFile;
  pFileEntry: Struct.PSetupFileEntry;
  pFileLocationEntry: PSetupFileLocationEntry;
  pRegistryEntry: PSetupRegistryEntry;
  pRunEntry: PSetupRunEntry;
  pIconEntry: PSetupIconEntry;
  pTaskEntry: PSetupTaskEntry;
  pComponentEntry: PSetupComponentEntry;
  pTypeEntry: PSetupTypeEntry;
  pCustomMessageEntry: PSetupCustomMessageEntry;
  pLanguageEntry: PSetupLanguageEntry;
  pDirEntry: PSetupDirEntry;
  pIniEntry: PSetupIniEntry;
  pDeleteEntry: PSetupDeleteEntry;
  RealReader: TAbstractBlockReader;
  Reader: TCacheReader;
  p:pointer;
  i:integer;

  procedure ReadString(const R: TAbstractBlockReader; var Str: AnsiString);
  var
    Len: Longint;
  begin
    R.Read(Len, SizeOf(Len));
    SetLength(Str, Len);
    R.Read((@Str[1])^, Len);
  end;

  procedure SkipEntries(const Count, Size, Strings, AnsiStrings: Integer);
  var
    i: integer;
  begin
    for i:=0 to Count-1 do
      SECompressedBlockSkip(Reader, Size, Strings, AnsiStrings);
  end;

  procedure ReadImageList(const R: TAbstractBlockReader; DestList: TStrings);
  var
    i, Len: Longint;
    Img: AnsiString;
  begin
    // In version 5.6.0 IS changed single wizard image to multiple images
    // but version signature was not changed (still 5.5.7)
    // Bacause of this (BAD decision) we have to rely on this heuristics to
    // distinguish if with have one image of several ones
    R.Read(Len, SizeOf(Len));
    if (Len < 20) then // this is probably an image counter
      for i := 0 to Len - 1 do
      begin
        ReadString(R, Img);
        DestList.Add(Img);
      end
    else begin
      // It looks like we have single image and we've already read its length
      SetLength(Img, Len);
      R.Read((@Img[1])^, Len);
      DestList.Add(Img);
    end;
  end;

  procedure ReadWizardAndBzipDll;
  begin
    { Wizard image }
    ReadImageList(Reader, WizardImages);

    if (Ver >= 2001) then
    begin
      ReadImageList(Reader, WizardSmallImages);
    end;
    
    { Decompressor DLL }
    DecompDll:='';
    if (SetupHeader.CompressMethod = cmBzip) or
       ((SetupHeader.CompressMethod = cmLZMA) and (Ver=4105)) or
       ((SetupHeader.CompressMethod = cmZip) and (Ver>=4206)) then
    begin
      ReadString(Reader, DecompDll);
    end;
  end;
var
  VerObject: TInnoVer;
  TestID: TSetupID;
  TheBlockReader: TCompressedBlockReaderClass;
begin
  { Read SETUP.0, or from EXE }

  // If SetupLdrMode=false, we're running on setup.0 (whatever the actual name is}
  // because the 'real' setup.exe (i.e. not the SetupLdr exe) would not pass the check
  // in SetupLdr proc
  // As a bonus, since 4.1.7 setup.0 must have the same base name as setup.exe
//  SetupFilename := SetupLdrOriginalFilename;

  SetupFile := TFile.Create(SetupFilename, fdOpenExisting, faRead, fsRead);
  try
    SetupFile.Seek(SetupLdrOffset0);
    if SetupFile.Read(TestID, SizeOf(TestID)) <> SizeOf(TestID) then
      AbortInit(msgSetupFileCorruptOrWrongVer);
    try
      TheBlockReader := GetBlockReaderClass(Ver);

      RealReader := TheBlockReader.Create(SetupFile, TLZMA1SmallDecompressor);
      Reader := TCacheReader.CreateCache(RealReader);
      
      if IsVersionSuspicious(Ver) then begin
        HeuristicVersionFinder(Reader, Ver);
        WriteNormalText('; Version detected: ',IntToStr(Ver));
        if (VerIsUnicode) then WriteNormalLine(' (Unicode)') else writeln('');
      end;
      VerObject := FindVerObject(IsUnknownVersion);
//      if VerObject=nil then exit; // that should not happen

      VerObject.SetupSizes;
      try
        { Header }
        p:=AllocMem(SetupHeaderSize);
        SECompressedBlockRead(Reader, p^, SetupHeaderSize, SetupHeaderStrings, SetupHeaderAnsiStrings);
        VerObject.UnifySetupHeader(p^, SetupHeader);
        SetUpMinVersion:=TSetupVersionData(SetupHeader.MinVersion);
        FreeMem(p);
        if Ver<4000 then begin // language options, wizard images and compressor dll are stored here in 3.x
          SECompressedBlockSkip(Reader, SetupLanguageEntrySize, SetupLanguageEntryStrings, SetupLanguageEntryAnsiStrings);
          ReadWizardAndBzipDll;
        end;
        
        { Language entries }
        if Ver>=4000 then begin
          p:=AllocMem(SetupLanguageEntrySize);
          for i:=0 to SetupHeader.NumLanguageEntries-1 do begin
            SECompressedBlockRead(Reader, p^, SetupLanguageEntrySize, SetupLanguageEntryStrings, SetupLanguageEntryAnsiStrings);
            pLanguageEntry:=AllocMem(sizeof(TSetupLanguageEntry));
            VerObject.UnifyLanguageEntry(p^, pLanguageEntry^);
            Entries[seLanguage].Add(pLanguageEntry);
          end;
          FreeMem(p);
        end;

        { CustomMessage entries }
        if Ver>=4201 then
        begin
          p:=AllocMem(SetupCustomMessageEntrySize);
          for i:=0 to SetupHeader.NumCustomMessageEntries-1 do begin
            SECompressedBlockRead(Reader, p^, SetupCustomMessageEntrySize,
              SetupCustomMessageEntryStrings, SetupCustomMessageEntryAnsiStrings);
            pCustomMessageEntry:=AllocMem(sizeof(TSetupCustomMessageEntry));
            VerObject.UnifyCustomMessageEntry(p^, pCustomMessageEntry^);
            Entries[seCustomMessage].Add(pCustomMessageEntry);
          end;
          FreeMem(p);
        end;
        
        { Permission entries }
        if Ver>=4100 then
          SkipEntries(SetupHeader.NumPermissionEntries, SetupPermissionEntrySize,
            SetupPermissionEntryStrings, SetupPermissionEntryAnsiStrings);
            
        { Type entries }
        if (SetupTypeEntrySize > 0) then
        begin
          p:=AllocMem(SetupTypeEntrySize);
          for i:=0 to SetupHeader.NumTypeEntries-1 do begin
            SECompressedBlockRead(Reader, p^, SetupTypeEntrySize, SetupTypeEntryStrings, SetupTypeEntryAnsiStrings);
            pTypeEntry:=AllocMem(sizeof(TSetupTypeEntry));
            VerObject.UnifyTypeEntry(p^, pTypeEntry^);
            Entries[seType].Add(pTypeEntry);
          end;
          FreeMem(p);
        end;

        { Component entries }
        if (SetupComponentEntrySize > 0) then
        begin
          p:=AllocMem(SetupComponentEntrySize);
          for i:=0 to SetupHeader.NumComponentEntries-1 do begin
            SECompressedBlockRead(Reader, p^, SetupComponentEntrySize, SetupComponentEntryStrings, SetupComponentEntryAnsiStrings);
            pComponentEntry:=AllocMem(sizeof(TSetupComponentEntry));
            VerObject.UnifyComponentEntry(p^, pComponentEntry^);
            Entries[seComponent].Add(pComponentEntry);
          end;
          FreeMem(p);
        end;

        { Task entries }
        if (SetupTaskEntrySize > 0) then
        begin
          p:=AllocMem(SetupTaskEntrySize);
          for i:=0 to SetupHeader.NumTaskEntries-1 do begin
            SECompressedBlockRead(Reader, p^, SetupTaskEntrySize, SetupTaskEntryStrings, SetupTaskEntryAnsiStrings);
            pTaskEntry:=AllocMem(sizeof(TSetupTaskEntry));
            VerObject.UnifyTaskEntry(p^,pTaskEntry^);
            Entries[seTask].Add(pTaskEntry);
          end;
          FreeMem(p);
        end;

        { Dir entries }
        p:=AllocMem(SetupDirEntrySize);
        for i:=0 to SetupHeader.NumDirEntries-1 do begin
          SECompressedBlockRead(Reader, p^, SetupDirEntrySize, SetupDirEntryStrings, SetupDirEntryAnsiStrings);
          pDirEntry:=AllocMem(sizeof(TSetupDirEntry));
          VerObject.UnifyDirEntry(p^,pDirEntry^);
          Entries[seDir].Add(pDirEntry);
        end;
        FreeMem(p);

        { File entries }
        p:=AllocMem(SetupFileEntrySize);
        for i:=0 to SetupHeader.NumFileEntries-1 do begin
          SECompressedBlockRead(Reader, p^, SetupFileEntrySize, SetupFileEntryStrings, SetupFileEntryAnsiStrings);
          pFileEntry:=AllocMem(sizeof(TSetupFileEntry));
          VerObject.UnifyFileEntry(p^,pFileEntry^);
          Entries[seFile].Add(pFileEntry);
//          Reader.Read(n,1);  // required for BCompareSetup.exe (Beyond Compare Vers. 5)
        end;
        FreeMem(p);

        { Icon entries }
        p:=AllocMem(SetupIconEntrySize);
        for i:=0 to SetupHeader.NumIconEntries-1 do begin
          SECompressedBlockRead(Reader, p^, SetupIconEntrySize, SetupIconEntryStrings, SetupIconEntryAnsiStrings);
          pIconEntry:=AllocMem(sizeof(TSetupIconEntry));
          VerObject.UnifyIconEntry(p^,pIconEntry^);
          Entries[seIcon].Add(pIconEntry);
        end;
        FreeMem(p);

        { INI entries }
        p:=AllocMem(SetupIniEntrySize);
        for i:=0 to SetupHeader.NumIniEntries-1 do begin
          SECompressedBlockRead(Reader, p^, SetupIniEntrySize, SetupIniEntryStrings, SetupIniEntryAnsiStrings);
          pIniEntry:=AllocMem(sizeof(TSetupIniEntry));
          VerObject.UnifyIniEntry(p^,pIniEntry^);
          Entries[seIni].Add(pIniEntry);
        end;
        FreeMem(p);

        { Registry entries }
        p:=AllocMem(SetupRegistryEntrySize);
        for i:=0 to SetupHeader.NumRegistryEntries-1 do begin
          SECompressedBlockRead(Reader, p^, SetupRegistryEntrySize, SetupRegistryEntryStrings, SetupRegistryEntryAnsiStrings);
          pRegistryEntry:=AllocMem(sizeof(TSetupRegistryEntry));
          VerObject.UnifyRegistryEntry(p^,pRegistryEntry^);
          Entries[seRegistry].Add(pRegistryEntry);
        end;
        FreeMem(p);

        { InstallDelete entries }
        p:=AllocMem(SetupDeleteEntrySize);
        for i:=0 to SetupHeader.NumInstallDeleteEntries-1 do begin
          SECompressedBlockRead(Reader, p^, SetupDeleteEntrySize, SetupDeleteEntryStrings, SetupDeleteEntryAnsiStrings);
          pDeleteEntry:=AllocMem(sizeof(TSetupDeleteEntry));
          VerObject.UnifyDeleteEntry(p^, pDeleteEntry^);
          Entries[seInstallDelete].Add(pDeleteEntry);
        end;
        FreeMem(p);

        { UninstallDelete entries }
        p:=AllocMem(SetupDeleteEntrySize);
        for i:=0 to SetupHeader.NumUninstallDeleteEntries-1 do begin
          SECompressedBlockRead(Reader, p^, SetupDeleteEntrySize, SetupDeleteEntryStrings, SetupDeleteEntryAnsiStrings);
          pDeleteEntry:=AllocMem(sizeof(TSetupDeleteEntry));
          VerObject.UnifyDeleteEntry(p^, pDeleteEntry^);
          Entries[seUninstallDelete].Add(pDeleteEntry);
        end;
        FreeMem(p);

        { Run entries }
        p:=AllocMem(SetupRunEntrySize);
        for i:=0 to SetupHeader.NumRunEntries-1 do begin
          SECompressedBlockRead(Reader, p^, SetupRunEntrySize, SetupRunEntryStrings, SetupRunEntryAnsiStrings);
          pRunEntry:=AllocMem(sizeof(TSetupRunEntry));
          VerObject.UnifyRunEntry(p^,pRunEntry^);
          Entries[seRun].Add(pRunEntry);
        end;
        FreeMem(p);
        
        { UninstallRun entries }
        p:=AllocMem(SetupRunEntrySize);
        for i:=0 to SetupHeader.NumUninstallRunEntries-1 do begin
          SECompressedBlockRead(Reader, p^, SetupRunEntrySize, SetupRunEntryStrings, SetupRunEntryAnsiStrings);
          pRunEntry:=AllocMem(sizeof(TSetupRunEntry));
          VerObject.UnifyRunEntry(p^,pRunEntry^);
          Entries[seUninstallRun].Add(pRunEntry);
        end;
        FreeMem(p);

        if Ver>=4000 then ReadWizardAndBzipDll;

        RealReader.Free; Reader.Free;

        RealReader := TheBlockReader.Create(SetupFile, TLZMA1Decompressor);
        Reader:=TCacheReader.CreateCache(RealReader);

        { File location entries }
        p:=AllocMem(SetupFileLocationEntrySize);
        for i:=0 to SetupHeader.NumFileLocationEntries-1 do begin
          SECompressedBlockRead(Reader, p^, SetupFileLocationEntrySize,
            SetupFileLocationEntryStrings, SetupFileLocationEntryAnsiStrings);
          pFileLocationEntry:=AllocMem(sizeof(TSetupFileLocationEntry));
          VerObject.UnifyFileLocationEntry(p^,pFileLocationEntry^);
          if Ver<4000 then with pFileLocationEntry^ do begin
            Dec(FirstSlice);
            Dec(LastSlice);
          end;
          Entries[seFileLocation].Add(pFileLocationEntry);
        end;
        FreeMem(p);

        for i:=0 to Entries[seFileLocation].Count-1 do
          if foTimeStampInUTC in PSetupFileLocationEntry(Entries[seFileLocation][i])^.Flags then begin
            TimeStampsInUTC:=true;
            break;
          end;

      finally
        Reader.Free; RealReader.Free;
      end;
    except
      on ECompressDataError do
        AbortInit(msgSetupFileCorrupt);
    end;
  finally
    FreeAndNil(SetupFile);
  end;
end;

function AskFileOverwrite(DestFile: string) : boolean;
var
  Resp: string;
begin
  Result := (OverwriteAction = oaOverwrite);
  if (OverwriteAction <> oaAsk) then Exit;
  Writeln('File "' + DestFile + '" already exists. Overwrite?');
  repeat
    Write('(Y)es / (N)o / (A)lways / (S)kip all / (Q)uit ?');
    Readln(Resp);
    if (Resp = '') then continue;

    case UpCase(Resp[1]) of
      'Y': begin Result := True; break; end;
      'N': break;
      'A': begin Result := True; OverwriteAction := oaOverwrite; break; end;
      'S': begin OverwriteAction := oaSkip; break; end;
      'Q': begin OverwriteAction := oaAbort; break; end;
    end;
  until False;
end;

procedure ExtractorProgressProc(Bytes: Cardinal);
begin
end;

function ProcessFileEntry(const Caption : string; const FileExtractor: TFileExtractor;
  const CurFile: Struct.PSetupFileEntry) : boolean;
var
  CurFileLocation: PSetupFileLocationEntry;
  DestFile, TempFile: String;
  DestF: TFile;
  CurFileDate: TFileTime;
  se,
  s,PasswdStr: String;
  sr : RawByteString;
  ck : TSetupEncryptionKey;
begin
  Result:=true;
  if CurFile^.LocationEntry = -1 then Exit;

  CurFileLocation := PSetupFileLocationEntry(Entries[seFileLocation][CurFile^.LocationEntry]);
  DestFile:=CurFile^.SourceFileName;

  if length(BaseDirToStrip)>0 then delete(DestFile,1,length(BaseDirToStrip));
  if StripPaths then DestFile:=ExtractFileName(DestFile);
  if not QuietExtract then begin
    // Check if file already exists and ask if necessary
    if (InteractiveMode) then begin          // changes: JR - August 2020
      if (OverwriteAction = oaSkip) and FileExists(DestFile) then begin
        WriteBlueLine(Caption+' - skipped'); Exit;
        end;
      if (OverwriteAction = oaAsk) and FileExists(DestFile) then
        if not AskFileOverwrite(DestFile) then Exit;
      end
    else if (OverwriteAction <> oaOverwrite) and FileExists(DestFile) then begin
      WriteBlueLine(Caption+' - skipped'); Exit;
      end
    end;
  // Ask password if file is encrypted
  if (foChunkEncrypted in CurFileLocation^.Flags) and (FileExtractor.CryptKey = '') then begin
    if InteractiveMode then begin
      repeat
        writeln('Type in a password (empty string to quit)');
        readln(PasswdStr);
        if (PasswdStr = '') then begin
          Result:=false; break;
          end;
        if (Ver < 6400) then begin
          PasswdStr := FixPasswordEncoding(PasswdStr); // For proper Unicode/Ansi support
          if TestPassword(PasswdStr) then begin
            FileExtractor.CryptKey := PasswdStr;
            break;
            end;
          end
        else begin    // new password handling in IS version 6.4
          with SetupHeader.Is64Encryption do
            GenerateEncryptionKey(PasswdStr, EncryptionKDFSalt, EncryptionKDFIterations, ck);
          if TestPasswordIs64(ck) then begin
            TFileExtractor6400(FileExtractor).Is64CryptKey:=ck;
            FileExtractor.CryptKey := PasswdStr;
            break;
            end;
          end;
        WriteErrorLine('Wrong password');
        until false;
      Writeln('');
      end
    else begin
      WriteErrorLine('Password required to read encrypted file');
      Result:=false;
      end;
    end;
  if not Result then Exit;
  WriteNormalText(Caption);
  if (ExtractTestOnly) then DestF := TNullFile.Create()
  else begin
    TempFile := GenerateUniqueName(ExtractFilePath(ExpandFilename(DestFile)), '.tmp');
    MakeDir(ExtractFilePath(TempFile));
    DestF := TFile.Create(TempFile, fdCreateAlways, faWrite, fsNone);
  end;
  se:='';
  try
    try
      if CurFile^.FileType<>ftFakeFile then begin
        { Decompress a file }
        FileExtractor.SeekTo(CurFileLocation^);
        try
          FileExtractor.DecompressFile(CurFileLocation^, DestF, ExtractorProgressProc);
        except
          on E:Exception do se:=E.Message;
          end;
      end else begin
        s:=CurFileLocation^.Contents;
//        len:=Length(s);
        sr:=s;
//        SetLength(sr,len*SizeOf(Char)+1);
//        len:=UnicodeToUtf8(PAnsiChar(sr),Length(sr),PWideChar(s),len);
//        SetLength(sr,len);
  // Add UTF-8 BOM to script start for Unicode versions.
//        if (VerIsUnicode) then sr:=#$EF#$BB#$BF+sr;
        DestF.WriteBuffer(sr[1],length(sr));
      end;

      { Set time/date stamp }
      CurFileDate:=CurFileLocation^.TimeStamp;
      if not TimeStampsInUTC then LocalFileTimeToFileTime(CurFileLocation^.TimeStamp, CurFileDate);
      SetFileTime(DestF.Handle, @CurFileDate, nil, @CurFileDate);
    finally
      FreeAndNil(DestF);
    end;

    if (not ExtractTestOnly) then begin
      DeleteFile(DestFile);
      if (length(ExtractFileName(DestFile))>0) and not RenameFile(TempFile,DestFile) then Win32ErrorMsg('MoveFile');
      end;
  finally
    DeleteFile(TempFile);
  end;
  if length(se)>0 then begin
    writeln; WriteErrorLine('  *** '+se);
    end
  else if ExtractTestOnly then WriteHighLightLine(' - checked')
  else if not QuietExtract then  WriteHighLightLine(' - extracted');
  end;

function ShouldProcessFileEntry(AFile: Struct.PSetupFileEntry):boolean;
var
  i:integer;
  FilePath,MaskPath,FileName,MaskName:string;
begin
  Result:=false;
  if not ExtractEmbedded and (AFile^.FileType in [ftUninstExe, ftRegSvrExe]) then exit;
  if (AFile^.LocationEntry=-1) then exit;
  if length(BaseDirToStrip)>0 then
    if pos(BaseDirToStrip, AnsiLowercase(AFile^.SourceFileName))<>1 then exit;
  if length(FileMasks)>0 then begin
    for i:=0 to High(FileMasks) do begin
      MaskPath:=AnsiLowercase(PathExtractPath(FileMasks[i]));
      FilePath:=AnsiLowercase(PathExtractPath(AFile^.SourceFileName)); //AFile^.DestDir));
      FileName:=AnsiLowercase(PathExtractName(AFile^.SourceFileName));
//      if AFile^.DestName<>'' then FileName:=AnsiLowercase(PathExtractName(AFile^.DestName));
      MaskName:=AnsiLowercase(PathExtractName(FileMasks[i]));
      if MaskPath ='' then
        Result:=WildcardMatch(PChar(FileName),PChar(AnsiLowercase(FileMasks[i])))
      else if IsWildcard(MaskName) then
        Result := WildcardMatch(PChar(FilePath + FileName), PChar(MaskPath + MaskName))
      else
        Result := WildcardMatch(PChar(FilePath), PChar(MaskPath)) and (FileName = MaskName);
//      else Result:=false;
      if Result then break;
    end;
  end else Result:=true;
end;

procedure ExtractorNotification(MessageText: String);
begin
  if (not QuietExtract) then WriteErrorLine(MessageText);
end;

function CreateFileExtractor : TFileExtractor;
var
  ck : TSetupEncryptionKey;
const
  DecompClasses: array[TSetupCompressMethod] of TCustomDecompressorClass =
    (TStoredDecompressor, TZDecompressor, TBZDecompressor, TLZMA1Decompressor, TLZMA2Decompressor);
begin
  if (Ver <= 4000) then
    Result := TFileExtractor4000.Create(TZDecompressor)
  else if (Ver < 6400) then
    Result := TFileExtractor.Create(DecompClasses[SetupHeader.CompressMethod])
  else
    Result := TFileExtractor6400.Create(DecompClasses[SetupHeader.CompressMethod]);

//  Result.OnNotification := ExtractorNotification;
  Result.Interactive := InteractiveMode;

  if (Ver < 6400) then begin
    Password := FixPasswordEncoding(Password);  // For proper Unicode/Ansi support
    if SetupHeader.EncryptionUsed and (Password <> '') then begin
      if TestPassword(Password) then
        Result.CryptKey := Password
      else if (InteractiveMode) then
        WriteErrorLine('Warning: ',SetupMessages[msgIncorrectPassword])
      else
        AbortInit(msgIncorrectPassword);
      end
    end
  else begin    // new password handling in IS version 6.4
    if SetupHeader.EncryptionUsed and (Password <> '') then begin
      with SetupHeader.Is64Encryption do
        GenerateEncryptionKey(Password, EncryptionKDFSalt, EncryptionKDFIterations, ck);
      if TestPasswordIs64(ck) then begin
        TFileExtractor6400(Result).Is64CryptKey:=ck;
        Result.CryptKey := Password;
        end
      else if (InteractiveMode) then WriteErrorLine('Warning: ',SetupMessages[msgIncorrectPassword])
      else AbortInit(msgIncorrectPassword);
      end;
    end;
  end;

procedure CopyFiles;
{ Copies all the application's files }
var
  CurFileNumber: Integer;
  CurFile: Struct.PSetupFileEntry;
  loc:PSetupFileLocationEntry;
  FileExtractor: TFileExtractor;
begin
  OverwriteAction := oaAsk;
  if (AutoYes) then OverwriteAction := oaOverwrite;

  FileExtractor := CreateFileExtractor;
  try
    for CurFileNumber := 0 to Entries[seFile].Count-1 do begin
      CurFile := Struct.PSetupFileEntry(Entries[seFile][CurFileNumber]);
      if not ShouldProcessFileEntry(CurFile) then continue;
      loc:=PSetupFileLocationEntry(Entries[seFileLocation][CurFile^.LocationEntry]);
      if not ExtractAllCopies and (loc^.PrimaryFileEntry<>-1) and (loc^.PrimaryFileEntry<>CurFileNumber) then continue;
      with CurFile^ do begin
        if LocationEntry <> -1 then
          if not ProcessFileEntry('#'+IntToStr(CurFileNumber)+' '+SourceFilename,FileExtractor,CurFile) then break;
        end;

      if (OverwriteAction = oaAbort) then break;
    end;  //for
  finally
    FileExtractor.Free;
  end;
end;

procedure Usage;
begin
  writeln('Usage: innounp [command] [options] <setup.exe or setup.0> [@filelist] [filemask ...]');
  writeln('Commands:');
  writeln('  (no)   display general information about the installation archive');
  writeln('  -v     verbosely list the files (with sizes and timestamps)');
  writeln('  -x     extract the files from the installation (to the current directory, also see -d)');
  writeln('  -e     extract files without paths');
  writeln('  -t     test files for integrity');
  writeln('  -l     show list of languages supported by the installation');
  writeln('  -i     show list of all supported Inno Setup versions');
  writeln('Options:');
  writeln('  -b     batch (non-interactive) mode - will not prompt for password or disk changes');
  writeln('  -q     do not indicate progress while extracting');
  writeln('  -m     extract internal embedded files (such as license and uninstall.exe)');
  writeln('  -pPASS decrypt the installation with a password');
  writeln('  -dDIR  extract the files into DIR (can be absolute or relative path)');
  writeln('  -cDIR  specifies that DIR is the current directory in the installation');
  writeln('  -n     don''t attempt to unpack new versions');
  writeln('  -fFILE same as -p but reads the password from FILE');
  writeln('  -a     extract all copies of duplicate files');
  writeln('  -y     assume Yes on all queries (e.g. overwrite files)');
  writeln('  -o     no colored console output');
  writeln('  -h     do not display headline with program info');
  writeln('  -u     use UTF-8 for console output');
end;

function ParseCommandLine : integer;
var
  InstallNameParsed  : boolean;
  s,PasswordFileName : string;
  i  : integer;
begin
  Result:=-1; CommandAction:=caInstallInfo; InstallNameParsed:=false; SetLength(FileMasks,0);
  PasswordFileName:=''; ExitCode:=0;
  ExtractTestOnly := false; Filelist:='';
  if (ParamCount<1) then exit;
  for i:=1 to ParamCount do begin
    if (ParamStr(i)[1]='-') and (length(ParamStr(i))>=2) then begin
      case UpCase(ParamStr(i)[2]) of
        'A': ExtractAllCopies:=true;
        'B': InteractiveMode:=false;    // changes: JR - August 2020
        'C': BaseDirToStrip:=PathLowercase(AddBackslash(copy(ParamStr(i),3,length(ParamStr(i))-2)));
        'D': OutDir:=copy(ParamStr(i),3,length(ParamStr(i))-2);
        'E': begin CommandAction:=caExtractFiles; StripPaths:=true; end;
        'F': PasswordFileName:=copy(ParamStr(i),3,length(ParamStr(i))-2);
        'H': NoHeader:=true;
        'I': CommandAction:=caVersionList;
        'L': CommandAction:=caLanguageList;
        'M': ExtractEmbedded:=true;
        'N': AttemptUnpackUnknown:=false;
        'P': Password:=copy(ParamStr(i),3,length(ParamStr(i))-2);
        'O': ColorMode:=0;
        'Q': QuietExtract:=true;
        'T': begin CommandAction:=caExtractFiles; ExtractTestOnly:=true; AutoYes:=true; end;
        'U': UseUtf8:=true;   // console output
        'V': CommandAction:=caVerboseList;
        'X': CommandAction:=caExtractFiles;
        'Y': AutoYes:=true;
        'Z': ColorMode:=2;    // used for InnoUnpacker GUI
        else Exit;
        end;
      end
    else if (ParamStr(i)[1]='@') and (length(ParamStr(i))>=2) then
      Filelist:=copy(ParamStr(i),2,length(ParamStr(i))-1)
//      AssignFile(f,copy(ParamStr(i),2,length(ParamStr(i))-1));
//      try
//        Reset(f);
//        while not eof(f) do begin
//          readln(f,s); // s:=OemToStr(s);// changes: JR - August 2020
//          if s<>'' then begin
//            SetLength(FileMasks,length(FileMasks)+1);
//            FileMasks[High(FileMasks)]:=s;
//          end;
//        end;
//      except
//        on E: Exception do raise EFatalError.Create('2');
//      end;
//      CloseFile(f);
    else if InstallNameParsed then begin
      SetLength(FileMasks,length(FileMasks)+1); FileMasks[High(FileMasks)]:=ParamStr(i);
      end
    else begin
      SetupFileName:=ExpandFileName(ParamStr(i));
      InstallNameParsed:=true;
      end;
    end;
  Result:=0;
  if PasswordFileName<>'' then begin
    try
      Password:=FileContents(PasswordFileName);
    except
      WriteErrorLine('Failed to read "'+PasswordFileName+'". No password will be used');
      Password:='';
      end;
    end;
  for i:=0 to High(FileMasks) do
    if (FileMasks[i,1]='"') and (FileMasks[i,length(FileMasks[i])]='"') then
      FileMasks[i]:=copy(FileMasks[i],2,length(FileMasks[i])-2);
  if (length(SetupFileName)=0) and (CommandAction<>caVersionList) then Result:=-1;
  end;

// no BOM found, check if UTF8 character in text
function CheckForUtf8(const s : string) : boolean;
const
  Utf8Mask1 = $C0;
  Utf8Mask2 = $80;
var
  i : integer;
begin
  Result:=false;
  if length(s)>0 then for i:=1 to length(s) do begin
    if cardinal(s[i]) and Utf8Mask1 = Utf8Mask1 then begin
      if (i<length(s)) and (cardinal(s[i+1]) and Utf8Mask1 = Utf8Mask2) then begin
        Result:=true; Break; // UTF8 caracter found
        end;
      end;
    end;
  end;

// ----------------------------------------------------------------------------
// main program
var
  i,n,ml : integer;
  systime   : TSystemTime;
  TimeStamp : TFileTime;
  loc       : PSetupFileLocationEntry;
  ReconstructedScript,
  s         : string;
  TotalFileSize : Int64;
  TotalFiles,TotalEncryptedFiles,MaxSlice : Integer;
  cbi : TConsoleScreenBufferInfo;
  attr,cp  : word;
  sl       : TStringList;
begin
  StdOutputHandle:=GetStdHandle(STD_OUTPUT_HANDLE);
  if GetConsoleScreenBufferInfo(StdOutputHandle,cbi) then with cbi do begin
     ConsoleBg:=wAttributes and $f0; ConsoleFg:=wAttributes and $F;
     attr:=wAttributes;
    end
  else attr:=ConsoleBg or ConsoleFg;
  cp:=GetConsoleOutputCP;
  if UseUtf8 then SetConsoleOutputCP(65001) else SetConsoleOutputCP(850);
  CreateEntryLists;
  n:=ParseCommandLine;

  if not NoHeader or (n<0) then begin
    writeln;
    if ColorMode>0 then s:='innounp' else s:='*innounp*';
    WriteColorText(s,Format(' - the Inno Setup Unpacker, Version %u.%u.%u (%s)',
      [IUV_MAJOR, IUV_MINOR,IUV_RELEASE,DateToStr(FileDateToDateTime(FileAge(ParamStr(0))))]),clRed,clWhite);
    end;
  writeln;

  if (n=0) and (length(FileList)>0) then begin
    if FileExists(FileList) then begin
      sl:=TStringList.Create;
      try
        with sl do begin
         LoadFromFile(FileList);   // try with automatic detection of encoding, needs BOM
         if (Encoding<>TEncoding.UTF8) and CheckForUtf8(Text) then
           LoadFromFile(FileList,TEncoding.UTF8);   // load again with Utf8 encoding
         if Count>0 then begin
           ml:=length(FileMasks);
           SetLength(FileMasks,ml+Count);
           for i:=0 to Count-1 do if not Strings[i].IsEmpty then begin
             FileMasks[ml]:=Strings[i];
             inc(ml);
             end;
           SetLength(FileMasks,ml);
           end;
         end;
      except
        on E: Exception do begin
          WriteColorText('Reading the command line failed. Invalid filelist: ','"'+FileList+'"',clRed,clGreen);
          writeln;
          ExitCode:=3; n:=1;
          end;
        end;
      sl.Free;
      end
    else begin
      WriteColorText('Filelist not found: ','"'+FileList+'"',clRed,clGreen);
      writeln;
      ExitCode:=3; n:=1;
      end;
    end;

  if n=0 then begin
    if CommandAction=caVersionList then begin
      writeln('Supported Inno Setup Versions');
      for i:=Low(VerList) to High(VerList) do with VerList[i] do begin
        s:=IntToStr(VerSupported div 1000)+'.'+IntToStr(VerSupported mod 1000 div 100)+'.'
          +IntToStr(VerSupported mod 100)+' ';
        if IsUnicode then s:=s+'u';
        if IsRT then s:=s+'r';
        writeln('  '+s);
        end;
      end
    else begin
      WriteNormalLine(ExtSp('Inno Setup archive:',TextAlign),ExtractFilename(SetupFileName));
      try
        if OutDir<>'' then begin MakeDir(OutDir); SetCurrentDir(OutDir) end;
        SetupLdr;
        InitializeSetup;
        if ExtractEmbedded then AddEmbeddedFiles;

        if CommandAction<>caInstallInfo then begin // save some time
          RenameFiles(ExtractAllCopies); // all the fake files must be added before this

          ReconstructedScript := GetInstallScript;
          AddFakeFile('install_script.iss', ReconstructedScript, true);
          end;

        WriteLn;
        case CommandAction of
        caLanguageList: begin
          WriteNormalLine('Supported languages');
          WriteLn; ml:=0;
          for i:=0 to Entries[seLanguage].Count-1 do with PSetupLanguageEntry(Entries[seLanguage][i])^ do
            ml:=Max(ml,length(Name));
          for i:=0 to Entries[seLanguage].Count-1 do with PSetupLanguageEntry(Entries[seLanguage][i])^ do begin
            WriteColorText('  '+ExtSp(Name+':',ml+2),LanguageName,clGreen,clWhite);
            end;
          end;
        caInstallInfo: begin
            with SetupHeader do begin
              WriteNormalLine(ExtSp('Application name:',TextAlign),AppName);
              WriteNormalLine(ExtSp('Application version:',TextAlign),AppVersion);
              end;
            WriteNormalLine(ExtSp('Compression used:',TextAlign),GetCompressMethodName(SetupHeader.CompressMethod));
            TotalFileSize:=0; TotalFiles:=0; TotalEncryptedFiles:=0; MaxSlice:=0;
            for i:=0 to Entries[seFile].Count-1 do
              with Struct.PSetupFileEntry(Entries[seFile][i])^ do begin
                if LocationEntry<>-1 then
                with PSetupFileLocationEntry(Entries[seFileLocation][LocationEntry])^ do begin
                  if not ExtractAllCopies and (PrimaryFileEntry<>-1) and (PrimaryFileEntry<>i) then continue;
                  TotalFileSize := TotalFileSize + OriginalSize;
                  if LastSlice>MaxSlice then MaxSlice:=LastSlice;
                  if foChunkEncrypted in Flags then Inc(TotalEncryptedFiles);
                end;
                Inc(TotalFiles);
              end;
            WriteNormalLine(ExtSp('Files:',TextAlign),IntToStr(TotalFiles));
            WriteNormalLine(ExtSp('Bytes:',TextAlign),IntToStr(TotalFileSize));
            if MaxSlice>0 then WriteNormalLine(ExtSp('Disk slices: ',TextAlign),IntToStr(MaxSlice+1));
            if TotalEncryptedFiles>0 then

              WriteNormalLine(ExtSp('Encryted files:',TextAlign),IntToStr(TotalEncryptedFiles));
            if length(SetupHeader.CompiledCodeText)>0 then
              WriteNormalLine(ExtSp('Compiled Pascal script: ',TextAlign),IntToStr(length(SetupHeader.CompiledCodeText))+' byte(s)');
          end;
        caVerboseList: begin
            WriteNormalLine('Size        Time              ','Filename');
            WriteNormalLine('----------------------------------------------');
            for i:=0 to Entries[seFile].Count-1 do
              with Struct.PSetupFileEntry(Entries[seFile][i])^ do begin
    //            if not (FileType in [ftUserFile,ftFakeFile]) and not ExtractEmbedded then continue;
                if ((FileType in [ftUninstExe, ftRegSvrExe]) or not (FileType in [ftUserFile, ftFakeFile]))
                  and not ExtractEmbedded then continue;
                if (LocationEntry=-1) then continue;
                loc:=PSetupFileLocationEntry(Entries[seFileLocation][LocationEntry]);
                if not ExtractAllCopies and (loc^.PrimaryFileEntry<>-1) and (loc^.PrimaryFileEntry<>i) then continue;
                if TimeStampsInUTC then FileTimeToLocalFileTime(loc^.TimeStamp, TimeStamp)
                else TimeStamp:=loc^.TimeStamp;
                FileTimeToSystemTime(TimeStamp, systime);
                str(loc^.OriginalSize:10,s);
                WriteNormalLine(s+'  '+FormatDateTime('yyyy.mm.dd hh:mm', SystemTimeToDateTime(systime))+
                  '  ',SourceFileName);
              end;
            WriteNormalLine('----------------------------------------------');
          end;
        caExtractFiles: begin
            QuietExtract:=false; CopyFiles;
            end;
        end;
      except
        on E: EAbort do
          ExitCode:=2; //This is a silent exception so just exit with error code
        on E: Exception do
          if IsUnknownVersion then begin
            WriteErrorLine('Unpacking failed. This version is not supported!'); ExitCode:=1;
            end
          else if E.Message=SSetupFileCorrupt then begin
            WriteFormatLine('The setup files are corrupted or made by incompatible version ',
              '(Address: '+inttohex(cardinal(ExceptAddr),8)+')',clRed,clBlue);
            WriteErrorLine('Maybe it''s not an Inno Setup installation at all!');
            ExitCode:=2;
            end
          else if E.Message='' then begin //SetupMessages[msgSourceIsCorrupted] then
            WriteFormatLine('Setup files are corrupted ','(Address: '+inttohex(cardinal(ExceptAddr),8)+')',clRed,clBlue);
            ExitCode:=2;
            end
          else begin
            if (E.Message <> '1') then // Hide service exceptions
              WriteErrorLine(sLineBreak,'Error ('+E.ClassName+') "'+E.Message+'" at address '+inttohex(cardinal(ExceptAddr),8));
              ExitCode:=3;
            end;
          end;
      end
    end
  else if n<0 then Usage;
  ReleaseEntryList;
  SetConsoleTextAttribute(StdOutputHandle,attr);  // restore console settings
  SetConsoleOutputCP(cp);
{$ifdef DEBUG}
  WriteLn;
  WriteNormalLine('Strike enter key to continue ...');
  readln;
{$endif}
  Halt(ExitCode);
end.
