unit StructVerDEFVER_EXTSUF_;

interface

{$DEFINE ARCH_EXTSUF_}
// If unit for non-unicode version then previous define will be just ARCH
// so UNICODE will not be defined
{$IF Defined(ARCHu) or Defined(ARCHur)}
  {$DEFINE ISUNICODE}
{$ENDIF}

{$DEFINE EXT_EXTSUF_}
// Same trick as with Unicode but for RT versions
{$IF Defined(EXTr) or Defined(EXTur)}
  {$DEFINE RTVER}
{$ENDIF}

uses
  MyTypes, StructDEFVER_EXTSUF_;

type
  TAnInnoVer = class(TInnoVer)
  public
    constructor Create; override;
    procedure SetupSizes; override;
    procedure UnifySetupLdrOffsetTable(const p; var ot:TMySetupLdrOffsetTable); override;
    procedure UnifySetupHeader(const p; var sh:TMySetupHeader); override;
    procedure UnifyFileEntry(const p; var fe:TMySetupFileEntry); override;
    procedure UnifyFileLocationEntry(const p; var fl:TMySetupFileLocationEntry); override;
    procedure UnifyRegistryEntry(const p; var re:TMySetupRegistryEntry); override;
    procedure UnifyRunEntry(const p; var re:TMySetupRunEntry); override;
    procedure UnifyIconEntry(const p; var ie:TMySetupIconEntry); override;
    procedure UnifyTaskEntry(const p; var te:TMySetupTaskEntry); override;
    procedure UnifyComponentEntry(const p; var ce:TMySetupComponentEntry); override;
    procedure UnifyTypeEntry(const p; var te:TMySetupTypeEntry); override;
    procedure UnifyCustomMessageEntry(const p; var ce:TMySetupCustomMessageEntry); override;
    procedure UnifyLanguageEntry(const p; var le:TMySetupLanguageEntry); override;
    procedure UnifyDirEntry(const p; var de: TMySetupDirEntry); override;
    procedure UnifyIniEntry(const p; var ie: TMySetupIniEntry); override;
    procedure UnifyDeleteEntry(const p; var de: TMySetupDeleteEntry); override;
  end;

implementation

const
  SetupFileOptionTable: array [0..MySetupFileOptionLast] of byte = (
  ord(structDEFVER_EXTSUF_.foConfirmOverwrite)                                           ,  {foConfirmOverwrite        }
  ord(structDEFVER_EXTSUF_.foUninsNeverUninstall)                                        ,  {foUninsNeverUninstall     }
  ord(structDEFVER_EXTSUF_.foRestartReplace)                                             ,  {foRestartReplace          }
  ord(structDEFVER_EXTSUF_.foDeleteAfterInstall)                                         ,  {foDeleteAfterInstall      }
  ord(structDEFVER_EXTSUF_.foRegisterServer)                                             ,  {foRegisterServer          }
  ord(structDEFVER_EXTSUF_.foRegisterTypeLib)                                            ,  {foRegisterTypeLib         }
  ord(structDEFVER_EXTSUF_.foSharedFile)                                                 ,  {foSharedFile              }
  {$IF DEFVER>=3005}
  ord(structDEFVER_EXTSUF_.foCompareTimeStamp)                                           ,  {foCompareTimeStamp        }
  {$ELSE}
  ord(structDEFVER_EXTSUF_.foCompareTimeStampAlso)                                       ,  {foCompareTimeStamp        }
  {$IFEND}
  ord(structDEFVER_EXTSUF_.foFontIsntTrueType)                                           ,  {foFontIsntTrueType        }
  ord(structDEFVER_EXTSUF_.foSkipIfSourceDoesntExist)                                    ,  {foSkipIfSourceDoesntExist }
  ord(structDEFVER_EXTSUF_.foOverwriteReadOnly)                                          ,  {foOverwriteReadOnly       }
  ord(structDEFVER_EXTSUF_.foOverwriteSameVersion)                                       ,  {foOverwriteSameVersion    }
  ord(structDEFVER_EXTSUF_.foCustomDestName)                                             ,  {foCustomDestName          }
  {$IF DEFVER>=1325}ord(structDEFVER_EXTSUF_.foOnlyIfDestFileExists)    {$ELSE}255{$IFEND} ,  {foOnlyIfDestFileExists    }
  {$IF DEFVER>=2001}ord(structDEFVER_EXTSUF_.foNoRegError)              {$ELSE}255{$IFEND} ,  {foNoRegError              }
  {$IF DEFVER>=3001}ord(structDEFVER_EXTSUF_.foUninsRestartDelete)      {$ELSE}255{$IFEND} ,  {foUninsRestartDelete      }
  {$IF DEFVER>=3005}ord(structDEFVER_EXTSUF_.foOnlyIfDoesntExist)       {$ELSE}255{$IFEND} ,  {foOnlyIfDoesntExist       }
  {$IF DEFVER>=3005}ord(structDEFVER_EXTSUF_.foIgnoreVersion)           {$ELSE}255{$IFEND} ,  {foIgnoreVersion           }
  {$IF DEFVER>=3005}ord(structDEFVER_EXTSUF_.foPromptIfOlder)           {$ELSE}255{$IFEND} ,  {foPromptIfOlder           }
  {$IF DEFVER>=4000}ord(structDEFVER_EXTSUF_.foDontCopy)                {$ELSE}255{$IFEND} ,  {foDontCopy                }
  {$IF DEFVER>=4005}ord(structDEFVER_EXTSUF_.foUninsRemoveReadOnly)     {$ELSE}255{$IFEND} ,  {foUninsRemoveReadOnly     }
  {$IF DEFVER>=4108}ord(structDEFVER_EXTSUF_.foRecurseSubDirsExternal)  {$ELSE}255{$IFEND} ,  {foRecurseSubDirsExternal  }
  {$IF DEFVER>=4201}ord(structDEFVER_EXTSUF_.foReplaceSameVersionIfContentsDiffer){$ELSE}255{$IFEND} ,
  {$IF DEFVER>=4205}ord(structDEFVER_EXTSUF_.foDontVerifyChecksum)      {$ELSE}255{$IFEND} ,
  {$IF DEFVER>=5003}ord(structDEFVER_EXTSUF_.foUninsNoSharedFilePrompt) {$ELSE}255{$IFEND} ,
  {$IF DEFVER>=5100}ord(structDEFVER_EXTSUF_.foCreateAllSubDirs)        {$ELSE}255{$IFEND} ,
  {$IF DEFVER>=5102}ord(structDEFVER_EXTSUF_.fo32bit)                   {$ELSE}255{$IFEND} ,  {fo32bit                   }
  {$IF DEFVER>=5102}ord(structDEFVER_EXTSUF_.fo64bit)                   {$ELSE}255{$IFEND} ,  {fo64bit                   }
  {$IF DEFVER>=5200}ord(structDEFVER_EXTSUF_.foExternalSizePreset)      {$ELSE}255{$IFEND} ,  {foExternalSizePreset      }
  {$IF DEFVER>=5200}ord(structDEFVER_EXTSUF_.foSetNTFSCompression)      {$ELSE}255{$IFEND} ,  {foSetNTFSCompression      }
  {$IF DEFVER>=5200}ord(structDEFVER_EXTSUF_.foUnsetNTFSCompression)    {$ELSE}255{$IFEND} ,  {foUnsetNTFSCompression    }
  {$IF DEFVER>=5300}ord(structDEFVER_EXTSUF_.foGacInstall)              {$ELSE}255{$IFEND}    {foGacInstall              }
);

  SetupFileLocationFlagTable: array[0..MySetupFileLocationFlagLast] of byte = (
    ord(structDEFVER_EXTSUF_.foVersionInfoValid)                                              ,
    ord(structDEFVER_EXTSUF_.foVersionInfoNotValid)                                           ,
    {$IF DEFVER>=4010}ord(structDEFVER_EXTSUF_.foTimeStampInUTC)           {$ELSE}255{$IFEND} ,
    {$IF DEFVER>=4100}ord(structDEFVER_EXTSUF_.foIsUninstExe)              {$ELSE}255{$IFEND} ,
    {$IF DEFVER>=4108}ord(structDEFVER_EXTSUF_.foCallInstructionOptimized) {$ELSE}255{$IFEND} ,
    {$IF (DEFVER>=4200) AND (DEFVER<5507)}ord(structDEFVER_EXTSUF_.foTouch){$ELSE}255{$IFEND} ,
    {$IF DEFVER>=4202}ord(structDEFVER_EXTSUF_.foChunkEncrypted)           {$ELSE}255{$IFEND} ,
    {$IF DEFVER>=4205}ord(structDEFVER_EXTSUF_.foChunkCompressed)          {$ELSE}255{$IFEND} ,
    {$IF DEFVER>=5113}ord(structDEFVER_EXTSUF_.foSolidBreak)               {$ELSE}255{$IFEND} ,
    {$IF DEFVER>=6300}
      255,255 
    {$ELSE}       
      {$IF DEFVER>=5602}ord(structDEFVER_EXTSUF_.foSign)                     {$ELSE}255{$IFEND} ,
      {$IF DEFVER>=5602}ord(structDEFVER_EXTSUF_.foSignOnce)                 {$ELSE}255{$IFEND}
    {$ENDIF}
  );

  SetupRegistryOptionTable: array[0..MySetupRegistryOptionLast] of byte = (
  ord(structDEFVER_EXTSUF_.roCreateValueIfDoesntExist)                                   ,  {roCreateValueIfDoesntExist    }
  ord(structDEFVER_EXTSUF_.roUninsDeleteValue)                                           ,  {roUninsDeleteValue            }
  ord(structDEFVER_EXTSUF_.roUninsClearValue)                                            ,  {roUninsClearValue             }
  ord(structDEFVER_EXTSUF_.roUninsDeleteEntireKey)                                       ,  {roUninsDeleteEntireKey        }
  ord(structDEFVER_EXTSUF_.roUninsDeleteEntireKeyIfEmpty)                                ,  {roUninsDeleteEntireKeyIfEmpty }
  ord(structDEFVER_EXTSUF_.roPreserveStringType)                                         ,  {roPreserveStringType          }
  ord(structDEFVER_EXTSUF_.roDeleteKey)                                                  ,  {roDeleteKey                   }
  ord(structDEFVER_EXTSUF_.roDeleteValue)                                                ,  {roDeleteValue                 }
  ord(structDEFVER_EXTSUF_.roNoError)                                                    ,  {roNoError                     }
  ord(structDEFVER_EXTSUF_.roDontCreateKey)                                              ,  {roDontCreateKey               }
  {$IF DEFVER>=5100}ord(structDEFVER_EXTSUF_.ro32bit)                 {$ELSE}255{$IFEND} ,  {ro32bit                       }
  {$IF DEFVER>=5100}ord(structDEFVER_EXTSUF_.ro64bit)                 {$ELSE}255{$IFEND}    {ro64bit                       }
);

  SetupDirOptionTable: array[0..MySetupDirOptionLast] of byte = (
  ord(structDEFVER_EXTSUF_.doUninsNeverUninstall)                                          ,  {doUninsNeverUninstall   }
  ord(structDEFVER_EXTSUF_.doDeleteAfterInstall)                                           ,  {doDeleteAfterInstall    }
  ord(structDEFVER_EXTSUF_.doUninsAlwaysUninstall)                                         ,  {doUninsAlwaysUninstall  }
  {$IF DEFVER>=5200}ord(structDEFVER_EXTSUF_.doSetNTFSCompression)      {$ELSE}255{$IFEND} ,  {doSetNTFSCompression    }
  {$IF DEFVER>=5200}ord(structDEFVER_EXTSUF_.doUnsetNTFSCompression)    {$ELSE}255{$IFEND}    {doUnsetNTFSCompression  }
);

  SetupIniOptionTable: array[0..MySetupIniOptionLast] of byte = (
  ord(structDEFVER_EXTSUF_.ioCreateKeyIfDoesntExist)                       ,  {ioCreateKeyIfDoesntExist     }
  ord(structDEFVER_EXTSUF_.ioUninsDeleteEntry)                             ,  {ioUninsDeleteEntry           }
  ord(structDEFVER_EXTSUF_.ioUninsDeleteEntireSection)                     ,  {ioUninsDeleteEntireSection   }
  ord(structDEFVER_EXTSUF_.ioUninsDeleteSectionIfEmpty)                    ,  {ioUninsDeleteSectionIfEmpty  }
  ord(structDEFVER_EXTSUF_.ioHasValue)                                        {ioHasValue                   }
);

  SetupHeaderOptionTable: array[0..MySetupHeaderOptionLast] of byte = (
    ord(structDEFVER_EXTSUF_.shDisableStartupPrompt),
    {$IF DEFVER<5310}ord(structDEFVER_EXTSUF_.shUninstallable)             {$ELSE}255{$IFEND},
    ord(structDEFVER_EXTSUF_.shCreateAppDir),
    ord(structDEFVER_EXTSUF_.shAllowNoIcons),
    {$IF (DEFVER>=3003) OR (DEFVER < 3000)}ord(structDEFVER_EXTSUF_.shAlwaysRestart) {$ELSE}255{$IFEND},
    ord(structDEFVER_EXTSUF_.shAlwaysUsePersonalGroup),
    ord(structDEFVER_EXTSUF_.shWindowVisible),
    ord(structDEFVER_EXTSUF_.shWindowShowCaption),
    ord(structDEFVER_EXTSUF_.shWindowResizable),
    ord(structDEFVER_EXTSUF_.shWindowStartMaximized),
    ord(structDEFVER_EXTSUF_.shEnableDirDoesntExistWarning),
    ord(structDEFVER_EXTSUF_.shPassword),
    ord(structDEFVER_EXTSUF_.shAllowRootDirectory),
    ord(structDEFVER_EXTSUF_.shDisableFinishedPage),
    {$IF DEFVER<5602}ord(structDEFVER_EXTSUF_.shChangesAssociations)        {$ELSE}255{$IFEND},
    ord(structDEFVER_EXTSUF_.shUsePreviousAppDir),
    ord(structDEFVER_EXTSUF_.shBackColorHorizontal),
    ord(structDEFVER_EXTSUF_.shUsePreviousGroup),
    ord(structDEFVER_EXTSUF_.shUpdateUninstallLogAppName),
    {$IF DEFVER>=2001}ord(structDEFVER_EXTSUF_.shUsePreviousSetupType)      {$ELSE}255{$IFEND},
    {$IF DEFVER>=2001}ord(structDEFVER_EXTSUF_.shDisableReadyMemo)          {$ELSE}255{$IFEND},
    {$IF DEFVER>=2001}ord(structDEFVER_EXTSUF_.shAlwaysShowComponentsList)  {$ELSE}255{$IFEND},
    {$IF DEFVER>=2001}ord(structDEFVER_EXTSUF_.shFlatComponentsList)        {$ELSE}255{$IFEND},
    {$IF DEFVER>=2001}ord(structDEFVER_EXTSUF_.shShowComponentSizes)        {$ELSE}255{$IFEND},
    {$IF DEFVER>=2001}ord(structDEFVER_EXTSUF_.shUsePreviousTasks)          {$ELSE}255{$IFEND},
    {$IF DEFVER>=2001}ord(structDEFVER_EXTSUF_.shDisableReadyPage)          {$ELSE}255{$IFEND},
    {$IF DEFVER>=2001}ord(structDEFVER_EXTSUF_.shAlwaysShowDirOnReadyPage)  {$ELSE}255{$IFEND},
    {$IF DEFVER>=2001}ord(structDEFVER_EXTSUF_.shAlwaysShowGroupOnReadyPage){$ELSE}255{$IFEND},
    {$IF DEFVER>=2018}ord(structDEFVER_EXTSUF_.shAllowUNCPath)              {$ELSE}255{$IFEND},
    {$IF DEFVER>=3000}ord(structDEFVER_EXTSUF_.shUserInfoPage)              {$ELSE}255{$IFEND},
    {$IF DEFVER>=3000}ord(structDEFVER_EXTSUF_.shUsePreviousUserInfo)       {$ELSE}255{$IFEND},
    {$IF DEFVER>=3001}ord(structDEFVER_EXTSUF_.shUninstallRestartComputer)  {$ELSE}255{$IFEND},
    {$IF DEFVER>=3003}ord(structDEFVER_EXTSUF_.shRestartIfNeededByRun)      {$ELSE}255{$IFEND},
    {$IF DEFVER>=3008}ord(structDEFVER_EXTSUF_.shShowTasksTreeLines)        {$ELSE}255{$IFEND},
    {$IF DEFVER>=4009}ord(structDEFVER_EXTSUF_.shAllowCancelDuringInstall)  {$ELSE}255{$IFEND},
    {$IF DEFVER>=4103}ord(structDEFVER_EXTSUF_.shWizardImageStretch)        {$ELSE}255{$IFEND},
    {$IF DEFVER>=4108}ord(structDEFVER_EXTSUF_.shAppendDefaultDirName)      {$ELSE}255{$IFEND},
    {$IF DEFVER>=4108}ord(structDEFVER_EXTSUF_.shAppendDefaultGroupName)    {$ELSE}255{$IFEND},
    {$IF DEFVER>=4202}ord(structDEFVER_EXTSUF_.shEncryptionUsed)            {$ELSE}255{$IFEND},
    {$IF (DEFVER>=5004) AND (DEFVER<5602)}ord(structDEFVER_EXTSUF_.shChangesEnvironment){$ELSE}255{$IFEND},
    {$IF (DEFVER>=5107) AND (NOT DEFINED(ISUNICODE))}
      ord(structDEFVER_EXTSUF_.shShowUndisplayableLanguages)
    {$ELSE}255{$IFEND},
    {$IF DEFVER>=5113}ord(structDEFVER_EXTSUF_.shSetupLogging)             {$ELSE}255{$IFEND},
    {$IF DEFVER>=5201}ord(structDEFVER_EXTSUF_.shSignedUninstaller)        {$ELSE}255{$IFEND},
    {$IF DEFVER>=5308}ord(structDEFVER_EXTSUF_.shUsePreviousLanguage)      {$ELSE}255{$IFEND},
    {$IF DEFVER>=5309}ord(structDEFVER_EXTSUF_.shDisableWelcomePage)       {$ELSE}255{$IFEND},
    {$IF DEFVER>=5500}ord(structDEFVER_EXTSUF_.shCloseApplications)        {$ELSE}255{$IFEND},
    {$IF DEFVER>=5500}ord(structDEFVER_EXTSUF_.shRestartApplications)      {$ELSE}255{$IFEND},
    {$IF DEFVER>=5500}ord(structDEFVER_EXTSUF_.shAllowNetworkDrive)        {$ELSE}255{$IFEND},
    {$IF DEFVER>=5507}ord(structDEFVER_EXTSUF_.shForceCloseApplications)   {$ELSE}255{$IFEND},
    {$IF DEFVER>=6000}ord(structDEFVER_EXTSUF_.shAppNameHasConsts)         {$ELSE}255{$IFEND},
    {$IF DEFVER>=6000}ord(structDEFVER_EXTSUF_.shUsePreviousPrivileges)    {$ELSE}255{$IFEND},
    {$IF DEFVER>=6000}ord(structDEFVER_EXTSUF_.shWizardResizable)          {$ELSE}255{$IFEND},
    {$IF DEFVER>=6300}ord(structDEFVER_EXTSUF_.shUninstallLogging)         {$ELSE}255{$IFEND}
  );

  SetupRunOptionTable: array[0..MySetupRunOptionLast] of byte = (
    ord(structDEFVER_EXTSUF_.roShellExec),
    ord(structDEFVER_EXTSUF_.roSkipIfDoesntExist),
    {$IF DEFVER>=2001}ord(structDEFVER_EXTSUF_.roPostInstall)         {$ELSE}255{$IFEND},
    {$IF DEFVER>=2001}ord(structDEFVER_EXTSUF_.roUnchecked)           {$ELSE}255{$IFEND},
    {$IF DEFVER>=2001}ord(structDEFVER_EXTSUF_.roSkipIfSilent)        {$ELSE}255{$IFEND},
    {$IF DEFVER>=2001}ord(structDEFVER_EXTSUF_.roSkipIfNotSilent)     {$ELSE}255{$IFEND},
    {$IF DEFVER>=2008}ord(structDEFVER_EXTSUF_.roHideWizard)          {$ELSE}255{$IFEND},
    {$IF DEFVER>=5110}ord(structDEFVER_EXTSUF_.roRun32Bit)            {$ELSE}255{$IFEND},
    {$IF DEFVER>=5110}ord(structDEFVER_EXTSUF_.roRun64Bit)            {$ELSE}255{$IFEND},
    {$IF DEFVER>=5200}ord(structDEFVER_EXTSUF_.roRunAsOriginalUser)   {$ELSE}255{$IFEND},
    {$IF DEFVER>=6100}ord(structDEFVER_EXTSUF_.roDontLogParameters)   {$ELSE}255{$IFEND}, 
    {$IF DEFVER>=6300}ord(structDEFVER_EXTSUF_.roLogOutput)           {$ELSE}255{$IFEND}
  );

constructor TAnInnoVer.Create;
begin
  VerSupported:=DEFVER;
  IsUnicode:={$IFDEF ISUNICODE}true{$ELSE}false{$ENDIF};
  IsRT:={$IFDEF RTVER}true{$ELSE}false{$ENDIF};
  SetupID:=SetupLdrOffsetTableId;
  OfsTabSize:=sizeof(TSetupLdrOffsetTable);
end;

procedure TAnInnoVer.SetupSizes;
begin
  MyTypes.SetupHeaderSize:=sizeof(TSetupHeader);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
    MyTypes.SetupHeaderStrings:=SetupHeaderStrings;
    MyTypes.SetupHeaderAnsiStrings:=SetupHeaderAnsiStrings;
    {$ELSE}
    MyTypes.SetupHeaderStrings:=0;
    MyTypes.SetupHeaderAnsiStrings:=SetupHeaderStrings+SetupHeaderAnsiStrings;
    {$ENDIF}
  {$ELSE}
  MyTypes.SetupHeaderStrings:=0;
  MyTypes.SetupHeaderAnsiStrings:=SetupHeaderStrings;
  {$IFEND}

  {$IF DEFVER>=4000}
  MyTypes.SetupLanguageEntrySize:=sizeof(TSetupLanguageEntry);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
    MyTypes.SetupLanguageEntryStrings:=SetupLanguageEntryStrings;
    MyTypes.SetupLanguageEntryAnsiStrings:=SetupLanguageEntryAnsiStrings;
    {$ELSE}
    MyTypes.SetupLanguageEntryStrings:=0;
    MyTypes.SetupLanguageEntryAnsiStrings:=SetupLanguageEntryStrings+SetupLanguageEntryAnsiStrings;
    {$ENDIF}
  {$ELSE}
  MyTypes.SetupLanguageEntryStrings:=0;
  MyTypes.SetupLanguageEntryAnsiStrings:=SetupLanguageEntryStrings;
  {$IFEND}
  {$ELSEIF DEFVER >= 2001}
  MyTypes.SetupLanguageEntrySize:=sizeof(TSetupLangOptions);
  MyTypes.SetupLanguageEntryStrings:=0;
  MyTypes.SetupLanguageEntryAnsiStrings:=SetupLangOptionsStrings;
  {$ELSE}
  MyTypes.SetupLanguageEntrySize:=0;
  MyTypes.SetupLanguageEntryStrings:=0;
  MyTypes.SetupLanguageEntryAnsiStrings:=0;
  {$IFEND}

  {$IF DEFVER>=4201}
  MyTypes.SetupCustomMessageEntrySize:=sizeof(TSetupCustomMessageEntry);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
    MyTypes.SetupCustomMessageEntryStrings:=SetupCustomMessageEntryStrings;
    MyTypes.SetupCustomMessageEntryAnsiStrings:=SetupCustomMessageEntryAnsiStrings;
    {$ELSE}
    MyTypes.SetupCustomMessageEntryStrings:=0;
    MyTypes.SetupCustomMessageEntryAnsiStrings:=SetupCustomMessageEntryStrings+SetupCustomMessageEntryAnsiStrings;
    {$ENDIF}
  {$ELSE}
  MyTypes.SetupCustomMessageEntryStrings:=0;
  MyTypes.SetupCustomMessageEntryAnsiStrings:=SetupCustomMessageEntryStrings;
  {$IFEND}
  {$ELSE}
  MyTypes.SetupCustomMessageEntrySize:=0;
  MyTypes.SetupCustomMessageEntryStrings:=0;
  MyTypes.SetupCustomMessageEntryAnsiStrings:=0;
  {$IFEND}

  {$IF DEFVER>=4100}
  MyTypes.SetupPermissionEntrySize:=sizeof(TSetupPermissionEntry);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
    MyTypes.SetupPermissionEntryStrings:=SetupPermissionEntryStrings;
    MyTypes.SetupPermissionEntryAnsiStrings:=SetupPermissionEntryAnsiStrings;
    {$ELSE}
    MyTypes.SetupPermissionEntryStrings:=0;
    MyTypes.SetupPermissionEntryAnsiStrings:=SetupPermissionEntryStrings+SetupPermissionEntryAnsiStrings;
    {$ENDIF}
  {$ELSE}
  MyTypes.SetupPermissionEntryStrings:=0;
  MyTypes.SetupPermissionEntryAnsiStrings:=SetupPermissionEntryStrings;
  {$IFEND}
  {$ELSE}
  MyTypes.SetupPermissionEntrySize:=0;
  MyTypes.SetupPermissionEntryStrings:=0;
  MyTypes.SetupPermissionEntryAnsiStrings:=0;
  {$IFEND}

{$IF DEFVER >= 2001}
  MyTypes.SetupTypeEntrySize:=sizeof(TSetupTypeEntry);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
      MyTypes.SetupTypeEntryStrings:=SetupTypeEntryStrings;
      MyTypes.SetupTypeEntryAnsiStrings:=SetupTypeEntryAnsiStrings;
    {$ELSE}
      MyTypes.SetupTypeEntryStrings:=0;
      MyTypes.SetupTypeEntryAnsiStrings:=SetupTypeEntryStrings+SetupTypeEntryAnsiStrings;
    {$ENDIF}
  {$ELSE}
    MyTypes.SetupTypeEntryStrings:=0;
    MyTypes.SetupTypeEntryAnsiStrings:=SetupTypeEntryStrings;
  {$IFEND}
{$ELSE}
  MyTypes.SetupTypeEntrySize:=0;
  MyTypes.SetupTypeEntryStrings:=0;
  MyTypes.SetupTypeEntryAnsiStrings:=0;
{$IFEND}

{$IF DEFVER >= 2001}
  MyTypes.SetupComponentEntrySize:=sizeof(TSetupComponentEntry);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
      MyTypes.SetupComponentEntryStrings:=SetupComponentEntryStrings;
      MyTypes.SetupComponentEntryAnsiStrings:=SetupComponentEntryAnsiStrings;
    {$ELSE}
      MyTypes.SetupComponentEntryStrings:=0;
      MyTypes.SetupComponentEntryAnsiStrings:=SetupComponentEntryStrings+SetupComponentEntryAnsiStrings;
    {$ENDIF}
  {$ELSE}
    MyTypes.SetupComponentEntryStrings:=0;
    MyTypes.SetupComponentEntryAnsiStrings:=SetupComponentEntryStrings;
  {$IFEND}
{$ELSE}
  MyTypes.SetupComponentEntrySize:=0;
  MyTypes.SetupComponentEntryStrings:=0;
  MyTypes.SetupComponentEntryAnsiStrings:=0;
{$IFEND}

{$IF DEFVER >= 2001}
  MyTypes.SetupTaskEntrySize:=sizeof(TSetupTaskEntry);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
      MyTypes.SetupTaskEntryStrings:=SetupTaskEntryStrings;
      MyTypes.SetupTaskEntryAnsiStrings:=SetupTaskEntryAnsiStrings;
    {$ELSE}
      MyTypes.SetupTaskEntryStrings:=0;
      MyTypes.SetupTaskEntryAnsiStrings:=SetupTaskEntryStrings+SetupTaskEntryAnsiStrings;
    {$ENDIF}
  {$ELSE}
    MyTypes.SetupTaskEntryStrings:=0;
    MyTypes.SetupTaskEntryAnsiStrings:=SetupTaskEntryStrings;
  {$IFEND}
{$ELSE}
  MyTypes.SetupTaskEntrySize:=0;
  MyTypes.SetupTaskEntryStrings:=0;
  MyTypes.SetupTaskEntryAnsiStrings:=0;
{$IFEND}

  MyTypes.SetupDirEntrySize:=sizeof(TSetupDirEntry);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
    MyTypes.SetupDirEntryStrings:=SetupDirEntryStrings;
    MyTypes.SetupDirEntryAnsiStrings:=SetupDirEntryAnsiStrings;
    {$ELSE}
    MyTypes.SetupDirEntryStrings:=0;
    MyTypes.SetupDirEntryAnsiStrings:=SetupDirEntryStrings+SetupDirEntryAnsiStrings;
    {$ENDIF}
  {$ELSE}
  MyTypes.SetupDirEntryStrings:=0;
  MyTypes.SetupDirEntryAnsiStrings:=SetupDirEntryStrings;
  {$IFEND}

  MyTypes.SetupFileEntrySize:=sizeof(TSetupFileEntry);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
    MyTypes.SetupFileEntryStrings:=SetupFileEntryStrings;
    MyTypes.SetupFileEntryAnsiStrings:=SetupFileEntryAnsiStrings;
    {$ELSE}
    MyTypes.SetupFileEntryStrings:=0;
    MyTypes.SetupFileEntryAnsiStrings:=SetupFileEntryStrings+SetupFileEntryAnsiStrings;
    {$ENDIF}
  {$ELSE}
  MyTypes.SetupFileEntryStrings:=0;
  MyTypes.SetupFileEntryAnsiStrings:=SetupFileEntryStrings;
  {$IFEND}

  MyTypes.SetupIconEntrySize:=sizeof(TSetupIconEntry);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
    MyTypes.SetupIconEntryStrings:=SetupIconEntryStrings;
    MyTypes.SetupIconEntryAnsiStrings:=SetupIconEntryAnsiStrings;
    {$ELSE}
    MyTypes.SetupIconEntryStrings:=0;
    MyTypes.SetupIconEntryAnsiStrings:=SetupIconEntryStrings+SetupIconEntryAnsiStrings;
    {$ENDIF}
  {$ELSE}
  MyTypes.SetupIconEntryStrings:=0;
  MyTypes.SetupIconEntryAnsiStrings:=SetupIconEntryStrings;
  {$IFEND}

  MyTypes.SetupIniEntrySize:=sizeof(TSetupIniEntry);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
    MyTypes.SetupIniEntryStrings:=SetupIniEntryStrings;
    MyTypes.SetupIniEntryAnsiStrings:=SetupIniEntryAnsiStrings;
    {$ELSE}
    MyTypes.SetupIniEntryStrings:=0;
    MyTypes.SetupIniEntryAnsiStrings:=SetupIniEntryStrings+SetupIniEntryAnsiStrings;
    {$ENDIF}
  {$ELSE}
  MyTypes.SetupIniEntryStrings:=0;
  MyTypes.SetupIniEntryAnsiStrings:=SetupIniEntryStrings;
  {$IFEND}

  MyTypes.SetupRegistryEntrySize:=sizeof(TSetupRegistryEntry);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
    MyTypes.SetupRegistryEntryStrings:=SetupRegistryEntryStrings;
    MyTypes.SetupRegistryEntryAnsiStrings:=SetupRegistryEntryAnsiStrings;
    {$ELSE}
    MyTypes.SetupRegistryEntryStrings:=0;
    MyTypes.SetupRegistryEntryAnsiStrings:=SetupRegistryEntryStrings+SetupRegistryEntryAnsiStrings;
    {$ENDIF}
  {$ELSE}
  MyTypes.SetupRegistryEntryStrings:=0;
  MyTypes.SetupRegistryEntryAnsiStrings:=SetupRegistryEntryStrings;
  {$IFEND}

  MyTypes.SetupDeleteEntrySize:=sizeof(TSetupDeleteEntry);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
    MyTypes.SetupDeleteEntryStrings:=SetupDeleteEntryStrings;
    MyTypes.SetupDeleteEntryAnsiStrings:=SetupDeleteEntryAnsiStrings;
    {$ELSE}
    MyTypes.SetupDeleteEntryStrings:=0;
    MyTypes.SetupDeleteEntryAnsiStrings:=SetupDeleteEntryStrings+SetupDeleteEntryAnsiStrings;
    {$ENDIF}
  {$ELSE}
  MyTypes.SetupDeleteEntryStrings:=0;
  MyTypes.SetupDeleteEntryAnsiStrings:=SetupDeleteEntryStrings;
  {$IFEND}

  MyTypes.SetupRunEntrySize:=sizeof(TSetupRunEntry);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
    MyTypes.SetupRunEntryStrings:=SetupRunEntryStrings;
    MyTypes.SetupRunEntryAnsiStrings:=SetupRunEntryAnsiStrings;
    {$ELSE}
    MyTypes.SetupRunEntryStrings:=0;
    MyTypes.SetupRunEntryAnsiStrings:=SetupRunEntryStrings+SetupRunEntryAnsiStrings;
    {$ENDIF}
  {$ELSE}
  MyTypes.SetupRunEntryStrings:=0;
  MyTypes.SetupRunEntryAnsiStrings:=SetupRunEntryStrings;
  {$IFEND}

  MyTypes.SetupFileLocationEntrySize:=sizeof(TSetupFileLocationEntry);
  {$IF DEFVER>=UNI_FIRST}
    {$IFDEF ISUNICODE}
    MyTypes.SetupFileLocationEntryStrings:=SetupFileLocationEntryStrings;
    MyTypes.SetupFileLocationEntryAnsiStrings:=SetupFileLocationEntryAnsiStrings;
    {$ELSE}
    MyTypes.SetupFileLocationEntryStrings:=0;
    MyTypes.SetupFileLocationEntryAnsiStrings:=SetupFileLocationEntryStrings+SetupFileLocationEntryAnsiStrings;
    {$ENDIF}
  {$ELSE}
  MyTypes.SetupFileLocationEntryStrings:=0;
  MyTypes.SetupFileLocationEntryAnsiStrings:=SetupFileLocationEntryStrings;
  {$IFEND}
end;

// the following procedures extract information from important
// version-specific data structures. they are applied to whatever
// data is read from the installation file. all the other extraction
// code operates on the produced unified version-independent structures

procedure TAnInnoVer.UnifySetupLdrOffsetTable(const p; var ot:TMySetupLdrOffsetTable);
var
  oot:TSetupLdrOffsetTable absolute p;
begin
  with ot do begin
    ID                          :=oot.ID;
    {$IF DEFVER>=5105}
      Version                   :=oot.Version;
    {$ELSE}
      Version                   :=0;
    {$IFEND}
    TotalSize                   :=oot.TotalSize;
    OffsetEXE                   :=oot.OffsetEXE;
//    {$IF DEFVER<=4106} // since the main installer exe is not unpacked, there's no need for its size
//    CompressedSizeEXE           :=oot.CompressedSizeEXE;
//    {$IFEND}
    UncompressedSizeEXE         :=oot.UncompressedSizeEXE;
    {$IF DEFVER>=4003}
      CRCEXE                    :=oot.CRCEXE;
    {$ELSE}
      CRCEXE                    :=oot.AdlerEXE;
    {$IFEND}
    Offset0                     :=oot.Offset0;
    Offset1                     :=oot.Offset1;
    {$IF DEFVER>=4010}
      TableCRC                  :=oot.TableCRC;
      TableCRCUsed:=true;
    {$ELSE}
      TableCRCUsed:=false;
    {$IFEND}
  end;
end;

procedure TAnInnoVer.UnifySetupHeader(const p; var sh:TMySetupHeader);
var
  osh:TSetupHeader absolute p;
begin
  with sh do begin
    AppName                     :=NormalizeStringVal(osh.AppName);
    AppVerName                  :=NormalizeStringVal(osh.AppVerName);
    AppId                       :=osh.AppId;
    AppCopyright                :=osh.AppCopyright;
    AppPublisher                :=osh.AppPublisher;
    AppPublisherURL             :=osh.AppPublisherURL;
{$IF DEFVER>=5113}
    AppSupportPhone             :=osh.AppSupportPhone;
{$ELSE}
    AppSupportPhone             :='';
{$IFEND}    
    AppSupportURL               :=osh.AppSupportURL;
    AppUpdatesURL               :=osh.AppUpdatesURL;
    AppVersion                  :=osh.AppVersion;
    DefaultDirName              :=osh.DefaultDirName;
    DefaultGroupName            :=osh.DefaultGroupName;
    BaseFilename                :=osh.BaseFilename;
{$IF DEFVER>=4000}
    NumLanguageEntries          :=osh.NumLanguageEntries;
    CompiledCodeText            :=osh.CompiledCodeText;
{$ELSE}
    NumLanguageEntries          :=0;
    CompiledCodeText            :='';
{$IFEND}
    LicenseText                 :=osh.LicenseText;
    InfoBeforeText              :=osh.InfoBeforeText;
    InfoAfterText               :=osh.InfoAfterText;
{$IF DEFVER>=4201}
    NumCustomMessageEntries     :=osh.NumCustomMessageEntries;
{$ELSE}
    NumCustomMessageEntries     :=0;
{$IFEND}
{$IF DEFVER>=4100}
    NumPermissionEntries        :=osh.NumPermissionEntries;
{$ELSE}
    NumPermissionEntries        :=0;
{$IFEND}
{$IF DEFVER>=2001}
    NumTypeEntries              :=osh.NumTypeEntries;
    NumComponentEntries         :=osh.NumComponentEntries;
    NumTaskEntries              :=osh.NumTaskEntries;
{$ELSE}
    NumTypeEntries              :=0;
    NumComponentEntries         :=0;
    NumTaskEntries              :=0;
{$IFEND}
    NumDirEntries               :=osh.NumDirEntries;
    NumFileEntries              :=osh.NumFileEntries;
    NumFileLocationEntries      :=osh.NumFileLocationEntries;
    NumIconEntries              :=osh.NumIconEntries;
    NumIniEntries               :=osh.NumIniEntries;
    NumRegistryEntries          :=osh.NumRegistryEntries;
    NumInstallDeleteEntries     :=osh.NumInstallDeleteEntries;
    NumUninstallDeleteEntries   :=osh.NumUninstallDeleteEntries;
    NumRunEntries               :=osh.NumRunEntries;
    NumUninstallRunEntries      :=osh.NumUninstallRunEntries;
    MinVersion                  :=TMySetupVersionData(osh.MinVersion);
    OnlyBelowVersion            :=TMySetupVersionData(osh.OnlyBelowVersion);
    ExtraDiskSpaceRequired      :=Int64(osh.ExtraDiskSpaceRequired);
    BackColor                   :=osh.BackColor;
    BackColor2                  :=osh.BackColor2;
{$IF DEFVER>=4000}
    SlicesPerDisk               :=osh.SlicesPerDisk;
{$ELSE}
    SlicesPerDisk               :=1;
{$IFEND}
{$IF DEFVER>=4105}
    case osh.CompressMethod of
      {$IF DEFVER>=4205}
      cmStored:                 CompressMethod:=MyTypes.cmStored;
      {$IFEND}
      {$IF DEFVER<>4205}
      cmZip:                    CompressMethod:=MyTypes.cmZip;
      {$IFEND}
      cmBzip:                   CompressMethod:=MyTypes.cmBzip;
      cmLZMA:                   CompressMethod:=MyTypes.cmLZMA;
      {$IF DEFVER>=5309}
      cmLZMA2:                  CompressMethod:=MyTypes.cmLZMA2;
      {$IFEND}
    end;
{$ELSEIF DEFVER >= 2017}
    if shBzipUsed in osh.Options then CompressMethod:=MyTypes.cmBzip
    else CompressMethod:=MyTypes.cmZip;
{$ELSE}
    CompressMethod:=MyTypes.cmZip;
{$IFEND}
{$IF DEFVER>=4202}
    EncryptionUsed := shEncryptionUsed in osh.Options;
    {$IF DEFVER>=5309}
    PasswordHash.HashType := htSHA1;
    Move(osh.PasswordHash, PasswordHash.SHA1, sizeof(PasswordHash.SHA1));
    {$ELSE}
    PasswordHash.HashType := htMD5;
    Move(osh.PasswordHash, PasswordHash.MD5, sizeof(PasswordHash.MD5));
    {$IFEND}
    Move(osh.PasswordSalt, PasswordSalt, sizeof(PasswordSalt));
{$ELSE}
    EncryptionUsed := False;
    FillChar(PasswordSalt, SizeOf(PasswordSalt), 0);
{$IFEND}
{$IF DEFVER>=6300}
    ArchitecturesAllowed:=osh.ArchitecturesAllowed;
    ArchitecturesInstallIn64BitMode:=osh.ArchitecturesInstallIn64BitMode;
{$ELSEIF DEFVER>=5100}
    ArchitecturesAllowed:='';
    if (paX86 in osh.ArchitecturesAllowed) then ArchitecturesAllowed:='x86 '; 
{$IF DEFVER>=5102}
      if (paX64 in osh.ArchitecturesAllowed) then ArchitecturesAllowed:=ArchitecturesAllowed+'x64 '; 
{$ELSE}
      if (paAMD64 in osh.ArchitecturesAllowed) then ArchitecturesAllowed:=ArchitecturesAllowed+'x64 '; 
{$IFEND}
    if (paIA64 in osh.ArchitecturesAllowed) then ArchitecturesAllowed:=ArchitecturesAllowed+'ia64';
    ArchitecturesInstallIn64BitMode:='';
    if (paX86 in osh.ArchitecturesInstallIn64BitMode) then ArchitecturesInstallIn64BitMode:='x86 '; 
{$IF DEFVER>=5102}
    if (paX64 in osh.ArchitecturesInstallIn64BitMode) then ArchitecturesInstallIn64BitMode:=ArchitecturesInstallIn64BitMode+'x64 '; 
{$ELSE}
    if (paAMD64 in osh.ArchitecturesInstallIn64BitMode) then ArchitecturesInstallIn64BitMode:=ArchitecturesInstallIn64BitMode+'x64 '; 
{$IFEND}
    if (paIA64 in osh.ArchitecturesInstallIn64BitMode) then ArchitecturesInstallIn64BitMode:=ArchitecturesInstallIn64BitMode+'ia64';
{$ELSE}
    ArchitecturesAllowed:='';
    ArchitecturesInstallIn64BitMode:='';
{$IFEND}
{$IF DEFVER>=3004}
    case osh.PrivilegesRequired of
      prNone:       PrivilegesRequired := MyTypes.prNone;
      prPowerUser:  PrivilegesRequired := MyTypes.prPowerUser;
      prAdmin:      PrivilegesRequired := MyTypes.prAdmin;
      {$IF DEFVER>=5307}
      prLowest:     PrivilegesRequired := MyTypes.prLowest;
      {$IFEND}
    end;
{$ELSE}
    if (shAdminPrivilegesRequired in osh.Options) then
      PrivilegesRequired := MyTypes.prAdmin
    else
      PrivilegesRequired := MyTypes.prNone;
{$IFEND}
    UninstallFilesDir     := osh.UninstallFilesDir;
    UninstallDisplayName  := osh.UninstallDisplayName;
    UninstallDisplayIcon  := osh.UninstallDisplayIcon;
    AppMutex              := osh.AppMutex;
{$IF DEFVER>=3000}
    DefaultUserInfoName   := osh.DefaultUserInfoName;
    DefaultUserInfoOrg    := osh.DefaultUserInfoOrg;
{$ELSE}
    DefaultUserInfoName   := '';
    DefaultUserInfoOrg    := '';
{$IFEND}
{$IF DEFVER>=3008}
    DefaultUserInfoSerial := osh.DefaultUserInfoSerial; 
{$ELSE}
    DefaultUserInfoSerial := ''; 
{$IFEND}
{$IF DEFVER>=4204}
    AppReadmeFile         := osh.AppReadmeFile;
    AppContact            := osh.AppContact;
    AppComments           := NormalizeStringVal(osh.AppComments);
    AppModifyPath         := osh.AppModifyPath;
{$ELSE}
    AppReadmeFile         := '';
    AppContact            := '';
    AppComments           := '';
    AppModifyPath         := '';
{$IFEND}
{$IF DEFVER>=5308}
    CreateUninstallRegKey := osh.CreateUninstallRegKey;
{$ELSE}
    if (shCreateUninstallRegKey in osh.Options) then
      CreateUninstallRegKey := 'yes'
    else
      CreateUninstallRegKey := 'no';
{$IFEND}
{$IF DEFVER>=5310}
    Uninstallable := osh.Uninstallable;
{$ELSE}
    Uninstallable := '';
{$IFEND}
{$IF DEFVER>=5500}
    CloseApplicationsFilter := osh.CloseApplicationsFilter;
{$ELSE}
    CloseApplicationsFilter := '';
{$IFEND}
{$IF DEFVER>=5506}
    SetupMutex := osh.SetupMutex;
{$ELSE}
    SetupMutex := '';
{$IFEND}
{$IF DEFVER>=5602}
    ChangesEnvironment := osh.ChangesEnvironment;
{$ELSEIF DEFVER>=5004}
    if (shChangesEnvironment in osh.Options) then
      ChangesEnvironment := 'yes'
    else
      ChangesEnvironment := 'no';
{$ELSE}
    ChangesEnvironment := 'no';
{$IFEND}
{$IF DEFVER<5602}
    if (shChangesAssociations in osh.Options) then
      ChangesAssociations := 'yes'
    else
      ChangesAssociations := 'no';
{$ELSE}
    ChangesAssociations := osh.ChangesAssociations;
{$IFEND}
{$IF DEFVER>=5303}
    case osh.DisableDirPage of
      dpAuto: DisableDirPage := MyTypes.dpAuto;
      dpNo:   DisableDirPage := MyTypes.dpNo;
      dpYes:  DisableDirPage := MyTypes.dpYes;
    end;
    case osh.DisableProgramGroupPage of
      dpAuto: DisableProgramGroupPage := MyTypes.dpAuto;
      dpNo:   DisableProgramGroupPage := MyTypes.dpNo;
      dpYes:  DisableProgramGroupPage := MyTypes.dpYes;
    end;
{$ELSE}
    if (shDisableDirPage in osh.Options) then
      DisableDirPage := MyTypes.dpYes
    else
      DisableDirPage := MyTypes.dpNo;
    if (shDisableProgramGroupPage in osh.Options) then
      DisableProgramGroupPage := MyTypes.dpYes
    else
      DisableProgramGroupPage := MyTypes.dpNo;
{$IFEND}
{$IF DEFVER>=4010}
    case osh.LanguageDetectionMethod of
      ldUILanguage: LanguageDetectionMethod := MyTypes.ldUILanguage;
      ldLocale:     LanguageDetectionMethod := MyTypes.ldLocale;
      ldNone:       LanguageDetectionMethod := MyTypes.ldNone;
    end;
{$ELSE}
    LanguageDetectionMethod := MyTypes.ldNone;
{$IFEND}
{$IF DEFVER>=5306}
    UninstallDisplaySize := Int64(osh.UninstallDisplaySize);
{$ELSE}
    UninstallDisplaySize := 0;
{$IFEND}
  end;
  TranslateSet(osh.Options, sh.Options, PByteArray(@SetupHeaderOptionTable)^, MySetupHeaderOptionLast);
end;

procedure TAnInnoVer.UnifyFileEntry(const p; var fe:TMySetupFileEntry);
var
  ofe: TSetupFileEntry absolute p;
begin
  with fe do begin
    SourceFilename              := ofe.SourceFilename;
    DestName                    := ofe.DestName;
    {$IF DEFVER>=2001}
    Components                  := ofe.Components;
    Tasks                       := ofe.Tasks;
    {$ELSE}
    Components                  := '';
    Tasks                       := '';
    {$IFEND}
    {$IF DEFVER>=4000}
    Check                       := ofe.Check;
    {$ELSE}
    Check                       := '';
    {$IFEND}
    {$IF DEFVER>=4001}
    Languages                   := ofe.Languages;
    {$ELSE}
    Languages                   := '';
    {$IFEND}
    {$IF DEFVER>=4100}
    AfterInstall                := ofe.AfterInstall;
    BeforeInstall               := ofe.BeforeInstall;
    PermissionsEntry            := ofe.PermissionsEntry;
    {$ELSE}
    AfterInstall                := '';
    BeforeInstall               := '';
    PermissionsEntry            := 0;
    {$IFEND}
    MinVersion                  := TMySetupVersionData(ofe.MinVersion);
    OnlyBelowVersion            := TMySetupVersionData(ofe.OnlyBelowVersion);
    LocationEntry               := ofe.LocationEntry;
    ExternalSize                := Int64(ofe.ExternalSize);
    Attribs                     := ofe.Attribs;
    case ofe.FileType of
      ftUserFile: FileType  := MyTypes.ftUserFile;
      ftUninstExe: FileType := MyTypes.ftUninstExe;
      {$IF DEFVER<5000}
      ftRegSvrExe: FileType := MyTypes.ftRegSvrExe;
      {$IFEND}
    end;
  end;
  TranslateSet(ofe.Options, fe.Options, PByteArray(@SetupFileOptionTable)^, MySetupFileOptionLast);
end;

procedure TAnInnoVer.UnifyFileLocationEntry(const p; var fl:TMySetupFileLocationEntry);
var
  ofl: TSetupFileLocationEntry absolute p;
begin
  with fl do begin
{$IF DEFVER>=4000}
    FirstSlice                  :=ofl.FirstSlice;
    LastSlice                   :=ofl.LastSlice;
{$ELSE}
    FirstSlice                  :=ofl.FirstDisk;
    LastSlice                   :=ofl.LastDisk;
{$IFEND}
    StartOffset                 :=ofl.StartOffset;
{$IF DEFVER<=4000}
    ChunkSuboffset              :=0;
    ChunkCompressedSize         :=Int64(ofl.CompressedSize);
{$ELSE}
    ChunkSuboffset              :=Int64(ofl.ChunkSuboffset);
    ChunkCompressedSize         :=Int64(ofl.ChunkCompressedSize);
{$IFEND}
    OriginalSize                :=Int64(ofl.OriginalSize);

{$IF DEFVER>=5309}
    SHA1Sum                     := ofl.SHA1Sum;
    HashType                    := htSHA1;
{$ELSEIF DEFVER>=4200}
    MD5Sum                      := ofl.MD5Sum;
    HashType                    := htMD5;
{$ELSEIF DEFVER>=4001}
    CRC                         := ofl.CRC;
    HashType                    := htCRC32;
{$ELSE}
    CRC                         := ofl.Adler;
    HashType                    := htAdler;
{$IFEND}

{$IF DEFVER<4010}
    TimeStamp                   :=ofl.Date;
{$ELSEIF DEFVER<5507}
    TimeStamp                   :=ofl.TimeStamp;
{$ELSE}
    TimeStamp                   :=ofl.SourceTimeStamp;
{$IFEND}
    FileVersionMS               :=ofl.FileVersionMS;
    FileVersionLS               :=ofl.FileVersionLS;
    Contents:='';
    PrimaryFileEntry:=-1;
{$IF DEFVER>=6300}
    Sign:=TMySetupFileLocationSign(ofl.Sign);
{$ELSEIF DEFVER>=5602}
    if foSign in ofl.Flags then Sign:=fsYes 
    else if foSign in ofl.Flags then Sign:=fsOnce
    else Sign:=fsNoSetting;
{$ELSE}
    Sign:=fsNoSetting;
{$IFEND}
  end;
  TranslateSet(ofl.Flags, fl.Flags, PByteArray(@SetupFileLocationFlagTable)^, MySetupFileLocationFlagLast);
{$IF DEFVER<4205}
  Include(fl.Flags, MyTypes.foChunkCompressed);
{$IFEND}

end;

procedure TAnInnoVer.UnifyRegistryEntry(const p; var re:TMySetupRegistryEntry);
var
  ore: TSetupRegistryEntry absolute p;
begin
  with re do begin
    RootKey                       :=ore.RootKey;
    Subkey                        :=ore.Subkey;
    ValueName                     :=ore.ValueName;
    ValueData                     :=ore.ValueData;
    {$IF DEFVER>=2001}
    Components                    :=ore.Components;
    Tasks                         :=ore.Tasks;
    {$ELSE}
    Components                    := '';
    Tasks                         := '';
    {$IFEND}
    {$IF DEFVER>=4000}
    Check                         :=ore.Check;
    {$ELSE}
    Check                         :='';
    {$IFEND}
    {$IF DEFVER>=4001}
    Languages                     :=ore.Languages;
    {$ELSE}
    Languages                     :='';
    {$IFEND}
    {$IF DEFVER>=4100}
    AfterInstall                  :=ore.AfterInstall;
    BeforeInstall                 :=ore.BeforeInstall;
    {$ELSE}
    AfterInstall                  :='';
    BeforeInstall                 :='';
    {$IFEND}
    MinVersion                    :=TMySetupVersionData(ore.MinVersion);
    OnlyBelowVersion              :=TMySetupVersionData(ore.OnlyBelowVersion);

    case ore.Typ of
      rtNone         : Typ:=MyTypes.rtNone;
      rtString       : Typ:=MyTypes.rtString;
      rtExpandString : Typ:=MyTypes.rtExpandString;
      rtDWord        : Typ:=MyTypes.rtDWord;
      rtBinary       : Typ:=MyTypes.rtBinary;
      rtMultiString  : Typ:=MyTypes.rtMultiString;
      {$IF DEFVER>=5205}
      rtQWord        : Typ:=MyTypes.rtQWord;
      {$IFEND}
    end;
  end;
  TranslateSet(ore.Options, re.Options, PByteArray(@SetupRegistryOptionTable)^, MySetupRegistryOptionLast);
end;

procedure TAnInnoVer.UnifyRunEntry(const p; var re:TMySetupRunEntry);
var
  ore: TSetupRunEntry absolute p;
begin
  with re do begin
    Name                          :=ore.Name;
    Parameters                    :=ore.Parameters;
    WorkingDir                    :=ore.WorkingDir;
    RunOnceId                     :=ore.RunOnceId;
    {$IF DEFVER>=5113}
    Verb                          :=ore.Verb;
    {$ELSE}
    Verb                          :='';
    {$IFEND}
    {$IF DEFVER>=2001}
    StatusMsg                     :=ore.StatusMsg;
    Description                   :=NormalizeStringVal(ore.Description);
    Components                    :=ore.Components;
    Tasks                         :=ore.Tasks;
    {$ELSE}
    StatusMsg                     :='';
    Description                   :='';
    Components                    :='';
    Tasks                         :='';
    {$IFEND}
    {$IF DEFVER>=4000}
    Check                         :=ore.Check;
    {$ELSE}
    Check                         :='';
    {$IFEND}
    {$IF DEFVER>=4001}
    Languages                     :=ore.Languages;
    {$ELSE}
    Languages                     :='';
    {$IFEND}
    {$IF DEFVER>=4100}
    AfterInstall                  :=ore.AfterInstall;
    BeforeInstall                 :=ore.BeforeInstall;
    {$ELSE}
    AfterInstall                  :='';
    BeforeInstall                 :='';
    {$IFEND}
    MinVersion                    :=TMySetupVersionData(ore.MinVersion);
    OnlyBelowVersion              :=TMySetupVersionData(ore.OnlyBelowVersion);

    case ore.Wait of
      rwWaitUntilTerminated : Wait := MyTypes.rwWaitUntilTerminated;
      rwNoWait              : Wait := MyTypes.rwNoWait;
      rwWaitUntilIdle       : Wait := MyTypes.rwWaitUntilIdle;
    end;
  end;
  TranslateSet(ore.Options, re.Options, PByteArray(@SetupRunOptionTable)^, MySetupRunOptionLast);
end;

procedure TAnInnoVer.UnifyIconEntry(const p; var ie:TMySetupIconEntry);
var
  oie: TSetupIconEntry absolute p;
begin
  with ie do begin
    IconName                      :=NormalizeStringVal(oie.IconName);
    Filename                      :=oie.Filename;
    Parameters                    :=oie.Parameters;
    WorkingDir                    :=oie.WorkingDir;
    IconFilename                  :=oie.IconFilename;
    Comment                       :=NormalizeStringVal(oie.Comment);
    IconIndex                     :=oie.IconIndex;
    {$IF DEFVER>=1325}
    ShowCmd                       :=oie.ShowCmd;
    {$ELSE}
    ShowCmd                       :=0;
    {$IFEND}
    CloseOnExit                   :=TMySetupIconCloseOnExit(ord(oie.CloseOnExit));
    {$IF DEFVER>=2001}
    HotKey                        :=oie.HotKey;
    Components                    :=oie.Components;
    Tasks                         :=oie.Tasks;
    {$ELSE}
    HotKey                        :=0;
    Components                    :='';
    Tasks                         :='';
    {$IFEND}
    {$IF DEFVER>=4000}
    Check                         :=oie.Check;
    {$ELSE}
    Check                         :='';
    {$IFEND}
    {$IF DEFVER>=4001}
    Languages                     :=oie.Languages;
    {$ELSE}
    Languages                     :='';
    {$IFEND}
    {$IF DEFVER>=4100}
    AfterInstall                  :=oie.AfterInstall;
    BeforeInstall                 :=oie.BeforeInstall;
    {$ELSE}
    AfterInstall                  :='';
    BeforeInstall                 :='';
    {$IFEND}
    MinVersion                    :=TMySetupVersionData(oie.MinVersion);
    OnlyBelowVersion              :=TMySetupVersionData(oie.OnlyBelowVersion);
  end;
end;

procedure TAnInnoVer.UnifyTaskEntry(const p; var te:TMySetupTaskEntry);
{$IF DEFVER>=2001}
var
  ote: TSetupTaskEntry absolute p;
begin
  with te do begin
    Name                          :=ote.Name;
    Description                   :=NormalizeStringVal(ote.Description);
    GroupDescription              :=NormalizeStringVal(ote.GroupDescription);
    Components                    :=ote.Components;
    {$IF DEFVER>=4001}
    Languages                     :=ote.Languages;
    {$ELSE}
    Languages                     :='';
    {$IFEND}
    {$IF DEFVER>=4000}
    Check                         :=ote.Check;
    {$ELSE}
    Check                         :='';
    {$IFEND}
    MinVersion                    :=TMySetupVersionData(ote.MinVersion);
    OnlyBelowVersion              :=TMySetupVersionData(ote.OnlyBelowVersion);
    Move(ote.Options,Options,sizeof(ote.Options));
  end;
{$ELSE}
begin
{$IFEND}
end;

procedure TAnInnoVer.UnifyComponentEntry(const p; var ce:TMySetupComponentEntry);
{$IF DEFVER>=2001}
var
  oce: TSetupComponentEntry absolute p;
begin
  with ce do begin
    Name                          :=oce.Name;
    Description                   :=NormalizeStringVal(oce.Description);
    Types                         :=oce.Types;
    ExtraDiskSpaceRequired        :=Int64(oce.ExtraDiskSpaceRequired);
    {$IF DEFVER>=4001}
    Languages                     :=oce.Languages;
    {$ELSE}
    Languages                     :='';
    {$IFEND}
    {$IF DEFVER>=4000}
    Check                         :=oce.Check;
    {$ELSE}
    Check                         :='';
    {$IFEND}
    MinVersion                    :=TMySetupVersionData(oce.MinVersion);
    OnlyBelowVersion              :=TMySetupVersionData(oce.OnlyBelowVersion);
    Move(oce.Options,Options,sizeof(oce.Options));
  end;
{$ELSE}
begin
{$IFEND}  
end;

procedure TAnInnoVer.UnifyTypeEntry(const p; var te:TMySetupTypeEntry);
{$IF DEFVER>=2001}
var
  ote: TSetupTypeEntry absolute p;
begin
  with te do begin
    Name                          :=ote.Name;
    Description                   :=NormalizeStringVal(ote.Description);
    {$IF DEFVER>=4001}
    Languages                     :=ote.Languages;
    {$ELSE}
    Languages                     :='';
    {$IFEND}
    {$IF DEFVER>=4000}
    Check                         :=ote.Check;
    {$ELSE}
    Check                         :='';
    {$IFEND}
    MinVersion                    :=TMySetupVersionData(ote.MinVersion);
    OnlyBelowVersion              :=TMySetupVersionData(ote.OnlyBelowVersion);
    Move(ote.Options,Options,sizeof(ote.Options));
    {$IF DEFVER>=4003}
    Typ                           :=TMySetupTypeType(ote.Typ);
    {$ELSE}
    Typ                           :=Low(TMySetupTypeType);
    {$IFEND}
  end;
{$ELSE}
begin
{$IFEND}
end;

procedure TAnInnoVer.UnifyCustomMessageEntry(const p; var ce:TMySetupCustomMessageEntry);
{$IF DEFVER>=4201}
var
  oce: TSetupCustomMessageEntry absolute p;
begin
  with ce do begin
    Name                          :=NormalizeStringVal(oce.Name);
    Value                         :=NormalizeStringVal(oce.Value);
    LangIndex                     :=oce.LangIndex;
  end;
{$ELSE}
begin
{$IFEND}
end;

procedure TAnInnoVer.UnifyLanguageEntry(const p; var le:TMySetupLanguageEntry);
{$IF DEFVER>=4000}
var
  ole: TSetupLanguageEntry absolute p;
begin
  with le do begin
    Name                          :=ole.Name;
    LanguageName                  :=CopyStringVal(ole.LanguageName);
    DialogFontName                :=ole.DialogFontName;
    TitleFontName                 :=ole.TitleFontName;
    WelcomeFontName               :=ole.WelcomeFontName;
    CopyrightFontName             :=ole.CopyrightFontName;
    Data                          :=ole.Data;
{$IF DEFVER>=4001}
    LicenseText                   :=ole.LicenseText;
    InfoBeforeText                :=ole.InfoBeforeText;
    InfoAfterText                 :=ole.InfoAfterText;
{$ELSE}
    LicenseText                   :='';
    InfoBeforeText                :='';
    InfoAfterText                 :='';
{$IFEND}
{$IF (DEFVER>=4202) AND not Defined(ISUNICODE)}
    LanguageCodePage              :=ole.LanguageCodePage;
{$ELSE}
    LanguageCodePage              :=0;
{$IFEND}
    LanguageID                    :=ole.LanguageID;
    DialogFontSize                :=ole.DialogFontSize;
    TitleFontSize                 :=ole.TitleFontSize;
    WelcomeFontSize               :=ole.WelcomeFontSize;
    CopyrightFontSize             :=ole.CopyrightFontSize;
    RightToLeft                   :={$IF DEFVER>=5203} ole.RightToLeft {$ELSE} false {$IFEND};
  end;
{$ELSE}
begin
{$IFEND}
end;

procedure TAnInnoVer.UnifyDirEntry(const p; var de: TMySetupDirEntry);
var
  ode: TSetupDirEntry absolute p;
begin
  with de do begin
    DirName             := ode.DirName;
    {$IF DEFVER>=2001}
    Components          :=ode.Components;
    Tasks               :=ode.Tasks;
    {$ELSE}
    Components          :='';
    Tasks               :='';
    {$IFEND}
    Languages           := {$IF DEFVER>=4001} ode.Languages {$ELSE} '' {$IFEND};
    Check               := {$IF DEFVER>=3008} ode.Check {$ELSE} '' {$IFEND};
{$IF DEFVER>=4100}
    AfterInstall        := ode.AfterInstall;
    BeforeInstall       := ode.BeforeInstall;
{$ELSE}
    AfterInstall        := '';
    BeforeInstall       := '';
{$IFEND}
    Attribs             := {$IF DEFVER>=2011} ode.Attribs {$ELSE} 0 {$IFEND}; 
    MinVersion          := TMySetupVersionData(ode.MinVersion);
    OnlyBelowVersion    := TMySetupVersionData(ode.OnlyBelowVersion);
  end;
  TranslateSet(ode.Options, de.Options, PByteArray(@SetupDirOptionTable)^, MySetupDirOptionLast);
end;

procedure TAnInnoVer.UnifyIniEntry(const p; var ie: TMySetupIniEntry);
var
  oie: TSetupIniEntry absolute p;
begin
  with ie do begin
    Filename            := oie.Filename;
    Section             := oie.Section;
    Entry               := oie.Entry;
    Value               := oie.Value;
    {$IF DEFVER>=2001}
    Components          := oie.Components;
    Tasks               := oie.Tasks;
    {$ELSE}
    Components          := '';
    Tasks               := '';
    {$IFEND}
    Languages           := {$IF DEFVER>=4001} oie.Languages {$ELSE} '' {$IFEND};
    Check               := {$IF DEFVER>=3008} oie.Check {$ELSE} '' {$IFEND};
{$IF DEFVER>=4100}
    AfterInstall        := oie.AfterInstall;
    BeforeInstall       := oie.BeforeInstall;
{$ELSE}
    AfterInstall        := '';
    BeforeInstall       := '';
{$IFEND}
    MinVersion          := TMySetupVersionData(oie.MinVersion);
    OnlyBelowVersion    := TMySetupVersionData(oie.OnlyBelowVersion);
  end;
  TranslateSet(oie.Options, ie.Options, PByteArray(@SetupIniOptionTable)^, MySetupIniOptionLast);
end;

procedure TAnInnoVer.UnifyDeleteEntry(const p; var de: TMySetupDeleteEntry);
var
  ode: TSetupDeleteEntry absolute p;
begin
  with de do begin
    Name                := ode.Name;
    {$IF DEFVER>=2001}
    Components          :=ode.Components;
    Tasks               :=ode.Tasks;
    {$ELSE}
    Components          :='';
    Tasks               :='';
    {$IFEND}
{$IF DEFVER>=4001}
    Languages           := ode.Languages;
{$ELSE}
    Languages           := '';
{$IFEND}
{$IF DEFVER>=3008}
    Check               := ode.Check;
{$ELSE}
    Check               := '';
{$IFEND}
{$IF DEFVER>=4100}
    AfterInstall        := ode.AfterInstall;
    BeforeInstall       := ode.BeforeInstall;
{$ELSE}
    AfterInstall        := '';
    BeforeInstall       := '';
{$IFEND}
    MinVersion          := TMySetupVersionData(ode.MinVersion);
    OnlyBelowVersion    := TMySetupVersionData(ode.OnlyBelowVersion);

    case ode.DeleteType of
      dfFiles:              DeleteType := MyTypes.dfFiles;
      dfFilesAndOrSubdirs:  DeleteType := MyTypes.dfFilesAndOrSubdirs;
      dfDirIfEmpty:         DeleteType := MyTypes.dfDirIfEmpty;
    end;
  end;  
end;

begin
  SetLength(VerList, Length(VerList)+1);
  VerList[High(VerList)] := TAnInnoVer.Create;
end.

