@echo Prepare before build. Expand the templates
@echo Run this before first compile
@echo Needs Win2K+
del StructList.inc
del CompressList.inc
del StructVer??????.pas
del CompressVer????.pas
for /f "tokens=1,2" %%a in (StructList) do (
  replace DEFVER %%a _EXTSUF_ "%%b" <StructTemplate.pas >StructVer%%a%%b.pas
  echo StructVer%%a%%b,>> StructList.inc
)
echo empty>>StructList.inc
for /f %%a in (CompressList) do (
  replace DEFVER %%a <CompressTemplate.pas >CompressVer%%a.pas
  echo CompressVer%%a,>> CompressList.inc
)
echo empty>>CompressList.inc

@rem Prepare version information
copy version.rc.in version.rc > nul
for /f "tokens=1,2 delims==;" %%a in (VersionInfo.inc) do (
   replace %%a %%b -- version.rc
)
brcc32 -foversion.res version.rc > nul
del version.rc > nul
