@rem Prepare version information
copy version.rc.in version.rc > nul
for /f "tokens=1,2 delims==;" %%a in (VersionInfo.inc) do (
   replace %%a %%b -- version.rc
)
brcc32 -foversion.res version.rc > nul
del version.rc > nul
