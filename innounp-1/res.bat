@rem Prepare version information
copy version.rc.in version.rc > nul
for /f "tokens=1,2 delims==;" %%a in (VersionInfo.inc) do (
   replace %%a %%b -- version.rc
)
"%ProgramFiles%\Embarcadero\Studio\17.0\bin\brcc32.exe" -foversion.res version.rc > nul
rem del version.rc > nul
