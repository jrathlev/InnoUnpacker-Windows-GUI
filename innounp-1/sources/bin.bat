@echo off
if -%1==- goto usage
rem goto skipopt

set DCC_BIN=dcc32

rem Optimization: reduce exe size
del *.dcu
ren innounp.cfg innounp.cfg.ren
call %DCC_BIN% innounp.dpr
ren innounp.cfg.ren innounp.cfg
if not exist StripReloc.exe call %DCC_BIN% StripReloc.dpr
StripReloc /b innounp.exe
rem End of optimization
goto pack

:skipopt
call %DCC_BIN% innounp.dpr

:pack
if not exist release md release
if exist release\%1.rar del release\%1.rar
rar a -m5 release\%1.rar innounp.exe innounp.htm
goto end

:usage
echo Makes a binary release (output is in release\release-name.rar)
echo Assumes 'dcc somefile.dpr' can be used to compile sources
echo Usage: bin release-name

set DCC_BIN=
:end
