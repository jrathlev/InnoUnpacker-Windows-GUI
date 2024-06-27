@echo off
if -%1==- goto usage
if not exist release md release
if exist release\%1.rar del release\%1.rar
rar a -s -m5 -md1024 release\%1.rar innounp.dpr *.pas *.bat StructList CompressList VersionInfo.inc StripReloc.dpr version.inc licenses\*.* innounp.htm zlib\*.obj LzmaDecode\*.obj lzma2\Decoder\*.* bzip\*.obj crypt\*.obj replace.exe *.in innounp.manifest -xStructVer*.pas -xCompressVer????.pas
goto end

:usage
echo Makes a source release (output is release\release-name.rar)
echo Usage: src release-name

:end
