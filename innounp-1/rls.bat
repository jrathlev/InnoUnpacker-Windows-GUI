@echo off
if -%1==- goto usage
call bin %1
call src %1src
rem copy /b innounp.htm + count.php index.phtml
copy /b innounp.htm release\index.phtml
goto end

:usage
echo Makes a source release and a binary release
echo Usage: rls release-name

:end
