@echo off
setlocal

rem https://github.com/Microsoft/vscode/issues/4651
rem If you don't add path for msys2 into %PATH%, enable following line.
set PATH=c:\msys64\usr\bin;%PATH%

if "%1" equ "rev-parse" goto rev_parse
git %*
goto :eof
:rev_parse
for /f %%1 in ('git %*') do cygpath -w %%1
