@echo off
setlocal enabledelayedexpansion
echo 电脑版微信新信息提示工具
echo.
echo 作者:phenix
echo.
echo 邮箱：279682817@qq.com
echo.
:main
echo wscript.sleep 1000*5>%temp%\sleep.vbs
echo msgbox "您有新的微信消息！">%temp%\msg.vbs
echo wscript.echo "暂无新的微信消息！">%temp%\nomsg.vbs
set wechat_root=%USERPROFILE%\Documents\WeChat Files\
set idx1=1
set idx2=2
set msg_flag=0
for /f "tokens=* delims=" %%a in ('dir /b "!wechat_root!"^|find "wxid_"') do (
set wechat_dir=%%a
set wechat_file=Msg\Multi\MSG0.db-wal
set wechat_fullpath=!wechat_root!\!wechat_dir!\!wechat_file!
if exist "!wechat_fullpath!" (
copy /y "!wechat_fullpath!" /b %temp%\MSG0!idx1!.db-wal>nul
cscript /nologo %temp%\sleep.vbs
copy /y "!wechat_fullpath!" /b %temp%\MSG0!idx2!.db-wal>nul
call :showmsg !idx1! !idx2!
if %errorlevel% equ 1 (
set msg_flag=1
)
)
set /a idx1+=2
set /a idx2+=2
)
if !msg_flag! equ 0 (
cscript /nologo %temp%\nomsg.vbs
)
goto :main

:showmsg
for /f "skip=1 tokens=* delims=" %%m in ('fc %temp%\MSG0%1.db-wal %temp%\MSG0%2.db-wal /u') do (
set tmp=%%m
set tmp=!tmp:找不到差异=!
if "!tmp!" equ "%%m" (
cscript /nologo %temp%\msg.vbs
exit /b 1
) else (
exit /b 0
)
)
