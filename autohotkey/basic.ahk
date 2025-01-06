LWin & s::
{
    Run '"d:/tools/alacritty/alacritty.exe" "-e" "d:/tools/msys64/usr/bin/bash -c d:/files/private/kkv/kkv-wrapper"'
}

#SingleInstance Force
loop {
    WinWait "d:\tools\msys64\usr\bin\bash.exe"
    WinActivate("d:\tools\msys64\usr\bin\bash.exe")
    WinWaitClose
}

LWin & t:: {                          ; Alt + t
    t := WinGetTitle("A")
    ExStyle := WinGetExStyle(t)
    if (ExStyle & 0x8) {            ; 0x8 is WS_EX_TOPMOST
        WinSetAlwaysOnTop 0, t      ; Turn OFF and remove Title_When_On_Top
        ToolTip "Top OFF"
    }
    else {
        WinSetAlwaysOnTop 1, t      ; Turn ON and add Title_When_On_Top
        ToolTip "Top ON"
    }
}
