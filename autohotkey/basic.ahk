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
        SetTimer(ToolTip, 1000)
    }
    else {
        WinSetAlwaysOnTop 1, t      ; Turn ON and add Title_When_On_Top
        ToolTip "Top ON"
        SetTimer(ToolTip, 1000)

    }
}

Workspace(num) {
    ;; https://github.com/MScholtes/VirtualDesktop
    RunWait(format("d:/Tools/cmd/VirtualDesktop.exe /Switch:{}", num), , "Hide")
}

LWin & 1:: Workspace(0)
LWin & 2:: Workspace(1)
LWin & 3:: Workspace(2)
LWin & 4:: Workspace(3)
LWin & 5:: Workspace(4)
LWin & 6:: Workspace(5)
LWin & 7:: Workspace(6)
LWin & 8:: Workspace(7)
