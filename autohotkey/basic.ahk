;; https://stackoverflow.com/questions/45642727/what-is-ahk-class-how-can-i-use-it-for-window-matching
;; ^1::WinGetClass, Clipboard, A ; Will copy the ahk_class of the Active Window to clipboard
isCxDown = 0
isCSpaceDown = 0

isEmacsLike() {
    IfWinActive,ahk_class Emacs
        Return 1
    IfWinActive,ahk_class CASCADIA_HOSTING_WINDOW_CLASS ; Windows terminal
        Return 1
    IfWinActive,ahk_class SunAwtFrame ; JetBrains
        Return 1
    Return 0
}

Shift & Capslock::
if GetKeyState("CapsLock" , "T")
    Send {CapsLock}
Return

Capslock::
Return

CapsLock & Space::
if isEmacsLike() {
    Send ^{Space}
    return
}
if isCSpaceDown {
    Send {Shift up}
    global isCSpaceDown = 0
} else {
    Send {Shift down}
    global isCSpaceDown = 1
}
Return


Capslock & a::
If isEmacsLike() {
    Send ^a
} Else {
    Send {Home}
}
Return

Capslock & b::
If isEmacsLike()
    Send ^b
Else
    Send {Left}
    Return

Capslock & c::
If isEmacsLike()
    Send ^c
Else
    Send ^c
Return

Capslock & d::
If isEmacsLike()
    Send ^d
Else
    Send {Delete}
    Return

Capslock & e::
If isEmacsLike()
    Send ^e
Else
    Send {End}
    Return

Capslock & f::
If isEmacsLike()
    Send ^f
Else
    Send {Right}
    Return

Capslock & g::
If isEmacsLike()
    Send ^g
Else {
    Send ^c
    if isCSpaceDown {
        Send {Shift up}
        global isCSpaceDown = 0
    }
}
Return

Capslock & h::
If isEmacsLike()
    Send ^h
Else
    Send {BackSpace}
    Return

Capslock & i::
If isEmacsLike()
    Send ^i
Else

    Return

Capslock & j::
If isEmacsLike()
    Send ^j
Else
    Send {Enter}
    Return

Capslock & l::
If isEmacsLike()
    Send ^l
Else

    Return

Capslock & k::
If isEmacsLike()
    Send ^k
Else {
    Send {Shift down}
    Send {End}
    Send {Shift up}
    Send ^x
}
Return

Capslock & /::
If isEmacsLike()
    Send ^/
Else
    Send ^z
Return

Capslock & m::
If isEmacsLike()
    Send ^m
Else
    Return

Capslock & n::
If isEmacsLike()
    Send ^n
Else
    Send {Down}
    Return

Capslock & o::
If isEmacsLike()
    Send ^o
Else
    Return

Capslock & p::
If isEmacsLike()
    Send ^p
Else
    Send {Up}
    Return

Capslock & q::
If isEmacsLike()
    Send ^q
Else
    Return

Capslock & r::
If isEmacsLike()
    Send ^r
Else
    Return

Capslock & s::
If isEmacsLike()
    Send ^s
Else
    If isCxDown {
        Send ^s
        global isCxDown = 0
    } Else {
        Send ^f
    }
    Return

Capslock & t::
If isEmacsLike()
    Send ^t
Else
    Send ^t
Return

Capslock & u::
If isEmacsLike()
    Send ^u
Else
    Send ^u
Return

Capslock & v::
If isEmacsLike()
    Send ^v
Else
    Send ^v
Return

Capslock & w::
If isEmacsLike()
    Send ^w
Else
    Send ^c
Return

Capslock & x::
If isEmacsLike()
    Send ^x
Else
    isCxDown = 1
Return

Capslock & y::
If isEmacsLike()
    Send ^y
Else
    Send ^v
Return

Capslock & z::
If isEmacsLike()
    Send ^z
Return

LWin & Right::
Send {LWin down}{LCtrl down}{Right down}{LWin up}{LCtrl up}{Right up}
return

Ctrl & Right::
Send {LWin down}{LCtrl down}{Right down}{LWin up}{LCtrl up}{Right up}
return

LWin & Left::
Send {LWin down}{LCtrl down}{Left down}{LWin up}{LCtrl up}{Left up}
return

Ctrl & Left::
Send {LWin down}{LCtrl down}{Left down}{LWin up}{LCtrl up}{Left up}
return

LWin & x::
run C:\msys64\mingw64\bin\emacsclientw.exe -n -a "" -e "(chin/raise-or-suspend-frame)"
Return
