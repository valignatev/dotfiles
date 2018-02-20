#!/bin/sh

if [ "$(uname)" = "Linux" ]; then

    userresources=$HOME/.Xresources
    usermodmap=$HOME/.Xmodmap
    sysresources=/etc/X11/xinit/.Xresources
    sysmodmap=/etc/X11/xinit/.Xmodmap

    # merge in defaults and keymaps

    if [ -f $sysresources ]; then
        xrdb -merge $sysresources
    fi

    if [ -f $sysmodmap ]; then
        xmodmap $sysmodmap
    fi

    if [ -f "$userresources" ]; then
        xrdb -merge "$userresources"
    fi

    if [ -f "$usermodmap" ]; then
        xmodmap "$usermodmap"
    fi

    # start some nice programs

    if [ -d /etc/X11/xinit/xinitrc.d ] ; then
     for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
      [ -x "$f" ] && . "$f"
     done
     unset f
    fi

    # Set faster key repeat
    xset r rate 200 70

    # Matches 1920X1200 for my 15'' mid-2015 macbook pro
    xrandr --dpi 150
    libinput-gestures-setup start
    export TERMINAL=alacritty
    # https://wiki.archlinux.org/index.php/Java#Non-reparenting_window_managers_.2F_Grey_window_.2F_Programs_not_drawing_properly
    export _JAVA_AWT_WM_NONREPARENTING=1
    exec i3
fi
