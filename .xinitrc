#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap

xrdb -merge "$userresources"
xmodmap "$usermodmap"

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

xset b off
xrandr --dpi 150

libinput-gestures-setup start

xclear
picom --config ~/.config/picom.conf -b
~/.fehbg &
redshift-gtk &
nm-applet &
$TERMINAL -t "terminal_scratchpad" &
# Get my location, used by Redshift
/usr/lib/geoclue-2.0/demos/agent &
exec i3
