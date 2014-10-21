#!/bin/bash
# .xsession must be in the home directory. If the file you are currently viewing
# is not ~/.xsession, make a hardlink there to this file.

xsetroot -fg bisque3 -bg grey20 -mod 2 2 -rv

# Tray for skype, keyboard layout, ...
trayer --edge top --align right --SetDockType true --SetPartialStrut true  --expand true --widthtype pixel --width 59 --transparent true --alpha 0 --tint 0x333333 --height 17 &

# Keyboard layout switcher
setxkbmap -layout us,se -option 'grp:alt_shift_toggle' &
# Per window keyboard layout
kbdd

# Startup apps
xscreensaver -nosplash &
dropbox start
skype &
emacs &
gnome-terminal &

if [-x /usr/bin/gnome-power-manger ] ; then
    sleep 3
    gnome-power-manager &
fi
