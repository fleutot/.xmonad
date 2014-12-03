#!/bin/bash
# .xsession must be in the home directory. If the file you are currently viewing
# is not ~/.xsession, make a hardlink there to this file.

xsetroot -fg bisque3 -bg grey20 -mod 2 2 -rv

# Tray for skype and other icons
trayer --edge top --align right --SetDockType true --SetPartialStrut true  --expand true --widthtype pixel --width 77 --transparent true --alpha 0 --tint 0x333333 --height 16 &

# Keyboard layout switcher
setxkbmap -layout us,se -variant ,kinesis -option 'grp:alt_shift_toggle' &
# Per window keyboard layout
kbdd

# Startup apps
xscreensaver -nosplash &

sleep 1

if [[ -x "/usr/bin/gmail-notify" ]] ; then
    gmail-notify &
fi

if [[ -x "/usr/bin/dropbox" ]] ; then
    dropbox start
fi

if [[ -x "/usr/bin/emacs" ]] ; then
    emacs &
fi

gnome-terminal &

if [-x /usr/bin/gnome-power-manager ] ; then
    sleep 3
    gnome-power-manager &
fi

if [[ -x "/usr/bin/skype" ]] ; then
    sleep 2
    skype &
fi

if [[ -x "/opt/thefanclub/grive-tools/grive-indicator" ]] ; then
    /opt/thefanclub/grive-tools/grive-indicator &
fi
