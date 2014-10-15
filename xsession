#!/bin/bash
# .xsession must be in the home directory. If the file you are currently viewing
# is not ~/.xsession, make a hardlink there to this file.

# Tray for skype, keyboard layout, ...
trayer --edge top --align right --SetDockType true --SetPartialStrut true  --expand true --width 10 --transparent true --tint 0x000000 --height 17 &

# Keyboard layout switcher
setxkbmap -layout se_kinesis,us -option 'grp:alt_shift_toggle' &

xscreensaver -nosplash &


