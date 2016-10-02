#!/bin/bash
# By default, xmonad looks for this in ~/.xmonad/xmonad-session-rc.
# If this file does not exist, create it, or a link to this content.

# Background pattern
xsetroot -fg bisque3 -bg grey20 -mod 2 2 -rv

# Wallpaper
if [[ -e "$HOME/wallpaper.jpg" && -x "/usr/bin/compton" && -x "/usr/bin/feh" ]]; then
    compton --config "$HOME/compton.conf" &
    feh --bg-fill "$HOME/wallpaper.jpg"
fi

# Tray for skype and other icons
trayer --edge top --align right --SetDockType true --SetPartialStrut true  --expand true --widthtype request --transparent true --alpha 0 --tint 0x333333 --height 16 --monitor 1 &

# Keyboard layout switcher
setxkbmap -layout us,se -option 'grp:alt_shift_toggle' &
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

<<<<<<< HEAD
if [[ -x "/usr/bin/gnome-terminal" ]] ; then
    gnome-terminal &
fi

if [ -x /usr/bin/gnome-power-manager ] ; then
=======
if [[ -x "/usr/bin/gnome-power-manager" ]] ; then
>>>>>>> master
    sleep 3
    gnome-power-manager &
elif [ -x /usr/bin/xfce4-power-manager ] ; then
    sleep 2
    # This might require superuser in order to allow hibernate.
    sudo xfce4-power-manager &
fi

if [[ -x "/usr/bin/skype" ]] ; then
    sleep 3
    skype &
fi

# if [[ -x "/opt/thefanclub/grive-tools/grive-indicator" ]] ; then
#     /opt/thefanclub/grive-tools/grive-indicator &
# fi

if [[ -x "/usr/bin/unclutter" ]] ; then
    # -noevents is needed in order not to give focus to the window under the
    # possibly hidden cursor upon switching workspace.
    # unclutter -grab -noevents &
fi

nm-applet &
