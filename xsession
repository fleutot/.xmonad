#!/bin/bash
# By default, xmonad looks for this in ~/.xmonad/xmonad-session-rc.
# If this file does not exist, create it, or a link to this content.

# Background pattern
xsetroot -fg bisque3 -bg grey20 -mod 2 2 -rv

# Compositor
if [[ -x "/usr/bin/compton" ]]; then
    compton --config "$HOME/.xmonad/compton.conf" &
fi

# Wallpaper
# To randomly switch wallpaper every hour, add a crontab:
# env | grep -i display # get the name of the current display, e.g. DISPLAY=:0.0
# crontab -e
# 0 *    *   *   *     DISPLAY=:0.0 feh --bg-scale $(find /opt/gauthier/Pictures/wallpapers -type f |sort -R |head -1)
if [[ -e "$HOME/wallpaper.jpg" && -x "/usr/bin/feh" ]]; then
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

if [[ -x "/usr/bin/gnome-terminal" ]] ; then
    gnome-terminal &
fi

if [[ -x "/usr/bin/gnome-power-manager" ]] ; then
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
    # unclutter starts a default version by itself, see (and modify) /etc/default/unclutter
    # -noevents is needed in order not to give focus to the window under the
    # possibly hidden cursor upon switching workspace.
    # Try without -grab to let xscreensaver use the mouse, and lock faster (otherwise it tries
    # and fails to grab the mouse pointer, and times out).
    # -grab was added earlier to "avoid erroneous focus changes", let's test and see.
    unclutter -noevents &
fi

nm-applet &
