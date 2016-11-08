xmonad
======

ln -s path/to/repo/xmonad.hs ~/.xmonad/xmonad.hs
ln -s path/to/repo/xmobarrc ~/.xmonad/xmobarrc

~/.screenlayout/default.sh
#!/bin/sh
xrandr --output HDMI1 --off --output LVDS1 --mode 1600x900 --pos 160x1080 --rotate normal --output VIRTUAL1 --off --output DP1 --off --output VGA1 --mode 1920x1080 --pos 0x0 --rotate normal

add to .profile
setxkbmap -layout us,th -option 'grp:shift_caps_toggle'
setxkbmap -layout us,bg -variant ,phonetic -option 'grp:shift_caps_toggle'
