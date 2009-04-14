#!/bin/sh
. ~/.environment
# Useful on ThinkPads, and shouldn't break any other keyboards.
xmodmap -e "keycode 77 = Num_Lock" \
        -e "keycode 227 = Multi_key" \
        -e "remove lock = Caps_Lock" -e "keycode 66 = Super_L" -e "add mod4 = Super_L" \
        -e "keycode 234 = XF86Back" -e "keycode 233 = XF86Forward"
WINDOW_MANAGER=metacity
export WINDOW_MANAGER
exec gnome-session
