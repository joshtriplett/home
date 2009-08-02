#!/bin/sh
. ~/.environment
# Useful on ThinkPads, and shouldn't break any other keyboards.
xmodmap -e "keycode 151 = Multi_key" \
        -e "remove lock = Caps_Lock" -e "keycode 66 = Super_L"
WINDOW_MANAGER=metacity
export WINDOW_MANAGER
exec gnome-session
