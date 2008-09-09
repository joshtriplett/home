# Useful on ThinkPads, and shouldn't break any other keyboards.
xmodmap -e "keycode 77 = Num_Lock" \
        -e "keycode 227 = Multi_key" \
        -e "keycode 234 = XF86Back" -e "keycode 233 = XF86Forward"
exec gnome-session
