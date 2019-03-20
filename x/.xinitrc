dbus-update-activation-environment --systemd DISPLAY
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

export QT_QPA_PLATFORMTHEME="qt5ct"

export XMODIFIERS="@im=fcitx"
export QT_IM_MODULE=fcitx
export GTK_IM_MODULE=fcitx

fcitx &
feh --bg-fill ~/dotfiles/nostow/rocket.png --no-fehbg &
mpd &
udiskie &
unclutter -grab &
xss-lock slimlock &
urxvtd -q -o -f &
xrdb ~/.Xresources &

exec stack exec xmonad
