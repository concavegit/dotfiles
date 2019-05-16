if [[ -z "${DBUS_SESSION_BUS_ADDRESS}" ]];
then
    eval $(dbus-launch --sh-syntax --exit-with-session)
fi

export $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh,gpg)

dbus-update-activation-environment --systemd DISPLAY
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK


export QT_QPA_PLATFORMTHEME="qt5ct"

export XMODIFIERS="@im=fcitx"
export QT_IM_MODULE=fcitx
export GTK_IM_MODULE=fcitx

export _JAVA_AWT_WM_NONREPARENTING=1

fcitx &
feh --bg-fill ~/dotfiles/nostow/rocket.png --no-fehbg &
mpd &
udiskie &
unclutter &
urxvtd -q -o -f &
xrdb ~/.Xresources &

exec xmonad
