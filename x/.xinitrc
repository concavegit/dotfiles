export XMODIFIERS="@im=fcitx"
export QT_IM_MODULE=fcitx
export GTK_IM_MODULE=fcitx

fcitx &
feh --bg-fill ~/dotfiles/nostow/rocket.png --no-fehbg &
mpd &
offlineimap &
udiskie &
unclutter -b &
urxvtd -q -o -f &
xrdb ~/.Xresources &
xss-lock slimlock &

exec xmonad
