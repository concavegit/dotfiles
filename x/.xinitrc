export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

fcitx &
feh --bg-fill ~/dotfiles/nostow/rocket.png --no-fehbg &
mpd &
offlineimap &
unclutter -b &
urxvtd -q -o -f &
xrdb ~/.Xresources &
xss-lock slimlock &

exec xmonad
