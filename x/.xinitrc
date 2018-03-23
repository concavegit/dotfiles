export XMODIFIERS="@im=fcitx"
export QT_IM_MODULE=fcitx
export _JAVA_AWT_WM_NONREPARENTING=1
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
