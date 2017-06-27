feh --bg-fill ~/dotfiles/nostow/rocket.png --no-fehbg &
mpd &
offlineimap &
unclutter -b &
urxvtd -q -o -f &
xrdb ~/.Xresources &
xss-lock slimlock &

exec xmonad
