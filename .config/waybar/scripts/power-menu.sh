#!/bin/bash

entries="Logout Hibernate Reboot Shutdown"

selected=$(printf '%s\n' $entries | wofi --conf=$HOME/.config/wofi/config.power --style=$HOME/.config/wofi/style.widgets.css | awk '{print tolower($1)}')

case $selected in
  logout)
    swaymsg exit;;
  hibernate)
	exec systemctl hibernate;;
  reboot)
    exec reboot;;
  shutdown)
    exec systemctl poweroff -i;;
esac
