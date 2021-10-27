#!/bin/sh
while true; do
       	wallpaper=$(find "$HOME/Pictures/Wallpapers" -type f | shuf -n 1)
       	killall swaybg
       	swaybg -i "$wallpaper" &
       	sleep 10s; done
