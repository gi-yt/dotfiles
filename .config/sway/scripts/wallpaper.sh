#!/bin/sh
killall swaybg
while true; do
	kill $PID
       	wallpaper=$(find "$HOME/Pictures/Wallpapers" -type f | shuf -n 1)
       	swaybg -i "$wallpaper" &
	PID=$!
       	sleep 10s; done
