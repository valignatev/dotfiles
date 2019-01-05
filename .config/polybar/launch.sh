#!/usr/bin/env sh
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null;do sleep 1; done

MONITOR=eDP-1 polybar top &

echo "Polybar launched..."
