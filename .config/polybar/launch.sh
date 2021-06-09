#!/usr/bin/env sh
killall -q polybar -u ${USER}

# Wait until the processes have been shut down
while pgrep -U ${USER} -x polybar >/dev/null;do sleep 1; done

MONITOR=eDP-1 polybar bottom &

echo "Polybar launched..."
