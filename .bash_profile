export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export PATH="/usr/local/bin:/usr/local/share/npm/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:$PATH"

if command -v docker-machine &>/dev/null; then
    eval "$(docker-machine env dev)"
fi
