export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export PATH="/usr/local/bin:/usr/local/share/npm/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:$HOME/.local/bin:$PATH"

if [ "$(uname)" = "Darwin" ]; then
    docker-machine start dev
    eval "$(docker-machine env dev)"
fi

if [ "$(uname)" = "Linux" ]; then
    # Use keychain to enter ssh passphrase only once
    eval `keychain --agents ssh --eval --quiet id_rsa`
fi
