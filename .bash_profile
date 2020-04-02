export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export PATH="$HOME/.local/bin:$PATH"

# Use keychain to enter ssh passphrase only once
eval `keychain --agents ssh --eval --quiet id_rsa id_ed25519`
