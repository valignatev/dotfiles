export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export PATH="$HOME/.local/bin:$PATH"

# Use keychain to enter ssh passphrase only once
eval `keychain --agents ssh --eval --quiet id_rsa id_ed25519`

export TERMINAL=alacritty
export EDITOR=nvim
# https://wiki.archlinux.org/index.php/Java#Non-reparenting_window_managers_.2F_Grey_window_.2F_Programs_not_drawing_properly
export _JAVA_AWT_WM_NONREPARENTING=1

