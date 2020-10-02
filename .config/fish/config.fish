eval (keychain --agents ssh --eval --quiet id_rsa id_ed25519)

set -x TERMINAL alacritty
# https://wiki.archlinux.org/index.php/Java#Non-reparenting_window_managers_.2F_Grey_window_.2F_Programs_not_drawing_properly
set -x _JAVA_AWT_WM_NONREPARENTING 1

alias cdm="cd $HOME/workspace/me"
alias cdw="cd $HOME/workspace/work"
