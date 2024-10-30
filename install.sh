#!/usr/bin/env bash

function have() {
  command -v "$1" &>/dev/null
}

printf "%b\n" "Installing Emacs"
if have pacman; then
  sudo pacman -S emacs ttf-jetbrains-mono-nerd
fi

# Clonning doom emacs repo
printf "%b\n" "Setting Up Doom Emacs"
git clone --depth 1 "https://github.com/doomemacs/doomemacs" ~/.config/emacs

~/.config/emacs/bin/doom install
~/.config/emacs/bin/doom sync
