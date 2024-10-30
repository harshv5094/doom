#!/usr/bin/env bash

printf "%b\n" "Installing emacs config"
git clone --depth 1 "https://github.com/doomemacs/doomemacs" ~/.config/emacs
~/.config/emacs/bin/doom install
~/.config/emacs/bin/doom sync

printf "%b\n" "Setting up Emacs Fonts"
if command -v pacman &>/dev/null; then
  sudo paru -S ttf-ubuntu-font-family ttf-jetbrains-mono-nerd
fi
