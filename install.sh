#!/usr/bin/env bash

function have() {
    command -v "$1" &>/dev/null
}

printf "%b\n" "Installing Emacs"
if have pacman; then
    sudo pacman -S emacs ttf-jetbrains-mono-nerd libvterm
fi

if [ -e "$HOME/.config/emacs" ]; then
    printf "%b\n" "Removing Old Emacs Directory"
    rm -rf "$HOME/.config/emacs"
elif [ -e "$HOME/.emacs.d" ]; then
    printf "%b\n" "Removing Old Emacs Directory"
    rm -rf "$HOME/.emacs.d"
else
    printf "%b\n" "This folder doesn't exist"
fi

# Clonning doom emacs repo
printf "%b\n" "Setting Up Doom Emacs"
git clone --depth 1 "https://github.com/doomemacs/doomemacs" ~/.config/emacs

~/.config/emacs/bin/doom install
~/.config/emacs/bin/doom sync
