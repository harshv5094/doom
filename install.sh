#!/usr/bin/env bash

RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
CYAN=$(tput setaf 6)
RESET=$(tput sgr0)

function have() {
    command -v "$1" &>/dev/null
}

function cmdCheck() {
    if [ $? -eq 0 ]; then
        printf "%b\n" "${GREEN}Success!${RESET}"
    else
        printf "%b\n" "${RED}Fail!!${RESET}"
    fi
}

printf "%b\n" "${CYAN}Installing Emacs.${RESET}"
if have pacman; then
    sudo pacman -S emacs ttf-jetbrains-mono-nerd libvterm
    cmdCheck
fi

# Turning on emacs daemon
systemctl --user enable --now emacs

printf "%b\n" "${CYAN}Checking for old emacs directory (if exist).${RESET}"
if [ -e "$HOME/.config/emacs" ]; then
    printf "%b\n" "${YELLOW}Removing Old Emacs Directory.${RESET}"
    rm -rf "$HOME/.config/emacs"
    cmdCheck
elif [ -e "$HOME/.emacs.d" ]; then
    printf "%b\n" "${YELLOW}Removing Old Emacs Directory.${RESET}"
    rm -rf "$HOME/.emacs.d"
    cmdCheck
elif [ -e "$HOME/.doom.d" ]; then
    printf "%b\n" "${YELLOW}Removing Old Doom Emacs Directory.${RESET}"
    rm -rf "$HOME/.doom.d"
    cmdCheck
else
    printf "%b\n" "${GREEN}Old Config Directories are not present in this system.${GREEN}"
fi

# Clonning doom emacs repo
printf "%b\n" "${YELLOW}Clonning Doom Emacs Repo.${RESET}"
git clone --depth 1 "https://github.com/doomemacs/doomemacs" ~/.config/emacs
cmdCheck

printf "%b\n" "${YELLOW}Installing Doom Emacs.${RESET}"
# Starting Installation Process
~/.config/emacs/bin/doom install

# Checking if last commands execution returns 0 as exit code
if [ $? -eq 0 ]; then
    printf "%b\n" "${GREEN} Installation process is finished.${RESET}"
fi
