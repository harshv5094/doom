(beacon-mode 1)

(setq doom-theme 'doom-one)

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15 :weight 'regular))

; NOTE setting relative line number
(setq display-line-numbers-type 'relative)

; NOTE Default Org Directory
(setq org-directory "~/org/")
; NOTE Default Note File
(setq org-default-notes-file (concat org-directory "/refile.org"))
; NOTE Default Org Roam Directory
(setq org-roam-directory "~/notebook/")

; NOTE Setting vterm keybinding
(map! :leader
      :desc "Vterm Toggle" "v t" #'+vterm/toggle)
