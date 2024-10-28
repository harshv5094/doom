(beacon-mode 1)

(setq doom-theme 'doom-one)

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15 :weight 'regular))

; NOTE setting relative line number
(setq display-line-numbers-type 'relative)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

; NOTE Setting vterm keybinding
(map! :leader
      :desc "Vterm Toggle" "v t" #'+vterm/toggle)

; NOTE Default Org Directory
(setq org-directory "~/org/")
; NOTE Default Note File
(setq org-default-notes-file (concat org-directory "/refile.org"))
; NOTE Default Org Roam Directory
(setq org-roam-directory "~/notebook/")

; NOTE Setting up org journal directory
(setq org-journal-dir "~/org/journal/"
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org")
