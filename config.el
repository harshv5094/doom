(beacon-mode 1)

(setq doom-theme 'doom-one)

(map! :leader
      (:prefix ("=" . "open file")
       :desc "Edit agenda file"      "a" #'(lambda () (interactive) (find-file "~/org/agenda.org"))
       :desc "Edit doom config.org"  "c" #'(lambda () (interactive) (find-file "~/.config/doom/config.org"))
       :desc "Edit doom init.el"     "i" #'(lambda () (interactive) (find-file "~/.config/doom/init.el"))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file "~/.config/doom/packages.el"))))

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15)
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font Mono" :size 15)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 20))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

; NOTE setting relative line number
(setq display-line-numbers-type 'relative)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(setq doom-modeline-height 30                    ;; Sets modeline height
      doom-modeline-bar-width 5                  ;; sets right bar width
      doom-modeline-buffer-file-name-style 'auto ;; auto setup doom modeline filename
      doom-modeline-persp-name t                 ;; adds perspective name to modeline
      doom-modeline-persp-icon t)                ;; adds folder icon next to persp name

(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.2)))))

(xterm-mouse-mode 1)

(after! org
  (setq org-agenda-files '("~/org/agenda.org")))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(defun insert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (evil-org-open-below 1)
  (insert "#+auto_tangle: t ")
  (evil-force-normal-state))

(map! :leader
      :desc "Insert auto_tangle tag" "i a" #'insert-auto-tangle-tag)

; NOTE Custom function to change header size
(defun my-custom-header ()
  "Enable Doom Emacs Custom Header Size"
  (interactive)
  (with-eval-after-load 'org-faces
  (dolist
      (face
       '((org-level-1 1.7)
         (org-level-2 1.6)
         (org-level-3 1.5)
         (org-level-4 1.4)
         (org-level-5 1.3)
         (org-level-6 1.2)
         (org-level-7 1.1)
         (org-level-8 1.0)))
    (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :height (nth 1 face)))))
(my-custom-header)

; NOTE Default Org Directory
(setq org-directory "~/org/")
; NOTE Default Note File
(setq org-default-notes-file (concat org-directory "/notes.org"))

; NOTE Setting up org journal directory
(setq org-journal-dir "~/org/journal/"
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org")

; NOTE Default Org Roam Directory
(setq org-roam-directory "~/org/roam")
