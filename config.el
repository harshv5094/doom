; Beacon Mode -> Allows user to show cursor
(beacon-mode 1)
(beacon-blink-automated)

; NOTE Setting default doom emacs banner
(defun my-custom-banner ()
  (let* ((banner'("______ _____ ____ ___ ___"
  "`  _  V  _  V  _ \\|  V  ´"
  "| | | | | | | | | |     |"
  "| | | | | | | | | | . . |"
  "| |/ / \\ \\| | |/ /\\ |V| |"
  "|   /   \\__/ \\__/  \\| | |"
  "|  /                ' | |"
  "| /     E M A C S     \\ |"
  "´´                     ``"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

;;(setq +doom-dashboard-ascii-banner-fn #'my-custom-banner)
;;(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(setq doom-theme 'doom-one)

(map! :leader
      (:prefix ("=" . "open file")
       :desc "Edit agenda file"      "a" #'(lambda () (interactive) (find-file "~/org/agenda.org"))
       :desc "Edit doom config.org"  "c" #'(lambda () (interactive) (find-file "~/.config/doom/config.org"))
       :desc "Edit doom init.el"     "i" #'(lambda () (interactive) (find-file "~/.config/doom/init.el"))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file "~/.config/doom/packages.el"))))

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15)
      doom-variable-pitch-font (font-spec :family "JetBrainsMonoNL Nerd Font" :size 15)
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

; NOTE Setting vterm keybinding
(map! :leader
      :desc "Vterm Toggle" "v t" #'+vterm/toggle)

(after! org
  (setq org-agenda-files '("~/org/agenda.org")))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(defun hv/insert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (evil-org-open-below 1)
  (insert "#+auto_tangle: t ")
  (evil-force-normal-state))

(map! :leader
      :desc "Insert auto_tangle tag" "i a" #'hv/insert-auto-tangle-tag)

; NOTE Custom function to change header size
(defun my-custom-header ()
  "Enable Doom Emacs Custom Header Size"
  (interactive)
  (with-eval-after-load 'org-faces
    ;; (set-face-attribute 'org-document-title nil :font doom-big-font :weight 'bold :slant 'italic :height 1.4)
    ;; (set-face-attribute 'org-document-info nil :font doom-big-font :weight 'bold :slant 'italic :height 1.4)
  (dolist
      (face
       '((org-level-1 1.6)
         (org-level-2 1.5)
         (org-level-3 1.4)
         (org-level-4 1.3)
         (org-level-5 1.2)
         (org-level-6 1.1)
         (org-level-7 1.0)
         (org-level-8 0.9)))
    (set-face-attribute (nth 0 face) nil :font doom-font :weight 'bold :slant 'italic :height (nth 1 face)))))
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
(setq org-roam-directory "~/notebook/")
