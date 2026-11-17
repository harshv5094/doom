; NOTE: Common paths used across this config
(defvar my/org-dir "~/org/"
  "Root directory for Org files.")

(defvar my/org-agenda-file (expand-file-name "agenda.org" my/org-dir)
  "Main Org agenda file.")

(defvar my/org-journal-dir (expand-file-name "journal/" my/org-dir)
  "Directory for Org journal entries.")

(defvar my/mail-dir "~/.mails/"
  "Directory holding mbsync config, the maildir and encrypted credentials.")

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
; NOTE: Setup clipboard
(setq select-enable-clipboard t)

;NOTE: Color Scheme
(setq doom-theme 'doom-gruvbox)

; NOTE: Setting Basic Font Family and Font Size
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 15 :weight 'normal)
      doom-big-font (font-spec :family "FiraCode Nerd Font" :Font 18 :weight 'normal))

; NOTE: Basic font style
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

; NOTE: Custom font faces for text
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :weight bold)
  '(font-lock-function-name-face :weight bold))

; NOTE: global revert mode settings
(setq global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

; NOTE: add lsp delays
(after! flycheck
  (setq flycheck-idle-change-delay 0.2))

(after! lsp-mode
  (setq lsp-idle-delay 0.2
        lsp-completion-enable-additional-text-edit t
        lsp-modeline-code-actions-enable t))

; NOTE: Customizing modeline design
(setq doom-modeline-height 30                    ;; Sets modeline height
      doom-modeline-bar-width 5                  ;; sets right bar width
      doom-modeline-buffer-file-name-style 'auto ;; auto setup doom modeline filename
      doom-modeline-persp-name t                 ;; adds perspective name to modeline
      doom-modeline-persp-icon t)                ;; adds folder icon next to persp name

; NOTE: User information for Emacs to use
(setq user-full-name "Harsh Vyapari"
      user-mail-address "harshvy5094@proton.me")

; NOTE: setting relative line number
(setq display-line-numbers-type 'relative)

; NOTE: Ensure LSP also starts in tree-sitter shell buffers (e.g. .bashrc)
(after! sh-script
  (add-hook 'bash-ts-mode-local-vars-hook #'lsp! 'append))

; NOTE: Add which key delay for searching
(setq which-key-idle-delay 0.5)

; NOTE: Setting up split window rules for evil mode
(setq evil-vsplit-window-right t)
(setq evil-split-window-below t)

(after! mu4e
  (setq mu4e-root-maildir (expand-file-name "maildir/gmail" my/mail-dir)
        mu4e-get-mail-command (format "mbsync -c %s -a" (expand-file-name "mbsyncrc" my/mail-dir))
        mu4e-update-interval 300
        mu4e-change-filenames-when-moving t
        mu4e-maildir-shortcuts '((:maildir "/INBOX"             :key ?i)
                                 (:maildir "/[Gmail]/Sent Mail" :key ?s)
                                 (:maildir "/[Gmail]/Drafts"    :key ?d)
                                 (:maildir "/[Gmail]/All Mail"  :key ?a))
        mu4e-drafts-folder "/[Gmail]/Drafts"
        mu4e-sent-folder   "/[Gmail]/Sent Mail"
        mu4e-trash-folder  "/[Gmail]/Trash"
        mu4e-refile-folder "/[Gmail]/All Mail"

        user-mail-address "harshv5094@gmail.com"
        user-full-name    "Harsh Vyapari"

        ;; Send through Gmail SMTP; credentials come from auth-sources
        message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server       "smtp.gmail.com"
        smtpmail-smtp-service      587
        smtpmail-stream-type       'starttls
        send-mail-function         'smtpmail-send-it))

;; Read credentials from the encrypted file
(after! auth-source
  (setq auth-sources (list (expand-file-name "authinfo.gpg" my/mail-dir))))

; NOTE: Set local leader to \
(setq doom-localleader-key "\\")

; NOTE: Custom doom emacs config keymaps
(map! :leader
      (:prefix ("=" . "open file")
       :desc "Edit doom agenda.org"  "a" #'(lambda () (interactive) (find-file my/org-agenda-file))
       :desc "Edit doom config.org"  "c" #'(lambda () (interactive) (find-file (expand-file-name "config.org" doom-user-dir)))
       :desc "Edit doom config.el"   "e" #'(lambda () (interactive) (find-file (expand-file-name "config.el" doom-user-dir)))
       :desc "Edit doom elfeed.org"  "r" #'(lambda () (interactive) (find-file (expand-file-name "elfeed.org" doom-user-dir)))
       :desc "Edit doom init.el"     "i" #'(lambda () (interactive) (find-file (expand-file-name "init.el" doom-user-dir)))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file (expand-file-name "packages.el" doom-user-dir)))))

; NOTE: Open specific apps
(map! :leader
      (:prefix ("\\" . "Open Apps")
       :desc "elfeed" "e" #'elfeed
       :desc "mu4e"   "m" #'mu4e))

; NOTE: Enable mouse mode
(setq xterm-mouse-mode 1)

; NOTE: Setting up org agenda file
(after! org
  (setq org-agenda-files (list my/org-agenda-file)))

; NOTE Custom function to change header size
(custom-theme-set-faces! nil
'(org-level-8 :inherit outline-3 :height 1.0)
'(org-level-7 :inherit outline-3 :height 1.0)
'(org-level-6 :inherit outline-3 :height 1.02)
'(org-level-5 :inherit outline-3 :height 1.05)
'(org-level-4 :inherit outline-3 :height 1.08)
'(org-level-3 :inherit outline-3 :height 1.12)
'(org-level-2 :inherit outline-2 :height 1.18)
'(org-level-1 :inherit outline-1 :height 1.25)
'(org-document-title  :height 1.3 :bold t :underline nil))

; NOTE Default Org Directory
(setq org-directory my/org-dir)
; NOTE Default Note File
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))

; NOTE Setting up org journal directory
(setq org-journal-dir my/org-journal-dir
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org")

; NOTE: Elfeed RSS reader
(setq rmh-elfeed-org-files
      (list (expand-file-name "elfeed.org" doom-user-dir)))

; Setting search feed to one month ago
(setq elfeed-search-filter "@1-month-ago")
(add-hook 'elfeed-search-mode-hook #'elfeed-update)
