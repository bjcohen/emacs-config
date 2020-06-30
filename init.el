;;; package --- Summary
;;; Commentary:
;;; Code:

(eval-when-compile
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'use-package))

(setq straight-use-package-by-default t)

;;; OS X Config

(use-package diminish)
(use-package bind-key)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(use-package yascroll
  :config
  (global-yascroll-bar-mode 1))

(toggle-frame-maximized)
(setq frame-resize-pixelwise t)

(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier nil)

;;; General Config

(require 'saveplace)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-set-key [?\M-g] 'goto-line)  ; easier to jump to specific lines (M-g)

;; ispell
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(diminish 'flyspell-mode)

(use-package egg
  :diminish egg-minor-mode)

(electric-pair-mode)

;;; random stuff

(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face t)))))

(use-package haskell-mode
  :config
  (require 'inf-haskell)
  :hook
  ((haskell-mode . turn-on-haskell-doc-mode)
   (haskell-mode . turn-on-haskell-indentation)))

(use-package compile
  :config
  (defun close-on-successful-exit (buffer desc)
    "Close the compilation BUFFER in two seconds if DESC indei it exited successfully."
    (unless (string-match "exited abnormally" desc)
      (run-at-time
       "2 sec" nil 'delete-windows-on
       (get-buffer-create "*compilation*"))
      (message "No Compilation Errors!")))

  (add-hook 'compilation-finish-functions 'close-on-successful-exit))

;;; hilight matching parens
(defvar show-paren-overlay nil)
(defvar show-paren-overlay-1 nil)
(show-paren-mode)

(use-package projectile
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . 'projectile-command-map)
  :config
  (projectile-mode +1)
  (projectile-register-project-type 'automake '("Makefile.am")
                                    :project-file "Makefile.am"
                                    :compile "autogen.sh && make"
                                    :test "make test"))

(use-package projectile-ripgrep
  :requires (projectile ripgrep))

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

(use-package ruby-mode
  :config
  (autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode)))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-c TAB") 'yas-expand))

(use-package auto-complete
  :diminish auto-complete-mode
  :config
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (ac-config-default))

;; tabs

(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook #'whitespace-mode)
(setq whitespace-style '(face trailing lines-tail tabs empty))
(diminish 'global-whitespace-mode)

(setq whitespace-trailing 'font-lock-warning-face)
(setq whitespace-line 'font-lock-warning-face)
(setq whitespace-line-column 100)
(setq whitespace-empty 'font-lock-warning-face)

;; Sometimes I use these...

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package perspective
  :config
  (persp-mode)
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (add-hook 'after-init-hook
            (lambda () (persp-state-load (concat user-emacs-directory "persp-save"))))
  :custom
  (persp-state-default-file (concat user-emacs-directory "persp-save")))

(use-package evil
  :diminish undo-tree-mode
  :config
  (evil-mode 1)
  (setq evil-default-state 'emacs))

(use-package evil-surround
  :requires evil
  :config
  (global-evil-surround-mode))

(use-package helm
  :diminish helm-mode
  :bind (("C-x r b" . 'helm-filtered-bookmarks)
         ("C-x C-f" . 'helm-find-files)
         ("C-x C-r" . 'helm-recentf)
         ("C-x b" . 'helm-mini))
  :config
  (recentf-mode t)
  (setq recentf-max-menu-items 25)
  (helm-mode t))

(use-package helm-projectile
  :requires (helm projectile)
  :bind
  (:map projectile-mode-map
        ("C-c p s g" . 'helm-projectile-grep)
        ("C-c p s r" . 'helm-projectile-rg)))

(use-package helm-lsp
  :requires (help lsp-mode)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package flx)

(use-package helm-flx
  :requires (helm flx)
  :config
  (helm-flx-mode +1))

(use-package ripgrep)

(use-package helm-rg
  :requires (helm ripgrep))

(use-package smex
  :bind
  (("M-x" . 'smex)
   ("M-X" . 'smex-major-mode-commands))
  :config
  (smex-initialize))

(use-package go-mode)

(use-package go-autocomplete
  :requires (go-mode auto-complete))

(use-package iedit)
(use-package google-this)
(use-package bm)

(global-set-key (kbd "C-`") 'other-frame)

;; python
;; pip install elpy rope jedi
(use-package elpy
  :bind
  (:map elpy-mode-map
        ("M-n" . 'elpy-nav-next-iblock)
        ("M-p" . 'elpy-nav-previous-iblock)
        ("S-n" . 'elpy-nav-forward-iblock)
        ("S-p" . 'elpy-nav-backward-iblock))
  :config
  (elpy-enable)
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  ;;; (elpy-clean-modeline)
  (setq elpy-modules (remove 'elpy-module-flymake elpy-modules)))

(use-package ess)

;; html/js stuff
(add-hook 'html-mode-hook
          (lambda ()
            (setq sgml-basic-offset 2)
            (setq indent-tabs-mode nil)))

;; quick register access to this file
(set-register ?e '(file . "~/.emacs.d/init.el"))

;; auto-open
(defun my-check-modified ()
  "Interactive prompt to check file modification."
  (interactive)
  (if (not (verify-visited-file-modtime (current-buffer)))
      (if (yes-or-no-p "Buffer modified, reload? ")
          (if (buffer-modified-p)
              (revert-buffer)
            (revert-buffer t t))
        (if (yes-or-no-p "Overwrite external modifications? ")
            (clear-visited-file-modtime)
          (set-buffer-modified-p (current-buffer))
          (save-buffer)))))

(advice-add 'helm-mini :after #'my-check-modified)

;; miscellaneous stuff

(setq echo-keystrokes 0.1)
(subword-mode 1)

;; powerline
(use-package powerline
  :config
  (setq powerline-arrow-shape 'curve)
  (powerline-default-theme))

;; flycheck
(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)

  (defun my/use-eslint-from-node-modules ()
    "Get local eslint."
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (defun my-get-venv-path (&optional path)
    "Get the virtualenv path starting at PATH if it exists."
    (cond ((or (string= path "/") (eq (buffer-file-name) nil)) nil)
          ((eq path nil) (my-get-venv-path (buffer-file-name)))
          ((file-directory-p (concat path "venv")) (concat path "venv"))
          ((file-exists-p path) (my-get-venv-path (file-name-directory (directory-file-name path))))
          (t "default")))
  (declare-function python-shell-calculate-exec-path "python")
  (defun flycheck-virtualenv-executable-find (executable)
    "Find an EXECUTABLE in the current virtualenv if any."
    (if (bound-and-true-p python-shell-virtualenv-root)
        (let ((exec-path (python-shell-calculate-exec-path)))
          (executable-find executable))
      (executable-find executable)))
  (defun flycheck-virtualenv-setup ()
    "Setup Flycheck for the current virtualenv."
    (progn
      (setq-local python-shell-virtualenv-path (my-get-venv-path))
      (setq-local flycheck-executable-find #'flycheck-virtualenv-executable-find)))

  :hook
  ((flycheck-mode . my/use-eslint-from-node-modules)
   (flycheck-mode . flycheck-virtualenv-setup)))

(use-package flycheck-inline
  :requires flycheck
  :hook ((flycheck-mode . flycheck-inline-mode)))

(use-package flycheck-rust
  :requires (flycheck rust-mode))

(use-package flycheck-pycheckers
  :requires (flycheck elpy)
  :hook
  ((flycheck-mode . flycheck-pycheckers-setup)))

(use-package tox)

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(set-frame-font "Mononoki")

(use-package rust-mode
  :hook
  ((flycheck-mode . flycheck-rust-setup)))

(use-package lsp-mode
  :config
  (add-hook 'prog-mode-hook #'lsp))

(use-package lsp-ui
  :requires lsp-mode)

(use-package dap-mode
  :config
  (require 'dap-lldb)
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup))

(use-package treemacs)

(use-package treemacs-projectile
  :requires (treemacs projectile))

(use-package treemacs-evil
  :requires (treemacs evil))

(use-package auctex
  :defer t
  :ensure t)
(use-package ecb)
(use-package gist)
(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t))
(use-package handlebars-mode)
(use-package jinja2-mode)
(use-package paradox)
(use-package popup)

(if (version< "27.0" emacs-version)
    (set-fontset-font
     "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
  (set-fontset-font
   t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :config
  (require 'org-mu4e)

  (setq
   mu4e-get-mail-command "offlineimap"
   mu4e-update-interval 600
   mu4e-trash-folder "/Trash")
  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-action-view-in-browser) t)
  (setq mu4e-completing-read-function 'completing-read)
  (setq mu4e-bookmarks '())
  (add-to-list 'mu4e-bookmarks
               '(:name "Reading list"
                       :query "tag:reading-list and (flag:unread or date:today)"
                       :key ?l))
  (add-to-list 'mu4e-bookmarks
               '(:name "Today's messages"
                       :query "date:today..now and not maildir:/Trash"
                       :key ?t))
  (add-to-list 'mu4e-bookmarks
               '(:name "Last 7 days"
                       :query "date:7d..now and not maildir:/Trash"
                       :key ?w))
  (setq mu4e-bookmarks (reverse mu4e-bookmarks))
  (add-to-list 'mu4e-marks
               '(tag
                 :char       "l"
                 :prompt     "reading list"
                 :show-target (lambda (target) "Add to reading list")
                 :action      (lambda (docid msg target)
                                (mu4e-action-retag-message msg "+reading-list"))))
  (mu4e~headers-defun-mark-for tag)
  (define-key mu4e-headers-mode-map (kbd "l") 'mu4e-headers-mark-for-tag)
  (setq mu4e-headers-fields '((:human-date . 8) (:flags . 6) (:tags . 12) (:from . 22) (:subject)))

  (defun read-all-from-string (string)
    "Call `read-from-string` iteratively on the STRING until the end."
    (let ((offset 0) (result '()))
      (condition-case nil
          (while t
            (let ((readresult (read-from-string string offset)))
              (progn
                (add-to-list 'result (car readresult))
                (setq offset (cdr readresult)))))
        (end-of-file (reverse result)))))
  (defun autotag-reading-list-from (email)
    "Auto-tag messages from EMAIL."
    (let* ((temp-file (make-temp-file "mu4e"))
           (mu-output (shell-command-to-string
                       (concat "mu find not tag:reading-list from:" email " --format=sexp")))
           (messages (if (string-prefix-p "error: no matches for search expression" mu-output)
                         '()
                       (read-all-from-string mu-output)))
           (paths (mapcar (lambda (x) (plist-get x :path)) messages))
           )
      (mapcar (lambda (path)
                (progn
                  (call-process-shell-command
                   (concat "formail -A \"X-Keywords: reading-list\" > " temp-file) path)
                  (rename-file temp-file path t)
                  (mu4e-refresh-message path)
                  path))
              paths)))
  (defvar reading-list-emails '() "Email addresses to filter to reading-list in mu4e.")
  (defun autotag-reading-list ()
    "Hook to auto-tag messages from certain senders."
    (progn
      (mapc (lambda (e) (autotag-reading-list-from e)) reading-list-emails)
      (mu4e-update-index)))
  (add-hook 'mu4e-index-updated-hook #'autotag-reading-list)
  (setq shr-use-colors nil))

(use-package pocket-reader
  :config
  (setq pocket-reader-open-url-default-function
        (lambda (url)
          (funcall #'org-web-tools-read-url-as-org url)
          (visual-line-mode))))

;; Fix broken org-web-tools functions used by pocket-reader

(defun org-web-tools--read-org-bracket-link (&optional link)
  "Return (TARGET . DESCRIPTION) for Org bracket LINK or next link on current line."
  ;; Searching to the end of the line seems the simplest way
  (save-excursion
    (let (target desc)
      (if link
          ;; Link passed as arg
          (when (string-match org-link-bracket-re link)
            (setq target (match-string-no-properties 1 link)
                  desc (match-string-no-properties 3 link)))
        ;; No arg; get link from buffer
        (when (re-search-forward org-link-bracket-re (point-at-eol) t)
          (setq target (match-string-no-properties 1)
                desc (match-string-no-properties 2))))
      (when (and target desc)
        ;; Link found; return parts
        (cons target desc)))))

(defun after-org-web-tools-read-url-as-org (&rest r)
  "Extra setup after `org-web-tools-read-url-as-org' for use in `pocket-reader'.  Ignore R."
  (let* ((link (org-web-tools--read-org-bracket-link))
         (title (cdr link))
         (url (car link)))
    (insert "#+title: " title "\n#+roam_tags: website pocket\n#+roam_key: " url "\n\n"))
  (add-hook 'after-save-hook #'org-roam-db--update-file nil t)
  (reading-mode))

(defun reading-mode ()
  "Pseudo-'mode' to set up some nice display settings for reading things."
  (interactive)
  (outline-show-all)
  (view-mode)
  (setq-local line-spacing .1)
  (variable-pitch-mode)
  (let* ((ww (window-width))
         (mw (/ ww 10)))
    (set-window-margins
     (car (get-buffer-window-list (current-buffer) nil t))
     mw mw)))

(add-hook 'org-mode-hook #'visual-line-mode)

(advice-add 'org-web-tools-read-url-as-org :after #'after-org-web-tools-read-url-as-org)

(use-package org-roam
  :diminish org-roam-mode
  :hook
  (after-init . org-roam-mode)
  :config
  (setq org-roam-completion-system 'helm)
  (setq org-roam-dailies-capture-templates
        '(("d" "daily" plain (function org-roam-capture--get-point)
           ""
           :immediate-finish t
           :file-name "%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d>\n#+roam_tags: daily")))
  :bind (("C-c l" . org-store-link)
         :map org-roam-mode-map
         ("C-c n l" . org-roam)
         ("C-c n f" . org-roam-find-file)
         ("C-c n g" . org-roam-show-graph)
         ("C-c n t" . org-roam-dailies-today)
         ("C-c n y" . org-roam-dailies-yesterday)
         :map org-mode-map
         ("C-c n i" . org-roam-insert)))

(add-hook 'org-mode-hook (lambda () (electric-pair-mode 0)))
(setq org-pretty-entities t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(use-package helm-org-rifle
  :requires helm)

(define-key visual-line-mode-map (kbd "M-n") #'next-logical-line)
(define-key visual-line-mode-map (kbd "M-p") #'previous-logical-line)

(let ((init-local (concat user-emacs-directory "init-local.el")))
  (when (file-exists-p init-local)
    (load-file init-local)))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

(provide 'init)
;;; init.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime unresolved)
;; End:
