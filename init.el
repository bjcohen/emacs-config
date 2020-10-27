;;; init.el --- bjcohen's Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq straight-profiles
      '((nil . "default.el")
        ;; Packages which are pinned to a specific commit.
        (pinned . "pinned.el")))

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
(autoload #'straight-x-pull-all "straight-x")
(autoload #'straight-x-freeze-versions "straight-x")

;;; General Config

(use-package diminish)
(use-package bind-key)
(use-package el-patch)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(display-battery-mode 1)

(global-set-key (kbd "C-w") #'backward-kill-word)

(use-package yascroll
  :config
  (global-yascroll-bar-mode 1))

(toggle-frame-maximized)
(setq frame-resize-pixelwise t)

(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier nil)

(save-place-mode 1)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-set-key [?\M-g] 'goto-line)  ; easier to jump to specific lines (M-g)

;; ispell
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(diminish 'flyspell-mode)

(use-package magit)

(use-package forge
  :after magit)

(electric-pair-mode)

;;; random stuff

(add-hook 'prog-mode-hook
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
  (defun close-on-successful-exit (_ desc)
    "Close the compilation BUFFER in two seconds if DESC indei it exited successfully."
    (unless (string-match "exited abnormally" desc)
      (run-at-time
       "2 sec" nil 'delete-windows-on
       (get-buffer-create "*compilation*"))
      (message "No Compilation Errors!")))

  (add-hook 'compilation-finish-functions 'close-on-successful-exit))

(use-package projectile
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (projectile-register-project-type 'automake '("Makefile.am")
                                    :project-file "Makefile.am"
                                    :compile "autogen.sh && make"
                                    :test "make test"))

(use-package projectile-ripgrep)

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

(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook #'whitespace-mode)
(setq whitespace-style '(face trailing lines-tail tabs empty))
(diminish 'whitespace-mode)

(setq whitespace-trailing 'font-lock-warning-face)
(setq whitespace-line 'font-lock-warning-face)
(setq whitespace-line-column 100)
(setq whitespace-empty 'font-lock-warning-face)

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
  :config
  (global-evil-surround-mode))

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

(diminish 'auto-revert-mode)

(use-package helm
  :diminish helm-mode
  :bind (("C-x r b" . 'helm-filtered-bookmarks)
         ("C-x C-f" . 'helm-find-files)
         ("C-x C-r" . 'helm-recentf)
         ("C-x b" . 'helm-mini))
  :config
  (recentf-mode t)
  (setq recentf-max-menu-items 25)
  (helm-mode t)
  :config/el-patch
  (defun helm-mini ()
    "Preconfigured `helm' displaying `helm-mini-default-sources'."
    (interactive)
    (require 'helm-x-files)
    (unless helm-source-buffers-list
      (setq helm-source-buffers-list
            (helm-make-source "Buffers" 'helm-source-buffers)))
    (helm :sources helm-mini-default-sources
          :buffer "*helm mini*"
          :ff-transformer-show-only-basename nil
          :truncate-lines helm-buffers-truncate-lines
          :left-margin-width helm-buffers-left-margin-width)
    (el-patch-add (my-check-modified))))

(use-package helm-icons
  :config
  (helm-icons-enable))

(use-package helm-posframe
  :custom
  (helm-display-header-line nil)
  (helm-echo-input-in-header-line nil)
  (helm-posframe-border-width 1)
  (helm-posframe-parameters '((left-fringe . 1)
                              (right-fringe . 1)))
  (helm-posframe-poshandler #'posframe-poshandler-point-top-left-corner)
  :config
  (helm-posframe-enable))

(use-package helm-projectile
  :bind
  (:map projectile-mode-map
        ("C-c p s g" . 'helm-projectile-grep)
        ("C-c p s r" . 'helm-projectile-rg)))

(use-package helm-lsp
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  :bind
  (:map lsp-mode-map
        ("C-<return>" . 'helm-lsp-code-actions)))

(use-package flx)

(use-package helm-flx
  :config
  (helm-flx-mode +1))

(use-package ripgrep)

(use-package helm-rg)

(use-package smex
  :bind
  (("M-x" . 'smex)
   ("M-X" . 'smex-major-mode-commands))
  :config
  (smex-initialize))

(use-package go-mode)

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
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
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
  (add-hook 'flycheck-mode-hook #'flycheck-virtualenv-setup))

(use-package flycheck-inline
  :hook ((flycheck-mode . flycheck-inline-mode)))

(use-package flycheck-rust)

(use-package flycheck-pycheckers
  :hook
  ((flycheck-mode . flycheck-pycheckers-setup)))

(use-package tox)

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(set-frame-font "Mononoki")
(global-prettify-symbols-mode)

(use-package rust-mode
  :hook
  (flycheck-mode . flycheck-rust-setup)
  (rust-mode . lsp-ui-imenu)
  :config
  (lsp-rust-switch-server 'rust-analyzer))

(use-package lsp-mode
  :hook
  (prog-mode . lsp))

(use-package lsp-ui
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (lsp-ui-peek-enable t)
  (setq lsp-ui-doc-position 'at-point
        lsp-ui-sideline-show-hover t))

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))

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

(use-package treemacs-projectile)

(use-package treemacs-evil)

(use-package all-the-icons
  :config
  (all-the-icons-install-fonts t))

(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

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

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(use-package org
  :hook
  (org-mode . (lambda () (electric-pair-local-mode 0)))
  :config
  (setq org-pretty-entities t
        org-use-speed-commands t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))

(let ((straight-current-profile 'pinned))
  (add-to-list 'straight-x-pinned-packages
               '("org" . "fbccf09c740a298e39daa9170bceaa92684f2fe0")))

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :config
  (require 'org-mu4e)

  (setq
   mu4e-get-mail-command "offlineimap"
   mu4e-update-interval 600
   mu4e-trash-folder "/Trash"
   mu4e-split-view 'single-window)
  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-action-view-in-browser) t)
  (setq mu4e-completing-read-function 'completing-read)
  (setq mu4e-bookmarks '())
  (add-to-list 'mu4e-bookmarks
               '(:name "Reading list"
                       :query "tag:reading-list and (flag:unread or date:today) and not maildir:/Trash"
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

(cl-defun my/pocket-reader-open-url-function (url &key (show-buffer-fn #'switch-to-buffer))
  "Open a URL with org-web-tools and then do post-conversion setup.  Pass SHOW-BUFFER-FN on."
  (org-web-tools-read-url-as-org url :show-buffer-fn show-buffer-fn)
  (let* ((link (org-web-tools--read-org-bracket-link))
         (title (cdr link))
         (url (car link)))
    (insert "#+title: " title "\n#+roam_tags: website pocket\n#+roam_key: " url "\n\n")
    (set-visited-file-name (concat (funcall org-roam-title-to-slug-function title) ".org")))
  (add-hook 'after-save-hook #'org-roam-db--update-file nil t)
  (reading-mode))

(use-package pocket-reader
  :straight (pocket-reader :type git :host github :repo "alphapapa/pocket-reader.el"
                           :fork "bjcohen/pocket-reader.el")
  :custom
  (pocket-reader-open-url-default-function #'my/pocket-reader-open-url-function))

(use-package org-web-tools
  :defer
  :config/el-patch
  (defun org-web-tools--read-org-bracket-link (&optional link)
    "Return (TARGET . DESCRIPTION) for Org bracket LINK or next link on current line."
    ;; Searching to the end of the line seems the simplest way
    (save-excursion
      (let (target desc)
        (if link
            ;; Link passed as arg
            (when (string-match (el-patch-swap org-bracket-link-regexp org-link-bracket-re) link)
              (setq target (match-string-no-properties 1 link)
                    desc (match-string-no-properties (el-patch-swap 3 2) link)))
          ;; No arg; get link from buffer
          (when (re-search-forward (el-patch-swap org-bracket-link-regexp org-link-bracket-re) (point-at-eol) t)
            (setq target (match-string-no-properties 1)
                  desc (match-string-no-properties (el-patch-swap 3 2)))))
        (when (and target desc)
          ;; Link found; return parts
          (cons target desc))))))

(use-package org-superstar
  :hook
  (org-mode . (lambda () (org-superstar-mode 1)))
  :custom
  (org-superstar-special-todo-items t)
  (org-hide-leading-stars t))

(defun reading-mode ()
  "Pseudo-'mode' to set up some nice display settings for reading things."
  (interactive)
  (visual-line-mode t)
  (outline-show-all)
  (view-mode)
  (setq-local line-spacing .1)
  (variable-pitch-mode)
  (let* ((ww (window-width))
         (mw (/ ww 10)))
    (set-window-margins
     (car (get-buffer-window-list (current-buffer) nil t))
     mw mw)))

(use-package view
  :bind
  (:map view-mode-map
        ("j" . forward-line)
        ("k" . previous-line)
        ("h" . backward-char)
        ("l" . forward-char)))

(add-hook 'org-mode-hook #'visual-line-mode)
(diminish 'visual-line-mode)

(use-package org-roam
  :diminish org-roam-mode
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-enable-headline-linking nil)
  (org-roam-completion-system 'helm)
  (org-roam-dailies-capture-templates
   '(("d" "daily" plain (function org-roam-capture--get-point)
      ""
      :immediate-finish t
      :file-name "%<%Y-%m-%d>"
      :head "#+title: %<%Y-%m-%d>\n#+roam_tags: daily\n\n* Reading")))
  :bind (("C-c l" . org-store-link)
         :map org-roam-mode-map
         ("C-c n l" . org-roam)
         ("C-c n f" . org-roam-find-file)
         ("C-c n g" . org-roam-show-graph)
         ("C-c n t" . org-roam-dailies-today)
         ("C-c n y" . org-roam-dailies-yesterday)
         ("C-c n m" . org-roam-dailies-tomorrow)
         :map org-mode-map
         ("C-c n i" . org-roam-insert)))

(use-package helm-org-rifle)

(define-key visual-line-mode-map (kbd "M-n") #'next-logical-line)
(define-key visual-line-mode-map (kbd "M-p") #'previous-logical-line)

(use-package vterm
  :if module-file-suffix
  :ensure t)

(use-package spotify
  :straight (spotify :type git :host github :repo "danielfm/spotify.el"
                     :fork "bjcohen/spotify.el"
                     :files (:defaults "spotify_oauth2_callback_server.py"))
  :config
  (setq spotify-transport 'connect
        spotify-player-status-refresh-interval 15)
  (global-spotify-remote-mode 0)
  :bind-keymap ("C-c ." . spotify-command-map))

(use-package unfill)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package org-noter)

(use-package pdf-tools
  :config
  (pdf-tools-install))

(use-package org-pdftools
  :hook (org-load . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode))

(use-package company-statistics
  :config
  (company-statistics-mode))

(use-package company-org-roam
  :config
  (push 'company-org-roam company-backends)
  :config/el-patch
  (defun company-org-roam--filter-candidates (prefix candidates)
    "Filter CANDIDATES that start with PREFIX.
The string match is case-insensitive."
    (-filter (lambda (candidate)
               (el-patch-swap
                 (string-prefix-p prefix candidate t)
                 (string-match-p (concat "\\(^\\|[^[:word:]]\\)" prefix) candidate))) candidates))
  (defun company-org-roam (command &optional arg &rest _)
    "Define a company backend for Org-roam.
COMMAND and ARG are as per the documentation of `company-backends'."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend #'company-org-roam))
      (prefix
       (and
        (bound-and-true-p org-roam-mode)
        (org-roam--org-roam-file-p (buffer-file-name (buffer-base-buffer)))
        (or (el-patch-add (company-grab-line org-heading-regexp 2)) (company-grab-symbol) 'stop)))
      (candidates
       (company-org-roam--get-candidates arg))
      (post-completion (company-org-roam--post-completion arg))
      (el-patch-add (location
                     (let* ((cache (gethash (file-truename org-roam-directory) company-org-roam-cache))
                            (path (gethash arg cache)))
                       (cons path 0))))
      ))
  )

(use-package s)
(use-package dash)

(defun org-link-get-title (s)
  "Get the title from a (possibly nested) link string S."
  (if (string-match org-link-any-re s)
      (match-string 3 s)
    s))

(defun company-org-headings (command &optional arg &rest _)
  "Define a company backend for Org headings.
COMMAND and ARG are as per the documentation of `company-backends'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend #'company-org-headings))
    (prefix (save-excursion
              (if (looking-back "]][[:word:]]*" (point-at-bol))
                  (let ((start (point))
                        (c 1))
                    (re-search-backward "]]\\|\\[\\[" (point-at-bol))
                    (while (and (> c 0) (ignore-errors (re-search-backward "]]\\|\\[\\[" (point-at-bol))))
                      (if (string= (match-string-no-properties 0) "]]")
                          (setq c (1+ c))
                        (setq c (1- c))))
                    (if (= c 0) (buffer-substring-no-properties (point) start))))))
    (candidates
     (let* ((link-and-rest
             (with-temp-buffer
               (insert arg)
               (goto-char 0)
               (let* ((link (org-element-link-parser)))
                 (re-search-forward org-link-any-re)
                 (cons link (buffer-substring-no-properties (point) (point-max))))))
            (link (car link-and-rest))
            (rest (cdr link-and-rest))
            (type (org-element-property :type link))
            (path (org-element-property :path link)))
       (if (string= type "file")
           (->> (with-temp-buffer
                 (insert-file-contents path)
                 (s-match-strings-all org-heading-regexp (buffer-string)))
                (-map (lambda (c) (nth 2 c)))
                (-filter (lambda (c) (string-match-p (concat "\\(^\\|[^[:word:]]\\)" rest) c)))
                (-map (lambda (c) (format "[[file:%s::*%s][%s]]"
                                          path
                                          (replace-regexp-in-string "\]\\[" "\\\\][" (replace-regexp-in-string "\]\]" "]\\\\]" c))
                                          (org-link-get-title c))))))))
    (location
     (let* ((link (with-temp-buffer
                    (insert arg)
                    (goto-char 0)
                    (org-element-link-parser)))
            (path (org-element-property :path link))
            (search-option (org-element-property :search-option link))
            (line (with-temp-buffer
                    (insert-file-contents path)
                    (org-link-search search-option)
                    (count-lines 1 (point)))))
       (cons path line)))
    ))

(push 'company-org-headings company-backends)

(use-package ace-jump-mode
  :bind
  (("C-c SPC" . ace-jump-mode)
   ("C-c C-c SPC" . ace-jump-line-mode)))

(use-package elfeed)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(defun my/paredit-backward-kill-word-and-reindent ()
  (interactive)
  (paredit-backward-kill-word)
  (paredit-reindent-defun))

(use-package paredit
  :diminish
  :hook
  (lisp-data-mode . paredit-mode)
  :bind
  (:map paredit-mode-map ("M-DEL" . my/paredit-backward-kill-word-and-reindent)))

;; (use-package lispy
;;   :diminish
;;   :hook
;;   (prog-mode . lispy-mode))

(use-package ace-window
  :bind
  ("C-o" . ace-window))

(use-package avy
  :bind
  ("C-:" . avy-goto-char-timer))

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-enable-buffer-reordering)
  (setq centaur-tabs-adjust-buffer-order 'left)
  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       ;; Buffer name not match below blacklist.
       (string-prefix-p "*org-roam" name)
       (string-prefix-p "*company" name)
       (string-equal "*mu4e-loading*" name))))
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode'
`emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-prefix-p "*mu4e" (buffer-name))
           (string-equal "*pocket-reader*" (buffer-name))
           (string-equal "*elfeed-search*" (buffer-name))
           (memq major-mode '(org-mode org-agenda-mode diary-mode)))
       "Reading")
      ((string-equal "*" (substring (buffer-name) 0 1))
       "Emacs")
      ((memq major-mode '(magit-process-mode
                          magit-status-mode
                          magit-diff-mode
                          magit-log-mode
                          magit-file-mode
                          magit-blob-mode
                          magit-blame-mode
                          ))
       "Magit")
      ((derived-mode-p 'eshell-mode)
       "EShell")
      ((derived-mode-p 'emacs-lisp-mode)
       "Elisp")
      ((derived-mode-p 'dired-mode)
       "Dired")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  (setq centaur-tabs-cycle-scope 'tabs)
  :config/el-patch
  (defun company-box--get-buffer (&optional suffix)
    "Construct the buffer name, it should be unique for each frame."
    (el-patch-let ((mkbuf (get-buffer-create
                           (concat " *company-box-" (company-box--get-id) suffix "*"))))
      (el-patch-swap
        mkbuf
        (let ((buf mkbuf))
          (with-current-buffer buf
            (centaur-tabs-local-mode))
          buf))))
  :bind
  ("S-s-<tab>" . centaur-tabs-backward)
  ("s-<tab>" . centaur-tabs-forward)
  :hook
  (treemacs-mode . centaur-tabs-local-mode)
  (mu4e~update-mail-mode . centaur-tabs-local-mode)
  (ediff-mode . centaur-tabs-local-mode)
  (lsp-ui-doc-frame-mode . centaur-tabs-local-mode)
  (lsp-ui-imenu-mode . centaur-tabs-local-mode)
  )

(el-patch-validate-all)

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
