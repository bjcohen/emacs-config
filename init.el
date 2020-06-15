;;; package --- Summary
;;; Commentary:
;;; Code:

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

;;; Configure use-package

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;; OS X Config

(use-package diminish)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(if (eq system-type 'darwin)
    (ns-toggle-toolbar))

(add-hook 'after-make-frame-functions 'ns-toggle-toolbar)

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
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (require 'inf-haskell))

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

(use-package ido
  :config
  (use-package flx)
  (use-package flx-ido)

  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)

  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (setq ido-use-faces nil)

  (use-package ido-completing-read+)
  (ido-ubiquitous-mode 1)
  (use-package ido-sort-mtime)
  (use-package idomenu))

(use-package projectile
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . 'projectile-command-map)
  :config
  (projectile-mode +1)
  (use-package projectile-ripgrep))

(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t))

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
;; (standard-display-ascii ?\t "^I")
(global-whitespace-mode)
(setq whitespace-style '(face trailing lines-tail))
(diminish 'global-whitespace-mode)

(defface redbackground
  '((t :background "dark red"))
  "Red background."
  :group 'custom-faces)

(setq whitespace-trailing 'redbackground)
(setq whitespace-line 'font-lock-warning-face)
(setq whitespace-line-column 100)


;; Sometimes I use these...

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; disable scrollbars

(set-scroll-bar-mode nil)

(use-package perspective
  :config
  (persp-mode))

(use-package evil
  :diminish undo-tree-mode
  :config
  (evil-mode 1)
  (setq evil-default-state 'emacs)
  (use-package evil-surround
    :config
    (global-evil-surround-mode)))

(use-package helm
  :bind (("C-x r b" . 'helm-filtered-bookmarks)
         ("C-x C-f" . 'helm-find-files)
         ("C-x C-r" . 'helm-recentf))
  :config
  (use-package helm-projectile
    :bind
    (:map projectile-mode-map
          ("C-c p s g" . 'helm-projectile-grep)
          ("C-c p s r" . 'helm-projectile-rg)))
  (use-package helm-lsp)
  (recentf-mode t)
  (setq recentf-max-menu-items 25))

(use-package helm-flx
  :requires helm
  :config
  (helm-flx-mode +1))

(use-package ripgrep
  :config
  (use-package helm-rg))

(use-package smex
  :bind
  (("M-x" . 'smex)
   ("M-X" . 'smex-major-mode-commands))
  :config
  (smex-initialize))

(use-package go-mode
  :config
  (use-package go-autocomplete))

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

(defadvice check-modified (after ido-switch-buffer activate)
  "Run a revert check when we switch to a buffer."
  (my-check-modified))

(ad-activate 'check-modified)

;; miscellaneous stuff

(setq echo-keystrokes 0.1)
(subword-mode 1)

;; powerline
(use-package powerline
  :config
  (setq powerline-arrow-shape 'curve)
  (powerline-default-theme))

(defun my-ido-project-files ()
  "Use ido to select a file from the project."
  (interactive)
  (let (my-project-root tbl)
    (setq my-project-root project-root)
    ;; get project files
    (setq project-files
          (split-string
           (shell-command-to-string
            (concat "find "
                    my-project-root
                    " \\( -name \"*.svn\" -o -name \"*.git\" \\) -prune -o -type f -print | grep -E -v \"\.(pyc)$\""
                    )) "\n"))
    ;; populate hash table (display repr => path)
    (setq tbl (make-hash-table :test 'equal))
    (let (ido-list)
      (mapc (lambda (path)
              ;; format path for display in ido list
              (setq key (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2|\\1" path))
              ;; strip project root
              (setq key (replace-regexp-in-string my-project-root "" key))
              ;; remove trailing | or /
              (setq key (replace-regexp-in-string "\\(|\\|/\\)$" "" key))
              (puthash key path tbl)
              (push key ido-list)
              )
            project-files
            )
      (find-file (gethash (ido-completing-read "project-files: " ido-list) tbl)))))

(global-set-key (kbd "C-c C-f") 'my-ido-project-files)

;; flycheck
(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)

  (use-package flycheck-inline)
  (use-package flycheck-rust)
  (use-package flycheck-pycheckers)

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

(use-package tox)

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(set-frame-font "Mononoki")

(use-package rust-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package lsp-mode
  :config
  (add-hook 'prog-mode-hook #'lsp)
  (use-package lsp-ui))

(use-package dap-mode
  :config
  (require 'dap-lldb)
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup))

(use-package treemacs
  :config
  (use-package treemacs-projectile)
  (use-package treemacs-evil))

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

;;; mu4e

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)
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
(setq mu4e-headers-fields '((:human-date . 12) (:flags . 6) (:tags . 12) (:mailing-list . 12) (:from . 22) (:subject)))

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
         (mu-output (shell-command-to-string (concat "mu find not tag:reading-list from:" email " --format=sexp")))
         (messages (if (string-prefix-p "error: no matches for search expression" mu-output)
                       '()
                     (read-all-from-string mu-output)))
         (paths (mapcar (lambda (x) (plist-get x :path)) messages))
         )
    (mapcar (lambda (path)
              (progn
                (call-process-shell-command (concat "formail -A \"X-Keywords: reading-list\" > " temp-file) path)
                (rename-file temp-file path t)
                (mu4e-refresh-message path)
                path))
            paths)))
(defvar reading-list-emails '() "Email addresses to filter to reading-list in mu4e.")
(defun autotag-reading-list ()
  "Hook to auto-tag messages from certain senders."
  (progn
    (mapcar (lambda (e) (autotag-reading-list-from e)) reading-list-emails)
    (mu4e-update-index)))
(add-hook 'mu4e-index-updated-hook #'autotag-reading-list)

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
          (when (string-match org-bracket-link-regexp link)
            (setq target (match-string-no-properties 1 link)
                  desc (match-string-no-properties 3 link)))
        ;; No arg; get link from buffer
        (when (re-search-forward org-bracket-link-regexp (point-at-eol) t)
          (setq target (match-string-no-properties 1)
                desc (match-string-no-properties 2))))
      (when (and target desc)
        ;; Link found; return parts
        (cons target desc)))))

(defun after-org-web-tools-read-url-as-org (&rest r)
  "Extra setup after org-web-tools-read-url-as-org for use in pocket-reader.  Ignore R."
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
  (whitespace-mode 0)
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
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph)
               ("C-c n t" . org-roam-dailies-today)
               ("C-c n y" . org-roam-dailies-yesterday))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(add-hook 'org-mode-hook (lambda () (electric-pair-mode 0)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))


(let ((init-local (concat user-emacs-directory "init-local.el")))
  (when (file-exists-p init-local)
    (load-file init-local)))

;;; Customize Section

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
