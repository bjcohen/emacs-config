;;; init.el --- bjcohen's Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq straight-profiles
      '((nil . "default.el")
        ;; Packages which are pinned to a specific commit.
        (pinned . "pinned.el")))

(defvaralias 'comp-deferred-compilation-deny-list 'native-comp-deferred-compilation-deny-list)

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

(use-package el-patch)

(if (version< "28.0" emacs-version)
    (progn
      (el-patch-defmacro define-obsolete-variable-alias ( obsolete-name current-name
                                                          (el-patch-add &optional) when
                                                          (el-patch-remove &optional) docstring)
        "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.

WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number.

This macro evaluates all its parameters, and both OBSOLETE-NAME
and CURRENT-NAME should be symbols, so a typical usage would look like:

  (define-obsolete-variable-alias 'foo-thing 'bar-thing \"27.1\")

This macro uses `defvaralias' and `make-obsolete-variable' (which see).
See the Info node `(elisp)Variable Aliases' for more details.

If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
where OBSOLETE-NAME may be set, e.g. in an init file, before the
alias is defined), then the define-obsolete-variable-alias
statement should be evaluated before the defcustom, if user
customizations are to be respected.  The simplest way to achieve
this is to place the alias statement before the defcustom (this
is not necessary for aliases that are autoloaded, or in files
dumped with Emacs).  This is so that any user customizations are
applied before the defcustom tries to initialize the
variable (this is due to the way `defvaralias' works).

For the benefit of Customize, if OBSOLETE-NAME has
any of the following properties, they are copied to
CURRENT-NAME, if it does not already have them:
`saved-value', `saved-variable-comment'."
        (declare (doc-string 4))
        `(progn
           (defvaralias ,obsolete-name ,current-name ,docstring)
           ;; See Bug#4706.
           (dolist (prop '(saved-value saved-variable-comment))
             (and (get ,obsolete-name prop)
                  (null (get ,current-name prop))
                  (put ,current-name prop (get ,obsolete-name prop))))
           (make-obsolete-variable ,obsolete-name ,current-name (el-patch-wrap 1 2 (if ,when ,when "0.0")))))
      (el-patch-defmacro define-obsolete-function-alias ( obsolete-name current-name
                                                          (el-patch-add &optional) when
                                                          (el-patch-remove &optional) docstring)
        "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.

\(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")

is equivalent to the following two lines of code:

\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"22.1\")

WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.

See the docstrings of `defalias' and `make-obsolete' for more details."
        (declare (doc-string 4))
        `(progn
           (defalias ,obsolete-name ,current-name ,docstring)
           (make-obsolete ,obsolete-name ,current-name (el-patch-wrap 1 2 (if ,when ,when "0.0")))))
      ))

;;; General Config

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
(display-battery-mode 1)

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

;; ispell
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(diminish 'flyspell-mode)

(use-package magit
  :demand t)

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
  (persp-state-default-file (concat user-emacs-directory "persp-save"))
  (persp-mode-prefix-key (kbd "C-x x")))

(use-package evil
  :diminish undo-tree-mode
  :config
  (evil-mode 1)
  (setq evil-default-state 'emacs))

(use-package evil-surround
  :config
  (global-evil-surround-mode))

(diminish 'auto-revert-mode)

(use-package ripgrep)

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
  )

(use-package rustic
  :config
  (setq rustic-lsp-server 'rust-analyzer))

(use-package lsp-mode
  :diminish lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(emacs-lisp-mode . "elisp"))
  :hook
  (prog-mode . lsp))

(use-package lsp-ui
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (lsp-ui-peek-enable t)
  (setq lsp-ui-doc-position 'at-point
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-peek-enable t))

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

;; (use-package unicode-fonts
;;   :config
;;   (unicode-fonts-setup))

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
               '("org" . "e0b05b07528dea684f3439c017370436b8d37b50"))) ;; 9.5.4

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :config
  (require 'org-mu4e)

  (setq
   mu4e-get-mail-command "offlineimap"
   mu4e-update-interval nil
   mu4e-split-view 'single-window
   mu4e-headers-date-format "%Y-%m-%d"
   mu4e-headers-fields '((:human-date . 10) (:flags . 6) (:tags . 12) (:from . 22) (:subject)))
  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-action-view-in-browser) t)
  (setq mu4e-completing-read-function 'completing-read)
  (setq mu4e-bookmarks '())
  (add-to-list 'mu4e-bookmarks
               '(:name "Reading list"
                       :query "(tag:reading-list or tag:\\\\Important and maildir:/Gmail/INBOX) and (flag:unread or date:today) and not (flag:trashed or maildir:/Ionos/Trash)"
                       :key ?l))
  (add-to-list 'mu4e-bookmarks
               '(:name "Today's messages"
                       :query "date:today..now and not (flag:trashed or maildir:/Ionos/Trash)"
                       :key ?t))
  (add-to-list 'mu4e-bookmarks
               '(:name "Last 7 days"
                       :query "date:7d..now and not (flag:trashed or maildir:/Ionos/Trash)"
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
           (messages (if (string-prefix-p "no matches for search expression" mu-output)
                         '()
                       (read-all-from-string mu-output)))
           (paths (mapcar (lambda (x) (plist-get x :path)) messages))
           )
      (mapcar (lambda (path)
                (progn
                  (call-process-shell-command
                   (concat "formail -A \"X-Keywords: reading-list\" > " temp-file) path)
                  (rename-file temp-file path t)
                  (mu4e--refresh-message path)
                  path))
              paths)))
  (defvar reading-list-emails '() "Email addresses to filter to reading-list in mu4e.")
  (defun autotag-reading-list ()
    "Hook to auto-tag messages from certain senders."
    (if (> (plist-get mu4e-index-update-status :updated) 0)
        (progn
          (mapc (lambda (e) (autotag-reading-list-from e)) reading-list-emails)))
    )
  (add-hook 'mu4e-index-updated-hook #'autotag-reading-list)
  (setq shr-use-colors nil))

(setq slug-trim-chars
      '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
        768 ; U+0300 COMBINING GRAVE ACCENT
        769 ; U+0301 COMBINING ACUTE ACCENT
        770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
        771 ; U+0303 COMBINING TILDE
        772 ; U+0304 COMBINING MACRON
        774 ; U+0306 COMBINING BREVE
        775 ; U+0307 COMBINING DOT ABOVE
        776 ; U+0308 COMBINING DIAERESIS
        777 ; U+0309 COMBINING HOOK ABOVE
        778 ; U+030A COMBINING RING ABOVE
        780 ; U+030C COMBINING CARON
        795 ; U+031B COMBINING HORN
        803 ; U+0323 COMBINING DOT BELOW
        804 ; U+0324 COMBINING DIAERESIS BELOW
        805 ; U+0325 COMBINING RING BELOW
        807 ; U+0327 COMBINING CEDILLA
        813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
        814 ; U+032E COMBINING BREVE BELOW
        816 ; U+0330 COMBINING TILDE BELOW
        817 ; U+0331 COMBINING MACRON BELOW
        ))

(defun my/title-to-slug (title)
  "Convert TITLE to a filename-suitable slug."
  (cl-flet* ((nonspacing-mark-p (char)
                                (memq char slug-trim-chars))
             (strip-nonspacing-marks (s)
                                     (ucs-normalize-NFC-string
                                      (apply #'string (seq-remove #'nonspacing-mark-p
                                                                  (ucs-normalize-NFD-string s)))))
             (cl-replace (title pair)
                         (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_")  ;; convert anything not alphanumeric
                    ("__*" . "_")  ;; remove sequential underscores
                    ("^_" . "")  ;; remove starting underscore
                    ("_$" . "")))  ;; remove ending underscore
           (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
      (downcase slug))))

(cl-defun my/pocket-reader-open-url-function (url &key (show-buffer-fn #'switch-to-buffer))
  "Open a URL with org-web-tools and then do post-conversion setup.  Pass SHOW-BUFFER-FN on."
  (org-web-tools-read-url-as-org url :show-buffer-fn show-buffer-fn)
  (let* ((link (org-web-tools--read-org-bracket-link))
         (title (cdr link))
         (url (car link)))
    (insert "#+title: " title "\n#+roam_tags: website pocket\n#+roam_key: " url "\n\n")
    (set-visited-file-name (concat (my/title-to-slug title) ".org")))
  (reading-mode))

(use-package pocket-reader
  :straight (pocket-reader :type git :host github :repo "alphapapa/pocket-reader.el"
                           :fork "bjcohen/pocket-reader.el")
  :custom
  (pocket-reader-open-url-default-function #'my/pocket-reader-open-url-function))

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
  :straight (org-roam :type git :flavor melpa :host github :repo "org-roam/org-roam"
                      :files (:defaults "extensions/*.el"))
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-setup)
  :custom
  (org-roam-enable-headline-linking nil)
  (org-roam-dailies-capture-templates
   `(("d" "default" entry
     "* %?"
     :if-new (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d>\n#+roam_tags: daily\n\n* Reading"))))
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  :bind (("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n t" . org-roam-dailies-goto-today)
         ("C-c n y" . org-roam-dailies-goto-yesterday)
         ("C-c n m" . org-roam-dailies-goto-tomorrow)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)))

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

(use-package paredit
  :diminish
  :hook
  (lisp-data-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode))

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
  :demand t
  :after company-box
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-enable-buffer-reordering)
  (setq centaur-tabs-adjust-buffer-order 'left)
  (defun my/centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       ;; Buffer name not match below blacklist.
       (string-prefix-p "*org-roam" name)
       (string-prefix-p "*company" name)
       (string-prefix-p " *mu4e" name)
       )))
  (setq centaur-tabs-hide-tab-function 'my/centaur-tabs-hide-tab)
  (defun my/centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode'
`emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (and (string-prefix-p "*mu4e" (buffer-name))
                (not (string-equal "*mu4e-server*" (buffer-name))))
           (string-equal "*pocket-reader*" (buffer-name))
           (and (string-prefix-p "*elfeed-" (buffer-name))
                (not (string-equal "*elfeed-log*" (buffer-name))))
           (and (derived-mode-p 'org-mode)
                (save-excursion
                  (goto-char (point-min))
                  (search-forward "#+roam_tags: website pocket" nil t)))
           (string-equal "*Article*" (buffer-name)))
       "Reading")
      ((memq major-mode '(org-mode org-agenda-mode diary-mode))
       "Writing")
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
  (setq centaur-tabs-buffer-groups-function 'my/centaur-tabs-buffer-groups)
  (setq centaur-tabs-cycle-scope 'tabs)
  (defun centaur-tabs-nth-tab (n)
    (let* ((tabset (centaur-tabs-current-tabset t))
           (tabs (centaur-tabs-tabs tabset))
           (tab (if (>= n 0)
                    (nth n tabs)
                  (nth (- (+ 1 n)) (reverse tabs)))))
      (when tab
        (centaur-tabs-buffer-select-tab tab))))
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
  ("C-1" . (lambda () (interactive) (centaur-tabs-nth-tab 0)))
  ("C-2" . (lambda () (interactive) (centaur-tabs-nth-tab 1)))
  ("C-3" . (lambda () (interactive) (centaur-tabs-nth-tab 2)))
  ("C-4" . (lambda () (interactive) (centaur-tabs-nth-tab 3)))
  ("C-5" . (lambda () (interactive) (centaur-tabs-nth-tab 4)))
  ("C-6" . (lambda () (interactive) (centaur-tabs-nth-tab 5)))
  ("C-7" . (lambda () (interactive) (centaur-tabs-nth-tab 6)))
  ("C-8" . (lambda () (interactive) (centaur-tabs-nth-tab -2)))
  ("C-9" . (lambda () (interactive) (centaur-tabs-nth-tab -1)))
  :hook
  (treemacs-mode . centaur-tabs-local-mode)
  (mu4e~update-mail-mode . centaur-tabs-local-mode)
  (ediff-mode . centaur-tabs-local-mode)
  (lsp-ui-doc-frame-mode . centaur-tabs-local-mode)
  (lsp-ui-imenu-mode . centaur-tabs-local-mode)
  (rustic-compilation-mode . centaur-tabs-local-mode)
  )

(use-package ctrlf
  :config
  (ctrlf-mode +1)
  (add-hook 'ido-minibuffer-setup-hook (lambda () (ctrlf-local-mode -1))))

(use-package protobuf-mode)

;; https://codeberg.org/vifon/emacs-config/src/branch/master/emacs.d/lisp/20-completing-read.el

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*"))
  :init
  (vertico-mode)

  :bind
  (("C-x M-r" . vertico-repeat)
   :map vertico-map
   ("C-l" . vertico-directory-delete-word)
   ("M-g" . vertico-multiform-grid)
   ("M-q" . vertico-multiform-flat))

  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (vertico-multiform-mode 1)
  (setq vertico-multiform-categories '((consult-grep buffer))
                  vertico-multiform-commands '((tmm-menubar flat)
                                               (tmm-shortcut flat))))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  (setq orderless-matching-styles '(orderless-regexp
                                    orderless-initialism
                                    orderless-prefixes)
        orderless-component-separator #'orderless-escapable-split-on-space))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  :bind
  (("M-g g" . consult-goto-line)
   ("M-g l" . consult-line)
   ("M-g o" . consult-outline)
   ("M-g i" . consult-imenu)
   ("M-g r" . consult-ripgrep)
   ("C-x C-r" . consult-recent-file)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap yank-pop] . consult-yank-pop)
   :map minibuffer-local-map
   ([remap previous-matching-history-element] . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-project-root-function #'vc-root-dir)
  (recentf-mode t)
  (setq recentf-max-menu-items 25)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (setq consult-narrow-key "<")
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package embark-consult)

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1))

(use-package corfu
  :straight t
  :init (global-corfu-mode 1))

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
