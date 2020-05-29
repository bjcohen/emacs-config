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

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;;; General Config

(setq-default save-place t)
(require 'saveplace)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-set-key [?\M-g] 'goto-line)  ; easier to jump to specific lines (M-g)

;; ispell
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))

(use-package egg)

(use-package autopair
  :config
  (autopair-global-mode)
  (setq autopair-autowrap t))

; random stuff

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

; hilight matching parens
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
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
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
  :config
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (ac-config-default))

;; tabs

(setq-default indent-tabs-mode nil)
(standard-display-ascii ?\t "^I")
(global-whitespace-mode)
(setq whitespace-style '(face trailing lines-tail))

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
  :config
  (evil-mode 1)
  (setq evil-default-state 'emacs)
  (use-package evil-surround
    :config
    (global-evil-surround-mode)))

(use-package helm
  :config
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (use-package helm-projectile
    :config
    (define-key projectile-mode-map (kbd "C-c p s g") 'helm-projectile-grep)
    (define-key projectile-mode-map (kbd "C-c p s r") 'helm-projectile-rg))
  (use-package helm-lsp)
  (recentf-mode t)
  (setq recentf-max-menu-items 25)
  (global-set-key "\C-x\ \C-r" 'helm-recentf))

(use-package ripgrep
  :config
  (use-package helm-rg))

;; smex
(use-package smex
  :config
  (smex-initialize))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
; Old M-x
; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; unlearn muscle memory...
(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier nil)

(use-package go-mode
  :config
  (use-package go-autocomplete))

(if (eq system-type 'darwin)
    (ns-toggle-toolbar))

(add-hook 'after-make-frame-functions 'ns-toggle-toolbar)

(use-package iedit)
(use-package google-this)
(use-package bm)

(global-set-key (kbd "C-`") 'other-frame)

;; python
;; pip install elpy rope jedi
(use-package elpy
  :config
  (elpy-enable)
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
                                        ; (elpy-clean-modeline)
  (define-key elpy-mode-map (kbd "M-n") 'elpy-nav-next-iblock)
  (define-key elpy-mode-map (kbd "M-p") 'elpy-nav-previous-iblock)
  (define-key elpy-mode-map (kbd "S-n") 'elpy-nav-forward-iblock)
  (define-key elpy-mode-map (kbd "S-p") 'elpy-nav-backward-iblock)
  (setq elpy-modules (remove 'elpy-module-flymake elpy-modules))
  (add-to-list 'auto-mode-alist '("BUCK\\'" . python-mode))
  (add-hook 'python-mode-hook
            (lambda()
              ;; ugh crazy addepar people
              (if (equal (file-name-nondirectory (buffer-file-name)) "BUCK")
                  (setq-local python-indent-offset 2)))))

(use-package ess)

;; html/js stuff
(add-hook 'html-mode-hook
          (lambda ()
            (setq sgml-basic-offset 2)
            (setq indent-tabs-mode nil)))

;; quick register access to this file
(set-register ?e '(file "~/.emacs.d/init.el"))

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
  (treemacs)
  (use-package treemacs-evil)
  (use-package treemacs-projectile)
  (use-package treemacs-evil))

(use-package auctex
  :defer t
  :ensure t)
(use-package ecb)
(use-package gist)
(use-package git-gutter
  :config
  (global-git-gutter-mode t))
(use-package handlebars-mode)
(use-package jinja2-mode)
(use-package paradox)
(use-package popup)

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
