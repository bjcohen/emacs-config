;;; package --- Summary
;;; Commentary:
;;; Code:

(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(package-refresh-contents)

(defvar my-packages '(
                      ag
                      auctex
                      auto-complete
                      autopair
                      bm
                      clojure-mode
                      coffee-mode
                      ecb
                      editorconfig
                      egg
                      elein
                      elpy
                      ember-mode
                      ess
                      evil
                      evil-surround
                      exec-path-from-shell
                      flx
                      flx-ido
                      flycheck
                      flycheck-pycheckers
                      gist
                      git-gutter
                      go-mode
                      go-autocomplete
                      google-this
                      groovy-mode
                      handlebars-mode
                      haskell-mode
                      helm
                      helm-ag
                      helm-projectile
                      helm-rg
                      ido-completing-read+
                      ido-sort-mtime
                      idomenu
                      iedit
                      jinja2-mode
                      perspective
                      popup
                      powerline
                      projectile
                      projectile-ripgrep
                      ruby-mode
                      smex
                      solarized-theme
                      tox
                      use-package
                      yasnippet
                      ))

(mapc (lambda (p)
        (when (not (package-installed-p p)) (package-install p)))
      my-packages)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(add-to-list 'load-path "~/.emacs.d/lisp")

(setq-default save-place t)
(require 'saveplace)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))
(add-to-list 'auto-mode-alist '("jenkins/" . groovy-mode))
(global-set-key [?\M-g] 'goto-line)  ; easier to jump to specific lines (M-g)

;; ispell
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))

; egg
(require 'egg)

; git-gutter
(global-git-gutter-mode t)

; autopair

(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap 'true)

; random stuff

(add-hook 'c-mode-common-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face t)))))

;; haskell mode
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(require 'inf-haskell)

;; Compile

(require 'compile)

(defun close-on-successful-exit (buffer desc)
	"Close the compilation BUFFER in two seconds if DESC indei it exited successfully."
        (unless (string-match "exited abnormally" desc)
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!")))

(add-hook 'compilation-finish-functions 'close-on-successful-exit)

; hilight matching parens
(defvar show-paren-overlay nil)
(defvar show-paren-overlay-1 nil)
(show-paren-mode)

;; ido and related
(require 'ido)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-use-faces nil)

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(require 'ido-sort-mtime)
(require 'idomenu)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(require 'projectile-ripgrep)

;; solarized
(require 'solarized)
(load-theme 'solarized-dark t)

;; ruby-mode
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; Yasnippet

(require 'yasnippet)
(yas-load-directory "~/.emacs.d/snippets")
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "C-c TAB") 'yas-expand)

;; autocomplete-mode

(require 'auto-complete-config)
(add-to-list 'ac-sources 'ac-source-yasnippet)
(ac-config-default)
(load "auto-complete-haskell.el")

;; tabs

;; (setq-default indent-tabs-mode nil)
;; (standard-display-ascii ?\t "^I")
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

;; clojure
(require 'clojure-mode)
; (require 'nrepl)
; (require 'ac-nrepl)

;; disable scrollbars

(set-scroll-bar-mode nil)

;; perspective

(require 'perspective)
(persp-mode)

;; Evil
(require 'evil)
(evil-mode 1)
(setq evil-default-state 'emacs)

;; Helm
(require 'helm-config)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(require 'helm-projectile)
(define-key projectile-mode-map (kbd "C-c p s g") 'helm-projectile-grep)
(define-key projectile-mode-map (kbd "C-c p s r") 'helm-projectile-rg)
(define-key projectile-mode-map (kbd "C-c p s s") 'helm-projectile-ag)

;; grep replacements
(require 'ripgrep)
(require 'helm-rg)

(require 'ag)

;; smex
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
; Old M-x
; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; unlearn muscle memory...
(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier nil)

;; golang
(require 'go-mode)
(require 'go-autocomplete)

(if (eq system-type 'darwin)
    (ns-toggle-toolbar))

(add-hook 'after-make-frame-functions 'ns-toggle-toolbar)

(require 'iedit)
(require 'google-this)
(require 'elein)
(require 'bm)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-set-key (kbd "C-`") 'other-frame)

;; python
;; pip install elpy rope jedi

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
                (setq-local python-indent-offset 2))))

;; surround

(require 'evil-surround)
(global-evil-surround-mode)

;; ESS
(require 'ess-site)

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

;; erc IRC client
(require 'erc)

;; powerline
(require 'powerline)
(setq powerline-arrow-shape 'curve)
(powerline-default-theme)

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

;; uniquify
(require 'uniquify)

;; ember
(require 'ember-mode)
(ember-mode)

;; flycheck
(require 'flycheck)
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
(add-hook 'flycheck-mode-hook #'flycheck-virtualenv-setup)

(require 'tox)

(require 'editorconfig)
(editorconfig-mode 1)

(set-frame-font "Mononoki")

;; custom file

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(erc-modules
	 (quote
		(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp track)))
 '(erc-nick "bjcohen")
 '(flycheck-pycheckers-checkers (quote (flake8)))
 '(package-selected-packages
	 (quote
		(go-autocomplete projectile-ripgrep flycheck-pycheckers use-package solarized-theme idomenu ido-completing-read+ projectile helm-rg ripgrep flycheck-pyflakes tox rust-mode markdown-mode+ csv-mode helm-ag helm-smex helm-git helm-google ido-yes-or-no jenkins-watch jenkins dockerfile-mode docker flymake-json powerline web-mode typescript-mode string-inflection smex sass-mode salt-mode ponylang-mode pony-snippets perspective nvm matlab-mode markdown-mode less-css-mode json-mode js2-mode jinja2-mode iedit ido-sort-mtime helm-projectile haskell-mode handlebars-sgml-mode handlebars-mode groovy-mode gradle-mode google-this go-mode git-gutter gist flycheck flx-ido exec-path-from-shell evil-surround ess ember-mode elpy elixir-mode elein egg editorconfig ecb cython-mode csharp-mode coffee-mode clojure-snippets clojure-mode bm autopair auto-complete auctex ag)))
 '(reb-re-syntax (quote string))
 '(safe-local-variable-values
	 (quote
		((project-root . "/Users/ben.cohen/dev/iverson/iverson/app")
		 (project-root . "/Users/ben.cohen/dev/iverson/iverson"))))
 '(tab-width 2)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
