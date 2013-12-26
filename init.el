(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(add-to-list 'exec-path "/usr/local/bin")

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-refresh-contents)
  (package-initialize)
)

(setq my-packages
      '(auto-complete autopair clojure-mode egg evil go-mode haskell-mode
        perspective popup ruby-mode smex solarized-theme undo-tree yasnippet
        iedit google-this elein bm auto-complete surround gist nrepl git-gutter
        ecb elpy auctex ess powerline ido-ubiquitous ido-sort-mtime ag ac-nrepl))

;; (mapc (lambda (p)
;;         (when (not (package-installed-p p)) (package-install p)))
;;       my-packages)

(add-to-list 'load-path "~/.emacs.d/")
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-hook 'c-mode-common-hook
        (lambda ()
                (define-key c-mode-map [(ctrl tab)] 'complete-tag)))

(require 'gas-mode)
(add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))
(global-set-key [?\M-g] 'goto-line)  ; easier to jump to specific lines (M-g)

(add-hook 'gas-mode-common-hook 'doxymacs-mode)

; egg
(require 'egg)

; git-gutter
(global-git-gutter-mode t)

; autopair

(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap 'true)

; random stuff

(add-hook 'makefile-mode-hook 
  (lambda()
    (setq show-trailing-whitespace t)))    

(add-hook 'c-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

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

;; this means hitting the compile button always saves the buffer
;; having to separately hit C-x C-s is a waste of time
(setq mode-compile-always-save-buffer-p t)
;; make the compile window stick at 12 lines tall
(setq compilation-window-height 12)

;; from enberg on #emacs
;; if the compilation has a zero exit code, 
;; the windows disappears after two seconds
;; otherwise it stays
(setq compilation-finish-function
      (lambda (buf str)
        (unless (string-match "exited abnormally" str)
          ;;no errors, make the compilation window go away in a few seconds
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!"))))



;; etags-select

;;load the etags-select.el source code
(load-file "~/.emacs.d/etags-select.el")
;;binding the key
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

(defun jds-find-tags-file ()
  "recursively searches each parent directory for a file named 'TAGS' and returns the
path to that file or nil if a tags file is not found. Returns nil if the buffer is
not visiting a file"
  (progn
      (defun find-tags-file-r (path)
         "find the tags file from the parent directories"
         (let* ((parent (file-name-directory path))
                (possible-tags-file (concat parent "TAGS")))
           (cond
             ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
             ((string= "/TAGS" possible-tags-file) (error "no tags file found"))
             (t (find-tags-file-r (directory-file-name parent))))))

    (if (buffer-file-name)
        (catch 'found-it 
          (find-tags-file-r (buffer-file-name)))
        (error "buffer is not visiting a file"))))

(defun jds-set-tags-file-path ()
  "calls `jds-find-tags-file' to recursively search up the directory tree to find
a file named 'TAGS'. If found, set 'tags-table-list' with that path as an argument
otherwise raises an error."
  (interactive)
  (setq tags-table-list (cons (jds-find-tags-file) tags-table-list)))

;; delay search the TAGS file after open the source file
(add-hook 'emacs-startup-hook 
	'(lambda () (jds-set-tags-file-path)))

;; sml-mode

(add-to-list 'load-path "~/.emacs.d/sml-mode-4.1/")
(load-file "~/.emacs.d/sml-mode-4.1/sml-mode-startup.el")

;; hilight matching parens
(defvar show-paren-overlay nil)
(defvar show-paren-overlay-1 nil)
(show-paren-mode)

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(ido-mode 1)

(require 'ido-ubiquitous)
(require 'ido-sort-mtime)
(require 'idomenu)

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
(yas-minor-mode)

;; autocomplete-mode

(require 'auto-complete)
(add-to-list 'ac-sources 'ac-source-yasnippet)
;; (ac-config-default)
(load "auto-complete-haskell.el")

;; don't use tabs

(setq-default indent-tabs-mode nil)

;; Sometimes I use these...

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; clojure
(require 'clojure-mode)
(require 'nrepl)
(require 'ac-nrepl)

;; disable scrollbars

(set-scroll-bar-mode nil)

;; perspective

(require 'perspective)
(persp-mode)

;; Evil
(require 'evil)
(evil-mode 1)
(setq evil-default-state 'emacs)

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
(require 'go-mode-load)

(if (eq system-type 'darwin)
    (ns-toggle-toolbar))

(require 'iedit)
(require 'google-this)
(require 'elein)
(require 'bm)
(require 'uniquify)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-set-key (kbd "C-`") 'other-frame)

;; python
;; pip install elpy rope jedi

(elpy-enable)
(elpy-use-ipython)
(elpy-clean-modeline)

;; surround

(require 'surround)
(global-surround-mode)

;; ESS
(require 'ess-site)

;; html/js stuff
(setq sgml-basic-offset 4)

;; quick register access to this file
(set-register ?e '(file "~/.emacs.d/init.el"))

;; auto-open
(defun my-check-modified ()
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
  "Run a revert check when we switch to a buffer"
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

;; silver searcher
(require 'ag)

;; custom file

(setq custom-file "custom.el")
(load custom-file)
