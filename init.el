;(load-file user-init-file)
;(load-file custom-file)

;; clojure-mode, deft, egg, perspective, slime, smex, solarized

;; submodules removed: egg, solarized, clojure-mode, perspective, smex
;; other dirs removed: auto-complete, ido, autopair, color-theme, ruby-mode, elscreen, auctex, yasnippet

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-refresh-contents)
  (package-initialize)
)

(setq my-packages
      '(auto-complete autopair clojure-mode egg evil go-mode haskell-mode
        perspective popup ruby-mode smex solarized-theme undo-tree yasnippet
        iedit google-this elein bm auto-complete surround gist nrepl))

(mapc (lambda (p)
        (when (not (package-installed-p p)) (package-install p)))
      my-packages)

(let ((default-directory "."))
  (normal-top-level-add-subdirs-to-load-path))

(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

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

(add-to-list 'load-path "~/.emacs.d/")
(require 'gas-mode)
(add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))
(global-set-key [?\M-g] 'goto-line)  ; easier to jump to specific lines (M-g)

(add-hook 'gas-mode-common-hook 'doxymacs-mode)

; ElScreen

;(normal-top-level-add-to-load-path '("~/.emacs.d/elscreen-1.4.6/"))
;(load "elscreen" "ElScreen" t)

; egg
(require 'egg)

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


;; AucTex

;(add-to-list 'load-path "~/.emacs.d/auctex-11.86")
;(add-to-list 'load-path "~/.emacs.d/auctex-11.86/preview")

;(load "auctex.el" nil t t)
;(load "preview-latex.el" nil t t)

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

(show-paren-mode)

(require 'ido)
(ido-mode)
;(require 'idomenu)
;(global-set-key (kbd "C-i") 'idomenu)

;; solarized
(require 'solarized)
(load-theme 'solarized-dark t)

;; ruby-mode

(add-to-list 'load-path "~/.emacs.d/ruby-mode/")
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))                    
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode)) 

;; Yasnippet

;; (add-to-list 'load-path "~/.emacs.d/yasnippet-0.6.1c/")
(require 'yasnippet)
(yas-load-directory "~/.emacs.d/snippets")
(yas-minor-mode)

;; autocomplete-mode

;; (add-to-list 'load-path "~/.emacs.d/auto-complete")
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
;; ;; (ac-config-default)
;; (setq ac-sources '(ac-source-yasnippet ac-source-dictionary))
;; (setq-default ac-sources '(ac-source-yasnippet ac-source-dictionary))
;; ;; (add-to-list 'ac-sources 'ac-source-yasnippet)
;; (add-hook 'after-change-major-mode-hook 'auto-complete-mode)
;; (load "auto-complete-haskell.el")

;; (defvar ac-source-etags
;;   '((candidates . (lambda ()
;;                     (all-completions ac-target (tags-completion-table))))
;;     (requires . 3)
;;     (symbol "e"))
;;   "Source for etags.")

;; fonts for trailing whitespace, special words in c-mode

;; (add-hook 'makefile-mode-hook
;;           (lambda()
;;             (setq show-trailing-whitespace t)))

;; (add-hook 'c-mode-hook
;;     (lambda ()
;;       (font-lock-add-keywords nil
;;             '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (font-lock-add-keywords nil
;;                                     '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face t)))))

;; don't use tabs

(setq-default indent-tabs-mode nil)

;; Sometimes I use these...

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Clojure-mode

(require 'clojure-mode)

;; disable scrollbars

(set-scroll-bar-mode nil)

;; perspective

(require 'perspective)
(persp-mode)

;; Evil
(require 'evil)
(evil-mode 1)

;; SLIME

; (add-to-list 'load-path "~/.emacs.d/slime")
; (setq inferior-lisp-program "")
; (require 'slime)
; (slime-setup '(slime-fancy))

;; Smex
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

;; A quick & ugly PATH solution to Emacs on Mac OSX
;; (if (string-equal "darwin" (symbol-name system-type))
;;     (setenv "PATH" (concat "/Users/bjcohen/dev/sml/bin:" (getenv "PATH"))))
;; (if (string-equal "darwin" (symbol-name system-type))
;;     (setenv "PATH" (concat "/Users/bjcohen/dev/leiningen/bin:" (getenv "PATH"))))

(if (eq system-type 'darwin)
    (ns-toggle-toolbar))

(require 'iedit)
(require 'google-this)
(require 'elein)
(require 'bm)

(global-set-key (kbd "C-`") 'other-frame)

;; TODO: cleanup and organize more

;; TODO: elscreen, autocomplete, auctex

;; TODO: hippie, helm

;; TODO: set up snippets and AC/hippie

;; TODO: slime, ritz, tools.nrepl, nrepl.el

;; TODO: company-mode?

;; pl1
(add-to-list 'load-path "~/.emacs.d/psl-mode/")
(require 'psl-mode)

(add-to-list 'load-path "~/.emacs.d/geiser/elisp/")
(require 'geiser)

(require 'surround)
(global-surround-mode)

;; (require 'quack)

;; ESS
(add-to-list 'load-path "~/.emacs.d/ESS/lisp/")
(require 'ess-site)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(geiser-racket-binary "/Applications/Racket v5.3/bin/racket")
 '(quack-default-program "racket")
 '(quack-remap-find-file-bindings-p nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
