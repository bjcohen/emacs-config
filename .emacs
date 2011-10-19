;(load-file user-init-file)
;(load-file custom-file)

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

; APEL

(normal-top-level-add-to-load-path '("~/.emacs.d/apel/"))
(normal-top-level-add-to-load-path '("~/.emacs.d/emu/"))

; ElScreen

(normal-top-level-add-to-load-path '("~/.emacs.d/elscreen-1.4.6/"))
(load "elscreen" "ElScreen" t)

; egg
(normal-top-level-add-to-load-path '("~/.emacs.d/egg/"))
(require 'egg)

; autopair

(require 'autopair)
(autopair-global-mode)

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

(load "~/.emacs.d/haskellmode-emacs/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))

(autoload 'haskell-cabal-mode "haskell-mode" "\
Major mode for Haskell Files

\(fn)" t nil)

(require 'inf-haskell)

;; More ELScreen stuff

;; F9 creates a new elscreen, shift-F9 kills it
(global-set-key (kbd "<f9>"    ) 'elscreen-create)
(global-set-key (kbd "S-<f9>"  ) 'elscreen-kill)  


;; Windowskey+PgUP/PgDown switches between elscreens
(global-set-key (kbd "<s-prior>") 'elscreen-previous) 
(global-set-key (kbd "<s-next>")  'elscreen-next) 

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


;; one-button testing, tada!
(global-set-key [f5] 'compile)

;; AucTex

(add-to-list 'load-path "~/.emacs.d/auctex-11.86")
(add-to-list 'load-path "~/.emacs.d/auctex-11.86/preview")

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

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

(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)

;; sml-mode

(add-to-list 'load-path "~/.emacs.d/sml-mode-4.1/")
(load-file "~/.emacs.d/sml-mode-4.1/sml-mode-startup.el")

;; thrift-mode

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'thrift-mode)

;; hilight matching parens

(show-paren-mode)

(require 'ido)

(normal-top-level-add-to-load-path '("~/.emacs.d/color-theme-6.6.0/"))
(require 'color-theme)

;; solarized

(load-file "~/.emacs.d/solarized/emacs-colors-solarized/color-theme-solarized.el")
(color-theme-solarized-dark)

;; ruby-mode

(add-to-list 'load-path "~/.emacs.d/ruby-mode/")
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))                    
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode)) 

;; Yasnippet

(add-to-list 'load-path "~/.emacs.d/yasnippet-0.6.1c/")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")
(add-hook 'after-change-major-mode-hook 'yas/minor-mode-on)

;; autocomplete-mode

(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
;; (ac-config-default)
(setq ac-sources '(ac-source-yasnippet ac-source-dictionary))
;; (add-to-list 'ac-sources 'ac-source-yasnippet)
(add-hook 'after-change-major-mode-hook 'auto-complete-mode)

(defvar ac-source-etags
  '((candidates . (lambda () 
		    (all-completions ac-target (tags-completion-table))))
    (requires . 3)
    (symbol "e"))
  "Source for etags.")

;; fonts for trailing whitespace, special words in c-mode

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

;; yay 15-411

(add-to-list 'auto-mode-alist '("\\.l1$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.l2$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.l3$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.l4$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.l5$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.l6$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.c0$" . c-mode))

;; don't use tabs

(setq-default indent-tabs-mode nil)

;; ocaml mode

(add-to-list 'load-path "./tuareg-mode")

(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg-mode/tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "tuareg-mode/camldebug" "Run the Caml debugger" t)
(dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"))
  (add-to-list 'completion-ignored-extensions ext))

;; Sometimes I use these...

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

