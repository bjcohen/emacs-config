;;; XEmacs backwards compatibility file
(setq user-init-file
      (expand-file-name "init.el"
			(expand-file-name ".xemacs" "~")))
(setq custom-file
      (expand-file-name "custom.el"
			(expand-file-name ".xemacs" "~")))

;(load-file user-init-file)
(load-file custom-file)

(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;(message "Deleting old backup files...")
;(let ((week (* 60 60 24 7))
;      (current (float-time (current-time))))
;  (dolist (file (directory-files temporary-file-directory t))
;    (when (and (backup-file-name-p file)
;               (> (- current (float-time ( fifth ( file-attributes file))))
;                  week))
;      (message file)
;      (delete-file file))))

(add-to-list 'load-path "~/doxymacs-1.8.0/lisp/")

(require 'doxymacs)

(add-hook 'c-mode-common-hook 'doxymacs-mode)
(add-hook 'asm-mode-common-hook 'doxymacs-mode)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

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

; color-theme & Zenburn

(normal-top-level-add-to-load-path '("~/.emacs.d/color-theme-6.6.0/"))
(require 'color-theme)

(normal-top-level-add-to-load-path '("~/.emacs.d/zenburn-emacs/"))
(require 'zenburn)
(zenburn)

; hl-line

; (global-hl-line-mode)
; (set-face-background 'hl-line "#020")

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

; lusty-explorer

(when (require 'lusty-explorer nil 'noerror)

  ;; overrride the normal file-opening, buffer switching
  (global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
  (global-set-key (kbd "C-x b")   'lusty-buffer-explorer))

;; F9 creates a new elscreen, shift-F9 kills it
(global-set-key (kbd "<f9>"    ) 'elscreen-create)
(global-set-key (kbd "S-<f9>"  ) 'elscreen-kill)  


;; Windowskey+PgUP/PgDown switches between elscreens
(global-set-key (kbd "<s-prior>") 'elscreen-previous) 
(global-set-key (kbd "<s-next>")  'elscreen-next) 