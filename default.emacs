;; Sample .emacs file for installing Asm86 mode. The file
;; must be saved as ".emacs" to be recognized properly. 
;;
;; The default mode behavior is used (no customization). Be
;; sure to change the file path in the (autoload ) statement
;; to reflect where you actually saved the file asm86-mode.elc.
;;
;; by Gabe Wenz
;; wenz@ugcs.caltech.edu
;;

;;; MISC. CUSTOMIZATION ;;;
;; The first statement below is essential to avoid FS problems, because  
;; windows is not good at handling >3 character extensions. This may not
;; be a problem on later versions of Windows. 
(setq make-backup-files nil)         ; no more of those annoying ~ files
(setq auto-save-default nil)         ; turn off auto-saving
(global-set-key [?\M-g] 'goto-line)  ; easier to jump to specific lines (M-g)



;;; ASM86 MODE "INSTALLATION" ;;;

;; This command loads the .elc file that defines Asm86 mode.  Be sure
;; that the file path is correct (forward slashes, not back slashes,
;; separate directories), and make the file extension ".el" instead
;; of ".elc" if you downloaded the Lisp source version instead of the
;; byte-compiled version.
(autoload 'asm86-mode "c:\\emacs-20.7\\asm86-mode.elc")       ;loads "asm86-mode.elc"

;; Make Emacs load Asm86 mode for .asm files. This command 
;; does not need any modifications. 
(setq auto-mode-alist
     (append '(("\\.asm\\'" . asm86-mode) ("\\.inc\\'" . asm86-mode)) auto-mode-alist))


;;; Customization for syntax highlighting ;;;

;; Enable syntax highlighting for Asm86 Mode:
(add-hook 'asm86-mode-hook 'turn-on-font-lock)

;; Customize syntax colors. This is the "recommended" 
;; color scheme. 
(cond ((fboundp 'global-font-lock-mode)
       ;; Customize face attributes
       (setq font-lock-face-attributes
             ;; Symbol-for-Face Foreground Background Bold Italic Underline
             '((font-lock-comment-face       "DarkGreen")
               (font-lock-string-face        "Sienna")
               (font-lock-keyword-face       "RoyalBlue")
               (font-lock-function-name-face "Red")
               (font-lock-variable-name-face "Black")
               (font-lock-type-face          "Blue")
               (font-lock-constant-face      "Purple")
               ))
       ;; Load the font-lock package.
       (require 'font-lock)))


;;; CUSTOMIZE ASM86 MODE BEHAVIOR ;;;

(setq asm86-extended-style-headers t); use more complete function headers
