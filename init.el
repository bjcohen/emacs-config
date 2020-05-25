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

;;; Install Packages

(defvar my-packages '(
                      auctex
                      auto-complete
                      autopair
                      bm
                      clojure-mode
                      dap-mode
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
                      flycheck-inline
                      flycheck-rust
                      flycheck
                      flycheck-pycheckers
                      gist
                      git-gutter
                      go-mode
                      go-autocomplete
                      google-this
                      handlebars-mode
                      haskell-mode
                      helm
                      helm-lsp
                      helm-projectile
                      helm-rg
                      ido-completing-read+
                      ido-sort-mtime
                      idomenu
                      iedit
                      jinja2-mode
                      lsp-mode
                      lsp-ui
                      paradox
                      perspective
                      popup
                      powerline
                      projectile
                      projectile-ripgrep
                      ruby-mode
                      rust-mode
                      smex
                      solarized-theme
                      tox
                      use-package
                      yasnippet
                      ))

(mapc (lambda (p) (straight-use-package p)) my-packages)

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
