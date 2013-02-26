;; .emacs

;; Time-stamp: <2012-03-06 09:53:05 dhisel>

(setq user-full-name "David Hisel")
(setq user-email-address "david.hisel@gmail.com")

;; Nothing usually has to be changed beyond this point
(setq user-home-dir (getenv "HOME"))
(setq user-emacs-dir (expand-file-name ".emacs.d" user-home-dir))
(setq user-lisp-dir (expand-file-name "lisp" user-emacs-dir))

(setq user-emacs-init-file (expand-file-name "init.el" user-emacs-dir))
(load user-emacs-init-file nil t)

;; Custom Settings
(setq custom-file (expand-file-name "custom.el" user-emacs-dir))
(load custom-file t t)
