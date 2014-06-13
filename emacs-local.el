;; .emacs

;; Time-stamp: <2014-06-13 08:23:48 davidh>

(setq user-full-name "David Hisel")
(setq user-email-address "davidh@julep.com")

;; Nothing usually has to be changed beyond this point
(setq user-home-dir (getenv "HOME"))
(setq user-emacs-dir (expand-file-name ".emacs.d" user-home-dir))
(setq user-lisp-dir (expand-file-name "lisp" user-emacs-dir))

;; Set PATH based on OS
(cond
 ((string-equal system-type "darwin")
  (progn
	(setenv "PATH" (concat "/opt/local/bin"
						   ":/opt/local/sbin"
						   ":" (getenv "PATH")))

	(add-to-list 'exec-path "/opt/local/bin")
	(add-to-list 'exec-path "/opt/local/sbin")))
 ((string-equal system-type "gnu/linux")
  (progn
	(setenv "PATH" (concat "/usr/bin:/bin:/usr/sbin:/sbin"
						   ":" (getenv "PATH")))))
 ((string-equal system-type "windows-nt")
  (progn
	(setenv "PATH" (concat "/opt/bin"
						   ";" (getenv "PATH")))))
 )


(setq user-emacs-init-file (expand-file-name "init.el" user-emacs-dir))
(load user-emacs-init-file nil t)

;; Custom Settings
(setq custom-file (expand-file-name "custom.el" user-emacs-dir))
(load custom-file t t)

