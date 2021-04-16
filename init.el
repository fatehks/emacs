;;; ~/.emacs.d/init.el

;; Time-stamp: <2021-04-16 17:19:14 dhisel1>


;;; User Settings:

(setq user-full-name "David Hisel")
(setq user-email-address "david@electronic-artisans.com")
 
;; Nothing usually has to be changed beyond this point
(setq user-home-dir (getenv "HOME"))
(setq user-emacs-dir (expand-file-name ".emacs.d" user-home-dir))
(setq user-lisp-dir (expand-file-name "lisp" user-emacs-dir))

;; Custom Settings
(setq custom-file (expand-file-name "custom.el" user-emacs-dir))
(load custom-file t t)


;;; Code:

(require 'cl)
(add-to-list 'load-path user-lisp-dir)

;;; Startup
(setq initial-scratch-buffer nil)
;; (setq initial-buffer-choice (expand-file-name "work" user-home-dir))
(setq initial-buffer-choice nil)

;;; Behaviour
(setq inhibit-startup-message t)
(setq default-tab-width 4)
(setq c-electric-flag nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(add-hook 'before-save-hook 'time-stamp) ; time-stamp.el
(iswitchb-mode 1)

;;; Appearance
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 0))
(if (fboundp 'display-time-mode) (display-time-mode 1))

(global-font-lock-mode 1)
(menu-bar-no-scroll-bar)
(line-number-mode t)
(column-number-mode t)
(transient-mark-mode 1)
(blink-cursor-mode 1)
(set-frame-name "Dromedary")
(set-cursor-color "red")
(set-background-color "black")
(set-foreground-color "white")

