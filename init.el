;; -*- ELisp -*-

;; Time-stamp: <2021-01-03 19:18:30 davidh>

;;; Behaviour
(setq inhibit-startup-message t)
(setq default-tab-width 4)
(setq c-electric-flag nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(add-hook 'before-save-hook 'time-stamp) ; time-stamp.el

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

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(projectile
		      clojure-mode
                      cider))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))
