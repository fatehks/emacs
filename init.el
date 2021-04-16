;;; ~/.emacs.d/init.el

;; Time-stamp: <2021-04-16 18:35:48 dhisel1>


;;; User Settings:

(setq user-full-name "David Hisel")
(setq user-email-address "david@electronic-artisans.com")


(setq user-home-dir (getenv "HOME"))
(setq user-emacs-dir (expand-file-name ".emacs.d" user-home-dir))
(setq user-lisp-dir (expand-file-name "lisp" user-emacs-dir))
(setq user-documents-dir (expand-file-name "Documents" (getenv "HOME")))

;; Default destination where you store Org-mode docs
(setq user-org-directory (expand-file-name "org" user-documents-dir))


;; Nothing usually has to be changed beyond this point

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
(setenv "PAGER" "cat")
(cd (getenv "HOME"))
(setq my:gobinpath "/opt/local/bin/go")
(setq my:gopath (concat
		 (replace-regexp-in-string "\r?\n$" ""
					   (shell-command-to-string (concat my:gobinpath " env GOPATH"))) "/bin"))

;;; Behaviour
(setq inhibit-startup-message t)
(setq default-tab-width 4)
(setq c-electric-flag nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(add-hook 'before-save-hook 'time-stamp) ; time-stamp.el
;(iswitchb-mode 1)

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


;;; Packages
;;; Melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents nil))
(package-initialize)

;; To list packages M-x list-packages RET
;; Install packages from Melpa
(package-install 'hyperbole)
(package-install 'magit)
(package-install 'buffer-move)

;; 3. Third, install the following packages with M-x package-install $PKG: go-projectile, flycheck, gotest, company, company-go. (the emacs package manager is weird; there doesn't seem to be a non-clunky way to just declare packages as dependencies of your .emacs).

(package-install 'go-mode)
(package-install 'go-projectile)
(package-install 'go-dlv)
(package-install 'go-playground)

(package-install 'flycheck)
(package-install 'gotest)
(package-install 'company)
(package-install 'company-go)
(package-install 'js2-mode)
(package-install 'json-navigator)
(package-install 'yaml-mode)
(package-install 'markdown-mode)
(package-install 'markdown-toc)
(package-install 'mustache)
(package-install 'mustache-mode)
(package-install 'w3m)



;;; Go
(add-hook 'before-save-hook 'gofmt-before-save)



;;; Key bindings
;;; Global Key Bindings
(global-set-key "\C-xo" 'next-multiframe-window)
(global-set-key "\C-xp" 'previous-multiframe-window)
;; (global-set-key "\C-hh" 'help-for-help)
(global-set-key "\C-hg" 'magit-status)
(global-set-key "\C-h\C-c" 'compile)

(global-set-key (kbd "<f12>") 'clipboard-kill-ring-save)
(global-set-key "\C-h\C-w"    'clipboard-kill-ring-save)
(global-set-key "\C-h\C-y"    'clipboard-yank)

(global-set-key "\C-x\C-b" 'list-buffers)
;; (global-set-key "\C-x\C-b" 'bs-show)
(global-set-key "\C-h\C-r" 'rename-buffer)
(global-set-key "\C-h\C-p" 'cperl-perldoc)


(global-set-key "\C-h6" 'my:insert-date)
(global-set-key "\C-h^" 'my:insert-timestamp)
(global-set-key "\C-h7" 'sql-send-region)
(global-set-key "\C-h8" '(lambda ()
			   (interactive)
			   (switch-to-buffer 
			    (find-file-noselect
			     (expand-file-name "init.el" user-emacs-directory)))))
(global-set-key (kbd "C-h C-n") '(lambda ()
				   (interactive)
				   (switch-to-buffer
				    (find-file-noselect
				     (expand-file-name "Notes.txt" user-documents-dir)))))
(global-set-key (kbd "C-h C-/") '(lambda ()
				   (interactive)
				   (switch-to-buffer 
				    (find-file-noselect
				     (expand-file-name "links.org" user-org-directory)))))

(global-set-key (kbd "C-h C-b") 'browse-url-at-point)

(require 'buffer-move)
(global-set-key "\C-h\C-h" 'buf-move-left)
(global-set-key "\C-h\C-j" 'buf-move-down)
(global-set-key "\C-h\C-k" 'buf-move-up)
(global-set-key "\C-h\C-l" 'buf-move-right)


(global-set-key "\C-h9" 'my:toggle-fullscreen)

(defun my:toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(eval-when-compile (require 'cl))
(set-frame-parameter nil 'alpha '(100 100))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(70 50))))
(global-set-key (kbd "C-h C-t") 'toggle-transparency)


;;; <http://wordaligned.org/articles/ignoring-svn-directories>
;;; Use ctrl-x backtick to jump to the right place in the matching file.
(global-set-key [f8] 'grep-find)
(setq grep-find-command
      "find . -path '*/.svn' -prune -o -type f -print | xargs -e grep -I -n -e ")

;;; From the Emacs FAQ
;;; '%' finds matching paren
(global-set-key "%" 'match-paren)
(show-paren-mode 1)
(defun match-paren (arg) 
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond 
   ((looking-at "[[{(<]") (forward-list 1) (backward-char 1))
   ((looking-at "[\]})>]") (forward-char 1) (backward-list 1))
   (t (self-insert-command (or arg 1)))))


;;; Retrieve and eval ELisp files from the web
;;; Example:
;;;   (url-retrieve "http://localhost/emacs.el" 'my:eval-url-callback)
(defun my:eval-url-callback ()
  (goto-char (point-min))
  (re-search-forward "^\r?$" nil 1)
  (delete-region (point-min) (point))
  (eval-buffer))


;;; Find File At Point ("FFAP") (ffap.el)
(require 'ffap)
(ffap-bindings)

;;; Find file at point, jump to line no.
;;; ====================================
(defun find-file-at-point-with-line (&optional filename)
  "Opens file at point and moves point to line specified next to file name."
  (interactive)
  (let* ((filename (or filename (ffap-prompter)))
	 (line-number
	  (and (or (looking-at ".* line \\(\[0-9\]+\\)")
		   (looking-at ".*:\\(\[0-9\]+\\):?"))
	       (string-to-number (match-string-no-properties 1)))))
    (message "%s --> %s" filename line-number)
    (cond ((ffap-url-p filename)
	   (let (current-prefix-arg)
	     (funcall ffap-url-fetcher filename)))
	  ((and line-number
		(file-exists-p filename))
	   (progn (find-file-other-window filename)
		  (goto-line line-number)))
	  ((and ffap-pass-wildcards-to-dired
		ffap-dired-wildcards
		(string-match ffap-dired-wildcards filename))
	   (funcall ffap-directory-finder filename))
	  ((and ffap-dired-wildcards
		(string-match ffap-dired-wildcards filename)
		find-file-wildcards
		;; Check if it's find-file that supports wildcards arg
		(memq ffap-file-finder '(find-file find-alternate-file)))
	   (funcall ffap-file-finder (expand-file-name filename) t))
	  ((or (not ffap-newfile-prompt)
	       (file-exists-p filename)
	       (y-or-n-p "File does not exist, create buffer? "))
	   (funcall ffap-file-finder
		    ;; expand-file-name fixes "~/~/.emacs" bug sent by CHUCKR.
		    (expand-file-name filename)))
	  ;; User does not want to find a non-existent file:
	  ((signal 'file-error (list "Opening file buffer"
				     "no such file or directory"
				     filename))))))
(defalias 'find-file-at-point 'find-file-at-point-with-line)

(defun my:create-file-buffer (filename)
  "Create a suitably named buffer for visiting FILENAME, and return it.
FILENAME (sans directory) is used unchanged if that name is free;
otherwise a string <2> or <3> or ... is appended to get an unused name.
Spaces at the start of FILENAME (sans directory) are removed."
  (let ((lastname (file-name-nondirectory filename)))
    ;;  (let ((lastname (replace-regexp-in-string
    ;;				   (file-name-directory 
    ;;					(directory-file-name (file-name-directory filename))) "" filename)))
    (if (string= lastname "")
	(setq lastname filename))
    (save-match-data
      (string-match "^ *\\(.*\\)" lastname)
      (generate-new-buffer (match-string 1 lastname)))))
(defalias 'create-file-buffer 'my:create-file-buffer)

;;; https://www.emacswiki.org/emacs/Desktop
(require 'desktop)
(desktop-save-mode 1)
(defun my:desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my:desktop-save)


;;; Tramp
(require 'tramp)
;; (setq tramp-default-proxies-alist nil)
;; (add-to-list 'tramp-default-proxies-alist '("stage"         "root" "/ssh:%h:"))

;;; EmacsServer  "server.el"
;;; Connect term via $ emacsclient -t   # exit with C-x 5 0
;;; Connect  gui via $ emacsclient FILE # exit with C-x #
(setq server-socket-dir (format "/tmp/emacs/%d" (user-uid)))
(server-start)

;;; EDiff
;; Split horizontally
(setq ediff-split-window-function 'split-window-horizontally)
;; keep the ediff control panel in the same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; Date and Time-stamp
(defun my:insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %A")))
(defun my:insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))

;;; Hyperbole (hyperbole.el)
(require 'hyperbole)

;; ========================================
;; == Mac OS X Settings
;;
(when (eq system-type 'darwin)
  ;; switch option <-> command key modifiers ... most keyboards have
  ;; cmd-key closer to spacebar making it easier to use cmd-key as
  ;; meta key
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)

  (setq browse-url-browser-function 'browse-url-default-macosx-browser)

  ;; Macports
  (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))
  (setq w3m-command "/opt/local/bin/w3m")
  (setq shell-file-name "/opt/local/bin/bash")
  (setq markdown-command "/opt/local/bin/markdown")
  (setq path-to-ctags "/opt/local/bin/ctags"))


;; ========================================
;; == Windows Settings
;;
(when (eq system-type 'windows-nt)
  (setq browse-url-browser-function 'browse-url-default-windows-browser)

  ;; Msys2 or Chocolatey tools
  (setq w3m-command "c:\\opt\\msys64\\usr\\bin\\w3m") ; FIXME
  (setq shell-file-name "C:\\opt\\msys64\\usr\\bin\\bash.exe")
  (setq markdown-command "c:\\opt\\msys64\\usr\\bin\\markdown")
  (setq path-to-ctags "c:/opt/bin/ctags.exe"))

;; https://emacs.stackexchange.com/questions/10722/emacs-and-command-line-path-disagreements-on-osx
(setenv "PATH" (concat my:gopath ":" (getenv "PATH")))


;;; Markdown-mode
(require 'markdown-mode)



;;; __END__
