;;; ~/.emacs.d/init.el

;; Time-stamp: <2021-04-16 11:04:50 dhisel1>

;;; Commentary:

;; Change these variables

(setq user-full-name "David Hisel")
(setq user-mail-address "David.Hisel1@T-Mobile.com")

;;; References:
;;  Some cool Emacs packages to consider https://github.com/emacs-tw/awesome-emacs

;;; EXTERNAL DEPENDENCIES
;; bzr -- dnf install bzr
;; cvs -- dnf install git-cvs # or just cvs
;; autoconf -- dnf install autoconf
;; makeinfo -- dnf install texinfo
;; w3m -- dnf install w3m w3m-img
;; markdown
;;
;; Install deps on Fedora 29:
;;     $ sudo dnf install bzr git-cvs autoconf texinfo w3m perl-Text-MultiMarkdown
;;
;; Install deps on Win10 using MSYS2:
;;     $ pacman -S bzr cvs autoconf texinfo w3m markdown
;;
;; Install deps on Mac using macports:
;;     $ sudo port install bzr cvs autoconf texinfo w3m multimarkdown

;;; Code:

(setq user-documents-dir (expand-file-name "Documents" (getenv "HOME")))

;; Default destination where you store Org-mode docs
(setq user-org-directory (expand-file-name "org" user-documents-dir))



(require 'cl)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)) ; user lisp dir

;;; Startup
(setq initial-scratch-buffer nil)
;; (setq initial-buffer-choice (expand-file-name "work" (getenv "HOME")))
(setq initial-buffer-choice nil)

;;;
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
;;(iswitchb-mode 1)

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
;; (set-background-color "white")
;; (set-foreground-color "black")
;; (set-background-color "grey20")
;; (set-foreground-color "grey90")
;;; (setq default-frame-alist
;;;       (append default-frame-alist
;;; 	      '((foreground-color . "grey90")
;;; 		(background-color . "grey20")
;;; 		(cursor-color . "red")
;;; 		)))


;;(set-face-background 'modeline "darkred")
;;(set-face-foreground 'modeline "white")

;; Required to get package-refresh-contents to work with elpa
(setq gnutls-algorithm-priority "normal:-vers-tls1.3")

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
(package-install 'csv-mode)
(package-install 'auto-complete)
(package-install 'fixmee)
(package-install 'buffer-move)
(package-install 'js2-mode)
(package-install 'json-navigator)
(package-install 'yaml-mode)
(package-install 'go-mode)
(package-install 'go-playground)
(package-install 'go-complete)
(package-install 'go-direx)
(package-install 'projectile)

(package-install 'cider)
(package-install 'paredit)
(package-install 'company)

(package-install 'markdown-mode)
(package-install 'markdown-toc)
(package-install 'edit-indirect)
(package-install 'dired-sidebar)
(package-install 'ansible)
(package-install 'ansible-doc)
(package-install 'ansible-vault)
(package-install 'dockerfile-mode)
(package-install 'gitlab)
(package-install 'gitlab-ci-mode)
(package-install 'gitlab-ci-mode-flycheck)
(package-install 'graphviz-dot-mode)
(package-install 'adoc-mode)
(package-install 'terraform-mode)
(package-install 'sr-speedbar)
(package-install 'mustache)
(package-install 'mustache-mode)

(package-install 'nginx-mode)
(package-install 'w3m)

(package-install 'geiser)
(package-install 'geiser-kawa)

;;* not used, but might be used in the future
;;(package-install 'iedit)
;;(package-install 'yasnippet)
;;(package-install 'puppet-mode)
;;(package-install 'geben)
;;(package-install 'php-mode)
;;(package-install 'xml-rpc)
;;(package-install 'groovy-mode)

;;(package-install 'edbi)
;;(package-install 'edbi-minor-mode)
;;(package-install 'edbi-sqlite)
;;(package-install 'emacsql)
;;(package-install 'emacsql-sqlite)
;;(package-install 'twittering-mode)
;;(package-install 'maxframe)
;;(package-install 'neotree)
;;(package-install 'treemacs)
;;(package-install 'treemacs-magit)


;;; Theme
;; https://draculatheme.com/emacs/
;;(package-install 'dracula-theme)
;;(load-theme 'dracula t)


;;; Zone Out
(if (boundp 'zone-timeout) (setq zone-timeout 300))
(if (fboundp 'zone-leave-me-alone) (zone-leave-me-alone))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modes, etc.

;;; Org mode
;;(setq org-directory (expand-file-name (concat (getenv "HOME") "/Documents/org")))

;;;https://orgmode.org/manual/Capture.html#Capture
;;(global-set-key (kbd "C-c l") 'org-store-link)
;;(global-set-key (kbd "C-c a") 'org-agenda)
;;(global-set-key (kbd "C-c c") 'org-capture)
;;(setq org-capture-templates
;;      '(
;;	("t" "Todo" entry (file+headline (concat user-org-directory "/gtd.org") "Tasks")
;;         "* TODO %?\n  %i\n  %a")
;;        ("j" "Journal" entry (file+datetree (concat user-org-directory "/journal.org"))
;;         "* %?\nEntered on %U\n  %i\n  %a")
;;	("l" "Link" entry (file (concat user-org-directory "/links.org"))
;;         "* Link %?\n  %i\n  %a")
;;	("e" "Error" entry (file (concat user-org-directory "/errors.org"))
;;         "* Error %?\n  %i\n  %a")
;;	))

;;; https://www.emacswiki.org/emacs/Desktop
(require 'desktop)
(desktop-save-mode 1)
(defun my:desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my:desktop-save)



;;; CPerl <http://www.emacswiki.org/cgi-bin/wiki/CPerlMode>
(require 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)
(setq cperl-invalid-face nil
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 0
      cperl-indent-level 4
      cperl-indent-parens-as-block t
      cperl-hook-after-change nil ;; fixes POD highlight issue
      cperl-tabs-always-indent t)

(defun my:cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car
   (let ((cperl-message-on-help-error nil))
     (cperl-get-help))))
(add-hook 'cperl-mode-hook
	  (lambda ()
	    (set (make-local-variable 'eldoc-documentation-function)
		 'my:cperl-eldoc-documentation-function)))

(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
		  (perltidy-region)))

;;; http://www.emacswiki.org/cgi-bin/wiki/BrowseUrl
(setq w3m-command "/opt/local/bin/w3m")
(require 'w3m)

(defun choose-browser (url &rest args)
  (interactive "sURL: ")
  (if (y-or-n-p "Use external browser? ")
      (browse-url-generic url)
    (w3m-browse-url url)))

(setq browse-url-browser-function 'choose-browser)


(defun browse-url-default-macosx-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (if (and new-window (>= emacs-major-version 23))
      (ns-do-applescript
       (format (concat "tell application \"Safari\" to make document with properties {URL:\"%s\"}\n"
		       "tell application \"Safari\" to activate") url))
    (start-process (concat "open " url) nil "open" url)))


;;; PHP-Mode
;;(require 'php-mode)
;;(add-hook 'php-mode-hook 'turn-on-font-lock)

;;; ASCII table
;;(autoload 'ascii-table "ascii-table" nil t)


;;; Tramp
(require 'tramp)
;; (setq tramp-default-proxies-alist nil)
;; (add-to-list 'tramp-default-proxies-alist '("stage"         "root" "/ssh:%h:"))

;;; sql-mysql
(require 'sql)
(setq sql-mysql-options '("-C" "-t" "-f"))
(defun sql-mysql-with-maybe-port (&optional port-p)
  "Function helper for sql-mysql, if prefixed, to specify a port."
  (interactive "P")
  (let ((sql-mysql-options
	 (append sql-mysql-options 
		 (if port-p
		     (list (concat "--port=" (read-string "Port: ")))))))
    (call-interactively 'sql-mysql)))

;; CTags
;; http://www.emacswiki.org/emacs/BuildTags
;; Usage:
;;    M-.       goes to the symbol definition
;;    M-0 M-.   goes to the next matching definition
;;    M-*       return to your starting point
;;
;;    M-x tags-search <type your regexp>       initiate a search
;;    M-,                                      go to the next match
;;
;; Navigating using tags
;; 
;; Once you have a tags file and M-x visit-tags-table, you can follow
;; tags (of functions, variables, macros, whatever) to their
;; definitions. These are the basic commands:
;; 
;;     `M-.’ (‘find-tag’) – find a tag, that is, use the Tags file to
;;          look up a definition. If there are multiple tags in the
;;          project with the same name, use `C-u M-.’ to go to the
;;          next match.
;;
;;     ‘M-x find-tag-other-window’ – selects the buffer containing the
;;          tag’s definition in another window, and move point there.
;;
;;     ‘M-*’ (‘pop-tag-mark’) – jump back
;;
;;     ‘M-x tags-search’ – regexp-search through the source files
;;          indexed by a tags file (a bit like ‘grep’)
;;
;;     ‘M-x tags-query-replace’ – query-replace through the source
;;          files indexed by a tags file
;;
;;     `M-,’ (‘tags-loop-continue’) – resume ‘tags-search’ or
;;          ‘tags-query-replace’ starting at point in a source file
;;
;;     ‘M-x tags-apropos’ – list all tags in a tags file that match a
;;          regexp
;;
;;     ‘M-x list-tags’ – list all tags defined in a source file
;; 
;; See the Emacs manual, node Tags for more information: Tags.

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name (directory-file-name dir-name))))

;;; EmacsServer  "server.el"
;;; Connect term via $ emacsclient -t   # exit with C-x 5 0
;;; Connect  gui via $ emacsclient FILE # exit with C-x #
(setq server-socket-dir (format "/tmp/emacs/%d" (user-uid)))
(server-start)


;;; Auto Complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;;; Yasnippet
;;(require 'yasnippet)
;;(yas-global-mode 1)

;;; EDiff
;; Split horizontally
(setq ediff-split-window-function 'split-window-horizontally)
;; keep the ediff control panel in the same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; JS Mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


;;; Date and Time-stamp
(defun my:insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %A")))
(defun my:insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))

;;; Markdown-mode
(require 'markdown-mode)


;;; dired-sidebar
(require 'dired-sidebar)
(global-set-key "\C-h\C-s" 'dired-sidebar-toggle-sidebar)

(defun my:dired-sidebar-find-file-alt ()
  "Show file in other buffer and keep focus in dired-sidebar."
  (interactive)
  (let ((dired-file-name (dired-get-file-for-visit)))
    (if (and (file-directory-p dired-file-name)
             ;; For "." open a full-blown dired buffer, since the directory is
             ;; already open in the sidebar.
             (not (string= (file-name-nondirectory dired-file-name) ".")))
	(switch-to-buffer (dired-sidebar-buffer))
      (display-buffer (find-file-noselect dired-file-name) t)
      (when dired-sidebar-close-sidebar-on-file-open
        (dired-sidebar-hide-sidebar)))))
(defalias 'dired-sidebar-find-file-alt 'my:dired-sidebar-find-file-alt)

;;; fixmee (fixmee.el)
;;; https://github.com/rolandwalker/fixmee
;;; C-c f 	fixmee-goto-nextmost-urgent
;;; C-c F 	fixmee-goto-prevmost-urgent
;;; C-c v 	fixmee-view-listing
;;; M-n 	fixmee-goto-next-by-position ; only when the point is
;;; M-p 	fixmee-goto-previous-by-position ; inside a fixme notice
(require 'button-lock)
(require 'fixmee)
(global-fixmee-mode 1)

;;; Hyperbole (hyperbole.el)
(require 'hyperbole)

;;; Speedbar
;;; https://www.emacswiki.org/emacs/SrSpeedbar
(require 'sr-speedbar)
(global-set-key "\C-h\C-f" 'sr-speedbar-toggle)


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
;; (setq my:path (concat
;; 		 (replace-regexp-in-string "\r?\n$" ""
;; 					   (shell-command-to-string "/opt/local/bin/bash -l -c \"echo $PATH\""))))

;; "/Users/dhisel1/go/bin:/opt/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14:/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_14"

;; PATH=/opt/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/local/sbin:/Users/dhisel1/go/bin:/Users/dhisel1/bin:/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14:/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_14:/opt/kawa/bin:/opt/local/Library/Frameworks/Python.framework/Versions/3.7/bin:/Users/dhisel1/Library/Python/3.7/bin



;; __END__

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (jiralib2 org-dashboard org-download org-jira org-kanban org-notebook org-timeline company paredit cider json-navigator geiser-kawa geiser groovy-mode dracula-theme gitlab-ci-mode markdown-mode go-mode yaml-mode w3m terraform-mode sr-speedbar projectile nginx-mode mustache-mode mustache markdown-toc magit js2-mode hyperbole graphviz-dot-mode go-playground go-direx go-complete gitlab-ci-mode-flycheck gitlab fixmee edit-indirect dockerfile-mode dired-sidebar csv-mode buffer-move auto-complete ansible-vault ansible-doc ansible adoc-mode))))
