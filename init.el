;;; ~/.emacs.d/init.el

;; Time-stamp: <2014-11-13 08:31:27 davidh>

;;; Commentary:

;; I usually create a file, ~/.emacs.d/emacs-local.el, and then
;; symlink ~/.emacs to that.  This way, I can keep it under version
;; control in one dir.
;;
;; Contents of ~/.emacs symlinked to ~/.emacs.d/emacs-local.el
;;
;; (setq user-full-name "John Doe")
;; (setq user-email-address "johndoe@example.com")
;; 
;; ;; Nothing usually has to be changed beyond this point
;; (setq user-home-dir (getenv "HOME"))
;; (setq user-emacs-dir (expand-file-name ".emacs.d" user-home-dir))
;; (setq user-lisp-dir (expand-file-name "lisp" user-emacs-dir))
;; 
;; (setq user-emacs-init-file (expand-file-name "init.el" user-emacs-dir))
;; (load user-emacs-init-file nil t)
;; 
;; ;; Custom Settings
;; (setq custom-file (expand-file-name "custom.el" user-emacs-dir))
;; (load custom-file t t)

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
;; (set-background-color "white")
;; (set-foreground-color "black")

;;(set-face-background 'modeline "darkred")
;;(set-face-foreground 'modeline "white")


;;; https://github.com/dimitri/el-get
(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-dir))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))


;;; local sources
(setq el-get-sources 
      '((:name buffer-move :after (progn () 
										 (global-set-key "\C-h\C-h" 'buf-move-left)
										 (global-set-key "\C-h\C-j" 'buf-move-down)
										 (global-set-key "\C-h\C-k" 'buf-move-up)
										 (global-set-key "\C-h\C-l" 'buf-move-right)))

		(:name js2-mode :after (progn ()
									  (add-to-list 'auto-mode-alist 
												   '("\\.json\\'" . js2-mode))
									  (add-to-list 'auto-mode-alist 
												   '("\\.js\\'" . js2-mode))))

		(:name css-mode :after (progn ()
									  (add-to-list 'auto-mode-alist 
												   '("\\.css\\'" . css-mode))
									  (setq cssm-indent-function 
											#'cssm-c-style-indenter)
									  (setq cssm-indent-level '2)))

		(:name geben
			   :website "http://code.google.com/p/geben-on-emacs/"
			   :description "DBGp protocol frontend, a script debugger"
			   :type http-tar
			   :options ("xzf")
			   :url "http://geben-on-emacs.googlecode.com/files/geben-0.26.tar.gz"
			   :load-path (".")
			   :after (progn ()
							 (autoload 'geben "geben" "PHP Debugger on Emacs" t)
							 (setq dbgp-default-port 9009)))

		(:name yasnippet
			   :website "https://github.com/capitaomorte/yasnippet.git"
			   :description "YASnippet is a template system for Emacs."
			   :type github
			   :pkgname "capitaomorte/yasnippet"
			   :features "yasnippet"
			   :compile "yasnippet.el")

		(:name rpm-spec-mode
			   :description "RPM spec file mode"
			   :type http
			   :url "http://tihlde.org/~stigb/rpm-spec-mode.el"
			   :features rpm-spec-mode)

		(:name jira
			   :website "http://www.emacswiki.org/emacs/JiraMode"
			   :description "Connect to JIRA issue tracking software"
			   :type http
			   :url "http://www.emacswiki.org/emacs/download/jira.el"
			   :after (progn ()
							 (autoload 'jira "jira" "JIRA mode" t)))
		(:name org-jira
			   :website "https://github.com/baohaojun/org-jira.git"
			   :description "Use JIRA in Emacs org-mode."
			   :type github
			   :pkgname "baohaojun/org-jira"
			   :features "org-jira"
			   :compile "yasnippet.el")




  		))

(setq my-packages 
	  (append '(
				el-get
				csv-mode
				magit
				;;ascii-table
				php-mode-improved
				puppet-mode
				maxframe
				nginx-mode
				auto-complete
				xml-rpc-el
				twittering-mode
				tail)
			  (mapcar 'el-get-source-name el-get-sources)))

(if (not (eq system-type 'windows-nt))
	(setq my-packages 
		  (append my-packages '(emacs-w3m))))



(el-get 'sync my-packages)
;; (el-get nil my-packages)
;;(el-get 'wait my-packages)



;;; Zone Out
(if (boundp 'zone-timeout) (setq zone-timeout 300))
(if (fboundp 'zone-leave-me-alone) (zone-leave-me-alone))

;;; Global Key Bindings
(global-set-key "\C-xo" 'next-multiframe-window)
(global-set-key "\C-xp" 'previous-multiframe-window)
(global-set-key "\C-hh" 'help-for-help)
(global-set-key "\C-hg" 'magit-status)

(global-set-key (kbd "<f12>") 'clipboard-kill-ring-save)
(global-set-key "\C-h\C-w"    'clipboard-kill-ring-save)
(global-set-key "\C-h\C-y"    'clipboard-yank)

(global-set-key "\C-x\C-b" 'list-buffers)
;; (global-set-key "\C-x\C-b" 'bs-show)
(global-set-key "\C-h\C-r" 'rename-buffer)
(global-set-key "\C-h\C-p" 'cperl-perldoc)


(global-set-key "\C-h7" 'sql-send-region)
(global-set-key "\C-h8" '(lambda ()
						   (interactive)
						   (switch-to-buffer 
							(find-file-noselect
							 (expand-file-name "init.el" user-emacs-dir)))))
(global-set-key "\C-h9" 'my-toggle-fullscreen)

(defun my-toggle-fullscreen ()
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
	(set-frame-parameter nil 'alpha '(85 50))))
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
;;;   (url-retrieve "http://localhost/emacs.el" 'my-eval-url-callback)
(defun my-eval-url-callback ()
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

(defun my-cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car
   (let ((cperl-message-on-help-error nil))
     (cperl-get-help))))
(add-hook 'cperl-mode-hook
		  (lambda ()
			(set (make-local-variable 'eldoc-documentation-function)
				 'my-cperl-eldoc-documentation-function)))

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

(if (not (eq system-type 'windows-nt))
	(progn

;;; http://www.emacswiki.org/cgi-bin/wiki/BrowseUrl
	  (require 'w3m-load)
	  (require 'w3m)
	  ;;(setq browse-url-browser-function 'browse-url-generic)
	  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
	  ;;(setq browse-url-generic-program "/opt/google/chrome/google-chrome")

	  (defun choose-browser (url &rest args)
		(interactive "sURL: ")
		(if (y-or-n-p "Use external browser? ")
			(browse-url-generic url)
		  (w3m-browse-url url)))

	  (setq browse-url-browser-function 'choose-browser)))

(defun browse-url-default-macosx-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (if (and new-window (>= emacs-major-version 23))
      (ns-do-applescript
       (format (concat "tell application \"Safari\" to make document with properties {URL:\"%s\"}\n"
		       "tell application \"Safari\" to activate") url))
    (start-process (concat "open " url) nil "open" url)))

(global-set-key "\C-h\C-b" 'browse-url-at-point)

;;; PHP-Mode-Improved
;;; http://emacswiki.org/emacs/php-mode-improved.el
;;; (fetched by el-get)
(require 'php-mode)
(add-hook 'php-mode-hook 'turn-on-font-lock)

;;; ASCII table
(autoload 'ascii-table "ascii-table" nil t)


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

;; ctags -- http://www.emacswiki.org/emacs/BuildTags
;; Usage:
;;    M-.       goes to the symbol definition
;;    M-0 M-.   goes to the next matching definition
;;    M-*       return to your starting point
;;
;;    M-x tags-search <type your regexp>       initiate a search
;;    M-,                                      go to the next match


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
(setq path-to-ctags "/usr/bin/ctags")
(if (eq system-type 'darwin)
	(setq path-to-ctags "/opt/local/bin/ctags"))

(if (eq system-type 'windows-nt)
	(setq path-to-ctags "c:/opt/bin/ctags.exe"))
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name (directory-file-name dir-name))))

;;; EmacsServer  "server.el"
;;; Connect term via $ emacsclient -t   # exit with C-x 5 0
;;; Connect  gui via $ emacsclient FILE # exit with C-x #
(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(server-start)

;;; Erlang emacs setup
;;(setq my-erlang-emacs-dir "/opt/erlang5.9.2/lib/tools-2.6.8/emacs")
;;(if (eq system-type 'windows-nt)
;;	(setq my-erlang-emacs-dir "C:/opt/erlang5.9.2/lib/tools-2.6.8/emacs"))
;;(add-to-list 'load-path my-erlang-emacs-dir)
;;(require 'erlang-start)
;;(add-hook 'erlang-mode-hook 'erlang-font-lock-level-3)

(require 'tail)

;;; Auto Complete (installed by el-get)
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;;; Yasnippet (installed by el-get)
(require 'yasnippet)
(yas-global-mode 1)


;;;
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

;;; EDiff
;; Split horizontally
(setq ediff-split-window-function 'split-window-horizontally)
;; keep the ediff control panel in the same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; JIRA
;;(setq jira-url "https://julepdev.atlassian.net/rpc/xmlrpc")
;;(require 'jira)

(setq jiralib-url "https://julepdev.atlassian.net") 
(require 'org-jira)
