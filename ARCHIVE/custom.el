(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(csv-separators (quote ("," "	" "|")))
 '(mu-worlds
   (quote
	(["Trek Mush" "ats.trekmush.org" 1701 "guest" "guest"])))
 '(newsticker-url-list
   (quote
	(("emacs.stackexchange.com" "http://emacs.stackexchange.com/feeds" nil nil nil))))
 '(org-agenda-files (quote ("~/Dropbox/bookmarks.org")))
 '(safe-local-variable-values
   (quote
	((mangle-whitespace . t)
	 (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
		   (add-hook
			(quote write-contents-functions)
			(lambda nil
			  (delete-trailing-whitespace)
			  nil))
		   (require
			(quote whitespace))
		   "Sometimes the mode needs to be toggled off and on."
		   (whitespace-mode 0)
		   (whitespace-mode 1))
	 (whitespace-line-column . 80)
	 (whitespace-style face trailing lines-tail)
	 (cperl-brace-imaginary-offset . 0))))
 '(sql-indent-first-column-regexp
   "^\\s-*\\(--\\|begin\\|create\\|d\\(?:el\\(?:ete\\|imiter\\)\\|rop\\)\\|end\\|alter\\|from\\|group\\|having\\|in\\(?:sert\\|t\\(?:ersect\\|o\\)\\)\\|order\\|se\\(?:\\(?:lec\\)?t\\)\\|truncate\\|u\\(?:nion\\|pdate\\)\\|where\\)\\(\\b\\|\\s-\\)"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
