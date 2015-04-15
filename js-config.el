;; ----------------------------------------------------
;; ------------ JS-Specific Configurations ------------
;; ----------------------------------------------------
;; ------ Methods Taken From Startup-Class Github -----
;; ---- https://github.com/startup-class/dotfiles -----
;; ----------------------------------------------------


;; -------------------- Node REPL ---------------------
(require 'js-comint)
(setq inferior-js-program-command "node")
(setq inferior-js-mode-hook
      (lambda ()
	;; We like nice colors
	(ansi-color-for-comint-mode-on)
	;; Deal with some prompt nonsense
	(add-to-list 'comint-preoutput-filter-functions
		     (lambda (output)
		       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
		     (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))))


;; --------------------- JSHint -----------------------
(defun clean-and-jslint ()
  (interactive)
  (whitespace-cleanup)
  (compile compile-command))

;; Configure jshint for JS style checking.
;;   - Install: $ npm install -g jshint
;;   - Usage: Hit C-cC-u within any emacs buffer visiting a .js file
(setq jshint-cli "jshint --show-non-errors ")
(setq compilation-error-regexp-alist-alist
      (cons '(jshint-cli "^\\([a-zA-Z\.0-9_/-]+\\): line \\([0-9]+\\), col \\([0-9]+\\)"
			 1 ;; file
			 2 ;; line
			 3 ;; column
			 )
	    compilation-error-regexp-alist-alist))
(setq compilation-error-regexp-alist
      (cons 'jshint-cli compilation-error-regexp-alist))


;; ------------- JS-Specific Key-Bindings -------------
(add-hook 'js-mode-hook '(lambda ()
			   ;;(local-set-key "\C-x\C-e" 'eval-last-sexp)
			   ;;(local-set-key "\C-cb" 'js-send-buffer)
			   ;;(local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			   ;;(local-set-key "\C-cl" 'js-load-file-and-go)
			   (local-set-key "\C-c!" 'run-js)                                 ;; starts node.js repl
			   (local-set-key "\C-c\C-r" 'js-send-region)                      ;; sends selected region in the file to the repl
			   (local-set-key "\C-c\C-j" 'js-send-line)                        ;; sends current line to repl
			   (set (make-local-variable 'compile-command)
				(let ((file buffer-file-name)) (concat jshint-cli file)))
			   (set (make-local-variable 'compilation-read-command) nil)
			   (local-set-key "\C-c\C-u" 'clean-and-jslint)                    ;; sends current buffer to jshint and dispays errors
			   ;; The other useful key in jshint buffer is C-m which jumps to the offending line number in the file you ran jshint on.
			   ))
