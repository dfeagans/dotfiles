;; ----------------------------------------------------
;; ------------ JS-Specific Configurations ------------
;; ----------------------------------------------------

(require 'js-comint)
(defun clean-and-jslint ()
  (interactive)
  (whitespace-cleanup)
  (compile compile-command))


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
			   ))

(setq inferior-js-program-command "node")
