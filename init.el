;; -*- mode: emacs-lisp -*-

;; ---------------------
;; -- Global Settings --
;; ---------------------
(add-to-list 'load-path "~/.emacs.d")
(require 'cl)                        ;; Includes the common lisp language. Used in my-packages and ...
;;(require 'dired-x)                 ;; Not going to use unless I find normal dired limiting
;;(require 'recentf)                 ;; Tracks recent files: http://emacsredux.com/blog/2013/04/05/recently-visited-files/
;;(require 'compile)                 ;; !!!!MIGHT BE NEEDED FOR THE JS-MODE!!!!
(require 'ido)                       ;; Interactive Do enables C-x b changes the buffer. Improves functionality of C-x C-f
(require 'uniquify)                  ;; Handles when you have two files of the same name in different directories opened.
(require 'ansi-color)                ;; Useful for the ansi-color-for-comint-mode-on. 
(load "~/dotfiles/my-packages.el")   ;; Loads AND configures the external packages I use.
(menu-bar-mode -1)                   ;; turns the top and bottom menus off because I won't need them in the terminal
(setq suggest-key-bindings t)        ;; If you run a command using M-x COMMAND, it will show the shortcut on the mini-buffer afterwards
(setq column-number-mode t)          ;; makes it so the line number the cursor is on is displayed next to the character above the mini-buffer
(normal-erase-is-backspace-mode 0)   ;; makes backspace function correctly on ubuntu
(put 'downcase-region 'disabled nil) ;; enables the downcase-region advanced feature
(put 'upcase-region 'disabled nil)   ;; enables the upcase-region advanced feature
(put 'erase-buffer 'disabled nil)    ;; enables erase-buffer, useful for clearing out repl buffer for more commands.
(setq save-abbrevs nil)              ;; stops emacs from prompting to save the abbreviations if you happen to use them.
(setq vc-follow-symlinks t)          ;; Tells emacs to open the actual file if you open a sym-linked file version controlled file.
(ido-mode t)                         ;; actually turns on ido-mode
(setq ido-use-filename-at-point 'guess) ;; uses the ffap built into ido. Pretty cool, actually works on (require 'library). Mildly annoying when you don't want it.
(setq ido-enable-flex-matching t)    ;; lets C-x C-f find file return the file test_name if you type, "tn" for example.
(setq uniquify-buffer-name-style 'forward) ;; makes it so that uniquify identifies files using the style: "as/many/parentdirs/as/needed/filename"
(setq inhibit-startup-message t)     ;; turns off startup message
(electric-pair-mode 1)               ;; makes parenthesis create two to stay matched
(global-linum-mode 1)                ;; Turns linum-mode on globally to add line numbers to all files. linum.elc is in emacs24 by default.
(fset 'yes-or-no-p 'y-or-n-p)        ;; shortens all the yes-or-no prompts to y-or-n

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit autoface-default :strike-through nil :underline nil :slant normal :weight normal :height 120 :width normal :family "monaco"))))
 '(column-marker-1 ((t (:background "red"))))
 '(diff-added ((t (:foreground "cyan"))))
 '(flymake-errline ((((class color) (background light)) (:background "Red"))))
 '(font-lock-comment-face ((((class color) (min-colors 8) (background light)) (:foreground "red"))))
 '(fundamental-mode-default ((t (:inherit default))))
 '(highlight ((((class color) (min-colors 8)) (:background "white" :foreground "magenta"))))
 '(isearch ((((class color) (min-colors 8)) (:background "yellow" :foreground "black"))))
 '(linum ((t (:foreground "black" :weight bold))))
 '(region ((((class color) (min-colors 8)) (:background "white" :foreground "magenta"))))
 '(secondary-selection ((((class color) (min-colors 8)) (:background "gray" :foreground "cyan"))))
 '(show-paren-match ((((class color) (background light)) (:background "black"))))
 '(vertical-border ((t nil)))
 '(minibuffer-prompt ((((class color) (min-colors 8)) (:background "black" :foreground "magenta"))))
)

;; ---------------------------
;; --------- Macros ----------
;; ---------------------------
(global-set-key (kbd "C-c r") 'revert-buffer)            ;; This enables reloading the file if you've modified it. Useful after Git checkout.
(global-set-key "\C-c;" 'comment-or-uncomment-region)    ;; Toggles commenting on selected region. M-; just comments the selection. This actually toggles it.
(global-set-key "\M-o" 'other-window)                    ;; Jumps to ther windows within emacs should you have them open.

(defun multi-occur-in-all-open-buffers(regexp &optional allbufs) ;; This just makes the function just absorbe addt'l arguments my making it optional ... I think.
  "Show all lines mathcing REGEXP in all buffers."
  (interactive (occur-read-primary-args))                        ;; This makes the function prompt leverage an existing occur-mode message in the minibuffer. Could have used "sSearch:" more simply.
  (multi-occur-in-matching-buffers ".*" regexp)                  ;; self-explanatory; runs the search for the term the user entered "regexp" through all buffers (.*)
)
(defun next5()
  (interactive)
  (next-line 5)
)
(defun prev5()
  (interactive)
  (previous-line 5)
)
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times.
http://www.emacswiki.org/emacs/BackwardDeleteWord
"
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "M-s s") 'multi-occur-in-all-open-buffers)
(global-set-key "\M-n" 'next5)
(global-set-key "\M-p" 'prev5)
(global-set-key "\M-d" 'delete-word)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-h" 'delete-backward-word)

;; -------- Used Less --------
(global-set-key "\M-u" 'whitespace-cleanup)               ;;This removes starting and trailing empty lines. Does something with tabs/spaces, but not sure.
(fset 'align-equals "\C-[xalign-regex\C-m=\C-m")
(global-set-key "\M-=" 'align-equals)                     ;; This aligns the assignment operators (=) throughout the document
(global-set-key "\C-x\C-m" 'execute-extended-command)     ;; This lets you run commands, just like M-x
(global-set-key "\C-z" 'zap-to-char)                      ;; Yanks up to the character you enter the prompt. Useful for deleting sentences (.

;; ---------------------------
;; -- JS Mode configuration --
;; ---------------------------
;; I AM WORKING ON DEVELOPING MY OWN JAVASCRIPT IDE
;; THIS IS JUST REFERNECE TO THE OLD ONE I USED>
;;(load "js-config.el")
;;(add-to-list 'load-path "~/.emacs.d/jade-mode") ;; github.com/brianc/jade-mode
;;(require 'sws-mode)
;;(require 'jade-mode)
;;(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
;;(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; ---------------------------
;; ---- Load Markdown Mode ---
;; ---------------------------
;; THIS MODE IS NO LONGER USED, JUST FOR REFERENCE NOW
;; Loads Markdown Mode (http://jblevins.org/projects/markdown-mode/) for the syntax highlighting.
;; The rest of the options of markdown mode are defined in markdown-mode.el
;;(autoload 'markdown-mode "markdown-mode"
;;   "Major mode for editing Markdown files" t)
;;(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;;(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;;(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; ---------------------------
;; ---- WriteGood Mode -------
;; ---------------------------
;; THIS MODE IS NO LONGER USED, JUST FOR REFERENCE NOW
;; USAGE INFO: bnbeckwith.com/code/writegood-mode.html
;; GIT PAGE: www.github.com/bnbeckwith/writegood-mode

;; Supply the path to the mode repository
;;(add-to-list 'load-path "~/.emacs.d/writegood-mode.el")
;;(require 'writegood-mode)
;; Set a global key to toggle the mode
;;(global-set-key "\C-cg" 'writegood-mode)
