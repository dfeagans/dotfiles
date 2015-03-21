;; -*- mode: emacs-lisp -*-
;; The items changed from the initital
;; start-up class init.el have beem marked with ;;NOT NEEDED

;; ---------------------
;; -- Global Settings --
;; ---------------------
(add-to-list 'load-path "~/.emacs.d")
;;(require 'cl)          ;;Includes the common lisp package
;;(require 'ido)         ;;Interactive Do enables C-x b changes the buffer. C-x C-f does find file.
;;(require 'ffap)        ;;Improves the power of file finds etc. Lets you used C-x d for ffap-list-directory
;;(require 'dired-x)     ;;Provides methods for viewing/visiting a file mentioned in a file opened in a buffer.
;;(require 'uniquify)    ;;Handles when you have two files of the same name in different directories opened.
;;(require 'ansi-color)
;;NOT NEEDED(require 'recentf)    ;;lets you interact with recentfiles, I don't have any functions key-bound though.
;;(require 'linum)                  ;;puts line numbers in files. Should be installed in emacs 22+
(require 'smooth-scrolling)     ;;Definitely have to install
;;NOT NEEDED(require 'whitespace)
;;(require 'compile)    
;;(ido-mode t)                         ;;actually turns on ido-moden
(menu-bar-mode -1)                   ;;turns the top and bottom menus off because I won't need them in the terminal
(normal-erase-is-backspace-mode 0)   ;;makes backspace function correctly on ubuntu
(put 'downcase-region 'disabled nil) ;;enables the downcase-region advanced feature
(put 'upcase-region 'disabled nil)   ;;enables the upcase-region advanced feature
(put 'erase-buffer 'disabled nil)    ;;enables erase-buffer, useful for clearing out repl buffer for more commands.
(setq column-number-mode t)          ;;makes it so the line number the cursor is on is displayed next to the character above the mini-buffer
(setq inhibit-startup-message t)     ;;turns off startup message
(setq save-abbrevs nil)              ;;stops emacs from prompting to save the abbreviations if you happen to use them.
(setq show-trailing-whitespace t)    
(setq suggest-key-bindings t)        ;;If you run a command using M-x COMMAND, it will show the shortcut on the mini-buffer afterwards
(setq vc-follow-symlinks t)          ;;Tells emacs to open the actual file if you open a sym-linked file version controlled file.
;;(global-linum-mode 1)                ;;Turns linum-mode on globally to add line numbers to all files.
(electric-pair-mode 1)               ;; makes parenthesis create two to stay matched

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
 '(highlight ((((class color) (min-colors 8)) (:background "hite" :foreground "magenta"))))
 '(isearch ((((class color) (min-colors 8)) (:background "yellow" :foreground "black"))))
 '(linum ((t (:foreground "black" :weight bold))))
 '(region ((((class color) (min-colors 8)) (:background "white" :foreground "magenta"))))
 '(secondary-selection ((((class color) (min-colors 8)) (:background "gray" :foreground "cyan"))))
 '(show-paren-match ((((class color) (background light)) (:background "black"))))
 '(vertical-border ((t nil)))
)

;; ------------
;; -- Macros --
;; ------------
(global-set-key (kbd "C-c r") 'revert-buffer) ;; This enables reloading the file if you've modified it. Useful after Git checkout.
;;NOT NEEDED (load "defuns-config.el") 
(fset 'align-equals "\C-[xalign-regex\C-m=\C-m")
(global-set-key "\M-=" 'align-equals)                     ;;This aligns the assignment operators (=) throughout the document
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c;" 'comment-or-uncomment-region)
(global-set-key "\M-n" 'next5)
(global-set-key "\M-p" 'prev5)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-i" 'back-window)
(global-set-key "\C-z" 'zap-to-char)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\M-d" 'delete-word)
(global-set-key "\M-h" 'backward-delete-word)
(global-set-key "\M-u" 'zap-to-char)

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
