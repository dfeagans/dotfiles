;; -*- mode: emacs-lisp -*-
;; The items changed from the initital
;; start-up class init.el have beem marked with ;;NOT NEEDED

;; ---------------------
;; -- Global Settings --
;; ---------------------
(add-to-list 'load-path "~/.emacs.d")
(require 'cl)          ;;Includes the common lisp package
(require 'ido)         ;;Interactive Do. Improves name completion and lots of other things.
(require 'ffap)        ;;Improves the power of file finds etc.
(require 'uniquify)    ;;Handles when you have two files of the same name in different directories opened.
(require 'ansi-color)
;;NOT NEEDED(require 'recentf) 
(require 'linum)       ;;Might have to install 
(require 'smooth-scrolling) ;;Might have to install
;;NOT NEEDED(require 'whitespace)
(require 'dired-x)     ;;Expanded version of dired-mode that lets you interactive with directories.
(require 'compile)    
(ido-mode t)
(menu-bar-mode -1)
(normal-erase-is-backspace-mode 0)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq column-number-mode t)
(setq inhibit-startup-message t)
(setq save-abbrevs nil)
(setq show-trailing-whitespace t)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)
;; Below adds row numbers to the left of all documents
(global-linum-mode 1)
;; Below makes parenthesis create two to stay matched
(electric-pair-mode 1)

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
;;NOT NEEDED (load "defuns-config.el")
(fset 'align-equals "\C-[xalign-regex\C-m=\C-m")
(global-set-key "\M-=" 'align-equals)
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
;; ---- DF's Custom Keys -----
;; ---------------------------
;; Below makes comments more visible
(set-face-foreground 'font-lock-comment-face "light pink")
;; This enables reloading the file if you've modified it. Useful after Git checkout.
(global-set-key (kbd "C-c r") 'revert-buffer)
;; This enables the erase-buffer command. It's turned off by default otherwise.
;; It was mildly useful to completely clear the repl out.
(put 'erase-buffer 'disabled nil)

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
