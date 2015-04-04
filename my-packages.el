;; ----------------------------------------------------
;; -------------------- REPO LIST ---------------------
;; ----------------------------------------------------

(require 'package)
;; ----- Probably Best Repository But Less Vetted -----
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; ---------- More Stable Package Repository ----------
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; ----------------------------------------------------
;; ------------------- PACKAGE LIST -------------------
;; ----------------------------------------------------

(defvar required-packages
  '(
    smooth-scrolling
    yasnippet
    auto-complete
  ) "List of packages to ensure are installed at launch.")

;; -------- Function for Checking if Installed --------
(defun packages-installed-p ()
  (loop for p in required-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

;; ---------- Actually Installs Package List ----------

(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; ----------------------------------------------------
;; -------------- PACKAGE CONFIGURATION ---------------
;; ----------------------------------------------------

(require 'smooth-scrolling)                          ;; instead of jumping new line to center of the window vertically, cursor stays on the bottom: https://github.com/aspiers/smooth-scrolling

(require 'yasnippet)
(yas-global-mode t)
(add-hook 'term-mode-hook (lambda()                   ;; Turns off yasnippet when in term-mode so tab-complete works as you'd expect
    (setq yas-dont-activate t)))
;; ---- The below was commented out because once you add yasnippets as a source to auto-complete, ac-complete (tab) automatically expands the snippet. ----
;;(define-key yas-minor-mode-map (kbd "TAB")    nil)       ;; the tab button is better used for auto-complete, so this stops yasnippet from using it. "TAB" changes both TAB and C-i, so works in terminals.
;;(define-key yas-minor-mode-map (kbd "\C-o") 'yas-expand) ;; makes C-o expanding snippets. Original plan was to totally separate yasnippet and auto-complete.
;;(define-key yas-minor-mode-map (kbd "<tab>")  nil)       ;; This is the tab-key in the graphical terminal (separate from C-i), and therefore not needed since I only use the terminal.

(require 'auto-complete-config)                       ;; This requires 'auto-complete, so I removed that step. It also includes already includes all the yasnippets
(ac-config-default)                                   ;; This configures auto-complete for several modes using several sources. C-x C-f auto-complete-config it to investigate which ones specifically.
;;(add-to-list 'ac-dictionary-files "~/.emacs.d/elpa/auto-complete-20150322.813/dict/") ;; Actually uses the all the libraries, like js-mode. Not sure if it uses them inteligently with hooks.
(define-key ac-complete-mode-map "\C-n" 'ac-next)     ;; Lets you use the standard emacs navigation keys to select auto-complete options (to keep hands on the home-row).
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(define-key ac-complete-mode-map "\C-s" 'ac-isearch)  ;; Lets you search through auto-complete options
(define-key ac-complete-mode-map "\t" 'ac-complete)   ;; These two lines make tab immedietely complete the auto-complete, it's necessary because even with ac-dwim, tab starts stepping through the list.
(define-key ac-complete-mode-map [tab] 'ac-complete)  ;; ac-dwim only ac-completes after you've adjusted the cursor. that means for the first one it's a pain.
(setq-default ac-sources (push 'ac-source-yasnippet ac-sources)) ;; adds yasnippets as a source for autocomplete by default.
(global-auto-complete-mode t)                         ;; This is already called in ac-config-default, but adding it here makes auto-complete mode actually work in all buffers.
