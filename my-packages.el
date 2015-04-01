1;3409;0c;; ----------------------------------------------------
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
;;(yas-load-directory "~/.emacs/customSnippets")      ;; Once I make my own snippets put them in here. See corresponding line in setup.sh to symlink the dir from the dotfiles repo.
(add-hook 'term-mode-hook (lambda()                   ;; Turns off yasnippet when in term-mode so tab-complete works as you'd expect
    (setq yas-dont-activate t)))
(define-key yas-minor-mode-map [(tab)]        nil)    ;; these lines make yas-snippet entirely use c-o key-bindings. c-o is normally associated with abbrevs, which i don't use.n
(define-key yas-minor-mode-map (kbd "TAB")    nil)    ;; the tab button interferes with auto-complete functionality, so i made it complete seperate.
(define-key yas-minor-mode-map (kbd "<tab>")  nil)
(define-key yas-minor-mode-map (kbd "\C-o") 'yas-expand) ;; makes C-o expanding snippets.

(require 'auto-complete-config)                       ;; This requires 'auto-complete, so I removed that step. It also includes already includes all the yasnippets
(ac-config-default)                                   ;; This configures several dictionaries. C-x C-f auto-complete-config it to investigate which ones specifically.
;;(add-to-list 'ac-dictionary-files "~/.emacs.d/elpa/auto-complete-20150322.813/dict/") ;; Actually uses the all the libraries, like js-mode. Not sure if it uses them inteligently with hooks.
(define-key ac-complete-mode-map "\C-n" 'ac-next)     ;; Lets you use the standard emacs navigation keys to select auto-complete options (to keep hands on the home-row).
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(define-key ac-complete-mode-map "\C-s" 'ac-isearch)  ;; Lets you search through auto-complete options
(define-key ac-complete-mode-map "\t" 'ac-complete)   ;; These two lines make tab immedietely complete the auto-complete, it's necessary because even with ac-dwim, tab starts stepping through the list.
(define-key ac-complete-mode-map [tab] 'ac-complete)  ;; ac-dwim only ac-completes after you've selected it. that means for the first one it's a pain.
;;(add-to-list 'ac-sources 'ac-source-yasnippet)      ;; Several references said this needed to be included to get yasnippets in the auto-complete menu.
