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

(require 'smooth-scrolling)                ;; instead of jumping new line to center of the window vertically, cursor stays on the bottom: https://github.com/aspiers/smooth-scrolling

(require 'yasnippet)
(yas-global-mode t)
;;(yas-load-directory "~/.emacs/customSnippets") ;; Once I make my own snippets put them in here. See corresponding line in setup.sh to symlink the dir from the dotfiles repo.
(add-hook 'term-mode-hook (lambda()        ;; Turns off yasnippet when in term-mode so tab-complete works as you'd expect
    (setq yas-dont-activate t)))

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
