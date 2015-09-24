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
    markdown-mode
    web-mode
    expand-region
    ;;js-comint         ;; Using customized js-comint package from Startup-Class instead.
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

;; --- SMOOTH SCROLLING -----------------------------;;
(require 'smooth-scrolling)                          ;; instead of jumping new line to center of the window vertically, cursor stays on the bottom: https://github.com/aspiers/smooth-scrolling


;; --- ACE-JUMP --------------------------------------;;
(autoload                                             ;; Sets up ace-jump-mode and tells it to use C-c SPC to start the search for the starting letter you type in.
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(eval-after-load "ace-jump-mode"                      ;; This just turns this option on after the mode code is loaded. Specifically this optoin lets you jump back to your original mark using C-x SPC.
  '(ace-jump-mode-enable-mark-sync))
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)


;; --- EXPAND-REGION ---------------------------------;;
(require 'expand-region)
(global-set-key (kbd "C-m") 'er/expand-region)


;; --- YASNIPPET -------------------------------------;;
(require 'yasnippet)
(yas-global-mode t)
(add-hook 'term-mode-hook (lambda()                   ;; Turns off yasnippet when in term-mode so tab-complete works as you'd expect
    (setq yas-dont-activate t)))
;; ---- The below was commented out because once you add yasnippets as a source to auto-complete, ac-complete (tab) automatically expands the snippet. ----
;; ---- The only annoying part is when you use yasnippet to expand something and you want to use tab to jump sections in the snippet. You might type something and want to hit tab to jump sections
;; but the autocomplete menu will come up, hitting tab would autocomplete your term, instead of jumping, you just have to hit C-g to escape the autocomplete menu first, then tab to jump sections ----
;;(define-key yas-minor-mode-map (kbd "TAB")    nil)       ;; the tab button is better used for auto-complete, so this stops yasnippet from using it. "TAB" changes both TAB and C-i, so works in terminals.
;;(define-key yas-minor-mode-map (kbd "\C-o") 'yas-expand) ;; makes C-o expanding snippets. Original plan was to totally separate yasnippet and auto-complete.
;;(define-key yas-minor-mode-map (kbd "<tab>")  nil)       ;; This is the tab-key in the graphical terminal (separate from C-i), and therefore not needed since I only use the terminal.


;; --- AUTO-COMPLETE ---------------------------------;;
(require 'auto-complete-config)                       ;; This requires 'auto-complete, so I removed that step. It also includes already includes all the yasnippets
(ac-config-default)                                   ;; This configures auto-complete for several modes using several sources. C-x C-f auto-complete-config it to investigate which ones specifically.
;;(add-to-list 'ac-dictionary-files "~/.emacs.d/elpa/auto-complete-BUNCHofNUMBERS/dict/") ;; Manually links up the major-mode dictionaries. Done automatically. User-Custom Dictionaries are another method.
(define-key ac-complete-mode-map "\C-n" 'ac-next)     ;; Lets you use the standard emacs navigation keys to select auto-complete options (to keep hands on the home-row).
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(define-key ac-complete-mode-map "\C-s" 'ac-isearch)  ;; Lets you search through auto-complete options
(define-key ac-complete-mode-map "\t" 'ac-complete)   ;; These two lines make tab immedietely complete the auto-complete, it's necessary because even with ac-dwim, tab starts stepping through the list.
(define-key ac-complete-mode-map "\r" nil)            ;; Turns off return from selecting the auto-complete. Force me to use C-i or "TAB".
(define-key ac-complete-mode-map [tab] 'ac-complete)  ;; ac-dwim only ac-completes after you've adjusted the cursor. that means for the first one it's a pain.
(setq-default ac-sources (push 'ac-source-yasnippet ac-sources))            ;; adds yasnippets as a source for autocomplete by default.
(setq-default ac-sources (push 'ac-source-files-in-current-dir ac-sources)) ;; Uses filenames within the current directory as a source for auto-complete
(setq-default ac-sources (push 'ac-source-filename ac-sources))             ;; Uses files and directories as a source for auto-complete, starts immedietely after "/"
(add-to-list 'ac-modes 'markdown-mode)                ;; Lets markdown-mode use auto-complete
(add-to-list 'ac-modes 'fundamental-mode)             ;; fundamental-mode use auto-complete (this is the most generic emacs mode)
;;(setq ac-auto-show-menu 0.2)                          ;; speeds up how quickly the auto-complet menu pops up. Default was 0.8. **Ended up being annoying having the menu flashing**
(setq ac-disable-faces nil)                           ;; AC won't activate over any faces in the ac-disable faces list (default: font-lock-comment-face font-lock-string-face font-lock-doc-face).
						      ;; The above line sets the ac-disable-faces to nothing so that AC is always active. It was annoying to have it off for strings and comments.

;; --- MARKDOWN-MODE -------------------------------- ;;
;; The below autoload method makes emacs startup quicker (autoload doesn't load unless it's needed), but it requires the special eval-after-load to do the keybinding since it can't do it until the
;; the package is loaded. It's also possible to do a similar set-up using the mode-hooks. mode-hooks are called every single time a buffer with that mode is opened, so you can using mode-hooks to define
;; per buffer settings, while eval-after-load only runs once when the package is loaded the first time (usually at start-up). Therefore do key-bindings in eval-after-loads so that they're only run once (that's all they need to be run, otherwise it's just running extra code everytime you open a buffer that calls the mode-hooks).
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(eval-after-load "markdown-mode"
  '(progn
     (define-key markdown-mode-map (kbd "<tab>") nil)                   ;; Removes the original tab key-binding in markdown-mode since it's already in heavy use by auto-complete.
     (define-key markdown-mode-map (kbd "<S-iso-lefttab>") nil)         ;; Removes the original shift-tab key-binding in markdown-mode
     (define-key markdown-mode-map (kbd "<S-tab>") nil)
     (define-key markdown-mode-map (kbd "<backtab>") nil)
     (define-key markdown-mode-map (kbd "<f7>") 'markdown-cycle)
     (define-key markdown-mode-map (kbd "<f8>") 'markdown-shifttab)
     (define-key markdown-mode-map "\M-n" nil)                          ;; Stops markdown-mode from clobbering my Next5 and Prev5 map-keys that are defined in init.el
     (define-key markdown-mode-map "\M-p" nil)))


;; --- HIDESHOW (JAVASCRIPT CODE FOLDING --------------;;
;; adds code-folding by using the HideShow library
;; Normal functions are hs-show-block, hs-hide-block, hs-show-all, and hs-hide-all
;; The below combines them into two toggling buttons.
(eval-after-load 'js
  '(progn
     (defvar my-hs-hide nil "Current state of hideshow for toggling all.")
     (defun my-toggle-hideshow-all () "Toggle hideshow all."
       (interactive)
       (setq my-hs-hide (not my-hs-hide))
       (if my-hs-hide
	   (hs-hide-all)
	 (hs-show-all)))

     (define-key js-mode-map (kbd "<f6>") 'my-toggle-hideshow-all)
     (define-key js-mode-map (kbd "C-\\") 'hs-toggle-hiding)
     (add-hook 'js-mode-hook 'hs-minor-mode)))                          ;; Note that this only turns on hs-mode for when the js-mode is on. Below, I use web-mode's built-in hide/show funcitonality for css and html.


;; --- JSHINT AND NODE REPL ---------------------------;;
(load "js-config.el")


;; --- WEB-MODE ---------------------------------------;;
;; web-mode is in the default ac-modes list, so you don't have to add web-mode to ac-modes to get auto-complete workign with it.
(autoload 'web-mode "web-mode"
  "MAJOR mode for editing web files (css and html)" t)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . web-mode))
(eval-after-load 'web-mode
  '(progn
     (setq-default indent-tabs-mode nil)                                ;; Turns off indenting with tabs. This made the indent settings below work (might be a bug?).
     (setq web-mode-markup-indent-offset 2)                             ;; HTML indent setting
     (setq web-mode-css-indent-offset 2)                                ;; CSS indent setting
     (setq web-mode-code-indent-offset 2)                               ;; Script block indent setting (embedded js, php, python, etc.)
     (setq web-mode-enable-current-element-highlight t)                 ;; Highlights matching tag element for current tag ex. Highlights the closing tag for the open tag your point is at.
     (setq web-mode-enable-current-column-highlight t)                  ;; Highlights a column to show the length of the current tag.
     (setq web-mode-enable-auto-closing t)                              ;; Makes it so that if you type <body>, it will automatically create the closing tag </body> once you type the "</"
     (setq web-mode-auto-close-style 2)                                 ;; This controls how the previous autoclose works. 0=auto-closing. 1=close on "</". 2=close on ">".
     (setq web-mode-enable-auto-expanding t)                            ;; Turns on auto-expansion of things like d/ to <div>|</div> see web-mode source for full list of items, but it's a-z followed by /. REMOVE LATER IF I DON'T USE.
;;     (setq web-mode-enable-auto-pairing t)
     (define-key web-mode-map (kbd "C-m") 'web-mode-mark-and-expand)    ;; this command is tied to c-c c-m by default. it's really useful though.
     (define-key web-mode-map (kbd "C-\\") 'web-mode-fold-or-unfold)))  ;; make the web-mode code-folding have the same shortcut as my js-mode code folding.

