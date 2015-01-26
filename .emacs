;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EDITOR SETTINGS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.elisp")
(add-to-list 'load-path "~/.emacs.d/elpa/autopair-0.6.1")
(require 'autopair)
(autopair-global-mode)
(require 'ido)
(ido-mode t)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))


(add-hook 'js-mode-hook 'js2-minor-mode)

;; Turn off mouse interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; No scratch message
(setq initial-scratch-message nil)

;; Default directory
(setq default-directory "~/")

;; Set linum and column mode
(global-linum-mode 1)
(setq column-number-mode t)
(setq line-number-mode t)

;; Tomorrow Night theme
(require 'color-theme-tomorrow)
(color-theme-tomorrow--define-theme night)

;; Mac keybindings
(setq mac-option-modifier 'super)
(setq ns-function-modifier 'hyper)
(setq mac-command-modifier 'meta)

;; New save and shutdown keystroek
(global-set-key (kbd "C-x C-h") 'save-buffers-kill-terminal)
(global-set-key (kbd "M-s d") 'save-buffer)

;; Font and other settings
(setq ring-bell-function 'ignore)
(set-default-font "-apple-Input_Sans_Narrow-medium-normal-condensed-*-11-*-*-*-p-0-iso10646-1")

;; Move more quickly



;;;;;;;;;;;;;;;;;;;;;;;
;;;; INSTALLATIONS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; MELPA package manager
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

;; SLIME
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; Ace-Jump-Mode major function
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-o") 'ace-jump-mode)
;; enable a more powerful jump back function from ace jump mode
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back :-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-back)

;; Ace Window Mode
(global-set-key (kbd "C-0") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?l))

;; Setting keystrokes for multiple cursors
(global-set-key (kbd "C-M-m C-M-m") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-w") 'mc/mark-next-word-like-this)
(global-set-key (kbd "C-?") 'mc/mark-all-like-this)

;; Expand Region keybinding
(global-set-key (kbd "C-=") 'er/expand-region)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 )
(custom-set-faces
 )
