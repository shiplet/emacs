;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EDITOR SETTINGS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
 (add-to-list 'package-archives
 	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)


(package-initialize)
(global-auto-revert-mode 1)

;; Mac keybindings
(setq mac-option-modifier 'super)
(setq ns-function-modifier 'hyper)
(setq mac-command-modifier 'meta)

;; Monokai
(load-theme 'monokai t)

(global-set-key (kbd "C-x C-k") 'ido-kill-buffer)
(global-set-key (kbd "M-s M-d") 'save-some-buffers)
(add-to-list 'load-path "~/.elisp")
(add-to-list 'load-path "~/.emacs.d/elpa/autopair-0.6.1")
(require 'autopair)
(autopair-global-mode)
(require 'ido)
(ido-mode t)
(require 'projectile)
(projectile-global-mode)
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
(global-unset-key (kbd "C-x C-n"))
(global-unset-key (kbd "C-h C-n"))
(global-unset-key (kbd "C-x C-c"))

;; Web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  
)
(add-hook 'web-mode-hook  'my-web-mode-hook)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-ac-sources-alist
  '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
  )
(setq web-mode-enable-current-column-highlight t)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq ac-js2-evaluate-calls t)


;; Popwin
(add-to-list 'load-path "~/.emacs.d/elpa/popwin-0.6.2")
(setq display-buffer-function 'popwin:display-buffer)
(require 'popwin)

;; Direx
(add-to-list 'load-path "~/.emacs.d/elpa/direx-el")
(push '(direx:direx-mode :position left :width 50 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
(global-set-key (kbd "C-x j") 'direx:jump-to-directory-other-window)
(require 'direx)

;; Adding js2-mode hook to js-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'javascript-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'skewer-mode)

;; Turn off mouse interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; No scratch message
(setq initial-scratch-message nil)

;; Default directory
(setq default-directory "~/dev/")

;; Set linum and column mode
(global-linum-mode 1)
(setq column-number-mode t)
(setq line-number-mode t)

;; Tomorrow Night theme
;; (require 'color-theme-tomorrow)
;; (color-theme-tomorrow--define-theme night)

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


;; SLIME
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")

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
 '(initial-frame-alist (quote ((width . 361) (height . 106))))
 )
(custom-set-faces
 )
