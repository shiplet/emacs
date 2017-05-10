;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Editor SETTINGS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install the following packages before loading:
;; ace-jump-mode
;; ace-window
;; js2-mode
;; multiple-cursors
;; popwin
;; projectile
;; sass-mode
;; web-mode
;; atom-one-dark-theme
;; neotree
;; fiplr
;; smartparens
;; elpy


(require 'package)
 (add-to-list 'package-archives
 	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "M-F"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "M-c"))
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
(global-unset-key (kbd "M-d"))
(global-unset-key (kbd "M-q"))
(global-unset-key (kbd "M-/"))

(global-set-key (kbd "M-F") 'rgrep)
;; (global-set-key (kbd "M-C-g") 'magit-status)
(global-set-key (kbd "M-c") 'compile)

(global-set-key (kbd "M-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-m C-M-m") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-w") 'mc/mark-next-word-like-this)
(global-set-key (kbd "C-?") 'mc/mark-all-like-this)

(global-set-key (kbd "C-x j") 'neotree)
(global-set-key (kbd "M-t") 'fiplr-find-file)
(global-set-key (kbd "C-x C-k") 'ido-kill-buffer)
(global-set-key (kbd "M-s M-d") 'save-some-buffers)
(global-set-key (kbd "M-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "M-s d") 'save-buffer)
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-0") 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?l))

(global-auto-revert-mode 1)
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
(global-hl-line-mode 1)
(elpy-enable)

;; Mac keybindings
(setq mac-option-modifier 'super)
(setq ns-function-modifier 'hyper)
(setq mac-command-modifier 'meta)
(desktop-save-mode 1)

(delete `elpy-module-highlight-indentation elpy-modules)

(smartparens-global-mode 1)

;; eshell clear
(defun eshell/clear()
  "Clear the eshell buffer"
  (let ((inhibit-read-only t))
    (erase-buffer)
    ))


(add-to-list 'load-path "~/.elisp")

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
(setq web-mode-enable-current-column-highlight nil)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq ac-js2-evaluate-calls t)


;; Popwin
(add-to-list 'load-path "~/.emacs.d/elpa/popwin-0.6.2")
(setq display-buffer-function 'popwin:display-buffer)
(require 'popwin)

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
(setq default-directory "~")

;; Set linum and column mode
(global-linum-mode 1)
(setq column-number-mode t)
(setq line-number-mode t)

;; Font and other settings
(setq ring-bell-function 'ignore)


;;;;;;;;;;;;;;;;;;;;;;;
;;;; INSTALLATIONS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; SLIME
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")

;; Ace-Jump-Mode major function
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-o") 'ace-jump-char-mode)

;; enable a more powerful jump back function from ace jump mode
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back :-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-back)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes (quote (atom-dark)))
 '(custom-safe-themes
   (quote
    ("a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" default)))
 '(fci-rule-color "#3E4451")
 '(hl-sexp-background-color "#1c1f26")
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (atom-dark-theme expand-region elpy sass-mode smartparens web-mode projectile popwin neotree multiple-cursors js2-mode fiplr ace-window ace-jump-mode)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Ubuntu Mono")))))
