(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Load Theme
(load-theme 'solarized-dark t)

;; Load Powerline
(powerline-default-theme)

;; Set registers to common files
(set-register ?e (cons 'file "~/.emacs.d/init.el"))
(set-register ?r (cons 'file "~/Dropbox/Simplenote/ws_tasklist.txt"))

;; Clean up windows
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Default Font
(set-default-font "Anonymous Pro 12")

;; Set Git-hub Flavored Markdown
(setq markdown-command "docter")

;; Setup modes
;; Set Markdown mode for text and md files
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; Podfile settings
(add-to-list 'auto-mode-alist '("\\.podspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Podfile\\'" . ruby-mode))

;; MATLAB files
;; Instead, just use OCTAVE mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Deft
(setq deft-directory "~/Dropbox/Simplenote")
(setq deft-text-mode 'gfm-mode)
(global-set-key (kbd "C-x C-j") 'deft) ; Ctrl+X,Ctrl+J
(global-set-key (kbd "<f8>") 'deft) ; Ctrl+X,Ctrl+J

;; Set all auto-saves to happen in central folder
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/.autosaves"))    ; don't litter my fs tree
   delete-old-versions t
   Kept-New-Versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Setup Mouse
;; one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 4))) 
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil) 
;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't) 
