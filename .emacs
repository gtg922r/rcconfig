(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-continuous t)
 '(doc-view-ghostscript-options (quote ("-dNOSAFER" "-dNOPAUSE" "-sDEVICE=png16m" "-dTextAlphaBits=4" "-dBATCH" "-dGraphicsAlphaBits=4" "-dQUIET")))
 '(doc-view-ghostscript-program "/usr/local/bin/gs")
 '(inhibit-startup-screen t)
 '(preview-auto-cache-preamble t)
 '(preview-gs-command "/usr/local/bin/gs")
 '(preview-gs-options (quote ("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Select Color Theme
(add-to-list 'load-path "~/.emacs.d/colors/")
(require 'color-theme)
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized/")
(require 'color-theme-solarized)


;; Set Environment Path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/texbin:/opt/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin:/usr/texbin:/opt/local/bin")))

;; Set registers to common files
(set-register ?e (cons 'file "~/.emacs"))
(set-register ?r (cons 'file "~/Dropbox/Simplenote/ws_tasklist.txt"))
(color-theme-solarized-dark)


 ;; Set Visual Line Mode for text files
(add-hook `text-mode-hook 'turn-on-visual-line-mode)
(defun turn-on-visual-line-mode-in-txt ()
  (when (and (buffer-file-name)
             (string-match ".txt$" (buffer-file-name)))
    (turn-on-visual-line-mode)))

;; Enable Markdown
(add-to-list 'load-path "~/.emacs.d/markdown/")
(require 'markdown-mode)

;; Set Markdown mode for text and md files
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; Auctex Settings
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)

;; Deft
(add-to-list 'load-path "~/.emacs.d/deft/")
(require 'deft)
(setq deft-directory "~/Dropbox/Simplenote")
(setq deft-text-mode 'gfm-mode)
(global-set-key (kbd "C-x C-j") 'deft) ; Ctrl+X,Ctrl+J
(global-set-key (kbd "<f8>") 'deft) ; Ctrl+X,Ctrl+J

;; File Shortcuts
(global-set-key (kbd "<f8>") (lambda() (interactive)(find-file "~/Dropbox/WSWork_control/branches/ep_1x2/vdcs/")))
(global-set-key (kbd "<f9>") (lambda() (interactive)(find-file "~/Dropbox/Simplenote/ws_tasklist.txt")))
(global-set-key (kbd "<f11>") (lambda() (interactive)(find-file "~/Dropbox/Simplenote/ws_scripts.txt")))
(global-set-key (kbd "<f12>") (lambda() (interactive)(find-file "~/Dropbox/Simplenote/ws_worklog.txt")))

;; Insert Data Macro
(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y.%m.%d: ")
                   ((equal prefix '(4)) "%m/%d/%YY")
                   ((equal prefix '(16)) "%A, %d. %B %Y")))
          )
      (insert (format-time-string format))))

(global-set-key (kbd "C-c d") 'insert-date)

;; Set Git-hub Flavored Markdown
(setq markdown-command "docter")

;; Set Chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; Set all auto-saves to happen in central folder
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/.autosaves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups



;; Replace path below to be where your matlab.el file is.
;;  (add-to-list 'load-path "~/.emacs.d/matlab-emacs/")
;;  (require 'matlab-load)
;; Enable CEDET feature support for MATLAB code. (Optional)
;; (matlab-cedet-setup)

;; Instead, just use OCTAVE mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))


;; one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 4))) 
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil) 
;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't) 

;;; No tool bar
(tool-bar-mode -1)
;;(menu-bar-mode -1)

;; Use CEDET
;(global-ede-mode 1)                      ; Enable the Project management system
;(semantic-mode 1)
