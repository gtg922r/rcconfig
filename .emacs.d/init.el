(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Load Theme
(load-theme 'solarized-dark t)

;; Load Powerline
(powerline-default-theme)

;; Set Environment Path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/texbin:/opt/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin" "/usr/texbin" "/opt/local/bin")))

;; Set registers to common files
(set-register ?t (cons 'file "~/Dropbox/RPC_Work_Documents/WS_Notes/tasklist.org"))
(set-register ?s (cons 'file "~/Dropbox/RPC_Work_Documents/WS_Notes/scratchpad.md"))
(set-register ?e (cons 'file "~/.emacs.d/init.el"))
(set-register ?c (cons 'file "~/.emacs.d/cask"))
;;(set-register ?r (cons 'file "~/Dropbox/Simplenote/ws_tasklist.txt"))

;; Clean up windows
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Default Font
(set-default-font "Anonymous Pro 11")

;; Set Git-hub Flavored Markdown
(setq markdown-command "pandoc -f markdown_github -t html5 --mathjax -H ~/.emacs.d/markdown/style_include.css")

;; Setup modes
;; Set Markdown mode for text, md files, and git merge message
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . gfm-mode))
(add-to-list 'auto-mode-alist '("NOTES_EDITMSG" . gfm-mode))
(add-to-list 'auto-mode-alist '("MERGE_MSG" . gfm-mode))
(add-to-list 'auto-mode-alist '("TAG_EDITMSG" . gfm-mode))

;; Markdown References go at the end
(setq markdown-reference-location 'end)

;; Bind C-c k in gfm and markdown to start kokoi for automatic pandoc conversion and preview
(defun rc-start-kokoi-in-WS-notes ()
  (interactive)
  (async-shell-command "kokoi --command \"pandoc -f markdown_github -t html5 --mathjax -H ~/.emacs.d/markdown/style_include.css\" --save --mathjax --extensions \"md\" ~/Dropbox/RPC_Work_Documents/WS_Notes/"))
  
(add-hook 'gfm-mode-hook      (lambda()(local-set-key (kbd "C-c k") 'rc-start-kokoi-in-WS-notes)))
(add-hook 'markdown-mode-hook (lambda()(local-set-key (kbd "C-c k") 'rc-start-kokoi-in-WS-notes)))

;; Enable Flyspell in gfm and markdown
(add-hook 'gfm-mode-hook      'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Turn on iimage in markdown-mode
(add-hook 'markdown-mode-hook 'turn-on-iimage-mode)
(add-hook 'gfm-mode-hook 'turn-on-iimage-mode)

;; Toggle iimage with C-c i
(add-hook 'gfm-mode-hook      (lambda()(local-set-key (kbd "C-c i") 'iimage-mode)))

;; Allow images with spaces in the name, since we are going to be looing inside markdown syntax (i.e. ![]())
(setq iimage-mode-image-filename-regex "[-+./_0-9a-zA-Z ]+\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|N[GM]\\|PM\\)\\|SVG\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|n[gm]\\|pm\\)\\|svg\\|tiff?\\|x\\(?:[bp]m\\)\\)")

;; Only display images for markdown-formated image links
;; i.e. ignore image file names scattered around.
(setq iimage-mode-image-regex-alist
  `((,(concat "!\\[.*?\\](\\(" iimage-mode-image-filename-regex "\\))") . 1)))


;; Take screenshots on Mac OSX and insert inline or reference markdown link
(defun rc-screenshot-md-insert (&optional reference image-name)
  (interactive "*P\nsImage Name: ")
  (setq 
   local-screenshot-directory
   (concat
    (file-name-directory(buffer-file-name))
    "images/"
    (file-name-base(buffer-file-name))))
  (unless(file-exists-p local-screenshot-directory)
    (message "Creating image directoy")
    (make-directory local-screenshot-directory))
  (setq 
   local-screenshot-directory-relative
   (concat
    "./images/"
    (file-name-base(buffer-file-name))))
  (setq local-screenshot-url (concat local-screenshot-directory-relative "/" image-name ".png"))
  (rc-countdown-with-alert 3)
  (message "Select Screen Area")
  (rc-shell-notify
   ""
   "Select Screen Area for Capture"
   ""
   "Purr") 
  (shell-command (concat
   "screencapture -i \""
   (concat 
    local-screenshot-directory
    "/"
    image-name
    ".png"
    )
   "\""
   ))
  (rc-shell-hide-notifications)
  (if reference
      (progn (insert "!")
       (markdown-insert-reference-link image-name image-name local-screenshot-url))
    (insert
     (concat "![" image-name "](" local-screenshot-url ")\n")))
  (if iimage-mode (iimage-recenter)))

(defun rc-screenshot-md-insert-reference (image-name)
  (interactive "sImage Name:")
  (rc-screenshot-md-insert t image-name))

;; Modify keymaps to `markdown' and `gfm' to add screenshot and inline link insert.
(add-hook 'gfm-mode-hook (lambda()(local-set-key (kbd "C-c s") 'rc-screenshot-md-insert)))
(add-hook 'markdown-mode-hook (lambda()(local-set-key (kbd "C-c s") 'rc-screenshot-md-insert)))

;; Modify keymaps to `markdown' and `gfm' to add screenshot and reference link insert.
(add-hook 'gfm-mode-hook (lambda()(local-set-key (kbd "C-c s") 'rc-screenshot-md-insert)))
(add-hook 'markdown-mode-hook (lambda()(local-set-key (kbd "C-c s") 'rc-screenshot-md-insert)))

;; Countdown with system-wide notifications
(defun rc-countdown-with-alert (length-seconds)
  "Start a countdown with alerts every second."
  (interactive "nCountdown length:\n")
  (let ((count length-seconds))
    (while (> count 0)
      (message (concat "Starting Screen Capture in " (number-to-string count)))
      (rc-shell-notify
       "Move windows as necessary"
       (concat "Capturing Screen in "(number-to-string count))
       ""
       "Submarine") 
      (setq count (1- count))
      (sleep-for 1))
    (rc-shell-hide-notifications)))

;; Notifications through notification center  
(defun rc-shell-notify (message title subtitle sound)
  (interactive "sMessage:\nsTitle:\nsSubtitle:\nsSound ID:\n")
  (shell-command-to-string
   (concat "terminal-notifier"
           " -sound \"" sound "\""
           " -title \"" title "\""
           " -subtitle \"" subtitle "\""
           " -message \"" message "\""
           " -sender org.gnu.Emacs"
           )))

(defun rc-shell-hide-notifications ()
  (interactive)
  (message "removing")
  (shell-command-to-string "terminal-notifier -remove ALL -sender org.gnu.Emacs")
)

;; Setup Dos-mode for batch file
(add-to-list 'auto-mode-alist '("\\.bat\\'" . dos-mode))

;; Podfile settings
(add-to-list 'auto-mode-alist '("\\.podspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Podfile\\'" . ruby-mode))

;; Modes for rainbow mode
(add-hook 'emacs-lisp-mode-hook 'my-emacs-mode-hook)
(defun my-emacs-mode-hook ()
  (rainbow-mode 1))

;; MATLAB files
;; Instead, just use OCTAVE mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Deft
(setq deft-directory "~/Dropbox/RPC_Work_Documents/WS_Notes")
(setq deft-text-mode 'gfm-mode)
(setq deft-extension "md")
(global-set-key (kbd "C-x C-j") 'deft) ; Ctrl+X,Ctrl+J
(global-set-key (kbd "<f8>") 'deft) ; Ctrl+X,Ctrl+J

(defun rc-grep-todos-in-dir (dir &optional not-recursive)
"Grep recursively for TODO comments in the given directory"
(interactive "Ddirectory:")
(let ((recur "-r"))
(if not-recursive
(setq recur "")
)
(grep (concat "grep -nH -I " recur " -E \"[\\#\\/\\-\\;\\*]\s*TODO|FIXME|XXX:?\" " dir " 2>/dev/null"))
)
(enlarge-window 7)
)
(global-set-key [f5] 'rc-grep-todos-in-dir)

;; Insert Data Macro
(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y-%m-%d")
                   ((equal prefix '(4)) "%b %d, %Y")
                   ((equal prefix '(16)) "%A, %d. %B %Y")))
          )
      (insert (format-time-string format))))

(global-set-key (kbd "C-c d") 'insert-date)


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


(defgroup hl-todo nil
  "Highlight TODO keywords in comments."
  :group 'convenience)

(defface hl-todo
  '((t (:bold t :foreground "#cc9393")))
  "Face used to highlight TODO keywords."
  :group 'hl-todo)

(defcustom hl-todo-activate-in-modes '(emacs-lisp-mode)
  "Major modes in which `hl-todo-mode' should be activated.
This is used by `global-hl-todo-mode'."
  :group 'orglink
  :type '(repeat function))

(defvar hl-todo-keywords nil)

(defcustom hl-todo-keyword-faces
  '(("HOLD" . "#d0bf8f")
    ("TODO" . "#ff0000")
    ("NEXT" . "#dca3a3")
    ("THEM" . "#dc8cc3")
    ("PROG" . "#7cb8bb")
    ("OKAY" . "#7cb8bb")
    ("DONT" . "#5f7f5f")
    ("FAIL" . "#8c5353")
    ("DONE" . "##2aa198")
    ("FIXME" . "#cc9393")
    ("XXX"   . "#cc9393")
    ("XXXX"  . "#cc9393")
    ("???"   . "#cc9393"))
  "Faces used to highlight specific TODO keywords."
  :group 'hl-todo
  :type '(repeat (cons (string :tag "Keyword")
                       (choice :tag "Face   "
                               (string :tag "Color")
                               (sexp :tag "Face"))))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq hl-todo-keywords
               `((,(concat "\\_<\\("
                           (mapconcat 'car value "\\|")
                           "\\)\\_>")
                  (1 (hl-todo-get-face) t))))))

(defun hl-todo-get-face ()
  (let ((f (cdr (assoc (match-string 1) hl-todo-keyword-faces))))
    (if (stringp f) (list :inherit 'hl-todo :foreground f) f)))

;;;###autoload
(define-minor-mode hl-todo-mode
  "Highlight TODO tags in comments."
  :lighter ""
  :group 'hl-todo
  (if hl-todo-mode
      (font-lock-add-keywords  nil hl-todo-keywords 'append)
    (font-lock-remove-keywords nil hl-todo-keywords))
  (font-lock-fontify-buffer))

;;;###autoload
(define-globalized-minor-mode global-hl-todo-mode
  hl-todo-mode turn-on-hl-todo-mode-if-desired)

(defun turn-on-hl-todo-mode-if-desired ()
  (when (apply 'derived-mode-p hl-todo-activate-in-modes)
    (hl-todo-mode 1)))

(provide 'hl-todo)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; hl-todo.el ends here

;; Modes for rainbow mode
(add-hook 'emacs-lisp-mode-hook 'hl-todo-mode)
(add-hook 'gfm-mode-hook 'hl-todo-mode)
(add-hook 'markdown-mode-hook 'hl-todo-mode)


;; Rename File and Buffer
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))
(global-set-key (kbd "C-c r")  'rename-file-and-buffer)

;; Tie Shortcut for full screen to Mac standard of Option-Esc
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
      (if (equal 'fullboth current-value)
        (if (boundp 'old-fullscreen) old-fullscreen nil)
        (progn (setq old-fullscreen current-value)
          'fullboth)))))
(global-set-key [s-escape] 'toggle-fullscreen)


;(custom-set-variables
; ;; custom-set-variables was added by Custom.
; ;; If you edit it by hand, you could mess it up, so be careful.
; ;; Your init file should contain only one such instance.
; ;; If there is more than one, they won't work right.
; '(fci-rule-color "#073642")
; '(ansi-color-names-vector ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
; '(compilation-message-face (quote default))
; '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
; '(desktop-save-mode nil)
; '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
; '(highlight-tail-colors (quote (("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50) ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85) ("#073642" . 100))))
; '(magit-diff-use-overlays nil)
; '(vc-annotate-background nil)
; '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#CF4F1F") (60 . "#C26C0F") (80 . "#b58900") (100 . "#AB8C00") (120 . "#A18F00") (140 . "#989200") (160 . "#8E9500") (180 . "#859900") (200 . "#729A1E") (220 . "#609C3C") (240 . "#4E9D5B") (260 . "#3C9F79") (280 . "#2aa198") (300 . "#299BA6") (320 . "#2896B5") (340 . "#2790C3") (360 . "#268bd2"))))
; '(vc-annotate-very-old-color nil)
; '(weechat-color-list (quote (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
;(custom-set-faces
; ;; custom-set-faces was added by Custom.
; ;; If you edit it by hand, you could mess it up, so be careful.
; ;; Your init file should contain only one such instance.
; ;; If there is more than one, they won't work right.
; )

