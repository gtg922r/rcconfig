;    ___  _____  _____          ____     
;;   / _ \/ ___/ / ___/__  ___  / _(_)__ _
;;  / , _/ /__  / /__/ _ \/ _ \/ _/ / _ `/
;; /_/|_|\___/  \___/\___/_//_/_//_/\_, / 
;;                                 /___/ 



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; Get rid of annoying start-up screen
(setq inhibit-startup-screen t)

;; Set registers to common files (do it now, before things that may fail)
(set-register ?e (cons 'file "~/.emacs.d/init.el"))
(set-register ?c (cons 'file "~/.emacs.d/Cask"))

;; Load Theme
(load-theme 'solarized-dark t)
(setq solarized-high-contrast-mode-line t)

;; Load Powerline
(powerline-default-theme)

;; Set Environment Path
(setenv "PATH" (concat (getenv "PATH") ":~/homebrew/bin:/usr/local/bin:/usr/texbin:/opt/local/bin"))
(setq exec-path (append exec-path '("~/homebrew/bin" "/usr/local/bin" "/usr/texbin" "/opt/local/bin")))

;; Clean up windows
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Its line numbers all the way down
(global-linum-mode t)

;; Python Setup
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")

;; Default Font
(cond
 ((member "Ubuntu Mono" (font-family-list))
  (set-default-font "Ubuntu Mono 10")
  (add-to-list 'default-frame-alist '(font . "Ubuntu Mono 10"  ))
  )
 ((member "Anonymous Pro" (font-family-list))
  (set-default-font "Anonymous Pro 10")
  (add-to-list 'default-frame-alist '(font . "Anonymous Pro 11"  ))
  )
 ((member "Monaco" (font-family-list))
  (set-default-font "Monaco 10")
  (add-to-list 'default-frame-alist '(font . "Monaco 11"  ))
  )
 )

;; Set Git-hub Flavored Markdown
(setq markdown-command "pandoc -f markdown_github -t html5 --mathjax -H ~/.emacs.d/markdown/style_include.css")

;; Setup modes
;; Set Markdown mode for text, md files, and git merge message
(autoload 'gfm-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'"     . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'"      . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'"       . gfm-mode))

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . gfm-mode))
(add-to-list 'auto-mode-alist '("NOTES_EDITMSG"  . gfm-mode))
(add-to-list 'auto-mode-alist '("MERGE_MSG"      . gfm-mode))
(add-to-list 'auto-mode-alist '("TAG_EDITMSG"    . gfm-mode))

;; Markdown References go at the end
(setq markdown-reference-location 'end)

;; Bind C-c k in gfm and markdown to start kokoi for automatic pandoc conversion and preview
(defun rc-start-kokoi-in-notes ()
  (interactive)
  (async-shell-command (concat "kokoi --command \"pandoc -f markdown_github -t html5 --mathjax -H ~/.emacs.d/markdown/style_include.css\" --save --extensions \"md\"" deft-directory)))

(defun rc-unison-sync-notes ()
  (interactive)
  (if (string-equal system-type "gnu/linux") ; If on linux, dont run unison
      (message "Currently on Linux, Unison configured for remote Mac")
      (progn
        (message "Launching Unison...")
        (shell-command "~/homebrew/bin/unison Notes"))
      )
  )

;; In Markdown/Likely Deft documents allow keyshorcuts to start kokoi or launch unison
;;   since we will be running unison, auto-revert these modes (they are auto-save so should be ok)

(add-hook 'gfm-mode-hook      (lambda()(local-set-key (kbd "C-c k") 'rc-start-kokoi-in-notes)))
(add-hook 'markdown-mode-hook (lambda()(local-set-key (kbd "C-c k") 'rc-start-kokoi-in-notes)))

(add-hook 'gfm-mode-hook      (lambda()(local-set-key (kbd "C-c u") 'rc-unison-sync-notes)))
(add-hook 'markdown-mode-hook (lambda()(local-set-key (kbd "C-c u") 'rc-unison-sync-notes)))

(add-hook 'markdown-mode-hook 'auto-revert-mode)
(add-hook 'gfm-mode-hook 'auto-revert-mode)

;; Enable Flyspell in gfm and markdown
(add-hook 'gfm-mode-hook      'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Turn on iimage in markdown-mode
(add-hook 'markdown-mode-hook 'turn-on-iimage-mode)
(add-hook 'gfm-mode-hook      'turn-on-iimage-mode)

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
  (setq local-screenshot-directory-relative
   (concat "./images/" (file-name-base(buffer-file-name))))
  (setq local-screenshot-url (concat local-screenshot-directory-relative "/" image-name ".png"))
  (rc-countdown-with-alert 3)
  (message "Select Screen Area")
  (rc-shell-notify "" "Select Screen Area for Capture" "" "Purr")
  (shell-command (concat
   "screencapture -i \""
   (concat local-screenshot-directory "/" image-name ".png") "\""))
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
  (shell-command-to-string "terminal-notifier -remove ALL -sender org.gnu.Emacs"))

;; Setup Dos-mode for batch file
(add-to-list 'auto-mode-alist '("\\.bat\\'" . dos-mode))

;; Podfile settings
(add-to-list 'auto-mode-alist '("\\.podspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Podfile\\'"  . ruby-mode))

;; Modes for rainbow mode
;; Difficulty downloading from elpa
;; (defun rainbow-activate ()
;;   (rainbow-mode 1))
;; (add-hook 'emacs-lisp-mode-hook 'rainbow-activate)
;; (add-hook 'css-mode-hook 'rainbow-activate)
;; (add-hook 'html-mode-hook 'rainbow-activate)


;; MATLAB files
;; Instead, just use OCTAVE mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Kill current buffer... because isnt that what you want to do anyway?
(global-set-key (kbd "C-x C-k") (lambda()(interactive)(kill-buffer nil)))

;; Move around buffers all easy like
;; Ace Window
(global-set-key (kbd "C-x o") 'ace-window)
;; Wind Move
(windmove-default-keybindings)
(global-set-key (kbd "M-L") `windmove-right)
(global-set-key (kbd "M-J") `windmove-left)
(global-set-key (kbd "M-I") `windmove-up)
(global-set-key (kbd "M-K") `windmove-down)

;; Insert Date Macro

(defun rc-date-string (&optional prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%b %d, %Y")
                 ((equal prefix '(16)) "%A, %d. %B %Y"))))
    (format-time-string format)))

(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (insert (rc-date-string prefix)))

(global-set-key (kbd "C-c d") 'insert-date)

;; Deft
(setq deft-directory "~/Documents/Notes")
(setq deft-text-mode 'gfm-mode)
(setq deft-extension "md")
(global-set-key (kbd "C-x C-j") 'deft) ; Ctrl+X,Ctrl+J
(global-set-key (kbd "<f8>") 'deft) ; Ctrl+X,Ctrl+J
(setq deft-use-filter-string-for-filename t)

(defun rc-prepend-date (string-in)
  (concat (rc-date-string ()) " " slug))

(setq deft-file-naming-rules '((case-fn . rc-prepend-date)))


(defun rc-grep-todos-in-dir ()
  "Grep recursively for TODO comments in the given directory"
  (interactive)
  (grep (concat "grep -EIr \"TODO\" " deft-directory  " 2>/dev/null"))
  (enlarge-window 7))

(global-set-key [f5] 'rc-grep-todos-in-dir)

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
    ("TODO" . "#dc322f")
    ("NEXT" . "#dca3a3")
    ("SQWK" . "#dc8cc3")
    ("ACTN" . "#dc8cc3")    
    ("PROG" . "#bf9f40")
    ("WIP" . "#bf9f40")
    ("OKAY" . "#7cb8bb")
    ("ELSE" . "#5f7f5f")
    ("WONT" . "#2a7f5f")
    ("FAIL" . "#8c5353")
    ("DONE" . "#2aa198")
    ("AAA"  . "#2aa198")
    ("FIXME" . "#cc9393")
    ("XXX"   . "#cc9393")
    ("NOTE"  . "#cc9393")
    ("QQQ"   . "#cc9393"))
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

;; Modes for hl-todo mode
(add-hook 'emacs-lisp-mode-hook 'hl-todo-mode)
(add-hook 'gfm-mode-hook 'hl-todo-mode)
(add-hook 'markdown-mode-hook 'hl-todo-mode)
(add-hook 'grep-mode-hook 'hl-todo-mode)

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

;; Set Default Frame Size
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)

;; TODO/QQQ Inserting and Cycling

(defun rc-action-insert (&optional insert-str)
  (interactive "*sAction POC:")
  (concat "@" insert-str))

(defun rc-value-toggle-insert (val1 val2 &optional add-poc)
  "Cycle between two values in the current line. If neither exists, insert [val1] after white space and bullets. Optionally specifies a string to follow @, e.g. [ACTN@bob]"
  (move-beginning-of-line nil)
  (if (re-search-forward (concat val1 "\\|" val2) (line-end-position) t)
      (replace-match (if (equal (match-string 0) val1) val2 val1))
    (skip-chars-forward "-*\s\t")
    (insert (concat "[" val1 (if add-poc (call-interactively `rc-action-insert) "")  "] "))))

(defun rc-cycle-todo ()
  "Cycle between TODO and DONE. If neither are present insert [TODO] after whitespace and bullets"
  (interactive)
  (rc-value-toggle-insert "TODO" "DONE"))

(defun rc-cycle-else ()
  "Cycle between TODO and ELSE. If neither are present insert [ELSE] after whitespace and bullets"
  (interactive)
  (rc-value-toggle-insert "ELSE" "TODO"))

(defun rc-cycle-hold ()
  "Cycle between TODO and HOLD. If neither are present insert [HOLD] after whitespace and bullets"
  (interactive)
  (rc-value-toggle-insert "HOLD" "TODO"))

(defun rc-cycle-wont ()
  "Cycle between TODO and WONT. If neither are present insert [WONT] after whitespace and bullets"
  (interactive)
  (rc-value-toggle-insert "WONT" "TODO"))


(defun rc-cycle-question ()
  "Cycle between QQQ and AAA. If neither are present insert [QQQ] after whitespace and bullets"
  (interactive)
  (rc-value-toggle-insert "QQQ" "AAA"))

(defun rc-cycle-action ()
  (interactive)
  (rc-value-toggle-insert "ACTN" "DONE" t))


(defun rc-get-remaining-line-text ()
  (interactive)
  (buffer-substring-no-properties (point) (line-end-position)))

(defun rc-new-things-todo (new-todo-str)
  "Create a new ToDo in Things3, return the Things to-do id"
  (interactive)
  (do-applescript
   (format "
     tell application \"Things3\"
      set newToDo to make new to do with properties {name:\"%s\"} at beginning of list \"Inbox\"
      get id of newToDo
     end tell" new-todo-str)))

(defun rc-insert-new-things-todo ()
  "Creates a new thing todo or switches TODO to thing todo"
  (interactive)  
  (move-beginning-of-line nil)
  (re-search-forward "[-\s]*\\[[^\\[]*\\]" (line-end-position) t)
  (skip-chars-forward "-*\s\t")
  (let ((newToDoId (rc-new-things-todo (rc-get-remaining-line-text))))
    (rc-value-toggle-insert (format "THNGS<%s>" newToDoId) "TODO")))

(global-set-key (kbd "C-c n") 'rc-insert-new-things-todo)

(defun rc-new-things-quickentry (new-todo-str)
  "Bring up Things3 Quick Entry Panel with name pre-populated"
  (interactive)
  (do-applescript
   (format "
     tell application \"Things3\"
      show quick entry panel with properties {name:\"%s\"}
     end tell" new-todo-str)))

(defun rc-insert-new-things-with-quickentry ()
  "Creates a new thing todo or switches TODO to thing todo by openning quick entry panel"
  (interactive)  
  (move-beginning-of-line nil)
  (re-search-forward "[-\s]*\\[[^\\[]*\\]" (line-end-position) t)
  (skip-chars-forward "-*\s\t")
  (rc-new-things-quickentry (rc-get-remaining-line-text))
  (rc-value-toggle-insert "THNGS<QUICKENTRY>" "TODO"))
; TODO: If already exists on the line, then "tell application "Things3" to edit to do id "XXXX-XXXX" (unless its quick edit...
(global-set-key (kbd "C-c N") 'rc-insert-new-things-with-quickentry)

(defface rc-things-face
  '((t (:bold t :foreground "#7cb8bb")))
  "Face used to highlight THNGS keywords.")
 
(defface rc-things-id-face
  '((t (:bold t :foreground "base01")))
  "Face used to highlight THNGS keywords.")

(defun rc-things-font-lock ()
  "Activates Things font-lock"
  (font-lock-add-keywords nil
    '(("\\[\\(THNGS\\)\\(<.+>\\)\\]"
      (1 'rc-things-face prepend)
      (2 (progn (add-text-properties (match-beginning 2) 
				     (match-end 2) 
				     '(invisible dots))) prepend))))
  (add-to-invisibility-spec '(dots . t)))

(add-hook 'emacs-lisp-mode-hook 'rc-things-font-lock)
(add-hook 'gfm-mode-hook 'rc-things-font-lock)


(global-set-key (kbd "C-c t") 'rc-cycle-todo)
(global-set-key (kbd "C-c q") 'rc-cycle-question)
(global-set-key (kbd "C-c a") 'rc-cycle-action)
(global-set-key (kbd "C-c e") 'rc-cycle-else) 
(global-set-key (kbd "C-c h") 'rc-cycle-hold)
(global-set-key (kbd "C-c w") 'rc-cycle-wont)
