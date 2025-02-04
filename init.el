;;; init.el --- Modus Operandi Emacs Configuration -*- lexical-binding: t -*-

;; Author: Yoav Orot
;; Created: 2021
;; Homepage: https://github.com/manzaltu/modus-emacs

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Modus Operandi Emacs development environment configuration file

;;; Code:

(require 'cl-lib)

;; Set a directory path to be used for cache files
(defvar mo-cache-dir (expand-file-name ".cache" user-emacs-directory))

(defun mo-cache-path (filename)
  "Return a valid file path for FILENAME under the cache directory."
  (concat (file-name-as-directory mo-cache-dir) filename))

(defvar straight-base-dir mo-cache-dir)
(defvar straight-repository-branch "develop")

;; Init straight.el for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name ".cache/straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Show calls to use-package in imenu
(customize-set-variable 'use-package-enable-imenu-support t)

;; Enable package hooks
(defvar use-package-inject-hooks t)

;; Init straight for package management
(use-package straight
  :custom
  ;; Packages should be installed by default using straight
  ( straight-use-package-by-default t))

;; Optionally, load personal settings
(load (concat (file-name-directory load-file-name) "personal.el") t)

(defvar mo-font "PragmataPro Mono Liga-14"
  "Font to use.")

;; Add general.el key mapper
(use-package general
  :demand t
  :defines
  ( mo-quick-menu-definer
    mo--quick-menu-definer-evil
    mo--quick-menu-definer-non-evil)
  :functions ( general-create-definer mo-quick-menu-definer)
  :config
  ;; Create a definer and a leader for quick menu when in evil-mode
  (general-create-definer mo--quick-menu-definer-evil
    :states '( normal visual motion emacs)
    :prefix ",")
  ;; Create a definer and a leader for quick menu when not in evil-mode
  (general-create-definer mo--quick-menu-definer-non-evil
    :keymaps 'override
    :prefix "M-<insert>")

  (defmacro mo-quick-menu-definer (&rest args)
    "Define bindings for both the evil and the non-evil leaders."
    (declare (indent defun))
    `(progn
       (mo--quick-menu-definer-evil
         ,@args)
       (mo--quick-menu-definer-non-evil
         ,@args)))

  (mo-quick-menu-definer
    :prefix-map 'mo-quick-menu-map
    :which-key "Quick menu prefix key"
    "a" '( :which-key "Action")
    "b" '( :which-key "Buffer")
    "<insert>" '( :which-key Bookmark)
    "f" '( :which-key "File")
    "v" '( :which-key "View")
    "w" '( :which-key "Window")
    "x" '( :which-key "Utils")
    "z" '( :which-key "Repeat")
    "t" '( :which-key "Tab")
    "h" '( :which-key "Help")
    "DEL" '( :which-key "Project")
    "c" '( :which-key "Code")
    "d" '( :which-key "Debug")
    "l" '( :which-key "Emacs Lisp")
    "s" '( :which-key "Lisp")
    "g" '( :which-key "Git")
    "m" '( :which-key "Merge")
    "r" '( :which-key "Multiple Cursors")
    "n" '( :which-key "Notes")))

;; Init evil mode for Vim emulation in Emacs
(use-package evil
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "w"
    "H" #'evil-window-move-far-left
    "J" #'evil-window-move-very-bottom
    "K" #'evil-window-move-very-top
    "L" #'evil-window-move-far-right
    "v" #'evil-window-vsplit
    "V" #'mo-evil-vsplit-left
    "s" #'evil-window-split
    "S" #'mo-evil-split-above
    "c" #'evil-window-delete
    "+" #'evil-window-increase-height
    "-" #'evil-window-decrease-height
    ">" #'evil-window-increase-width
    "<" #'evil-window-decrease-width)
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "ESC" #'evil-ex-nohighlight)
  ;; We want C-f for moving forward a word
  ( :keymaps 'evil-ex-completion-map
    "C-f" nil
    "C-b" nil
    "C-a" nil
    "C-." #'evil-ex-command-window)
  ( :keymaps 'evil-command-line-map
    "C-f" nil
    "C-b" nil
    "C-a" nil
    "M-r" #'evil-paste-from-register)
  ( :states 'motion
    ;; We want C-<num> for jumping between tabs
    "C-6" nil
    "C-S-d" #'evil-scroll-up
    "C-}" #'mo-evil-forward-paragraph-recenter
    "C-{" #'mo-evil-backward-paragraph-recenter)
  ( :states 'insert
    ;; Evil, for historical reasons, binds the <delete> key to delete-char.
    ;; Today this is unnecessary, and may override other modes keybindings.
    "<delete>" nil)
  ( :keymaps 'global
    "C-M-o" #'evil-window-mru)
  :hook
  ;; Recenter after jump
  ( evil-jumps-post-jump . recenter)
  :init
  ;; Needed for evil-collection
  (setq evil-want-keybinding nil)
  ;; Enable in minibuffer
  (setq evil-want-minibuffer t)
  ;; Undo
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-fine-undo t)
  ;; Enable Emacs native bindings in insert mode
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-C-u-delete nil)
  (setq evil-want-C-w-delete nil)
  ;; Yanking
  (setq evil-want-Y-yank-to-eol t)
  ;; Use evil search instead of the native search module
  (setq evil-search-module 'evil-search)
  ;; Set word search to look for symbol boundaries
  (setq evil-symbol-word-search t)
  ;; Do not keep highlighting after search
  (setq evil-ex-search-persistent-highlight nil)
  ;; Set end of line selection to not include the newline character
  (setq evil-want-visual-char-semi-exclusive t)
  ;; Don't kill text when pasting over it
  (setq-default evil-kill-on-visual-paste nil)
  ;; Respect visual line mode
  (setq evil-respect-visual-line-mode t)
  ;; Create split windows below
  (setq evil-split-window-below t)
  ;; Create vertical split windows to the right
  (setq evil-vsplit-window-right t)
  ;; Cursor can move beyond eol to support Emacs sexp movement
  (setq evil-move-beyond-eol t)
  :config
  (defun mo-evil-split-above ()
    "Split and create a new window above."
    (interactive)
    (let ((evil-split-window-below nil))
      (call-interactively #'evil-window-split)))

  (defun mo-evil-vsplit-left ()
    "Vertically split and create a new window to the left."
    (interactive)
    (let ((evil-vsplit-window-right nil))
      (call-interactively #'evil-window-vsplit)))

  (defun mo-evil-correct-last-sexp (command &rest args)
    "In normal-state or motion-state, last sexp ends at point."
    ;; A false evil-move-beyond-eol is covered by evil-collection
    (if (and evil-move-beyond-eol
             (or (evil-normal-state-p) (evil-motion-state-p)))
        (save-excursion
          (unless (or (eobp) (eolp)) (forward-char))
          (apply command args))
      (apply command args)))

  (defun mo-evil-forward-paragraph-recenter ()
    "Move to the end of the next paragraph and recenter.
Briefly highlight previous location."
    (interactive)
    (pulse-momentary-highlight-one-line)
    (call-interactively #'evil-forward-paragraph)
    (recenter))

  (defun mo-evil-backward-paragraph-recenter ()
    "Move to the beginning of the previous paragraph and recenter.
Briefly highlight previous location."
    (interactive)
    (pulse-momentary-highlight-one-line)
    (call-interactively #'evil-backward-paragraph)
    (recenter))

  ;; Set word movement to operate on symbol boundaries
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; Start with Emacs mode in rustic-popup-mode buffers
  (evil-set-initial-state 'rustic-popup-mode 'emacs)
  (evil-mode 1))

;; Init Emacs core settings
(use-package emacs
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "n w" #'widen
    "n f" #'narrow-to-defun
    "n r" #'narrow-to-region)
  ( :keymaps 'mo-quick-menu-map
    :prefix "a"
    "k" #'kill-process
    "s" #'modify-syntax-entry
    "r" #'abort-recursive-edit)
  ( :keymaps 'mo-quick-menu-map
    :prefix "l"
    "b" #'mo-toggle-lexical-binding)
  ( :keymaps 'emacs-lisp-mode-map
    "C-M-s-r" #'eval-region)
  :config
  (defun mo-toggle-lexical-binding ()
    "Toggle lexical binding in the current buffer."
    (interactive)
    (message "Lexical binding is %s."
             (if (setq lexical-binding (not lexical-binding)) "on" "off")))
  ;; Inhibit the splash screen
  (setq inhibit-splash-screen t)
  ;; Set the default initial frame size
  (add-to-list 'default-frame-alist '( height . 55))
  (add-to-list 'default-frame-alist '( width . 210))
  ;; Don't display startup screen
  (setq inhibit-startup-screen t)
  ;; Scroll incrementally
  (setq scroll-step 1)
  ;; Don't automatically recenter after scrolling
  (setq scroll-conservatively 101)
  ;; Don't create lock files
  (setq create-lockfiles nil)
  ;; Configure auto-save-list
  (setq auto-save-list-file-prefix (mo-cache-path "auto-save-list/.saves-"))
  ;; Disable bell audio
  (setq ring-bell-function 'ignore)
  ;; Use short answers e.g. y or n
  (setq use-short-answers t)
  ;; Truncate lines by default
  (setq truncate-lines t)
  ;; Disable double space at sentence end
  (setq sentence-end-double-space nil)
  ;; Draw the underline at the descent line
  (setq x-underline-at-descent-line t)
  ;; Enable recursive minibuffer
  (setq enable-recursive-minibuffers t)
  ;; Set tab width
  (setq-default tab-width 4)
  ;; Enable indentation and completion using the TAB key
  (setq tab-always-indent 'complete)
  ;; Do not ignore extensions
  (setq completion-ignored-extensions nil)
  ;; Increase saved history size
  (setq history-length 1000)
  ;; Cap command history
  (put 'command-history 'history-length 100)
  ;; Increase undo limits
  (setq undo-limit 33554432)
  (setq undo-strong-limit 50331648)
  (setq undo-outer-limit 134217728)
  ;; Bind super keys in Windows
  (setq w32-lwindow-modifier 'super)
  (setq w32-rwindow-modifier 'super)
  ;; Enable scrolling left
  (put 'scroll-left 'disabled nil)
  ;; Remove vc info from modeline
  (setq-default mode-line-format (remove '(vc-mode vc-mode) (default-value 'mode-line-format))))

;; Init files for file related functionality
(use-package files
  :demand t
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    ;; Quick save key binding
    "SPC" #'save-buffer)
  ( :keymaps 'mo-quick-menu-map
    "_" #'mo-open-init-file)
  ( :keymaps 'mo-quick-menu-map
    :prefix "f"
    "f" #'find-file
    "F" #'find-file-literally
    "w" #'write-file
    "r" #'recover-this-file)
  ( :keymaps 'mo-quick-menu-map
    :prefix "l"
    "l" #'load-file)
  ( :keymaps 'emacs-lisp-mode-map
    "C-M-s-b" #'eval-buffer)
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "r" #'revert-buffer-quick
    "R" #'mo-reload-dir-locals-current-buffer)
  ( :keymaps 'mo-quick-menu-map
    "q" #'save-buffers-kill-terminal)
  :config
  (defun mo-open-init-file ()
    "Open the user's init file."
    (interactive)
    (find-file user-init-file))
  (defun mo-reload-dir-locals-current-buffer ()
    "Reload dir-locals for the current buffer."
    (interactive)
    (hack-dir-local-variables-non-file-buffer))
  ;; Do not prevent remembering "risky" local variables
  (advice-add 'risky-local-variable-p :override #'ignore)
  ;; Configure backup and auto-saves
  (setq backup-directory-alist `( ( "." . ,(mo-cache-path "backups"))))
  (setq backup-by-copying t)
  (setq delete-old-versions t)
  (setq version-control t)
  (setq kept-new-versions 6)
  (setq kept-old-versions 2)
  (setq auto-save-file-name-transforms `( ( ".*" ,(mo-cache-path "backups/") t)))
  (setq auto-save-include-big-deletions t)
  (setq auto-save-no-message t)
  ;; Ask for confirmation before exiting emacs
  (setq confirm-kill-emacs #'y-or-n-p))

;; Init frame for managing frames
(use-package frame
  :demand t
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "v"
    "f" #'make-frame-command
    "c" #'delete-frame)
  :config
  ;; Disable cursor blink
  (blink-cursor-mode 0)
  ;; Set font
  (set-frame-font mo-font nil t))

;; Init window for managing windows
(use-package window
  :demand t
  :straight nil
  :general
  ( :keymaps 'override
    "C-S-o" #'other-window-prefix
    "C-M->" #'mo-current-window-prefix)
  ( :keymaps 'mo-quick-menu-map
    :prefix "w"
    "=" #'balance-windows
    "C" #'delete-other-windows
    "q" #'quit-window
    "x" #'mo-quit-other-window)
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "q" #'mo-quit-window-kill-buffer
    "[" #'previous-buffer
    "]" #'next-buffer)
  :config
  (defun mo-quit-other-window ()
    "Quit the other window."
    (interactive)
    (quit-window nil (previous-window)))
  (defun mo-quit-window-kill-buffer ()
    "Quit and kill window."
    (interactive)
    (quit-window t))
  (defun mo-current-window-prefix ()
    "Display the buffer of the next command in the current window."
    (interactive)
    (let ((window (selected-window)))
      (display-buffer-override-next-command
       (lambda (buffer alist)
         (cons window 'reuse))
       nil "[current-window]")
      (message "Display next command buffer in the current window...")))
  ;; Don't split windows vertically by default
  (setq split-height-threshold nil)
  ;; A fast key binding for showing the next command's result in another window.
  ;; Make sure it also works when the command is using 'switch-to-buffer'.
  (setq switch-to-buffer-obey-display-actions t))

;; Init windmove for directional window selection
(use-package windmove
  :straight nil
  :general
  ( :keymaps 'override
    "M-<right>" #'windmove-right
    "M-<left>" #'windmove-left
    "M-<up>" #'windmove-up
    "M-<down>" #'windmove-down))

;; Init simple for basic and general Emacs commands
(use-package simple
  :demand t
  :straight nil
  :general
  ( :keymaps 'override
    "C-<f10>" #'mo-toggle-scratch-buffer)
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "t" #'toggle-truncate-lines
    "v" #'visual-line-mode
    "f" #'auto-fill-mode
    "F" #'set-fill-column
    "I" #'clone-indirect-buffer
    "k" #'kill-current-buffer)
  ( :keymaps 'mo-quick-menu-map
    ;; Universal argument key binding
    "u" #'universal-argument)
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    ;; List all sub processes
    "P" #'list-processes
    "x" #'async-shell-command)
  ( :keymaps 'mo-quick-menu-map
    :prefix "l"
    "e" #'eval-expression)
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "RET" #'mo-async-run-code)
  ( :states 'motion
    "g \"" #'end-of-buffer)
  ( :states 'motion
    "g '" #'beginning-of-buffer)
  :config
  (defvar-local mo-run-code-command nil
    "A local var that stores the run code command.
To be used with `mo-async-run-code'.")
  (put 'mo-run-code-command 'safe-local-variable 'stringp)

  (defun mo-async-run-code ()
    "Run code by asynchronously executing `mo-run-code-command'.
When running with a prefix argument, or if `mo-run-code-command' is null, prompt
the user to input the run command."
    (interactive)
    (setq-local mo-run-code-command
                (if (and (not current-prefix-arg)
                         (and (boundp 'mo-run-code-command) mo-run-code-command))
                    mo-run-code-command
                  (read-string "Run code command: ")))
    (let ((default-directory (project-root (project-current t))))
      (async-shell-command mo-run-code-command "*Run Code Command*")))

  (defun mo-toggle-scratch-buffer ()
    "Toggle the scratch buffer."
    (interactive)
    (let ((window (get-buffer-window "*scratch*")))
      (if window
          (quit-window nil window)
        (scratch-buffer))))

  ;; Set a wide enough default fill-column
  (setq-default fill-column 100)
  ;; Disable default tab indentation
  (setq-default indent-tabs-mode nil)
  ;; Disable blinking matching paren as we use show-paren-mode instead
  (setq blink-matching-paren nil)
  ;; Show cursor's column number
  (setq column-number-mode t)
  ;; Create a new buffer if async shell buffer already in use
  (setq async-shell-command-buffer 'new-buffer))

;; Init comp for native compilation settings
(use-package comp
  :straight nil
  :config
  (setq native-comp-async-report-warnings-errors 'silent))

;; Init custom for declaring and initializing user options
(use-package custom
  :straight nil
  :config
  (defvar after-enable-theme-hook nil
    "Hook run after a theme is enabled using `enable-theme'.")
  (defadvice enable-theme (after run-after-enable-theme-hook activate)
    "Run `after-enable-theme-hook'."
    (run-hooks 'after-enable-theme-hook)))

;; Init cus-edit for creating and editing customize buffers
(use-package cus-edit
  :straight nil
  :config
  ;; Set customization file path
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

;; Init minibuffer for minibuffer support
(use-package minibuffer
  :straight nil
  :config
  ;; Ignore case on file name completions
  (setq read-file-name-completion-ignore-case t))

;; Init imenu for imenu support
(use-package imenu
  :straight nil
  :config
  (setq imenu-max-item-length nil))

;; Init ibuffer for editing buffer lists
(use-package ibuffer
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "i" #'ibuffer)
  :custom
  ( ibuffer-formats '((mark modified read-only locked
                            " " (name 50 50 :left :elide)
				            " " (size 9 -1 :right)
				            " " (mode 16 16 :left :elide) " " filename-and-process)
			          (mark " " (name 16 -1) " " filename))))

;; Init ibuffer-project for grouping buffers per project in ibuffer
(use-package ibuffer-project
  :functions ( ibuffer-project-generate-filter-groups
               ibuffer-do-sort-by-project-file-relative)
  :defines ( ibuffer-filter-groups ibuffer-sorting-mode)
  :hook
  ( ibuffer . mo-ibuffer-project-init)
  :config
  (defun mo-ibuffer-project-init ()
    "Init ibuffer-project."
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative))))

;; Init repeat for repeating previous commands
(use-package repeat
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "z"
    "z" #'repeat))

;; Init kmacro for keyboard macros
(use-package kmacro
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "k"
    "k" #'kmacro-start-macro-or-insert-counter
    "c" #'kmacro-end-or-call-macro
    "e" #'kmacro-edit-macro
    "E" #'kmacro-step-edit-macro
    "s" #'kmacro-set-counter
    "f" #'kmacro-set-format
    "r" #'kmacro-edit-lossage
    "v" #'kmacro-view-macro
    "d" #'kmacro-delete-ring-head
    "n" #'kmacro-cycle-ring-next
    "p" #'kmacro-cycle-ring-previous)
  ;; Unbind global macro bindings
  ( :keymaps 'global
    "<f3>" nil
    "<f4>" nil))

;; Init macros for controlling macros
(use-package macros
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "k"
    "q" #'kbd-macro-query))

;; Init env for manipulating environment variables
(use-package env
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "a"
    "e" #'setenv))

;; Init modus-operandi-emacs for non-package related functionality
(use-package modus-operandi-emacs
  :after ( simple project tab-bar vertico consult)
  :straight nil
  :no-require t ; Not a package
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "p" #'mo-copy-file-path)
  ( :keymaps 'mo-quick-menu-map
    :prefix "DEL"
    "DEL" #'mo-open-project-with-tab
    "ESC" #'mo-close-project-with-tab
    "RET" #'mo-execute-predefined-command)
  ( :keymaps 'vertico-map
    "C-<return>" #'mo-minibuffer-insert-file-pattern
    "C-<escape>" #'mo-minibuffer-insert-file-excl-pattern)
  :preface
  (defun mo-copy-file-path ()
    "Copy the full path of the current buffer's file."
    (interactive)
    (let ((filepath (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (when filepath
        (kill-new filepath)
        (message "%s" filepath))))

  (defun mo-open-project-with-tab ()
    "Open project with new tab.
Tab is named after the project's name."
    (interactive)
    (tab-bar-new-tab)
    (condition-case err
        (progn
          (call-interactively #'project-switch-project)
          (let ((project-dir (directory-file-name (project-root (project-current))))
                (tab-name nil))
            (cl-loop for path-part in (reverse (file-name-split project-dir))
                     do (setq tab-name (file-name-concat path-part tab-name))
                     while (tab-bar--tab-index-by-name tab-name))
            (tab-bar-rename-tab tab-name)))
      (quit
       (tab-bar-close-tab))))

  (defun mo-close-project-with-tab ()
    "Kill project buffers and close tab.
Tab is named after the project's name."
    (interactive)
    (call-interactively #'project-kill-buffers)
    (tab-bar-close-tab))

  (defvar-local mo-predefined-commands nil
    "An alist containing command names and their respective command lines.")
  (put 'mo-predefined-commands 'safe-local-variable #'listp)

  (defun mo-execute-predefined-command ()
    "Select and execute a predefined command."
    (interactive)
    (let ((project-dir (project-root (project-current t)))
          (command (cdr
                    (assoc
                     (completing-read
                      "Execute Command: "
                      mo-predefined-commands
                      nil
                      t)
                     mo-predefined-commands))))
      (let ((default-directory project-dir))
        (async-shell-command command))))

  (defun mo-show-modified-buffer-changes ()
    "If a buffer is different from its file, show the changes."
    (let ((buffer buffer-file-name))
      (when (and buffer (file-exists-p buffer) (buffer-modified-p))
        (let ((diff-window (diff-buffer-with-file)))
          (add-hook 'kill-buffer-hook
                    (lambda ()
                      (quit-window nil diff-window))
                    nil t))))
    t)

  (defvar mo-minibuffer-file-excl-pattern '( ".*?" . ".*?:[[:digit:]]+:")
    "Minibuffer file exclusion pattern.")

  (defun mo-minibuffer-insert-file-excl-pattern ()
    "Insert a file exclusion pattern to the minibuffer filter."
    (interactive)
    (mo-minibuffer-insert-file-pattern t))

  (defun mo-minibuffer-insert-file-pattern (&optional arg)
    "Insert a file pattern to the minibuffer filter.
If universal ARG is set, exclude the pattern."
    (interactive "P")
    (when (minibufferp)
      (let* ((consult-asyncp (member consult-async-map (current-local-map)))
             (consult-async-style (and consult-asyncp consult-async-split-style))
             (consult-async-delimiter ?`)
             (prompt-end (minibuffer-prompt-end)))
        (if consult-async-style
            (progn
              (unless (eq consult-async-style 'perl)
                (user-error "Only 'perl' consult async split style is supported"))
              ;; Ensure initial consult async delimiter
              (goto-char prompt-end)
              (unless (eq (char-after) consult-async-delimiter)
                (insert consult-async-delimiter))
              ;; Ensure the second async delimiter
              (goto-char (point-max))
              (if (> (how-many (char-to-string consult-async-delimiter) prompt-end) 1)
                  (insert " ")
                (insert consult-async-delimiter)))
          (goto-char (point-max))
          (unless (eq (minibuffer-prompt-end) (point))
            (insert " ")))

        ;; Insert negation operator, if excluding
        (when arg
          (insert "!"))

        ;; Insert file pattern
        (dolist (pattern (list (car mo-minibuffer-file-excl-pattern)
                               (cdr mo-minibuffer-file-excl-pattern)))
          (insert (propertize pattern 'display (propertize (make-string 1 ?:) 'face 'shadow))))
        (backward-char (length (cdr mo-minibuffer-file-excl-pattern))))))

  ;; Modeline configuration
  (defvar-local mo-lsp-mode-mode-line nil
    "Holds the modeline list for lsp-mode.")

  ;; Needed for displaying in `mode-line-format'
  (put 'mo-lsp-mode-mode-line 'risky-local-variable t)

  ;; Add lsp modeline string to modeline format
  (setq-default mode-line-format (append
                                  (butlast (default-value 'mode-line-format) 2)
                                  (list 'mo-lsp-mode-mode-line)
                                  (last (default-value 'mode-line-format) 2)))

  :config
  ;; When killing a modified buffer, show the changes
  (add-to-list 'kill-buffer-query-functions #'mo-show-modified-buffer-changes t))

;; Add evil key bindings to other, non-default, modes
(use-package evil-collection
  :after ( general evil xref magit sly)
  :commands ( general-define-key
              evil-collection-init)
  :defines ( evil-collection-want-find-usages-bindings
             evil-collection-want-unimpaired-p)
  :demand t
  :init
  (defvar forge-add-default-bindings nil)
  :config
  (defun mo-evil-collection-remove-quick-menu-prefix (_mode keymaps &rest _rest)
    "Remove bindings conflicting with the quick menu prefix key from KEYMAPS."
    (dolist (keymap keymaps)
      (when (and (boundp keymap) (symbol-value keymap))
        (general-define-key :keymaps keymap :states 'normal "," nil))))
  ;; We have our own find references key binding. Remove evil-collection's one.
  ;; evil-collection's find usages overrides evil-mc key bindings.
  (setq evil-collection-want-find-usages-bindings nil)
  (setq evil-collection-want-unimpaired-p nil)
  (evil-collection-init)

  ;; Init dired+ for additional dired functionality
  ;; Init the package after evil-collection is both loaded and configured, so directory
  ;; reusing will be configured correctly.
  (use-package dired+
    :functions diredp-toggle-find-file-reuse-dir
    :custom
    ( diredp-hide-details-initially-flag nil)
    :config
    (setq diredp-ignore-compressed-flag nil)
    (diredp-toggle-find-file-reuse-dir 1)
    ;; These hooks seem to degrade performance on some scenarios
    (remove-hook 'dired-after-readin-hook 'diredp-nb-marked-in-mode-name)
    (remove-hook 'dired-mode-hook 'diredp-nb-marked-in-mode-name))

  :hook
  ( evil-collection-setup . mo-evil-collection-remove-quick-menu-prefix))

;; Init evil-org for supporting evil key bindings in org-mode
(use-package evil-org
  :after org
  :functions evil-org-agenda-set-keys
  :general
  ( :definer 'minor-mode
    :keymaps 'evil-org-mode
    :states 'normal
    ;; Remove evil org insert heading bindings
    "C-<return>" nil
    "C-S-<return>" nil)
  :hook
  ( org-mode . evil-org-mode)
  ( org-agenda-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Init evil-mc for supporting multiple cursors in evil mode
(use-package evil-mc
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "r"
    "RET" #'evil-mc-make-cursor-here
    "a" #'evil-mc-make-all-cursors
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg
    "o" #'evil-mc-make-cursor-move-next-line
    "O" #'evil-mc-make-cursor-move-prev-line
    "p" #'evil-mc-pause-cursors
    "r" #'evil-mc-resume-cursors
    "n" #'evil-mc-make-and-goto-next-match
    "N" #'evil-mc-make-and-goto-prev-match
    "s" #'evil-mc-skip-and-goto-next-match
    "S" #'evil-mc-skip-and-goto-prev-match
    "j" #'evil-mc-make-and-goto-next-cursor
    "J" #'evil-mc-skip-and-goto-next-cursor
    "k" #'evil-mc-make-and-goto-prev-cursor
    "K" #'evil-mc-skip-and-goto-prev-cursor
    "^" #'evil-mc-make-and-goto-first-cursor
    "$" #'evil-mc-make-and-goto-last-cursor
    "u" #'evil-mc-undo-last-added-cursor
    "ESC" #'evil-mc-undo-all-cursors)
  :config
  (global-evil-mc-mode))

;; Init evil-surround for quickly adding paired surrounding characters
(use-package evil-surround
  :demand t
  :general
  ( :states 'visual
    "s" #'evil-surround-region)
  :config
  (global-evil-surround-mode 1))

;; Init anzu for showing additional search match info
(use-package anzu
  :functions global-anzu-mode
  :config
  (global-anzu-mode +1))

;; Init evil-anzu for anzu integration with evil search
(use-package evil-anzu
  :after evil)

;; Init avy for text zapping using free text and a timeout
(use-package avy
  :demand t
  :hook
  ( after-enable-theme . mo-avy-configure-theme)
  :config
  :general
  ( :keymaps 'override
    "C-;" #'avy-goto-char-timer)
  :config
  (defun mo-avy-configure-theme ()
    "Set avy theme configuration."
    ;; Better highlight the leading characters
    (set-face-attribute 'avy-lead-face nil :background "gold2")
    (set-face-attribute 'avy-lead-face-0 nil :background "gold3")
    (set-face-attribute 'avy-lead-face-1 nil :background "gold4")
    (set-face-attribute 'avy-lead-face-2 nil :background "DarkGoldenrod4"))
  (setq avy-single-candidate-jump nil)
  (setq avy-all-windows 'all-frames)
  (setq avy-timeout-seconds 0.2))

;; Init evil-easymotion for using avy with evil motions
(use-package evil-easymotion
  :functions evilem-default-keybindings
  :config
  (evilem-default-keybindings ";"))

;; Init link-hint for quick link selection
(use-package link-hint
  :general
  ( :keymaps 'override
    "C-\"" #'link-hint-open-link))

;; Init evil-snipe for an improved 1 char evil search experience
(use-package evil-snipe
  :functions evil-snipe-override-mode
  :custom
  ( evil-snipe-override-evil-repeat-keys nil)
  :config
  (evil-snipe-override-mode 1))

;; Init expand-region for expanding the selected region by semantic units
(use-package expand-region
  :general
  ( :keymaps 'override
    "C-M-<return>" #'er/expand-region
    "C-M-<escape>" #'er/contract-region))

;; Init evil-numbers for increasing/decreasing number at point
(use-package evil-numbers
  :general
  ( :states '( normal visual)
    "z i" #'evil-numbers/inc-at-pt
    "z d" #'evil-numbers/dec-at-pt))

;; Init two-column for editing two-column texts
(use-package two-column
  :straight nil
  :general
  ;; Unbind global menu shortcut binding
  ( :keymaps 'global
    "<f2>" nil))

;; Init paredit for parenthetical editing in Emacs
(use-package paredit
  :general
  ( :keymaps 'paredit-mode-map
    "C-{" #'paredit-forward-barf-sexp
    "C-}" #'paredit-forward-slurp-sexp
    "M-{" #'paredit-backward-slurp-sexp
    "M-}" #'paredit-backward-barf-sexp
    "C-M-{" #'paredit-splice-sexp-killing-backward
    "C-M-}" #'paredit-splice-sexp-killing-forward
    "M-(" #'mo-paredit-wrap-round-with-space
    "C-M-(" #'paredit-wrap-round)
  ( :keymaps 'paredit-mode-map
    :states 'motion
    "[" nil
    "]" nil)
  ( :keymaps 'paredit-mode-map
    :states 'normal
    "[" #'paredit-splice-sexp-killing-backward
    "]" #'paredit-splice-sexp-killing-forward)
  :config
  (defun mo-paredit-wrap-round-with-space ()
    "Wrap the following S-expression and insert a space."
    (interactive)
    (paredit-wrap-round)
    (save-excursion
      (insert " ")))
  :hook
  (lisp-data-mode . enable-paredit-mode)
  (sly-mrepl . enable-paredit-mode))

;; Init evil-cleverparens for lisp modal editing
(use-package evil-cleverparens
  :general
  ( :keymaps 'evil-cleverparens-mode-map
    :states 'normal
    "M-k" #'evil-cp-drag-backward
    "M-j" #'evil-cp-drag-forward
    "M-a" #'evil-cp-insert-at-end-of-form
    "M-i" #'evil-cp-insert-at-beginning-of-form
    "M-w" #' evil-cp-copy-paste-form)
  :custom
  ( evil-cleverparens-use-additional-bindings nil)
  ( evil-cleverparens-use-additional-movement-keys nil)
  ( evil-cleverparens-use-s-and-S nil)
  ( evil-cleverparens-use-regular-insert t)
  :hook
  (lisp-data-mode . evil-cleverparens-mode)
  (sly-mrepl . evil-cleverparens-mode))

;; Init vundo for viewing and moving in the undo tree history
(use-package vundo
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "u" #'vundo))

;; Init undo-fu-session for undo persistence
(use-package undo-fu-session
  :demand t
  :commands undo-fu-session-global-mode
  :custom
  ( undo-fu-session-directory (mo-cache-path "undo-fu-session"))
  :config
  (undo-fu-session-global-mode))

;; Init better-jumper for better controlling the jump list logic
(use-package better-jumper
  :demand t
  :after evil
  :hook
  ;; Recenter after jump
  ( better-jumper-post-jump . recenter)
  :general
  ( [remap evil-jump-forward] 'better-jumper-jump-forward)
  ( [remap evil-jump-backward] 'better-jumper-jump-backward)
  ( :keymaps 'override
    "C-&" #'better-jumper-set-jump)
  :config
  (defvar mo-better-jumper-set-jump-on-push-mark 't
    "Dynamically controls if a jump is set on push-mark.
Used for preventing recursion when recording new jumps.")

  (defun mo-better-jumper-set-jump-on-push-mark (func &optional location nomsg activate)
    "Set jump on push-mark, if allowed."
    (unless (or (not mo-better-jumper-set-jump-on-push-mark)
                (eq this-command 'better-jumper-jump-backward)
                (eq this-command 'better-jumper-jump-forward))
      (better-jumper-set-jump location))
    (apply func (list location nomsg activate)))

  (defun mo-better-jumper-disable-set-jump-on-push-mark (func &rest args)
    "Disable setting jumps on push-mark to prevent recursion."
    (let ((mo-better-jumper-set-jump-on-push-mark nil))
      (apply func args)))

  ;; Record jumps when marks are pushed to the mark ring
  (advice-add 'better-jumper-set-jump :around #'mo-better-jumper-disable-set-jump-on-push-mark)
  (advice-add 'push-mark :around #'mo-better-jumper-set-jump-on-push-mark)
  ;; Jump list to work as a stack
  (setq better-jumper-add-jump-behavior 'replace)
  (setq better-jumper-max-length 1000)
  (setq better-jumper-buffer-savehist-size 1000)
  (better-jumper-mode +1))

;; Init xref for code reference lookup
(use-package xref
  :demand t
  :straight nil
  :hook
  ;; Recenter after returning to a pre-jump location
  ( xref-after-return . recenter)
  :general
  ( :keymaps 'mo-quick-menu-map
    ";" #'xref-find-definitions
    "'" #'xref-find-references)
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "<" #'xref-find-apropos)
  :config
  ;; When looking for references, don't ask for an identifier
  (setq xref-prompt-for-identifier nil)
  (setq xref-history-storage #'xref-window-local-history))

;; Init dump-jump for heuristics based reference lookup
(use-package dumb-jump
  :general
  ( :keymaps 'mo-quick-menu-map
    ":" #'dumb-jump-go)
  :init
  (setq dumb-jump-selector #'completing-read)
  :custom
  ( dumb-jump-prefer-searcher 'rg))

;; Init origami for text and code folding
(use-package origami
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "m" #'origami-recursively-toggle-node
    "M" #'origami-open-all-nodes)
  :config
  (global-origami-mode))

;; Init lsp-origami for code folding based on data from language server
(use-package lsp-origami
  :after ( origami lsp)
  :hook
  ( lsp-after-open . lsp-origami-try-enable))

;; Init kkp for supporting the Kitty Keyboard Protocol
(use-package kkp
  :commands global-kkp-mode
  :config
  (global-kkp-mode +1))

;; Init org mode for editing and managing notes
(use-package org
  :straight ( :type built-in)
  :general
  ( :keymaps 'org-mode-map
    "C-M-s-o" #'org-open-at-point
    "C-M-s-l" #'org-insert-link
    "C-M-s-S-l" #'org-latex-preview
    "C-M-s-p" #'org-toggle-pretty-entities
    "C-M-s-s" #'org-schedule
    "C-M-s-d" #'org-deadline
    "C-M-s-t" #'org-todo
    "C-M-s-h" #'org-toggle-heading
    "C-M-s-S-h" #'org-toggle-item
    "C-M-s-S-t" #'org-time-stamp
    "C-M-s-g" #'org-set-tags-command
    "C-M-s-i" #'org-insert-structure-template
    "C-M-s-e" #'org-edit-special
    "C-M-s-S-e" #'org-export-dispatch
    "C-M-s-<return>" #'org-open-at-point
    "C-M-s-," #'org-mark-ring-goto)
  ( :keymaps 'org-src-mode-map
    "C-M-s-e" #'org-edit-src-exit
    "C-M-s-k" #'org-edit-src-abort)
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "c" #'org-capture
    "s" #'org-store-link)
  ( :keymaps 'override
    "<f12>" #'mo-org-clock-toggle
    "C-<f12>" #'org-clock-cancel)
  ( :keymaps 'org-mode-map
    "C-<return>" #'org-meta-return
    "<tab>" #'org-cycle)
  ( :keymaps 'org-mode-map
    :states '( normal insert emacs)
    "M-<return>" #'org-insert-heading-respect-content)
  :hook
  ( after-enable-theme . mo-org-configure-theme)
  (org-mode . visual-line-mode)
  :config
  (defun mo-org-configure-theme ()
    "Set org theme configuration."
    ;; Resize org headings
    (dolist (face '( ( org-document-title . 1.2)
                     ( org-level-1 . 1.1)
                     ( org-level-2 . 1.0)
                     ( org-level-3 . 1.0)
                     ( org-level-4 . 1.0)
                     ( org-level-5 . 1.0)
                     ( org-level-6 . 1.0)
                     ( org-level-7 . 1.0)
                     ( org-level-8 . 1.0)))
      (set-face-attribute (car face) nil :height (cdr face)))
    ;; Make sure certain org faces always use the fixed-pitch face
    (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit '( shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '( shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '( font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '( font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
    ;; Set link style without changing foreground color
    (set-face-attribute 'org-link nil :inherit nil :foreground 'unspecified :underline t :slant 'italic)
    ;; Emphasis org clock status
    (copy-face 'org-clock-overlay 'org-mode-line-clock))
  (defun mo-org-clock-toggle ()
    "If not clocked, ask to start a recent clocks from list. If clocked, clock out."
    (interactive)
    (if org-clock-current-task
        (call-interactively #'org-clock-out)
      (let ((current-prefix-arg '( 4)))
        (call-interactively #'org-clock-in-last))))
  ;; Visually indent text under bullets
  (setq org-startup-indented t)
  ;; Allow resizing inline images
  (setq org-image-actual-width nil)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))
  (setq org-directory "~/org")
  (setq org-default-notes-file (concat (file-name-as-directory org-directory) "notes.org"))
  (setq org-capture-templates '( ( "t" "Task" entry (file+function org-default-notes-file org-goto)
                                   "** TODO %?\nSCHEDULED: %t")))
  (setq org-goto-interface 'outline-path-completion)
  ;; Unfold everything on startup, except for things that set to be hidden
  ;; by default (e.g. drawers)
  (setq org-startup-folded 'showall)
  (setq org-blank-before-new-entry '( ( heading . nil) ( plain-list-item . nil)))
  (setq org-M-RET-may-split-line '( ( default . nil)))
  (setq org-ellipsis " ▼")
  (setq org-cycle-level-faces nil)
  ;; This is needed per org-present-hide-stars-in-headings' documentation
  (setq org-hide-emphasis-markers t)
  (setq org-startup-with-inline-images t)
  ;; Prevent emphasis from crossing line boundaries
  ;; This is done to prevent unwanted emphasis and heading interactions
  (setcar (nthcdr 4 org-emphasis-regexp-components) 0)
  ;; Open links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (setq org-todo-keywords
        '( ( sequence "TODO" "NEXT" "PROG" "HOLD" "|" "DONE" "DONT" "FAIL")))
  (setq org-log-done t)
  (setq org-clock-persist t)
  (setq org-clock-persist-file (mo-cache-path "org-clock-save.el"))
  (org-clock-persistence-insinuate)
  (setq org-clock-mode-line-total 'current)
  (setq org-id-locations-file (mo-cache-path ".org-id-locations"))
  (setq org-src-preserve-indentation nil)
  (setq org-export-preserve-breaks t)
  (setq org-edit-src-content-indentation 0)
  (setq org-edit-src-persistent-message nil)
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '( ( emacs-lisp . t)
      ( lisp . t)
      ( org . t)
      ( calc . t)
      ( python . t)
      ( ruby . t)
      ( perl . t)
      ( C . t)
      ( java . t)
      ( groovy . t)
      ( haskell . t)
      ( clojure . t)
      ( julia . t)
      ( js . t)
      ( css . t)
      ( latex . t)
      ( sql . t)
      ( sqlite . t)
      ( makefile . t)
      ( eshell . t)
      ( shell . t))))

;; Init org-agenda for org agenda view
(use-package org-agenda
  :straight ( :type built-in)
  :after org
  :general
  ( :keymaps 'org-agenda-mode-map
    "C-M-s-s" #'org-agenda-schedule
    "C-M-s-d" #'org-agenda-deadline
    "C-M-s-g" #'org-agenda-set-tags)
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "a" #'mo-org-agenda-and-todo)
  ( :keymaps 'mo-quick-menu-map
    "-" #'mo-org-agenda-and-todo)
  ;; Close any loaded org buffer when exiting the agenda buffer
  ( :keymaps 'org-agenda-mode-map
    "q" #'org-agenda-exit)
  :hook
  ( after-enable-theme . mo-org-agenda-configure-theme)
  :config
  (defun mo-org-agenda-configure-theme ()
    "Set org theme configuration."
    ;; Resize org headings and agenda dates
    (dolist (face '(( org-agenda-date . 1.1)
                    ( org-agenda-date-today . 1.1)
                    ( org-agenda-date-weekend . 1.1)
                    ( org-agenda-date-weekend-today . 1.1)))
      (set-face-attribute (car face) nil :height (cdr face))))

  (defun mo-org-agenda-and-todo ()
    "Open org agenda with all TODOs"
    (interactive)
    (org-agenda nil "n"))

  (defvar mo-org-agenda-auto-save-agenda-interval 30
    "Interval in seconds for auto-saving opened agenda files.")

  (defun mo-org-agenda-save-agenda ()
    "Save all currently opened agenda files."
    (dolist (file (org-agenda-files))
      (when-let ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer
          (save-buffer)))))

  (setq org-agenda-files `( ,org-directory))
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-include-diary t)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-diary-file
        (concat (file-name-as-directory org-directory) "diary.org"))
  (setq org-agenda-prefix-format
        '( (agenda . "%i %-20c%?-12t% s")
           (todo . "%i %-20c")
           (tags . "%i %-20c")
           (search . "%i %-20c")))
  (setq org-agenda-category-icon-alist
        (append org-agenda-category-icon-alist
                '( ( "Diary" ("📆‍") nil nil :ascent center)
                   ( ".+" ("☑️‍") nil nil :ascent center)
                   ( "" ("  ") nil nil :ascent center))))

  ;; Auto-save agenda files
  (run-with-timer 0 mo-org-agenda-auto-save-agenda-interval #'mo-org-agenda-save-agenda)

  :hook
  ;; Show agenda on startup
  ( emacs-startup . mo-org-agenda-and-todo))

;; Init org-contrib for org add-ons
(use-package org-contrib)

;; Init org-checklist for handling checklists in org mode
(use-package org-checklist
  :after org-contrib
  :straight nil)

;; Init ol-git-link for org links to files with git revisions
(use-package ol-git-link
  :after org-contrib
  :straight nil)

;; Init ox-pandoc for exporting org files using pandoc
(use-package ox-pandoc)

;; Init ob-async for enabling asynchronous execution of org-babel blocks
(use-package ob-async)

;; Init org-journal for journal keeping
(use-package org-journal
  :after org
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "j" #'org-journal-new-entry
    "J" #'org-journal-new-date-entry)
  ( :keymaps 'org-journal-mode-map
    "C-M-s-j C-M-s-j" #'org-journal-new-entry
    "C-M-s-j C-M-s-n" #'org-journal-next-entry
    "C-M-s-j C-M-s-p" #'org-journal-previous-entry
    "C-M-s-j C-M-s-s" #'org-journal-search)
  ( :keymaps 'calendar-mode-map
    "C-M-s-j C-M-s-j" #'org-journal-new-date-entry
    "C-M-s-j C-M-s-d" #'org-journal-display-entry
    "C-M-s-j C-M-s-n" #'org-journal-next-entry
    "C-M-s-j C-M-s-p" #'org-journal-previous-entry
    "C-M-s-j C-M-s-m" #'org-journal-mark-entries
    "C-M-s-j C-M-s-r" #'org-journal-read-entry
    "C-M-s-j C-M-s-s" #'org-journal-search-forever)
  ( :keymaps '( org-agenda-mode-map org-super-agenda-header-map)
    "C-M-s-j" #'mo-org-journal-new-entry-from-agenda)
  :custom
  ( org-journal-dir
    (concat (file-name-as-directory org-directory) "journal"))
  ( org-journal-file-type 'weekly)
  ( org-journal-enable-agenda-integration t)
  ( org-journal-file-format "%Y-%m-%d.org")
  ( org-journal-carryover-items "")
  ( org-journal-file-header ":PROPERTIES:\n:agenda-group: Journal\n:END:")
  :config
  (defun mo-org-journal-new-entry-from-agenda (prefix)
    "Add journal entry for the date at point in the agenda."
    (interactive "P")
    (org-agenda-check-type t 'agenda)
    (let* ((day (or (get-text-property (min (1- (point-max)) (point)) 'day)
                    (user-error "Don't know which date to open in calendar")))
           (time (org-time-string-to-time
                  (format-time-string "%Y-%m-%d" (org-time-from-absolute day)))))
      (org-journal-new-entry prefix time))))

;; Init org-roam for Zettelkasten note management
(use-package org-roam
  :after vertico
  :demand t
  :general
  ( :keymaps 'org-mode-map
    "C-M-s-r C-M-s-r" #'org-roam-buffer-toggle
    "C-M-s-r C-M-s-g" #'org-roam-graph
    "C-M-s-r C-M-s-i" #'org-roam-node-insert
    "C-M-s-r C-M-s-t" #'org-roam-tag-add)
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "r" #'org-roam-node-find
    "R" #'org-roam-capture)
  :custom
  ( org-roam-directory "~/roam")
  :init
  (setq org-roam-v2-ack t)
  :config
  ;; Display link buffer as a side window
  (add-to-list 'display-buffer-alist
               '( "\\*org-roam\\*"
                  ( display-buffer-in-side-window)
                  ( side . right)
                  ( slot . 0)
                  ( window-width . 0.33)
                  ( window-parameters . ( ( no-other-window . t)
                                          ( no-delete-other-windows . t)))))
  (setq org-roam-db-location (mo-cache-path "org-roam.db"))
  (org-roam-db-autosync-mode))

;; Init consult-org-roam for org-roam and consult integration
(use-package consult-org-roam
  :after org-roam
  :demand t
  :commands consult-org-roam-mode
  :general
  ( :keymaps 'org-mode-map
    "C-M-s-r C-M-s-b" #'consult-org-roam-backlinks
    "C-M-s-r C-M-s-S-b" #'consult-org-roam-backlinks-recursive
    "C-M-s-r C-M-s-f" #'consult-org-roam-forward-links)
  :custom
  ( consult-org-roam-buffer-after-buffers t)
  :config
  (consult-org-roam-mode))

;; Init org-ql for advanced org search functionality
(use-package org-ql)

;; Init org-modern for a modern org buffer style
(use-package org-modern
  :functions global-org-modern-mode
  :custom
  ;; We disable prettifying tables as currently it is not pixel-aligned
  ( org-modern-table nil)
  ( org-modern-star 'replace)
  ( org-modern-replace-stars '( "●" "◍" "◉" "◎" "◌"))
  ( org-modern-list '( ( ?- . "▸") ( ?+ . "▹") ( ?* . "◦")))
  ( org-modern-hide-stars nil)
  :config
  (global-org-modern-mode))

;; Init org-modern-indent for a modern org indentation style
(use-package org-modern-indent
  :straight ( org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :functions org-modern-indent-mode
  :config
  (set-face-attribute 'fixed-pitch nil :family mo-font :height 1.0)
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

;; Init org-appear to show invisible org elements on cursor hover
(use-package org-appear
  :hook ( org-mode . org-appear-mode))

;; Init org-remark for highlighting text in org mode
(use-package org-remark
  :commands org-remark-global-tracking-mode
  :config
  (org-remark-global-tracking-mode))

;; Init org-pomodoro for using the Pomodoro technique with org mode
(use-package org-pomodoro
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "p" #'org-pomodoro))

;; Init dslide for creating presentations in Emacs
(use-package dslide
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "RET" #'dslide-deck-present
    "SPC" #'dslide-deck-start)
  ( :keymaps 'dslide-mode-map
    "C-M-s-j" #'dslide-deck-forward
    "C-M-s-k" #'dslide-deck-backward
    "C-M-s-s" #'dslide-deck-start
    "C-M-s-q" #'dslide-deck-stop)
  :config
  (defun mo-dslide-hide-evil-cursor ()
    "Hide evil cursor."
    (setq-local evil-default-cursor '( ignore))
    (dslide-cursor-hide))
  :hook
  ( dslide-develop . mo-dslide-hide-evil-cursor)
  ( dslide-present . mo-dslide-hide-evil-cursor)
  ( dslide-start . mo-dslide-hide-evil-cursor))

;; Init org-download for downloading and embedding images in org mode
(use-package org-download)

;; Init gnuplot for editing and plotting gnuplot graphs
(use-package gnuplot)

;; Init verb for sending and managing HTTP requests
(use-package verb
  :after org
  :general
  ( :keymaps 'org-mode-map
    "C-M-s-v" '( :keymap verb-command-map :package verb))
  ( :keymaps 'verb-command-map
    "C-M-s-<return>" #'verb-send-request-on-point-no-window
    "C-M-s-b" #'verb-export-request-on-point-verb
    "C-M-s-e" #'verb-export-request-on-point
    "C-M-s-f" #'verb-send-request-on-point
    "C-M-s-k" #'verb-kill-all-response-buffers
    "C-M-s-r" #'verb-send-request-on-point-other-window-stay
    "C-M-s-s" #'verb-send-request-on-point-other-window
    "C-M-s-u" #'verb-export-request-on-point-curl
    "C-M-s-v" #'verb-mode
    "C-M-s-S-v" #'verb-set-var
    "C-M-s-w" #'verb-export-request-on-point-eww))

;; Init consult-notes for selecting and previewing notes with consult
(use-package consult-notes
  :after ( org org-roam consult)
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "," #'consult-notes-search-in-all-notes
    "n" #'consult-notes)
  :config
  (defun mo-consult-notes-org-roam-annotate (cand)
    "Annotate org roam candidate with useful info."
    (let* ((node
            (org-roam-node-from-title-or-alias cand))
           (file
            (org-roam-node-file node))
           (dir
            (abbreviate-file-name (directory-file-name (file-name-directory file))))
           (size
            (file-size-human-readable (file-attribute-size (file-attributes file))))
           (time
            (consult-notes--time (org-roam-node-file-mtime node)))
           (links (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node))))
           (tags (mapconcat (lambda (tag) (concat "#" (car tag)))
                            (org-roam-db-query
                             [:select [tag]
                                      :from tags
                                      :where (=  node-id $s1)]
                             (org-roam-node-id (org-roam-node-from-title-or-alias cand))) " "))
           (name "Roam"))

      (put-text-property 0 (length name) 'face 'consult-notes-name name)
      (put-text-property 0 (length dir) 'face 'consult-notes-dir dir)
      (put-text-property 0 (length size) 'face 'consult-notes-size size)
      (put-text-property 0 (length time) 'face 'consult-notes-time time)
      (put-text-property 0 (length tags) 'face 'org-tag tags)

      (if (> links 0)
          (propertize (format "%3s" links) 'face 'consult-notes-backlinks)
        (propertize (format "%3s" "nil") 'face 'shadow))

      (format "%7s %8s  %12s  %8s %s %s" name size time dir links tags)))

  (setq consult-notes-org-roam-annotate-function #'mo-consult-notes-org-roam-annotate)
  (setq consult-notes-file-dir-sources '( ( "Org" ?o "~/org")))
  (consult-customize consult-notes :group nil) ; Remove grouping
  (consult-notes-org-roam-mode))

;; Init calendar for showing a calendar
(use-package calendar
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "c" #'calendar))

;; Init auctex for editing TeX files
(use-package tex
  :straight auctex)

;; Init cdlatex for improved editing of TeX files
(use-package cdlatex
  ;; Enable automatically in LaTeX-mode
  :hook ( LaTeX-mode . turn-on-cdlatex)
  ;; Enable automatically in org-mode
  :hook ( org-mode . turn-on-org-cdlatex)
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "x" #'cdlatex-environment))

;; Init evil-tex for improved editing of TeX files in evil-mode
(use-package evil-tex
  :hook ( LaTeX-mode . evil-tex-mode))

;; Init pdf-tools for better viewing pdf files in Emacs
(use-package pdf-tools
  ;; don't reinstall when package updates
  :mode  ( "\\.pdf\\'" . pdf-view-mode)
  :functions pdf-tools-install
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (pdf-tools-install :no-query))

;; Init saveplace-pdf-view for saving the last location in a pdf document
(use-package saveplace-pdf-view
  :after bookmark
  :config
  (save-place-mode 1))

;; Init nov for reading epub files in Emacs
(use-package nov
  :mode ( "\\.epub\\'" . nov-mode)
  :custom
  ( nov-save-place-file (mo-cache-path "nov-places")))

;; Init emacs-async for async processing in Emacs
(use-package emacs-async
  ;; This is not a loadable package
  :no-require t
  :demand t
  :functions dired-async-mode
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "a"
    "a" #'dired-async-mode
    "K" #'dired-async-kill-process)
  :custom
  ;; Make sure that the file threshold variable is passed to the async environment.
  ;; This is needed for disabling the large file warning on dired-async.
  ( dired-async-env-variables-regexp
    "\\`\\(\\(tramp-\\(default\\|connection\\|remote\\)\\|ange-ftp\\)-.*
\\|large-file-warning-threshold\\)")
  :config
  (defun mo-disable-large-file-warning-advice (func &rest args)
    "An advice function for disabling the large file warning."
    (let ((large-file-warning-threshold nil))
      (apply func args)))
  ;; Disable large file warning, that can happen, for example, when using tramp with ssh.
  ;; When large file warning appears in async flows, it blocks the process from
  ;; completing its job, as it waits for user input in the background.
  (advice-add 'dired-async-create-files :around #'mo-disable-large-file-warning-advice)
  (dired-async-mode))

;; Init orderless for advanced (e.g. fuzzy) completion styles
(use-package orderless
  :demand t
  :functions orderless-escapable-split-on-space
  :config
  ;; Set matching style to regexp and literal
  :custom
  ( orderless-matching-styles '( orderless-regexp orderless-literal))
  ( orderless-component-separator #'orderless-escapable-split-on-space)
  ( completion-styles '( orderless basic))
  :config
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '( ( file ( styles basic partial-completion)))))

;; Init vertico for item list selection
(use-package vertico
  :demand t
  ;; Load extensions
  :straight ( :files ( :defaults "extensions/*"))
  :functions vertico-mode
  :custom
  ( vertico-count 20)
  ( vertico-cycle t)
  ( vertico-sort-function #'vertico-sort-history-alpha)
  :config
  (defun mo-vertico-combined-sort (candidates)
    "Sort CANDIDATES using both display-sort-function and vertico-sort-function."
    (let ((candidates
           (let ((display-sort-func (vertico--metadata-get 'display-sort-function)))
             (if display-sort-func
                 (funcall display-sort-func candidates)
               candidates))))
      (if vertico-sort-function
          (funcall vertico-sort-function candidates)
        candidates)))

  (vertico-mode))

;; Init vertico-repeat for repeating the last minibuffer command
(use-package vertico-repeat
  :after vertico
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "z"
    "v" #'vertico-repeat-select)
  :hook
  ( minibuffer-setup . vertico-repeat-save))

;; Init vertico-directory for directory navigation commands
(use-package vertico-directory
  :after vertico
  :straight nil
  :general
  ( :keymaps 'vertico-map
    "M-<backspace>" #'vertico-directory-up))

;; Init vertico-quick for quick result selection
(use-package vertico-quick
  :after vertico
  :straight nil
  :general
  ( :keymaps 'vertico-map
    "M-k" #'vertico-quick-jump
    "M-j" #'vertico-quick-exit))

;; Init vertico-multiform for per command vertico configuration
(use-package vertico-multiform
  :after vertico
  :straight nil
  :config
  (vertico-multiform-mode))

;; Init vertico-buffer for viewing vertico results in a separate buffer
(use-package vertico-buffer
  :after vertico
  :straight nil
  :general
  ( :keymaps 'override
    "C-|" #'mo-vertico-buffer-next-command)
  :config
  (defun mo-vertico-buffer-next-command ()
    "Run next vertico command in a separate buffer."
    (interactive)
    (defun mo--vertico-buffer-disable-next-command ()
      "Called by a hook to disable vertico-buffer."
      (remove-hook 'minibuffer-setup-hook #'mo--vertico-buffer-disable-next-command)
      (vertico-buffer-mode -1))
    (add-hook 'minibuffer-setup-hook #'mo--vertico-buffer-disable-next-command 100)
    (vertico-buffer-mode)
    (message "Display the next vertico command in a dedicated buffer...")))

;; Init recursion-indicator for indicating minibuffer recursions
(use-package recursion-indicator
  :functions recursion-indicator-mode
  :config
  (recursion-indicator-mode))

;; Init corfu for auto completion
(use-package corfu
  ;; Load extensions
  :straight ( :files ( :defaults "extensions/*"))
  :demand t
  :functions
  ( corfu-mode
    global-corfu-mode)
  :general
  ( :keymaps 'corfu-map
    "M-SPC" #'corfu-insert-separator
    ;; We want TAB to complete
    "TAB" #'corfu-complete
    [tab] #'corfu-complete
    ;; We don't want RET to complete
    "RET" nil
    ;; We don't want M-n/p to complete
    "M-n" nil
    "M-p" nil)
  :custom
  ;; Enable auto completion
  ( corfu-auto t)
  ;; Keep popup only if there is a match or the separator was inserted
  ( corfu-quit-no-match 'separator)
  ;; Quit on exact match
  ( corfu-on-exact-match 'quit)
  ;; Set auto completion to be more responsive
  ( corfu-auto-delay 0)
  ( corfu-auto-prefix 0)
  ;; Sort candidates by calling corfu-sort-function on top of display-sort-function
  ( corfu-sort-override-function #'mo-corfu-combined-sort)
  :hook
  ;; Conditionally enable Corfu in the minibuffer
  ( minibuffer-setup . corfu-enable-in-minibuffer)
  ;; Disable auto mode in eshell
  ( eshell-mode . mo-corfu-enable-no-auto)
  ;; Disable auto mode in shell
  ( shell-mode . mo-corfu-enable-no-auto)
  ;; Close popup when exiting evil insert state
  ( evil-insert-state-exit . corfu-quit)
  :config
  (defun mo-corfu-enable-no-auto()
    "Enable corfu without auto completion."
    (setq-local corfu-auto nil)
    (corfu-mode))

  (defun mo-corfu-combined-sort (candidates)
    "Sort CANDIDATES using both display-sort-function and corfu-sort-function."
    (let ((candidates
           (let ((display-sort-func (corfu--metadata-get 'display-sort-function)))
             (if display-sort-func
                 (funcall display-sort-func candidates)
               candidates))))
      (if corfu-sort-function
          (funcall corfu-sort-function candidates)
        candidates)))

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  ;; Send selected candidate to shell, avoiding the need to press RET
  ;; twice when popup is visible
  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))
  (advice-add #'corfu-insert :after #'corfu-send-shell)
  (global-corfu-mode))

;; Init corfu-history for auto-completion sorting based on history
(use-package corfu-history
  :after ( corfu savehist)
  :straight nil
  :config
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; Init corfu-popupinfo for extra info on auto-completion candidates
(use-package corfu-popupinfo
  :after corfu
  :straight nil
  :config
  ;; Set a short popup delay
  (setq corfu-popupinfo-delay '( 0.5 . 0.5))
  (corfu-popupinfo-mode 1))

;; Init corfu-echo for auto-completion candidate doc in the echo area
(use-package corfu-echo
  :after corfu
  :straight nil
  :config
  (corfu-echo-mode 1))

;; Init corfu-quick for selecting auto-completion candidates using quick keys
(use-package corfu-quick
  :after corfu
  :straight nil
  :general
  ( :keymaps 'corfu-map
    "M-k" #'corfu-quick-jump
    "M-j" #'corfu-quick-complete))

;; Init corfu-terminal for using corfu in the terminal
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :functions corfu-terminal-mode
  :config
  (corfu-terminal-mode +1))

;; Init cape for completion at point extensions
(use-package cape
  :functions
  ( cape-capf-prefix-length
    cape-file cape-dabbrev)
  :custom
  ;; Do not scan every buffer with dabbrev (see dabbrev configuration)
  ( cape-dabbrev-check-other-buffers 'some)
  :config
  ;; Add completion functions
  (add-hook 'completion-at-point-functions (cape-capf-prefix-length #'cape-dabbrev 3))
  (add-hook 'completion-at-point-functions #'cape-file)

  (defun mo-cape-disable-file-comp-evil-search ()
    "Disable cape file completion on evil search."
    (when (or (eq current-minibuffer-command 'evil-ex-search-forward)
              (eq current-minibuffer-command 'evil-ex-search-backward))
      (make-local-variable 'completion-at-point-functions)
      (remove-hook 'completion-at-point-functions #'cape-file t)))

  :hook
  (minibuffer-setup . mo-cape-disable-file-comp-evil-search))

;; Init dabbrev for the automatic completion of dynamic abbreviations
(use-package dabbrev
  :straight nil
  :config
  (setq dabbrev-ignored-buffer-regexps '( "^\\*.+::stderr\\*$"))
  ;; Do not scan buffers that are too big
  (setq dabbrev-check-all-buffers nil)
  (defvar mo-dabbrev-max-file-size 1000000)
  (setq dabbrev-friend-buffer-function
        (lambda (buffer)
          (< (buffer-size buffer) mo-dabbrev-max-file-size))))

;; Init project for auto project detection
(use-package project
  :straight nil
  :general
  ( :keymaps 'override
    "C-M-<tab>" #'mo-project-other-buffer)
  ( :keymaps 'mo-quick-menu-map
    :prefix "DEL"
    "w" #'mo-project-save
    "W" #'project-forget-project
    "d" #'project-dired
    "f" #'mo-project-find-file
    "x" #'project-async-shell-command
    "k" #'project-kill-buffers
    "p" #'project-switch-project
    "i" #'project-list-buffers
    "r" #'mo-reload-dir-locals-project
    "l" #'mo-find-file-dir-locals-project)
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "c" #'mo-project-recompile
    "C" #'project-compile)
  :hook
  ( lisp-data-mode . mo-enable-reload-dir-locals-on-save)
  :config
  (defun mo-project-try-local (dir)
    "Determine if DIR is a project.
DIR must include a .project file to be considered a project."
    (let ((root (locate-dominating-file dir ".project")))
      (and root (cons 'transient root))))

  (defun mo-project-save ()
    "Save the current project to the persistent project list."
    (interactive)
    (message "Project saved: %s" (cdr (project-current t))))

  (defun mo-get-buffer-dir ()
    "Return buffer's directory.

The project root is used if found by project, with the default
directory as a fall back."
    (or
     (when-let ((project (project-current)))
       (project-root project))
     (expand-file-name default-directory)))

  (defun mo-project-find-file ()
    "Open find-file menu in project root directory."
    (interactive)
    (let* ((project (project-current t))
           (default-directory (project-root project)))
      (call-interactively #'find-file)))

  (defun mo-project-recompile ()
    "Run `recompile' in the project root."
    (declare (interactive-only recompile))
    (interactive)
    (let ((default-directory (project-root (project-current t)))
          (compilation-buffer-name-function
           (or project-compilation-buffer-name-function
               compilation-buffer-name-function)))
      (call-interactively #'recompile)))

  (defun mo-reload-dir-locals-project ()
    "Reload dir-locals for the current project."
    (interactive)
    (let ((project (project-current)))
      (dolist (buffer (project-buffers project))
        (with-current-buffer buffer
          (mo-reload-dir-locals-current-buffer)))
      (message "Dir locals loaded for %s" (project-root project))))

  (defun mo-enable-reload-dir-locals-on-save ()
    "Enable project dir-locals reload on dir-locals file save."
    (when-let ((buf-name (buffer-file-name)))
      (when (equal dir-locals-file (file-name-nondirectory buf-name))
        (add-hook 'after-save-hook #'mo-reload-dir-locals-project nil t))))

  (defun mo-find-file-dir-locals-project ()
    "Edit the dir-locals file in the current project."
    (interactive)
    (let* ((project (project-current))
           (root (project-root project)))
      (find-file (concat (file-name-as-directory root) dir-locals-file))))

  (defun mo-project-other-buffer ()
    "Switch to the next project buffer in buffer the list."
    (interactive)
    (let ((project (project-current)))
      (if project
          (let ((buffer (cadr (seq-filter
                               (lambda (buffer)
                                 (not (minibufferp buffer)))
                               (project-buffers project)))))
            (if buffer
                (switch-to-buffer buffer)
              (message "No other buffers")))
        (user-error "Current buffer is not part of a project"))))

  ;; Enable project detection using .project files
  (add-to-list 'project-find-functions #'mo-project-try-local)
  ;; Set project history file path
  (setq project-list-file (mo-cache-path "projects"))
  ;; Set project switch commands for a quick project based access
  (setq project-switch-commands
        '( (consult-project-buffer "Buffers and recent files" ?\r)
           (mo-project-find-file "Find file" ?f)
           (bookmark-in-project-jump "Bookmark" ?\t)
           (consult-fd "Fd" ?.)
           (consult-ripgrep "Ripgrep" ?,)
           (magit-status "Magit" ?g)
           (project-dired "Dired" ?d)
           (project-eshell "Eshell" ?e)
           (mo-vterm-project "Vterm" ?v)
           (project-async-shell-command "Async command" ?x))))

;; Init consult for enhanced search commands
(use-package consult
  :demand t
  :general
  ;; Quick bindings
  ( :keymaps 'mo-quick-menu-map
    "/" #'consult-line
    "?" #'consult-line-multi
    "." #'consult-fd
    "," #'consult-ripgrep
    "M" #'mo-consult-xref-history
    "<" #'mo-consult-xref-pop
    "RET" #'mo-consult-buffer-dwim)
  ( :keymaps 'mo-quick-menu-map
    :prefix "g"
    "," #'consult-git-grep)
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "q" #'consult-compile-error)
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "," #'consult-info
    "m" #'consult-man)
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "b" #'consult-buffer
    "/" #'consult-imenu
    "?" #'consult-imenu-multi
    ";" #'consult-focus-lines
    ":" #'consult-keep-lines
    "m" #'consult-minor-mode-menu
    "B" #'consult-recent-file)
  ( :keymaps 'mo-quick-menu-map
    :prefix "<insert>"
    "<insert>" #'consult-bookmark)
  ( :keymaps 'mo-quick-menu-map
    :prefix "f"
    "," #'mo-consult-ripgrep-current-dir
    "." #'mo-consult-fd-current-dir)
  ;; C-x bindings (ctl-x-map)
  ( "C-x M-:" #'consult-complex-command)     ;; orig. repeat-complex-command
  ( "C-x b" #'consult-buffer)                ;; orig. switch-to-buffer
  ( "C-x 4 b" #'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ( "C-x 5 b" #'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ;; Custom M-# bindings for fast register access
  ( "C-_" #'consult-register-load)
  ( "M-_" #'consult-register-store)
  ( "C-M-_" #'consult-register)
  ;; Other custom bindings
  ( "M-y" #'consult-yank-pop)                ;; orig. yank-pop
  ;; M-g bindings (goto-map)
  ( "M-g g" #'consult-goto-line)             ;; orig. goto-line
  ( "M-g M-g" #'consult-goto-line)           ;; orig. goto-line
  ;; Isearch integration
  ( :keymaps 'org-mode-map
    "M-e" #'consult-isearch)                 ;; orig. isearch-edit-string
  ( "M-s e" #'consult-isearch)               ;; orig. isearch-edit-string
  ( "M-s l" #'consult-line)                  ;; required by consult-line to detect isearch
  ( "M-s L" #'consult-line-multi)            ;; required by consult-line to detect isearch
  ;; Minibuffer history
  ( :keymaps 'minibuffer-local-map
    "C-S-p" #'mo-consult-toggle-preview
    "C-<menu>" #'mo-consult-preview
    "M-s" #'consult-history                 ;; orig. next-matching-history-element
    "M-r" #'consult-history                 ;; orig. previous-matching-history-element
    "C-r" #'consult-history)                ;; orig. isearch-backward
  ( :keymaps 'comint-mode-map
    "M-r" #'consult-history                 ;; orig. comint-history-isearch-backward-regexp
    "C-r" #'consult-history)
  ;; Eshell history
  ( :keymaps 'eshell-hist-mode-map
    "M-s" #'consult-history                 ;; orig. eshell-next-matching-input
    "M-r" #'consult-history                 ;; orig. eshell-previous-matching-input
    "C-r" #'consult-history)
  ( :keymaps 'evil-command-line-map
    "C-r" #'consult-history)                ;; orig. evil-paste-from-register
  ( :keymaps 'mo-quick-menu-map
    :prefix "DEL"
    "b" #'consult-project-buffer)
  ( :keymaps 'mo-quick-menu-map
    "*" #'mo-consult-line-symbol-at-point
    "&" #'mo-consult-line-symbol-at-point-other-window)
  ( :keymaps 'mo-quick-menu-map
    :prefix "v"
    "t" #'consult-theme)
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "/" #'consult-org-heading)
  ( :keymaps 'mo-quick-menu-map
    :prefix "z"
    "c" #'consult-complex-command)
  ( :keymaps 'mo-quick-menu-map
    :prefix "k"
    "l" #'consult-kmacro)

  :init
  ;; Improve register preview
  (setq register-preview-delay 0.5)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Configure the narrowing key.
  (setq consult-narrow-key "C-l")
  ;; Suppress file access error messages
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --no-messages"))

  ;; Change default async split character
  (plist-put (cdr (assq 'perl consult-async-split-styles-alist)) :initial "`")

  (defun mo-consult-buffer-dwim ()
    "If in project, list project buffers, otherwise show the global list."
    (interactive)
    (if (project-current)
        (call-interactively #'consult-project-buffer)
      (call-interactively #'consult-buffer)))

  ;; Add consult-fd command
  ;; Based on code from consult wiki:
  ;; https://github.com/minad/consult/wiki#find-files-using-fd
  (defun consult--fd-builder (input dir)
    (let ((fd-command
           (if (eq 0 (process-file-shell-command "fdfind"))
               "fdfind"
             "fd")))
      (pcase-let* ((`( ,arg . ,opts) (consult--command-split input))
                   (`( ,re . ,hl) (funcall consult--regexp-compiler
                                           arg 'extended t)))
        (when re
          (cons (append
                 (list fd-command
                       "--color=never" "-i" "-p" "-H" "-t" "f"
                       (concat (tramp-file-local-name dir) ".*" (consult--join-regexps re 'extended)))
                 opts)
                hl)))))

  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (pcase-let* ((`( ,prompt ,paths ,dir) (consult--directory-prompt "fd" dir))
                 (default-directory dir))
      (find-file
       (consult--find prompt
                      (lambda (input) (consult--fd-builder input dir))
                      initial))))

  (defun mo-consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))

  (defun mo-consult-line-symbol-at-point-other-window ()
    (interactive)
    (let ((thing (thing-at-point 'symbol))
          (src-window (selected-window)))
      (condition-case err
          (progn
            (other-window 1)
            (consult-line thing))
        (quit
         (when (window-live-p src-window)
           (select-window src-window))))))

  (evil-set-command-property #'mo-consult-line-symbol-at-point :jump t)

  (defun mo-consult-ripgrep-current-dir ()
    "Call consult-ripgrep on buffer's directory."
    (interactive)
    (consult-ripgrep default-directory))

  (defun mo-consult-fd-current-dir ()
    "Call consult-fd on buffer's directory."
    (interactive)
    (consult-fd default-directory))

  (defun mo-consult-xref-pop ()
    "Pop xref history list to a selected position.
The popped xref(s) will be pushed to the forward-history."
    (interactive)
    (let ((current-marker (point-marker))
          (history (funcall xref-history-storage))
          (dest-marker (call-interactively #'mo-consult-xref-history)))
      (push current-marker (car history))
      (cl-loop for item = (pop (car history))
               until (or (null item) (= item dest-marker))
               do (push item (cdr history)))))

  (defun mo-consult-xref-history ()
    "Jump to a position in the xref history list."
    (interactive)
    (if-let ((xref-history (delq nil (car (funcall xref-history-storage)))))
        (consult-global-mark xref-history)
      (user-error "Xref history is empty")))

  (defvar-local consult-toggle-preview-saved-func nil
    "Saved consult buffer preview function.")

  (defun mo-consult-toggle-preview ()
    "Enable or disable consult buffer preview."
    (interactive)
    (if consult-toggle-preview-saved-func
        (setq consult--preview-function consult-toggle-preview-saved-func
              consult-toggle-preview-saved-func nil)
      (setq consult-toggle-preview-saved-func consult--preview-function
            consult--preview-function #'ignore)))

  (defun mo-consult-preview ()
    "Force consult buffer preview.
Used while preview is toggled off."
    (interactive)
    (when consult-toggle-preview-saved-func
      (funcall consult-toggle-preview-saved-func))))

;; Init consult-dir for inserting paths into minibuffer prompts
(use-package consult-dir
  :general
  ( :keymaps 'vertico-map
    "M-;" #'consult-dir)
  ( :keymaps 'mo-quick-menu-map
    :prefix "f"
    "c" #'consult-dir))

;; Init consult-flycheck for showing syntax errors with consult
(use-package consult-flycheck
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "e" #'consult-flycheck))

;; Init marginalia for minibuffer result annotations
(use-package marginalia
  :demand t
  :after project
  :commands marginalia-mode
  :functions ( project-name
               marginalia-annotate-buffer
               marginalia--fields)
  :defines marginalia-annotator-registry
  :custom
  ( marginalia-field-width 200)
  :config
  (defun mo-marginalia-annotate-buffer-with-project (cand)
    "Annotate buffer with project name and other annotations."
    (let ((buffer-annotation (marginalia-annotate-buffer cand))
          (proj-name (when-let* ((buffer (get-buffer cand))
                                 (project (with-current-buffer buffer
                                            (project-current))))
                       (project-name project))))
      (marginalia--fields
       (proj-name :truncate 0.2 :face 'marginalia-function)
       (buffer-annotation))))

  ;; Add project name to buffer annotation
  (setf (alist-get 'buffer marginalia-annotator-registry)
        '( mo-marginalia-annotate-buffer-with-project builtin none))

  (marginalia-mode))

;; Init embark for enabling contextual actions
(use-package embark
  :general
  ( :keymaps 'override
    "C-'" #'embark-act
    "M-'" #'embark-collect
    "C-M-'" #'embark-export)
  ( :keymaps 'embark-file-map
    "g" #'mo-embark-magit-status)
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "b" #'embark-bindings)
  :config
  (defun mo-embark-magit-status (file)
    "Run `magit-status` on repo containing the embark target."
    (interactive "GFile: ")
    (magit-status (locate-dominating-file file ".git")))
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '( "\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                  nil
                  (window-parameters ( mode-line-format . none)))))

;; Init embark-consult for enabling embark actions on consult results
(use-package embark-consult
  :demand t
  :after ( embark consult)
  :hook
  ( embark-collect-mode . consult-preview-at-point-mode))

;; Init wgrep for editing grep-style results across files in grep buffer
(use-package wgrep)

;; Init tool-bar for tool-bar functionality
(use-package tool-bar
  :straight nil
  :config
  ;; Cleanup the frame UI
  (tool-bar-mode -1))

;; Init menu-bar for menu-bar functionality
(use-package menu-bar
  :straight nil
  :general
  ( :keymaps 'override
    "M-<f6>" #'toggle-debug-on-quit
    "<f6>" #'toggle-debug-on-error)
  :config
  (menu-bar-mode -1))

;; Init scroll-bar for scroll-bar functionality
(use-package scroll-bar
  :straight nil
  :config
  (scroll-bar-mode -1))

;; Init tab-bar for managing tab views
(use-package tab-bar
  :after doom-themes
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "t"
    "t" #'tab-bar-switch-to-tab
    "r" #'tab-bar-rename-tab
    "c" #'tab-bar-close-tab
    "f" #'tab-bar-move-tab
    "b" #'tab-bar-move-tab-backward
    "[" #'tab-previous
    "]" #'tab-next)
  ( :keymaps 'global
    "C-<tab>" nil
    "C-S-<tab>" nil
    "C-S-<iso-lefttab>" nil)
  :config
  ;; Disable tab bar buttons
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-button-show nil)
  ;; Customize the tab bar
  (setq tab-bar-format '( tab-bar-format-tabs-groups
                          tab-bar-separator
                          tab-bar-format-align-right
                          tab-bar-format-global))
  (setq tab-bar-tab-hints t)
  (setq tab-bar-auto-width-max '(300 27))
  ;; Switch to tab by pressing C-<hint num>
  (setq tab-bar-select-tab-modifiers '(control))
  ;; Init tab-bar for supporting multiple window layouts in frame
  (tab-bar-mode)
  ;; Set initial tab name
  (tab-bar-rename-tab "Agenda"))

;; Init dired for file management
(use-package dired
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "f"
    "d" #'dired-jump
    "RET" #'mo-open-with)
  :hook
  ;; Watch directories for changes
  ( dired-mode . auto-revert-mode)
  :config
  (defun mo-open-with (arg)
    "Open the visited file in the default external program.
When in Dired mode, open the file under the cursor.
When a prefix ARG is given always prompt for a command to use."
    (interactive "P")
    (let* ((current-file-name
            (if (derived-mode-p 'dired-mode)
                (dired-get-file-for-visit)
              buffer-file-name))
           (open (pcase system-type
                   (`darwin "open")
                   ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
           (program (if (or arg (not open))
                        (read-shell-command "Open current file with: ")
                      open)))
      (call-process program nil 0 nil current-file-name)))
  ;; Do not ignore extensions
  ;; Dired+ breaks when dired-omit-extensions contains no items
  (setq dired-omit-extensions '( ".qazwsxedc"))
  (setq dired-dwim-target t))

;; Init image-dired for viewing image thumbnails in dired
(use-package image-dired
  :straight nil
  :init
  (setq image-dired-dir (mo-cache-path "image-dired")))

;; Init dired-rsync for rsync support in dired
(use-package dired-rsync
  :general
  ( :keymaps 'dired-mode-map
    :states 'normal
    "s" #'dired-rsync))

;; Init treemacs for a tree-like sidebar file navigator
(use-package treemacs
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "DEL"
    ;; We want to present the current project only
    "m" #'treemacs-add-and-display-current-project-exclusively)
  :config
  (setq treemacs-persist-file (mo-cache-path "treemacs-persist"))
  (setq treemacs-width 50)
  (setq treemacs-no-png-images t)
  ;; Remove treemacs windows from ace-window ignored buffer list
  (with-eval-after-load 'ace-window
    (when (boundp 'aw-ignored-buffers)
      (setq aw-ignored-buffers (remove 'treemacs-mode aw-ignored-buffers))))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'simple))

;; Init treemacs-evil for treemacs and evil integration
(use-package treemacs-evil
  :after ( treemacs evil))

;; Init treemacs-magit for treemacs and magit integration
(use-package treemacs-magit
  :after ( treemacs magit))

;; Init dired-narrow for narrowing dired results using regexp
(use-package dired-narrow
  :general
  ( :keymaps 'dired-mode-map
    :states 'normal
    "/" #'dired-narrow-fuzzy))

;; Init nhexl-mode for editing binary files
(use-package nhexl-mode
  :demand t
  :general
  ( :definer 'minor-mode
    :keymaps 'nhexl-mode
    :states 'motion
    "j" #'nhexl-next-line
    "k" #'nhexl-previous-line
    "l" #'forward-char
    "h" #'backward-char
    "^" #'nhexl-move-beginning-of-line
    "$" #'nhexl-move-end-of-line)
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "x" #'nhexl-mode)
  :custom
  ( nhexl-group-size 4)
  ( nhexl-line-width 32))

;; Init calc for Emacs built-in calculator
(use-package calc
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "C" #'calc)
  ( :keymaps 'mo-quick-menu-map
    "C" #'quick-calc))

;; Init literate-calc-mode for inline calculations
(use-package literate-calc-mode
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "C" #'literate-calc-eval-buffer))

;; Init proced for viewing and managing running processes
(use-package proced
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "p" #'proced)
  :config
  (defvar mo-proced-instant-cpu-usage-cache nil
    "Instantaneous process CPU usage cache.")

  (defun mo-proced-instant-cpu-usage (attributes)
    "Add instantaneous process CPU usage percentage to proced process attributes."
    ;; Run top and cache process CPU usage if needed
    (when (or (not (car mo-proced-instant-cpu-usage-cache))
              (time-less-p (seconds-to-time 1)
                           (time-subtract nil (car mo-proced-instant-cpu-usage-cache))))
      (setq mo-proced-instant-cpu-usage-cache
            (cons (current-time)
                  (let ((cpu-data (make-hash-table)))
                    (with-temp-buffer
                      (insert (shell-command-to-string "top -bn1"))
                      (goto-char (point-min))
                      (when (search-forward-regexp "^[[:blank:]]*PID[[:blank:]]" nil t)
                        (forward-line 1))
                      (while (not (eobp))
                        (when-let* ((line (split-string (thing-at-point 'line t) nil t))
                                    (pid (string-to-number (nth 0 line)))
                                    (cpu-usage (string-to-number (nth 8 line))))
                          (puthash pid cpu-usage cpu-data))
                        (forward-line 1)))
                    cpu-data))))
    ;; Find CPU usage by PID
    (cons 'picpu
          (when-let* ((pid (cdr (assoc 'pid attributes))))
            (gethash pid (cdr mo-proced-instant-cpu-usage-cache)))))

  ;; Add instantaneous CPU usage stats
  (when (executable-find "top")
    (add-to-list 'proced-grammar-alist
                 '( picpu "%ICPU" proced-format-cpu right proced-< t ( picpu pid) ( nil t t)))
    (add-to-list 'proced-custom-attributes #'mo-proced-instant-cpu-usage)
    (setq-default proced-sort '( picpu pcpu)))

  (setq-default proced-filter 'all)
  (setq proced-enable-color-flag t)
  (add-to-list 'proced-format-alist '( custom start etime time utime
                                       stime vsize thcount pri nice
                                       group user pid ppid pcpu picpu
                                       pmem rss state (args comm)))
  (setq-default proced-format 'custom))

;; Init profiler for profiling lisp code
(use-package profiler
  :straight nil
  :general
  ( :keymaps 'override
    "<f7>" #'profiler-start
    "<f8>" #'profiler-stop
    "<f9>" #'profiler-report))

;; Init vc for version control functionality
(use-package vc
  :straight nil
  :config
  (setq vc-follow-symlinks t))

;; Init vc-git for configuring the git VC backend
(use-package vc-git
  :straight nil
  :config
  (setq vc-git-diff-switches '( "--histogram")))

;; Init git-commit for editing git commit messages
;; This package is used by magit.
(use-package git-commit
  :demand t
  :straight nil
  :after magit
  :hook
  ( git-commit-setup . mo-git-commit-set-fill-column)
  :config
  (defun mo-git-commit-set-fill-column ()
    "Set fill-column in git-commit mode."
    (setq fill-column 72)))

;; Init magit for a better git user experience
(use-package magit
  :demand t
  ;; Refine diff view to show sub hunk changes
  :general
  ;; Visit files in the other window
  ( :keymaps 'mo-quick-menu-map
    :prefix "g"
    "g" #'magit-status
    "G" #'magit-clone
    "i" #'magit-init
    "s" #'magit-stage
    "u" #'magit-unstage
    "c" #'magit-commit
    "d" #'magit-diff-buffer-file
    "b" #'magit-blame-addition
    "C" #'magit-file-checkout
    "l" #'magit-log-buffer-file
    "k" #'magit-toggle-buffer-lock
    "f" #'magit-file-dispatch
    "F" #'magit-find-file)
  ( :keymaps 'magit-diff-section-base-map
    "C-<return>" #'magit-diff-visit-worktree-file-other-window)
  ( :keymaps 'magit-mode-map
    :states '( normal)
    "y n" #'mo-magit-yank-branch-name)
  :init
  (setq magit-define-global-key-bindings nil)
  :config
  (defun mo-magit-yank-branch-name ()
    "Yank the current branch name."
    (interactive)
    (let ((branch (magit-get-current-branch)))
      (if branch
          (progn (kill-new branch)
                 (message "%s" branch))
        (user-error "There is not current branch"))))
  (setq magit-diff-refine-hunk 'all)
  (setq magit-delete-by-moving-to-trash nil))

;; Init magit-tbdiff for using git-range-diff command in magit
(use-package magit-tbdiff
  :after magit)

;; Init git-modes for editing git config files
(use-package git-modes)

;; Init smerge for helping with git merges
(use-package smerge-mode
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    "m" '( :keymap smerge-basic-map :package smerge)))

;; Init forge for working with git forges (e.g. Github, Gitlab)
(use-package forge
  :after magit
  :custom
  ( forge-database-file (mo-cache-path "forge-database.sqlite")))

;; Init ghub for working with various git forges
(use-package ghub
  :defines ghub-graphql-items-per-request
  :config
  ;; Lower per request item limit to avoid hitting service rate limits
  (setq ghub-graphql-items-per-request 20))

;; Init github-review for helping with code review on github
(use-package github-review
  :general
  ( :keymaps 'github-review-mode-map
    "C-M-s-a" #'github-review-approve
    "C-M-s-c" #'github-review-comment
    "C-M-s-r" #'github-review-reject)
  ( :keymaps 'mo-quick-menu-map
    :prefix "g"
    "r" #'github-review-forge-pr-at-point)
  :config
  (setq github-review-view-comments-in-code-lines t)
  (setq github-review-reply-inline-comments t))

;; Init diff for diff functionality
(use-package diff
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "-" #'diff-buffers))

;; Init ediff for better diff view and commands
(use-package ediff
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "=" #'ediff-buffers)
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "+" #'ediff-regions-linewise)
  ( :keymaps 'mo-quick-menu-map
    :prefix "f"
    "=" #'ediff-files)
  :hook
  ( after-enable-theme . mo-ediff-configure-theme)
  :init
  ;; Ignore space changes
  (setq ediff-diff-options "-b")
  :config
  (defun mo-ediff-configure-theme ()
    "Set ace-window theme configuration."
    (set-face-attribute 'ediff-even-diff-A nil :inherit nil :background "DarkSlateGray"))
  ;; Open ediff window in the current frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Set ediff to show diff changes in character-level
  (setq ediff-forward-word-function #'forward-char)
  ;; Split windows horizontally
  (setq ediff-split-window-function #'split-window-horizontally))

;; Init ztree for comparing folder content
(use-package ztree
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "f"
    "z" #'ztree-diff))

;; Init diff-hl for highlighting uncommitted changes
(use-package diff-hl
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "g"
    "R" #'diff-hl-set-reference-rev
    "n" #'diff-hl-next-hunk
    "p" #'diff-hl-previous-hunk)
  :hook
  ( magit-pre-refresh . diff-hl-magit-pre-refresh)
  ( magit-post-refresh . diff-hl-magit-post-refresh)
  ( dired-mode . diff-hl-dired-mode-unless-remote)
  :config
  (setq diff-hl-side 'right)
  (global-diff-hl-mode))

;; Init git-link for creating URLs for files in web git services
(use-package git-link
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "g"
    "L" #'git-link)
  :config
  (setq git-link-use-commit t))

;; Init elisp-mode for editing and running lisp code
(use-package elisp-mode
  :straight nil
  :general
  ( :keymaps '( emacs-lisp-mode-map lisp-interaction-mode-map)
    "C-M-s-e" #'eval-defun
    "C-M-s-s" #'eval-last-sexp)
  :config
  (advice-add 'eval-last-sexp :around #'mo-evil-correct-last-sexp))

;; Init sly for Common Lisp support
(use-package sly
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "s"
    "s" #'sly
    "S" #'sly-quit-lisp
    "c" #'sly-connect
    "C" #'sly-disconnect)
  ( :keymaps 'sly-mode-map
    "C-M-s-e" #'sly-eval-defun
    "C-M-s-s" #'sly-eval-last-expression
    "C-M-s-r" #'sly-eval-region
    "C-M-s-b" #'sly-eval-buffer
    "C-M-s-l" #'sly-load-file
    "C-M-s-d" #'sly-compile-defun
    "C-M-s-<return>" #'sly-expand-1)
  ( :keymaps 'sly-mrepl-mode-map
    "C-r" #'consult-history
    "C-M-s-c" #'sly-mrepl-clear-recent-output
    "C-M-s-S-c" #'sly-mrepl-clear-repl
    "C-<return>" #'sly-mrepl-return)
  (mo-quick-menu-definer
    :keymaps 'sly-mode-map
    "h h" #'sly-describe-symbol
    "c d" #'sly-hyperspec-lookup
    ";" #'sly-edit-definition
    "'" #'sly-edit-uses)
  :custom
  ( inferior-lisp-program "sbcl")
  ( sly-db-focus-debugger t)
  :config
  (advice-add 'sly-eval-last-expression :around #'mo-evil-correct-last-sexp))

;; Init sly-quicklisp for quicklisp support in sly
(use-package sly-quicklisp
  :general
  ( :keymaps 'sly-mode-map
    "C-M-s-q" #'sly-quickload))

;; Init sly-asdf for asdf support in sly
(use-package sly-asdf
  :general
  ( :keymaps 'sly-mode-map
    "C-M-s-a" #'sly-asdf-open-system
    "C-M-s-S-l" #'sly-asdf-load-system
    "C-M-s-t" #'sly-asdf-test-system))

;; Init ielm for lisp REPL
(use-package ielm
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "l"
    "r" #'ielm))

;; Init edebug for debugging lisp code
(use-package edebug
  :general
  ( :keymaps 'emacs-lisp-mode-map
    "C-M-s-d" #'edebug-defun
    "C-M-s-S-d" #'edebug-remove-instrumentation
    "C-M-s-i" #'edebug-set-initial-mode
    "C-M-s-t" #'mo-edebug-toggle-trace)
  :config
  (defun mo-edebug-toggle-trace ()
    "Toggle edebug trace setting."
    (interactive)
    (message "edebug-trace: %s "
             (setq edebug-trace (not edebug-trace)))))

;; Init treesit for tree-sitter support in Emacs
(use-package treesit
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "_" #'treesit-explore-mode
    "-" #'treesit-inspect-node-at-point)
  :config
  (setq treesit-font-lock-level 4))

;; Init treesit-auto for automatically using tree-sitter major modes
(use-package treesit-auto
  :defines treesit-auto-langs
  :functions global-treesit-auto-mode
  :custom
  ( treesit-auto-install t)
  :config
  ;; Do not auto enable treesit for the following languages
  (setq treesit-auto-langs
        (cl-set-difference treesit-auto-langs '( rust
                                                 c
                                                 cpp
                                                 c-sharp)))
  (global-treesit-auto-mode))

;; Init lsp mode for lsp support
(use-package lsp-mode
  :after orderless
  :hook
  ( after-enable-theme . mo-lsp-configure-theme)
  :general
  ;; Set the lsp prefix key
  ( :keymaps 'lsp-mode-map
    "C-c l" '( :keymap lsp-command-map :which-key "lsp"))
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "l" #'lsp
    "L" #'lsp-disconnect
    "D" #'lsp-describe-session
    "a" #'lsp-execute-code-action
    "r" #'lsp-rename
    "p" #'lsp-signature-activate
    "n" #'lsp-inlay-hints-mode
    "s" #'lsp-toggle-symbol-highlight
    "R" #'lsp-workspace-restart
    "=" #'lsp-format-region
    "o" #'lsp-clangd-find-other-file)
  ( :keymaps 'mo-quick-menu-map
    "\"" #'lsp-find-implementation
    "(" #'lsp-ui-peek-find-implementation
    "`" #'lsp-ui-peek-find-references)
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "l" #'lsp-org)
  (mo-quick-menu-definer
    :definer 'minor-mode
    :keymaps 'lsp-mode
    "h h" #'lsp-describe-thing-at-point)
  ( :keymaps 'rustic-mode-map
    "C-M-s-c" #'lsp-rust-analyzer-open-cargo-toml)
  (mo-quick-menu-definer
    :keymaps 'rustic-mode-map
    "h H" #'lsp-rust-analyzer-open-external-docs)
  :init
  ;; No completion provider as we use corfu
  (setq lsp-completion-provider :none)
  ;; Set a high read output max value for handling large language server responses
  (setq read-process-output-max (* 10 1024 1024))
  ;; Reduce the max number of files to watch before showing warning
  (setq lsp-file-watch-threshold 500)
  ;; Set a short delay for refreshing state after moving the cursor
  (setq lsp-idle-delay 0.2)
  ;; Enable which-key help on the lsp prefix key
  (setq lsp-keymap-prefix "C-c l")
  ;; Enable for the following modes
  (setq mo-lsp-enable-for-modes '( c-mode
                                   c-ts-mode
                                   c++-mode
                                   c++-ts-mode
                                   objc-mode
                                   swift-mode
                                   haskell-mode
                                   haskell-literate-mode
                                   go-mode
                                   go-ts-mode
                                   csharp-mode
                                   csharp-ts-mode
                                   java-mode
                                   java-ts-mode
                                   (python-mode (lambda () (require 'lsp-pyright)))
                                   (python-ts-mode (lambda () (require 'lsp-pyright)))
                                   ruby-ts-mode
                                   js-ts-mode
                                   typescript-ts-mode
                                   tsx-ts-mode
                                   julia-mode
                                   groovy-mode
                                   web-mode
                                   js-json-mode
                                   json-ts-mode
                                   yaml-mode
                                   yaml-ts-mode
                                   powershell-mode
                                   dockerfile-mode
                                   dockerfile-ts-mode
                                   nix-mode
                                   terraform-mode
                                   cmake-mode
                                   cmake-ts-mode
                                   sh-mode
                                   bash-ts-mode))

  (defvar mo-lsp-recursion-flag nil
    "Flag used for detecting recursion when enabling lsp.")
  (defun mo-maybe-enable-lsp ()
    "If mode in LSP-CONFIG is equal to the current major-mode,
run the attached function (if exists) and enable lsp"
    (unless lsp-mode ; Do not load if lsp is already loaded
      (unless (string-match "\\.~.+?~$" (buffer-name)) ; Do not load in magit diff buffers
        (if mo-lsp-recursion-flag
            (message "LSP recursion detected in %s" (buffer-name))
          (let ((mo-lsp-recursion-flag t))
            (seq-find (lambda (mode-config)
                        (pcase mode-config
                          (`( ,(pred (equal major-mode)) ,func) (funcall func) (lsp-deferred) t)
                          ((pred (equal major-mode)) (lsp-deferred) t)))
                      mo-lsp-enable-for-modes))))))

  ;; Kill language server after the last associated buffer was closed
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-session-file (mo-cache-path "lsp-session-v1"))
  (setq lsp-eslint-library-choices-file (mo-cache-path ".lsp-eslint-choices"))
  ;; Increase log size
  (setq lsp-log-max 100000)
  ;; Always ask before executing auto actions
  (setq lsp-auto-execute-action nil)
  ;; Force lsp mode to forget the workspace folders for multi root servers
  ;; so the folders are added on demand
  (advice-add 'lsp :before #'mo-lsp-forget-workspace-folders)
  ;; Flatten imenu results and show their respective types
  (setq lsp-imenu-index-function #'lsp-imenu-create-categorized-index)
  ;; Enable semantic token highlighting
  (setq lsp-semantic-tokens-enable t)
  ;; When completion is triggered inside symbols, prefer to insert than replace
  (setq lsp-completion-default-behaviour :insert)
  ;; Disable lenses
  (setq lsp-lens-enable nil)
  ;; Limit the number of signature doc lines
  (setq lsp-signature-doc-lines 5)
  ;; Set clangd default parameters
  (setq lsp-clients-clangd-args '( "--header-insertion-decorators=0"
                                   "--completion-style=detailed"))
  ;; Use clippy as the default linter
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; Set completion style to orderless
  (defun mo-lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '( orderless)))

  ;; Move lsp status from global mode string to modeline
  (defun mo-lsp-mode-move-global-string-to-mode-line (func &rest args)
    "Advice function that moves lsp global string modifications to modeline."
    (make-local-variable 'global-mode-string)

    ;; Make sure global-mode-string is a list
    (unless (listp global-mode-string)
      (setq global-mode-string (list global-mode-string)))

    (let ((global-mode-string-before global-mode-string)
          (global-mode-string (append mo-lsp-mode-mode-line global-mode-string)))
      (apply func args)
      (setq mo-lsp-mode-mode-line
            (cl-set-difference global-mode-string global-mode-string-before :test #'equal))))

  (advice-add 'lsp-modeline-code-actions-mode :around
              #'mo-lsp-mode-move-global-string-to-mode-line)
  (advice-add 'lsp-modeline-diagnostics-mode :around
              #'mo-lsp-mode-move-global-string-to-mode-line)
  (advice-add 'lsp-modeline--enable-workspace-status :around
              #'mo-lsp-mode-move-global-string-to-mode-line)
  (advice-add 'lsp-modeline--disable-workspace-status :around
              #'mo-lsp-mode-move-global-string-to-mode-line)
  (advice-add 'lsp-on-progress-modeline :around
              #'mo-lsp-mode-move-global-string-to-mode-line)
  :hook
  ;; Postpone lsp load for after dir local vars are read
  ;; Do not load lsp if dir local vars are not enabled (e.g. on preview)
  ( hack-local-variables . mo-lsp-enable-when-local-vars-enabled)

  ;; Setup completion to use orderless
  ( lsp-completion-mode . mo-lsp-mode-setup-completion)
  ;; Temporarily disable the breadcrumb headerline when the buffer is in vdiff-mode.
  ( vdiff-mode . mo-lsp-disable-breadcrumb-on-vdiff)
  :config
  (defun mo-lsp-disable-breadcrumb-on-vdiff ()
    "Disable lsp breadcrumb when using vdiff."
    (when (and lsp-mode lsp-headerline-breadcrumb-enable)
      (if vdiff-mode
          (lsp-headerline-breadcrumb-mode -1)
        (lsp-headerline-breadcrumb-mode))))
  (defun mo-lsp-enable-when-local-vars-enabled ()
    "Enable lsp mode if local variables are enabled."
    (when enable-dir-local-variables
      (mo-maybe-enable-lsp)))
  (defun mo-lsp-forget-workspace-folders (&rest _args)
    "Force lsp mode to forget the workspace folders for multi root servers."
    (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht))))
  (defun mo-lsp-configure-theme ()
    "Set lsp-mode theme configuration."
    ;; Distinguish between var reads and writes by underlining lsp write highlights
    (set-face-attribute 'lsp-face-highlight-write nil :underline t))
  :commands lsp-deferred)

;; Init lsp-ui for an interactive lsp interface
(use-package lsp-ui
  :after lsp-mode
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "I" #'lsp-ui-imenu
    "d" #'lsp-ui-doc-glance
    ";" #'lsp-ui-doc-focus-frame)
  :config
  ;; Do not show documentation automatically
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-max-height 60)
  ;; Show only symbols in headerline breadcrumb
  (setq lsp-headerline-breadcrumb-segments '(symbols)))

;; Init lsp-treemacs for an interactive lsp tree-like interface
(use-package lsp-treemacs
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "'" #'lsp-treemacs-call-hierarchy
    "\"" #'mo-lsp-treemacs-outgoing-call-hierarchy
    "t" #'lsp-treemacs-type-hierarchy
    "i" #'lsp-treemacs-symbols)
  :config
  (defun mo-lsp-treemacs-outgoing-call-hierarchy ()
    "Show the outgoing call hierarchy for the symbol at point."
    (interactive)
    (let ((current-prefix-arg '( 4)))
      (call-interactively #'lsp-treemacs-call-hierarchy)))

  (setf (alist-get 'side lsp-treemacs-symbols-position-params) 'right))

;; Init dape for interactive debugging
(use-package dape
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "d"
    "d" #'dape
    "c" #'dape-continue
    "n" #'dape-next
    "i" #'dape-step-in
    "o" #'dape-step-out
    "t" #'dape-select-thread
    "b b" #'dape-breakpoint-toggle
    "b e" #'dape-breakpoint-expression
    "b t" #'dape-breakpoint-log
    "b l" #'dape-breakpoint-load
    "b s" #'dape-breakpoint-save
    "b r" #'dape-breakpoint-remove-all
    "s" #'mo-dape-select-stack-ordered
    "SPC" #'dape-stack-select-down
    "RET" #'dape-stack-select-up
    "e" #'dape-evaluate-expression
    "r" #'dape-repl
    "R" #'dape-restart
    "m" #'dape-read-memory
    "p" #'dape-pause
    "k" #'dape-kill
    "w" #'dape-watch-dwim
    "q" #'dape-quit
    "Q" #'dape-disconnect-quit
    ";" #'dape-info)
  ( :keymaps 'dape-info-parent-mode-map
    "<backtab>" #'dape--info-buffer-tab)
  ( :keymaps 'dape-repl-mode-map
    "C-<return>" #'newline)
  ( :keymaps 'override
    "<f1>" #'dape-next
    "<f2>" #'dape-step-in
    "<f3>" #'dape-step-out
    "<f4>" #'dape-continue
    "M-<f4>" #'dape-pause
    "<f5>" #'dape-breakpoint-toggle)
  :hook
  ;; Kill compile buffer on build success
  ( dape-compile-compile . kill-buffer)
  ;; Save buffers on startup, useful for interpreted languages
  ( dape-on-start . mo-dape--save-on-start)

  :custom
  ( dape-adapter-dir (mo-cache-path "debug-adapters"))
  ( dape-buffer-window-arrangement 'right)
  ( dape-info-variable-table-row-config
    `( (name . 0) (value . 0) (type . 0)))
  ( dape-request-timeout 60)
  ( dape-stack-trace-levels 1000) ; Retrieve all stack frames
  ( dape-default-breakpoints-file (mo-cache-path "dape-breakpoints"))

  :config
  (defun dape-read-pid ()
    "Read pid of active processes if possible."
    (if-let ((pids (list-system-processes)))
        (let ((collection
               (mapcar (lambda (pid)
                         (let ((args (alist-get 'args (process-attributes pid))))
                           (cons (concat
                                  (format "%d" pid)
                                  (when args
                                    (format ": %s" args)))
                                 pid)))
                       pids)))
          (alist-get (completing-read "Pid: " collection)
                     collection nil nil 'equal))
      (read-number "Pid: ")))

  (defun mo-dape-select-stack-ordered ()
    "Dape select stack with stack ordering according to execution."
    (interactive)
    (let ((vertico-sort-function nil))
      (call-interactively #'dape-select-stack)))

  (defun mo-dape--save-on-start ()
    "Save dape buffers."
    (save-some-buffers t t)))

;; Init dap-mode for interactive debugging
(use-package dap-mode
  :custom
  ( dap-breakpoints-file (mo-cache-path ".dap-breakpoints")))

;; Init flycheck for on-the-fly syntax checking
(use-package flycheck
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "]" #'flycheck-next-error
    "[" #'flycheck-previous-error)
  :init
  (global-flycheck-mode)
  :config
  ;; Update diagnostics on switching to buffer
  (add-to-list 'flycheck-check-syntax-automatically 'idle-buffer-switch))

;; Init consult-lsp for additional interacitve lsp commands
(use-package consult-lsp
  :after ( lsp-mode consult)
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "E" #'consult-lsp-diagnostics
    "/" #'consult-lsp-file-symbols
    "." #'mo-consult-lsp-project-symbols
    "," #'mo-consult-lsp-symbols-unsorted)
  :config

  (defun mo-consult-lsp-symbols-unsorted ()
    "Query workspace symbols, disabling vertico sorting."
    (interactive)
    (let ((vertico-sort-function nil))
      (call-interactively #'consult-lsp-symbols)))

  (defun mo-consult-lsp-project-symbols (arg)
    "Query project symbols. When ARG is set through prefix, query all workspaces."
    (interactive "P")
    (let* ((initial "")
           (all-workspaces? arg)
           (ws (or (and all-workspaces? (-uniq
                                         (-flatten
                                          (ht-values
                                           (lsp-session-folder->servers
                                            (lsp-session))))))
                   (lsp-workspaces)
                   (lsp-get (lsp-session-folder->servers (lsp-session))
                            (lsp-workspace-root default-directory)))))
      (unless ws
        (user-error "There is no active workspace!"))
      (consult--read
       (thread-first
         (consult--async-sink)
         (consult--async-refresh-immediate)
         (consult--async-map consult-lsp-symbols-transformer-function)
         (consult--async-filter #'mo-consult-lsp-project-symbol-filter)
         (consult-lsp--symbols--make-async-source ws)
         (consult--async-throttle)
         (consult--async-split))
       :prompt "LSP Symbols "
       :annotate (funcall consult-lsp-symbols-annotate-builder-function)
       :require-match t
       :history t
       :add-history (consult--async-split-thingatpt 'symbol)
       :initial (consult--async-split-initial initial)
       :category 'consult-lsp-symbols
       :lookup #'consult--lookup-candidate
       :group (consult--type-group consult-lsp-symbols-narrow)
       :narrow (consult--type-narrow consult-lsp-symbols-narrow)
       :state (consult-lsp--symbols--state))))

  (defun mo-consult-lsp-project-symbol-filter (symbol-info)
    "Filter SYMBOL-INFO that is only part of the current project."
    (let ((file (lsp--uri-to-path
                 (lsp:location-uri
                  (lsp:symbol-information-location symbol-info))))
          (project-dir (project-root (project-current t))))
      (file-in-directory-p file project-dir)))

  ;; Manual preview key for symbols results
  (consult-customize consult-lsp-symbols :preview-key "M-."))

;; Init compile for compiling from Emacs
(use-package compile
  :after term
  :straight nil
  :config
  (setq compilation-scroll-output t)
  (defun mo-colorize-compilation-buffer ()
    "Called to colorize the compilation buffer."
    (ansi-color-apply-on-region compilation-filter-start (point)))
  :hook
  ( compilation-filter . mo-colorize-compilation-buffer))

;; Init rmsbolt for a compiler explorer in Emacs
(use-package rmsbolt
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "b" #'rmsbolt))

;; Init highlight-defined for highlighting Emacs Lisp symbols
(use-package highlight-defined
  :hook ( emacs-lisp-mode . highlight-defined-mode))

;; Init highlight-quoted for highlighting Emacs Lisp quoted symbols
(use-package highlight-quoted
  :hook ( emacs-lisp-mode . highlight-quoted-mode))

;; Init cc-mode for C/C++/Obj-C support
(use-package cc-mode
  :straight nil
  :general
  ( :keymaps 'c-mode-base-map
    "C-M-s-S-a" #'c-beginning-of-defun
    "C-M-s-S-e" #'c-end-of-defun
    "C-M-s-n" #'c-forward-conditional
    "C-M-s-p" #'c-backward-conditional
    "C-M-s-u" #'c-up-conditional
    "C-M-s-z" #'c-display-defun-name
    "C-M-s-a" #'c-beginning-of-statement
    "C-M-s-e" #'c-end-of-statement)
  :config
  ;; Associate objc-mode with Objective C files
  (add-to-list 'auto-mode-alist '( "\\.mm\\'" . objc-mode))
  ;; Set the default style
  (setf (alist-get 'other c-default-style) "stroustrup"))

;; Init c-ts-mode for C support using tree-sitter
(use-package c-ts-mode
  :straight nil
  :general
  ( :keymaps 'c-ts-mode-map
    "C-M-s-S-a" #'treesit-beginning-of-defun
    "C-M-s-S-e" #'treesit-end-of-defun)
  :config
  (setq c-ts-mode-indent-style 'bsd)
  (setq c-ts-mode-indent-offset 4))

;; Init c++-ts-mode for C++ support using tree-sitter
(use-package c++-ts-mode
  :straight nil
  :general
  ( :keymaps 'c++-ts-mode-map
    "C-M-s-S-a" #'treesit-beginning-of-defun
    "C-M-s-S-e" #'treesit-end-of-defun))

;; Init google-c-style for Google's C/C++ style
(use-package google-c-style
  :defines google-c-style
  :config
  (c-add-style "Google" google-c-style))

;; Init yasnippets for adding code snippet templates
(use-package yasnippet
  :demand t
  :functions yas-global-mode
  :general
  ( :keymaps 'override
    "C-(" #'yas-insert-snippet)
  ( :keymaps 'yas-keymap
    [tab] nil
    "TAB" nil
    [shift tab] nil
    [backtab] nil
    "M-n" (yas-filtered-definition #'yas-next-field)
    "M-p" (yas-filtered-definition #'yas-prev-field))
  :config
  (yas-global-mode 1))

;; Init yasnippet-snippets for common code templates
(use-package yasnippet-snippets)

;; Init rust-mode for Rust support
(use-package rust-mode
  :defines rust-mode-treesitter-derive
  :init
  (setq rust-mode-treesitter-derive t))

;; Init rustic for extended Rust support
(use-package rustic
  :after rust-mode
  :general
  ( :keymaps 'rustic-mode-map
    "C-M-s-;" #'rustic-docstring-dwim
    "C-M-s-b" #'rustic-cargo-build
    "C-M-s-t" #'rustic-cargo-test
    "C-M-s-S-t" #'rustic-cargo-current-test
    "C-M-s-o" #'rustic-cargo-outdated
    "C-M-s-f" #'rustic-cargo-clippy-fix
    "C-M-s-S-f" #'rustic-cargo-clippy
    "C-M-s-a" #'rustic-cargo-add
    "C-M-s-S-a" #'rustic-beginning-of-defun
    "C-M-s-S-e" #'rustic-end-of-defun
    "C-M-s-m" #'rustic-cargo-bench
    "C-M-s-x" #'rustic-cargo-clean
    "C-M-s-h" #'rustic-cargo-doc
    "C-M-s-i" #'rustic-cargo-init
    "C-M-s-n" #'rustic-cargo-new
    "C-M-s-S-r" #'rustic-cargo-rm
    "C-M-s-u" #'rustic-cargo-upgrade
    "C-M-s-d" #'rust-dbg-wrap-or-unwrap
    "C-M-s-p" #'rustic-popup)
  (mo-quick-menu-definer
    :keymaps 'rustic-mode-map
    "c RET" #'rustic-cargo-run
    "c c" #'rustic-recompile
    "c C" #'rustic-compile)
  :config
  ;; Add mode to markdown code block highlighting (needed for docs)
  (add-to-list 'markdown-code-lang-modes '( "rust" . rustic-mode))
  (setq rustic-compile-directory-method #'rustic-buffer-workspace))

;; Init swift-mode for Swift support
(use-package swift-mode)

;; Init lsp-sourcekit for SourceKit language server
(use-package lsp-sourcekit
  :after lsp-mode
  :custom
  ( lsp-sourcekit-executable
    (string-trim
     (if (eq system-type 'darwin)
         (shell-command-to-string "xcrun --find sourcekit-lsp")
       "sourcekit-lsp"))))

;; Init haskell-mode for Haskell support
(use-package haskell-mode)

;; Init lsp-haskell for Haskell language server
(use-package lsp-haskell)

;; Init go-mode for Go support
(use-package go-mode)

;; Init csproj-mode for editing C# project files
(use-package csproj-mode)

;; Init lsp-java for Eclipse JDT language server
(use-package lsp-java
  :defer t
  :custom
  ( lsp-java-workspace-dir (mo-cache-path "workspace")))

;; Init python for Python support
(use-package python
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "r" #'run-python)
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i"))

;; Init lsp-pyright for pyright python language server
(use-package lsp-pyright)

;; Init pip-requirements for editing pip requirements files
(use-package pip-requirements)

;; Init pyvenv for activating python virtual environments
(use-package pyvenv)

;; Init pipenv for supporting pipenv projects and commands
(use-package pipenv
  :hook ( python-mode . pipenv-mode)
  :custom
  ;; We don't use projectile
  ( pipenv-with-projectile nil))

;; Init typescript-ts-mode for typescript support
(use-package typescript-ts-mode
  :straight nil
  :mode "\\.ts\\'")

;; Init julia-mode for Julia support
(use-package julia-mode)

;; Init lsp-julia for Julia language server
(use-package lsp-julia
  :custom
  ( lsp-julia-default-environment "~/.julia/environments/v1.8"))

;; Init groovy-mode for Groovy support
(use-package groovy-mode)

;; Init jenkinsfile-mode for editing Jenkins files
(use-package jenkinsfile-mode)

;; Init web-mode for enhanced web files editing
(use-package web-mode
  :mode "\\.html?\\'" "\\.tsx\\'")

;; Init yaml-mode for enhanced YAML editing
(use-package yaml-mode)

;; Init powershell for editing PowerShell files
(use-package powershell)

;; Init dockerfile-mode for editing docker files
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; Init docker for managing docker from Emacs
(use-package docker
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "d" #'docker))

;; Init nix-mode for editing nix files
(use-package nix-mode
  :mode "\\.nix\\'")

;; Init terraform-mode for editing terraform files
(use-package terraform-mode)

;; Init protobuf-mode for editing protobuf files
(use-package protobuf-mode)

;; Init csv-mode for editing CSV files
(use-package csv-mode)

;; Init sql-indent for indenting SQL statements
(use-package sql-indent)

;; Init markdown-mode for enhanced markdown editing
(use-package markdown-mode)

;; Init cmake-mode for editing CMake files
(use-package cmake-mode)

;; Init bazel for editing Bazel related files
(use-package bazel)

;; Init just-mode for editing just files
(use-package just-mode)

;; Init restclient for sending rest requests from emacs
(use-package restclient)

;; Init logview for better viewing log files
(use-package logview
  :general
  ( :keymaps 'logview-mode-map
    "C-M-s-n" #'logview-add-include-name-filter
    "C-M-s-S-n" #'logview-add-exclude-name-filter
    "C-M-s-m" #'logview-add-include-message-filter
    "C-M-s-S-m" #'logview-add-exclude-message-filter
    "C-M-s-t" #'logview-add-include-thread-filter
    "C-M-s-S-t" #'logview-add-exclude-thread-filter
    "C-M-s-f" #'logview-edit-filters
    "C-M-s-r" #'logview-reset-all-filters
    "C-M-s-j" #'logview-next-timestamp-gap
    "C-M-s-k" #'logview-previous-timestamp-gap
    "C-M-s-l C-M-s-d" #'logview-show-errors-warnings-information-and-debug
    "C-M-s-l C-M-s-e" #'logview-show-only-errors
    "C-M-s-l C-M-s-i" #'logview-show-errors-warnings-and-information
    "C-M-s-l C-M-s-l" #'logview-show-all-levels
    "C-M-s-l C-M-s-w" #'logview-show-errors-and-warnings)
  :config
  (setq logview-views-file (mo-cache-path "logview.views"))
  (setq logview-cache-filename (mo-cache-path "logview-cache.extmap")))

;; Init syslog-mode for viewing syslog and strace files
(use-package syslog-mode)

;; Init journalctl-mode for viewing journalctl output
(use-package journalctl-mode
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "j" #'journalctl))

;; Init edit-indirect for editing regions in separate buffers
(use-package edit-indirect)

;; Init apheleia for code formatting
(use-package apheleia
  :demand t
  :commands apheleia-global-mode
  :defines apheleia-mode-alist
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "f" #'apheleia-mode
    "F" #'apheleia-format-buffer)
  :config
  (add-to-list 'apheleia-mode-alist '(lisp-data-mode . lisp-indent))
  (apheleia-global-mode))

;; Init dtrt-indent for auto indentation detection
(use-package dtrt-indent
  :defines dtrt-indent-hook-mapping-list
  :hook
  ( prog-mode . dtrt-indent-mode)
  :custom
  ( dtrt-indent-verbosity 0)
  :config
  (setq dtrt-indent-hook-mapping-list
        (append dtrt-indent-hook-mapping-list
                '( ( c-ts-mode c/c++/java c-ts-mode-indent-offset)
                   ( c++-ts-mode c/c++/java c-ts-mode-indent-offset)
                   ( java-ts-mode c/c++/java c-ts-mode-indent-offset)
                   ( json-ts-mode c/c++/java c-ts-mode-indent-offset)))))

;; Init indent-bars for showing indentation guides
(use-package indent-bars
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "|" #'indent-bars-mode))

;; Init display-fill-column-indicator for displaying fill column indicator
(use-package display-fill-column-indicator
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "\\" #'display-fill-column-indicator-mode))

;; Init editorconfig for applying EditorConfig settings
(use-package editorconfig
  :functions editorconfig-mode
  :config
  (editorconfig-mode 1))

;; Init url for url related functionality
(use-package url
  :straight nil
  :config
  ;; Set url configuration directory
  (setq url-configuration-directory (mo-cache-path "url")))

;; Init browse-url for configuring default openers for URLs
(use-package browse-url
  :straight nil
  :custom
  ( browse-url-handlers '( ( "hyperspec/body" . eww)
	                       ( "." . browse-url-default-browser))))

;; Init request for a HTTP function library in lisp
(use-package request
  :custom
  ( request-storage-directory (mo-cache-path "request")))

;; Init which-key for interactively displaying key bindings
(use-package which-key
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "w t" #'which-key-show-top-level
    "w k" #'which-key-show-keymap
    "w K" #'which-key-show-full-keymap
    "w i" #'which-key-show-minor-mode-keymap
    "w I" #'which-key-show-full-minor-mode-keymap
    "w m" #'which-key-show-major-mode
    "w M" #'which-key-show-full-major-mode)
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-add-column-padding 8)
  (which-key-mode))

;; Init help for built-in help system
(use-package help
  :straight nil
  :general
  ( :keymaps 'override
    "<f10>" #'mo-toggle-messages-buffer)
  ;; Unbind global help menu shortcut binding
  ( :keymaps 'global
    "<f1>" nil)
  :config
  (defun mo-toggle-messages-buffer ()
    "Toggle *Messages* buffer."
    (interactive)
    (let ((window (get-buffer-window "*Messages*")))
      (if window
          (quit-window nil window)
        (view-echo-area-messages))))
  ;; Always select help windows
  (setq help-window-select t))

;; Init info for browsing documentation
(use-package info
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "i" #'info))

;; Init help-fns for help functions
(use-package help-fns
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "F" #'describe-face
    "s" #'describe-syntax
    "C" #'describe-char
    "B" #'describe-bindings)
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "M" #'describe-mode))

;; Init helpful for better lisp help
(use-package helpful
  :general
  ( "C-h f" #'helpful-callable)
  ( "C-h v" #'helpful-variable)
  ( "C-h k" #'helpful-key)
  ( "C-h F" #'helpful-function)
  ( "C-h C" #'helpful-command)
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "f" #'helpful-callable
    "c" #'helpful-command
    "v" #'helpful-variable
    "k" #'helpful-key
    "h" #'helpful-at-point
    "ESC" #'helpful-kill-buffers))

;; Init mode-minder for showing all modes
(use-package mode-minder
  :straight
  ( mode-minder :type git :host github :repo "jdtsmith/mode-minder")
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "M" #'mode-minder))

;; Init eldoc for viewing documentation in echo area
(use-package eldoc
  :straight nil
  :config
  (setq eldoc-idle-delay 0.2))

;; Init devdocs for viewing online dev documentation
(use-package devdocs
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "h" #'devdocs-lookup)
  :config
  (setq devdocs-data-dir (mo-cache-path "devdocs")))

;; Init re-builder for interactive regexp building
(use-package re-builder
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "x" #'re-builder)
  :config
  (setq reb-re-syntax 'string))

;; Init engine-mode for querying search engines
(use-package engine-mode
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "g" #'engine/search-google)
  :config
  (defengine google
    "https://www.google.com/search?q=%s"))

;; Init gptel for LLM support in Emacs
(use-package gptel
  :general
  ( :keymaps 'mo-quick-menu-map
    "TAB" #'gptel
    "<backtab>" #'gptel-send)
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "TAB" #'gptel-add)
  ( :keymaps 'mo-quick-menu-map
    :prefix "f"
    "TAB" #'gptel-add-file)
  ( :keymaps 'gptel-mode-map
    "C-<return>" #'gptel-send
    "C-<escape>" #'gptel-menu)
  ( :keymaps 'gptel-context-buffer-mode-map
    :states 'normal
    "RET" #'gptel-context-visit
    "C-M-s-c" #'gptel-context-confirm
    "C-M-s-k" #'gptel-context-quit
    "C-M-s-d" #'gptel-context-flag-deletion
    "C-M-s-n" #'gptel-context-next
    "C-M-s-p" #'gptel-context-previous)
  :custom
  ( gptel-default-mode 'org-mode)
  ( gptel-response-prefix-alist '( ( markdown-mode . "AI:\n")
                                   ( org-mode . "AI:\n")
                                   ( text-mode . "AI:\n")))
  :config

  (add-to-list 'gptel-directives
               '( reviewer . "You are a large language model and a careful programmer. \
Your purpose is to help review and improve code. \
Provide code changes as GNU diff format, followed by brief explanations for each change.")
               t)
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response))

;; Init elysium for applying AI generated changes
(use-package elysium
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "<backtab>" #'elysium-query))

;; Init copilot for copilot support in Emacs
(use-package copilot
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "TAB" #'copilot-mode)
  ( :keymaps 'copilot-completion-map
    "M-<return>" #'copilot-accept-completion
    "C-p" #'copilot-previous-completion
    "C-n" #'copilot-next-completion)
  :custom
  ( copilot-max-char -1)
  ( copilot-indent-offset-warning-disable t))

;; Init rainbow-delimiters for highlighting parens by their depth
(use-package rainbow-delimiters
  :hook
  ( prog-mode . rainbow-delimiters-mode)
  ( sly-mrepl . rainbow-delimiters-mode))

;; Init paren for showing matching parentheses
(use-package paren
  :straight nil
  :config
  ;; Show matching parentheses
  (setq show-paren-delay 0)
  (show-paren-mode 1))

;; Init elec-pair for auto insertion of matching parentheses in code
(use-package elec-pair
  :straight nil
  :hook
  ( prog-mode . electric-pair-local-mode)
  ( lisp-data-mode . mo-electric-pair-disable-local)
  :config
  (defun mo-electric-pair-disable-local ()
    "Disable electric pair local mode."
    (electric-pair-local-mode -1)))

;; Init rainbow-mode for highlighting color strings
(use-package rainbow-mode
  :hook
  ( prog-mode . rainbow-mode))

;; Init auto-highlight-symbol for auto highlighting symbols in code.
;; This can be used when no LSP based highlighting is available.
(use-package auto-highlight-symbol
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "H" #'auto-highlight-symbol-mode)
  :custom-face
  ( ahs-face ( ( nil ( :inherit 'highlight))))
  ( ahs-face-unfocused ( ( nil ( :inherit 'highlight))))
  ( ahs-plugin-default-face ( ( nil ( :inherit 'highlight))))
  ( ahs-plugin-default-face-unfocused ( ( nil ( :inherit 'highlight))))
  :config
  (setq ahs-idle-interval 0.2))

;; Init highlight-numbers for highlighting numbers in code
(use-package highlight-numbers
  :hook ( prog-mode . highlight-numbers-mode))

;; Init vi-tilde-fringe for marking empty lines on the margin column
(use-package vi-tilde-fringe
  :hook
  ( prog-mode . vi-tilde-fringe-mode))

;; Init evil-nerd-commenter for comment editing
(use-package evil-nerd-commenter
  :after evil
  :general
  ( :states '( normal, visual) "gc" #'evilnc-comment-operator))

;; Init string-inflection for changing style of word at point
(use-package string-inflection
  :general
  ( :states 'normal
    "z SPC" #'string-inflection-cycle))

;; Init hl-todo for highlighting specific keywords (e.g. TODO)
(use-package hl-todo
  :demand t
  :commands global-hl-todo-mode
  :config
  (global-hl-todo-mode))

;; Init ace-window for fast window selection
(use-package ace-window
  :general
  ( :keymaps 'override
    "M-o" #'ace-window
    "M-O" #'mo-ace-window-with-action
    "C-M-S-o" #'mo-ace-selected-window-prefix)
  :hook
  ( after-enable-theme . mo-ace-window-configure-theme)
  :config
  (defun mo-ace-window-configure-theme ()
    "Set ace-window theme configuration."
    (set-face-attribute 'aw-leading-char-face nil :height 2.0))

  (defun mo-ace-window-with-action ()
    "Select window with dispatch action."
    (interactive)
    (let ((aw-dispatch-always t))
      (call-interactively #'ace-window)))

  (defun mo-ace-selected-window-prefix ()
    "Display the buffer of the next command in the selected window."
    (interactive)
    (let ((other-window-lambda
           (lambda (window)
             (display-buffer-override-next-command
              (lambda (buffer alist)
                (cons window 'reuse))
              nil "[selected-window]")
             (message "Display next command buffer in the selected window..."))))
      (aw-select " Ace - Other Window Prefix" other-window-lambda)))

  (setq aw-keys '( ?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (when (display-graphic-p)
    (ace-window-posframe-mode)))

;; Init eww for browsing the web using Emacs
(use-package eww
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "w" #'mo-eww-new-buffer)
  :config
  (defun mo-eww-new-buffer ()
    "Open URL in a new buffer."
    (interactive)
    (let ((current-prefix-arg '( 4)))
      (call-interactively #'eww)))

  (setq eww-search-prefix "https://www.google.com/search?q="))

;; Init erc for accessing IRC through Emacs
(use-package erc
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "i" #'erc)
  :custom
  ( erc-server "irc.libera.chat")
  ( erc-port 6667)
  ;; Hide other users' state change messages
  ( erc-hide-list '("JOIN" "PART" "QUIT" "NICK")))

;; Init app-launcher for launching desktop apps
(use-package app-launcher
  :straight ( :type git :host github :repo "SebastienWae/app-launcher")
  :after orderless
  :general
  ( :keymaps 'mo-quick-menu-map
    "ESC" #'mo-app-launcher-run-app-literal-prefix)
  :config
  (defun mo-app-launcher-run-app-literal-prefix ()
    "Launch applications matched by the literal prefix style."
    (interactive)
    (let ((orderless-matching-styles '( orderless-literal-prefix)))
      (call-interactively #'app-launcher-run-app))))

;; Init executalbe for handling script execution
(use-package executable
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "f"
    "x" #'executable-interpret))

;; Init term for terminal support
(use-package term
  :demand t
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "t" #'ansi-term)
  ( :keymaps 'mo-quick-menu-map
    :prefix "DEL"
    "t" #'mo-ansi-term-project)
  :config
  (defun mo-ansi-term-project ()
    "Create an ansi-term buffer with current directory set to the active project root.
If project root cannot be found, use the buffer's default directory."
    (interactive)
    (let* ((default-directory (mo-get-buffer-dir)))
      (call-interactively #'ansi-term)))
  (defun mo-term-handle-exit (&optional process-name msg)
    "Close term buffer after process has exited."
    (message "%s | %s" process-name msg)
    (kill-buffer (current-buffer)))

  (advice-add 'term-handle-exit :after #'mo-term-handle-exit)
  (setq term-buffer-maximum-size 100000))

;; Init tramp-term for launching term sessions with tramp
(use-package tramp-term
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "T" #'tramp-term))

;; Init vterm for terminal emulation
(use-package vterm
  :demand t
  :if (not (eq system-type 'windows-nt))
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "DEL"
    "v" #'mo-vterm-project)
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "v" #'mo-vterm-file)
  :commands vterm
  :init
  (defun mo-vterm-file ()
    "Create a vterm buffer with current directory set to the current buffer default directory."
    (interactive)
    (vterm vterm-buffer-name))

  (defun mo-vterm-project ()
    "Create a vterm buffer with current directory set to the active project root.
If project root cannot be found, use the buffer's default directory."
    (interactive)
    (let* ((default-directory (mo-get-buffer-dir)))
      (vterm vterm-buffer-name)))
  ;; Always compile module
  (setq vterm-always-compile-module t)
  ;; Set a low response delay
  (setq vterm-timer-delay 0.02)
  ;; Set longer scrollback history
  (setq vterm-max-scrollback 50000)
  :config
  (setq vterm-tramp-shells (append vterm-tramp-shells '(("ssh" "/bin/bash") ("scp" "/bin/bash")))))

;; Init comint for general interpreter support
(use-package comint
  :demand t
  :straight nil
  :general
  ( :keymaps 'comint-mode-map
    "C-p" #'comint-previous-input
    "C-n" #'comint-next-input
    "C-M-s-c" #'comint-clear-buffer)
  :config
  ;; Prevent deleting the prompt
  (setq comint-prompt-read-only t)
  (setq comint-buffer-maximum-size 100000))

;; Init shell for terminal emulation
(use-package shell
  :demand t
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "S" #'shell-new)
  ( :keymaps 'mo-quick-menu-map
    :prefix "DEL"
    "S" #'mo-shell-project)
  :config
  (defun shell-new ()
    "Open a new instance of shell."
    (interactive)
    (shell (generate-new-buffer-name "*shell*")))

  (defun mo-shell-project ()
    "Create a shell buffer with current directory set to the active project root.
If project root cannot be found, use the buffer's default directory."
    (interactive)
    (let* ((default-directory (mo-get-buffer-dir)))
      (shell-new)))

  (setq shell-kill-buffer-on-exit t)
  (setq-default explicit-shell-file-name "/bin/bash"))

;; Init eshell for terminal emulation
(use-package eshell
  :straight nil
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "DEL"
    "e" #'project-eshell)
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "e" #'eshell-new)
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "X" #'eshell-command)
  ( :keymaps 'eshell-mode-map
    "C-p" #'eshell-previous-matching-input-from-input
    "C-n" #'eshell-next-matching-input-from-input)
  :config
  (defun eshell-new()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N))
  (setq eshell-history-size 100000)
  (setq eshell-buffer-maximum-lines 100000)
  (defun eshell/ecat (&optional file)
    "Like `cat' but output with Emacs syntax highlighting."
    (if (stringp file)
        (with-temp-buffer
          (insert-file-contents file)
          (let ((buffer-file-name file))
            (delay-mode-hooks
              (set-auto-mode)
              (if (fboundp 'font-lock-ensure)
                  (font-lock-ensure)
                (with-no-warnings
                  (font-lock-fontify-buffer)))))
          (buffer-string))
      (error "Error: No file name was provided")))
  ;; Set shell prompt
  (setq eshell-prompt-function
        (lambda ()
          (concat (propertize (format-time-string "[%H:%M:%S] " (current-time))
                              'face '( :foreground "plum3" :weight bold))
                  (propertize (let ((dir-name (file-name-nondirectory
                                               (abbreviate-file-name (eshell/pwd)))))
                                (if (= (length dir-name) 0) "/" dir-name ))
                              'face '( :foreground "LightSeaGreen" :weight bold))
                  (propertize (if (= (user-uid) 0) " #" " λ")
                              'face (if (= eshell-last-command-status 0)
                                        '( :foreground "SteelBlue3" :weight bold)
                                      '( :foreground "coral3" :weight bold)))
                  " ")))
  (setq eshell-prompt-regexp "^[^#λ\n]* [#λ] ")
  ;; Remove login banner
  (delq 'eshell-banner eshell-modules-list)
  ;; Set eshell cache directory
  (setq eshell-directory-name (file-name-as-directory (mo-cache-path "eshell")))
  ;; Clear visual command list as we use eat for terminal emulation
  (setq eshell-visual-commands nil))

;; Init bash-completion for shell completions based on bash completion
(use-package bash-completion
  :after ( eshell)
  :demand t
  :commands ( bash-completion-setup)
  :functions ( bash-completion-dynamic-complete-nocomint
               eshell-bol)
  :hook ( eshell-mode . mo-bash-completion-setup-eshell)
  :config
  (defun mo-bash-completion-setup-eshell ()
    "Setup bash completions in eshell."
    (add-hook 'completion-at-point-functions
              'mo-bash-completion-eshell-capf nil t))

  (defun mo-bash-completion-eshell-capf ()
    "Bash completion function for eshell."
    (let ((compl (bash-completion-dynamic-complete-nocomint
                  (save-excursion (eshell-bol))
                  (point) t)))
      (when compl
        (append compl '(:exclusive no)))))

  (bash-completion-setup))

;; Init copy-as-format for copying regions as formatted code
(use-package copy-as-format
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "y" #'mo-copy-as-format-ask)
  :config
  (defun mo-copy-as-format-ask ()
    "Copy as format. Always ask for the format."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'copy-as-format))))

;; Init eat for terminal emulation
(use-package eat
  :demand t
  :after eshell
  :straight ( :files ("*.el" ("term" "term/*.el") "*.texi"
                      "*.ti" ("integration" "integration/*")
                      (:exclude ".dir-locals.el" "*-tests.el")))
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "s" #'eat)
  ( :keymaps 'mo-quick-menu-map
    :prefix "DEL"
    "s" #'eat-project)
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "h" #'mo-run-htop)
  :custom
  ( eat-shell "/bin/zsh")
  ( eat-kill-buffer-on-exit t)
  :config
  (defun mo-run-htop ()
    "Run htop command."
    (interactive)
    (eat "htop"))

  (defun mo-eat-check-compile-terminfo ()
    "If needed, compile eat terminfo databases."
    (unless (file-directory-p eat-term-terminfo-directory)
      (eat-compile-terminfo)))

  (mo-eat-check-compile-terminfo)
  (eat-eshell-mode))

;; Init kind-icon for icon support in auto completion
(use-package kind-icon
  :after corfu
  :defines corfu-margin-formatters
  :functions kind-icon-margin-formatter
  :custom
  ;; Compute blended backgrounds correctly
  ( kind-icon-default-face 'corfu-default)
  ;; Don't use icons, but text symbols
  ( kind-icon-use-icons nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Init emojify for emoji support
(use-package emojify
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "E" #'emojify-mode
    "e" #'emojify-insert-emoji)
  :config
  (setq emojify-emojis-dir (mo-cache-path "emojis")))

;; Init doom one theme
(use-package doom-themes
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "v"
    "v" #'mo-toggle-light-dark-themes)
  :hook ( after-enable-theme . mo-doom-themes-configure-theme)
  :config
  (defun mo-doom-themes-configure-theme ()
    "Set doom-themes configuration on theme change."
    ;; Make tab-bar text visible
    (set-face-attribute 'tab-bar nil :foreground (doom-color 'fg)))
  (defun mo-toggle-light-dark-themes ()
    "Toggle between light and dark themes"
    (interactive)
    (let ((theme (if (eq (car custom-enabled-themes) 'doom-one)
                     'doom-nord-light
                   'doom-one)))
      (mapc #'disable-theme custom-enabled-themes)
      (if (custom-theme-p theme)
          (enable-theme theme)
        (load-theme theme t))))
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Set brighter comments
  (setq doom-one-brighter-comments t)
  (setq doom-nord-light-brighter-comments t)
  (setq doom-one-light-brighter-comments t)
  (setq doom-solarized-light-brighter-comments t)
  (load-theme 'doom-one t)
  (mo-doom-themes-configure-theme)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Init solaire-mode for visually highlighting file backed buffers
(use-package solaire-mode
  :functions solaire-global-mode
  :config
  (solaire-global-mode +1))

;; Init minions for collapsing the minor mode indicator in the modeline
(use-package minions
  :functions minions-mode
  :config
  (minions-mode 1))

;; Init flyspell for spell checking
(use-package flyspell
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "S" #'flyspell-mode)
  :hook
  ;; Enable spell checking
  ( text-mode . flyspell-mode)
  ( prog-mode . flyspell-prog-mode))

;; Init flyspell-correct for spell correction
(use-package flyspell-correct
  :demand t
  :general
  ( :keymaps 'override
    "M-+" #'flyspell-correct-wrapper))

;; Init consult-flyspell for incorporating flyspell into consult
(use-package consult-flyspell
  :after flyspell-correct
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "s" #'consult-flyspell)
  :config
  (setq consult-flyspell-select-function 'flyspell-correct-at-point))

;; Init powerthesaurus for finding synonyms, antonyms and related terms
(use-package powerthesaurus
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "T" #'powerthesaurus-transient))

;; Init desktop+ for saving session configuration
(use-package desktop+
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "v"
    "d" #'desktop+-load
    "D" #'desktop+-create)
  :commands desktop+-create
  :init
  (setq desktop+-base-dir (mo-cache-path "desktops"))

  (defun mo-ask-save-desktop ()
    "If desktop save mode is not activated, ask the user to save the session"
    (unless desktop-save-mode
      (when (y-or-n-p "Save the current session? ")
        (call-interactively #'desktop+-create)))
    t))

;; Init zoom-window for toggling window zoom
(use-package zoom-window
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "w"
    "z" #'zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color "#3b404e"))

;; Init faces for face related functionality
(use-package faces
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "l" #'read-color))

;; Init face-remap for remapping face properties
(use-package face-remap
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "v"
    "z" #'global-text-scale-adjust
    "Z" #'text-scale-adjust))

;; Init focus for dimming surrounding text
(use-package focus
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "o o" #'focus-mode
    "o p" #'focus-pin
    "o u" #'focus-unpin
    "o c" #'focus-change-thing))

;; Init writeroom-mode for distraction free writing mode
(use-package writeroom-mode
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "w" #'writeroom-mode)
  :config
  (setq writeroom-maximize-window nil)
  (setq writeroom-width 100))

;; Init buffer-move for moving buffers between windows
(use-package buffer-move
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "w"
    "h" #'buf-move-left
    "j" #'buf-move-down
    "k" #'buf-move-up
    "l" #'buf-move-right)
  :config
  ;; We want to move, not to swap, the buffer
  (setq buffer-move-behavior 'move))

;; Init popper for managing popup windows
(use-package popper
  :after tab-bar
  :demand t
  :general
  ( :keymaps 'override
    "C-`" #'popper-toggle
    "M-`" #'popper-cycle
    "C-M-`" #'popper-cycle-backwards
    "M-~" #'popper-toggle-type
    "C-~" #'mo-popper-change-window-height)
  :init
  (defvar mo-popper-exclude-grouping '( "*Messages*" "*Warnings*" "*scratch*")
    "Buffer names to exclude from popper grouping.")

  (defun mo-popper-group-by-tab ()
    "Return an identifier (tab name) to group popups."
    (let ((buf-name (buffer-name)))
      ;; Group buffers if not in exclusion list
      (unless (member buf-name mo-popper-exclude-grouping)
        ;; We cannot simply use the current tab, as this function can be called during
        ;; tab switching, and that will incorrectly attach the popup to the newly
        ;; switched tab.
        (let ((tab-name (alist-get 'name (tab-bar-get-buffer-tab nil))))
          ;; Sometimes this function is called after the current buffer is hidden. In this
          ;; case, get the current tab. A null tab-name will not be returned when switching
          ;; tabs, hence there is no risk with just getting the current tab.
          (or tab-name
              (alist-get 'name (tab-bar--current-tab)))))))

  (defvar mo-popper-window-heights '( 0.2 0.33 0.5 0.66)
    "Popper window heights.")

  (defvar mo-popper-current-window-height-idx 1
    "An index into `mo-popper-window-heights' selecting the current popper window height.")

  (defun mo-popper-change-window-height (arg)
    "Toggle between lower and higher popup window heights."
    (interactive "P")
    (when popper-open-popup-alist
      (setq mo-popper-current-window-height-idx
            (mod (funcall (if arg '1- '1+) mo-popper-current-window-height-idx)
                 (length mo-popper-window-heights)))
      (setq popper-window-height
            (nth mo-popper-current-window-height-idx
                 mo-popper-window-heights))
      ;; Refresh popup
      (popper-toggle)
      (popper-toggle)))

  (setq popper-reference-buffers
        '( "\\*Messages\\*"
           "\\*Warnings\\*"
           "\\*scratch\\*"
           "Output\\*$"
           "\\*Async Shell Command\\*"
           "\\*ielm\\*"
           "^\\*HTTP Response .*\\*$"
           "^\\*.* Annotation\\*$"
           "\\*sly-description\\*"
           world-clock-mode
           compilation-mode
           rustic-compilation-mode
           rustic-cargo-run-mode
           rustic-cargo-plain-run-mode
           rustic-cargo-test-mode
           rustic-cargo-clippy-mode
           rustic-rustfix-mode
           rustic-cargo-install-mode
           rustic-cargo-expand-mode
           sly-mrepl-mode
           inferior-python-mode
           eat-mode
           "^\\*\\(.+-\\)?eshell\\*.*$" eshell-mode
           "^\\*shell.*\\*.*$" shell-mode
           "^\\*term.*\\*$" term-mode
           "^\\*vterm.*\\*.*$" vterm-mode))
  ;; Set fractional height
  (setq popper-window-height (nth mo-popper-current-window-height-idx
                                  mo-popper-window-heights))
  ;; Group popups by tabs
  (setq popper-group-function #'mo-popper-group-by-tab)
  (popper-mode +1)
  (popper-echo-mode +1))

;; Init transient for transient menus
(use-package transient
  :init
  (setq transient-levels-file (mo-cache-path "transient_levels.el"))
  (setq transient-values-file (mo-cache-path "transient_values.el"))
  (setq transient-history-file (mo-cache-path "transient_history.el")))

;; Enable winner-mode for window management
(use-package winner
  :straight nil
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "w"
    "u" #'winner-undo
    "r" #'winner-redo)
  :init
  (setq winner-dont-bind-my-keys t)
  :config
  (winner-mode 1))

;; Init follow-mode for scrolling buffer on multiple windows
(use-package follow-mode
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "v"
    "F" #'follow-mode))

;; Init time for showing time in the modeline
(use-package time
  :demand t
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "W" #'world-clock)
  :config
  ;; Remove average load time indicator from the modeline
  (setq display-time-default-load-average nil)
  ;; Add spacing at the beginning of the time format string
  (push " " display-time-string-forms)
  (display-time-mode 1))

;; Init alarm-clock for an alarm clock in Emacs
(use-package alarm-clock
  :functions alarm-clock-turn-autosave-on
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "a" #'alarm-clock-set
    "A" #'alarm-clock-list-view)
  :custom
  ( alarm-clock-cache-file (mo-cache-path ".alarm-clock.cache"))
  ( alarm-clock-play-sound nil)
  :config
  (alarm-clock-turn-autosave-on))

;; Init midnight for executing tasks during night time
(use-package midnight
  :demand t
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "DEL" #'clean-buffer-list)
  :config
  ;; Kill run code command buffers frequently
  (add-to-list 'clean-buffer-list-kill-regexps "^\\*Run Code Command\\*.*$")
  ;; Set to run at 4:30am
  (midnight-delay-set 'midnight-delay "16200"))

;; Init hi-lock for highlighting lines by regexp
(use-package hi-lock
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "l" #'highlight-lines-matching-regexp
    "L" #'unhighlight-regexp))

;; Init hilit-chg for highlighting changes in buffer
(use-package hilit-chg
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "c" #'highlight-changes-mode))

;; Init hl-line for highlighting the current line
(use-package hl-line
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "h" #'hl-line-mode)
  :hook
  ( text-mode . hl-line-mode)
  ( prog-mode . hl-line-mode))

;; Init whitespace for showing trailing whitespaces in code
(use-package whitespace
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "W" #'whitespace-mode)
  :config
  (setq whitespace-style '( face trailing))
  :hook
  ( prog-mode . whitespace-mode))

;; Init visual-fill-column for mimicking fill-column in visual-line-mode
(use-package visual-fill-column
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "V" #'visual-fill-column-mode))

;; Init pixel-scroll for smooth scrolling
(use-package pixel-scroll
  :straight nil
  :config
  (pixel-scroll-precision-mode))

;; Init server for running Emacs as a server
(use-package server
  :straight nil
  :config
  ;; Start Emacs server
  (server-start))

;; Init savehist for minibuffer history persistence over Emacs restarts
(use-package savehist
  :straight nil
  :config
  (setq savehist-file (mo-cache-path "history"))
  (savehist-mode))

;; Init saveplace for saving the last visited location in buffers
(use-package saveplace
  :straight nil
  :config
  (setq save-place-file (mo-cache-path "places"))
  (save-place-mode))

;; Init recentf for tracking recently opened files
(use-package recentf
  :straight nil
  :config
  (defun mo-recentf-file-remote-no-sudo-p (file)
    "Returns t if file is remote, except for local sudo paths."
    (and (string-match tramp-file-name-regexp file)
         (not (string-match "^/sudo:root@" file))))
  (setq recentf-save-file (mo-cache-path "recentf"))
  ;; Enlarge the max size of the recent files list
  (setq recentf-max-saved-items 10000)
  ;; Do not save remote files
  (add-to-list 'recentf-exclude #'mo-recentf-file-remote-no-sudo-p)
  ;; Do not check readability of remote files.
  ;; This is needed in order to prevent tramp from hanging Emacs when killing a buffer.
  (add-to-list 'recentf-keep #'file-remote-p)
  (recentf-mode t))

;; Init persist for persistent variables between sessions
(use-package persist
  :defines persist--directory-location
  :init
  (setq persist--directory-location (mo-cache-path "persist")))

;; Init persistent-scratch for preserving scratch buffer across sessions
(use-package persistent-scratch
  :demand t
  :commands persistent-scratch-setup-default
  :custom
  ( persistent-scratch-save-file (mo-cache-path ".persistent-scratch"))
  :config
  (persistent-scratch-setup-default))

;; Init autorevert for updating buffers that were changed on disk
(use-package autorevert
  :straight nil
  :config
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode))

;; Init bookmark for managing bookmarks
(use-package bookmark
  :demand t
  :straight nil
  :hook
  ;; Recenter after jump
  ( bookmark-after-jump . recenter)
  :config
  (setq bookmark-file (mo-cache-path "bookmarks")))

;; Init bookmark+ for enhanced bookmark functionality
(use-package bookmark+
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "<insert>"
    "RET" #'bookmark-bmenu-list
    "SPC" #'bmkp-bookmark-set-confirm-overwrite
    "f" #'bmkp-file-target-set
    "d" #'bmkp-delete-bookmarks
    "e" #'bmkp-edit-bookmark-name-and-location
    "E" #'bmkp-edit-bookmark-record
    "a" #'bookmark-show-annotation
    "A" #'bookmark-edit-annotation))

;; Init bookmark-in-project for managing per-project bookmarks
(use-package bookmark-in-project
  :general
  ( :keymaps 'mo-quick-menu-map
    "<deletechar>" #'bookmark-in-project-jump)
  :custom
  ( bookmark-in-project-project-root
    (lambda () (project-root (project-current)))))

;; Init tramp for accessing remote files
(use-package tramp
  :after files
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "a"
    "c" #'tramp-cleanup-all-buffers)
  ( :keymaps 'mo-quick-menu-map
    :prefix "a"
    "C" #'tramp-cleanup-connection)
  :hook
  ;; Disable auto-save
  ( find-file . mo-tramp-disable-auto-save)
  :config
  (defun mo-tramp-disable-auto-save ()
    "Disable auto-save when using TRAMP."
    (when (file-remote-p (buffer-file-name))
      (auto-save-mode -1)))
  (defun mo-tramp-disable-backup-predicate (name)
    "A predicate for disabling backup when using TRAMP."
    (not (file-remote-p name)))
  ;; Set default method to ssh as it is faster than scp
  (setq tramp-default-method "ssh")
  ;; Lower verbosity to avoid connection messages in the echo area
  (setq tramp-verbose 2)
  ;; Disable backup
  (setq backup-enable-predicate #'mo-tramp-disable-backup-predicate)
  ;; Preserve remote path value
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Init tramp-cache for caching in tramp
(use-package tramp-cache
  :straight nil
  :config
  (setq tramp-persistency-file-name (mo-cache-path "tramp")))

;; Init tramp-gvfs for gvfs support in tramp
(use-package tramp-gvfs
  :straight nil
  :config
  (setq tramp-gvfs-enabled t))

;; Load customization file
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
