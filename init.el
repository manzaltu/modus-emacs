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
    "f" '( :which-key "File")
    "v" '( :which-key "View")
    "w" '( :which-key "Window")
    "x" '( :which-key "Utils")
    "z" '( :which-key "Repeat")
    "t" '( :which-key "Tab")
    "h" '( :which-key "Help")
    "RET" '( :which-key "Project")
    "c" '( :which-key "Code")
    "d" '( :which-key "Debug")
    "l" '( :which-key "Lisp")
    "g" '( :which-key "Git")
    "m" '( :which-key "Merge")
    "r" '( :which-key "Multiple Cursors")
    "n" '( :which-key "Notes")))

;; Init evil mode for Vim emulation in Emacs
(use-package evil
  :demand t
  :general
  ( :states 'motion
    "C-S-d" #'evil-scroll-up)
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
  ( :keymaps 'evil-ex-search-keymap
    "C-f" nil
    "C-b" nil
    "C-a" nil
    "C-." #'evil-ex-search-command-window)
  ( :states 'motion
    ;; We want C-<num> for jumping between tabs
    "C-6" nil)
  :hook
  ;; Recenter after jump
  ( evil-jumps-post-jump . recenter)
  :init
  ;; Needed for evil-collection
  (setq evil-want-keybinding nil)
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
  ( :keymaps 'emacs-lisp-mode-map
    "C-M-s-r" #'eval-region)
  :config
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
  (setq history-length 100000)
  ;; Enable scrolling left
  (put 'scroll-left 'disabled nil))

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
    "w" #'write-file)
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
  ;; Don't create backup and autosave files
  (setq make-backup-files nil)
  (setq auto-save-default nil)
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
  (blink-cursor-mode 0))

;; Init window for managing windows
(use-package window
  :demand t
  :straight nil
  :general
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
  ;; Don't split windows vertically by default
  (setq split-height-threshold nil)
  ;; A fast key binding for showing the next command's result in another window.
  ;; Make sure it also works when the command is using 'switch-to-buffer'.
  (setq switch-to-buffer-obey-display-actions t))

;; Init simple for basic and general Emacs commands
(use-package simple
  :demand t
  :straight nil
  :general
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

  ;; Set a wide enough default fill-column
  (setq-default fill-column 100)
  ;; Disable default tab indentation
  (setq-default indent-tabs-mode nil)
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
    "i" #'ibuffer))

;; Init bufler for an improved ibuffer
(use-package bufler
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "SPC" #'bufler
    "RET" #'bufler-switch-buffer))

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
    "p" #'kmacro-cycle-ring-previous))

;; Init macros for controlling macros
(use-package macros
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "k"
    "q" #'kbd-macro-query))

;; Init modus-operandi-emacs for non-package related functionality
(use-package modus-operandi-emacs
  :after ( simple project tab-bar)
  :straight nil
  :no-require t ; Not a package
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "p" #'mo-copy-file-path)
  ( :keymaps 'mo-quick-menu-map
    :prefix "RET"
    "SPC" #'mo-open-project-with-tab
    "ESC" #'mo-close-project-with-tab)
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
          (tab-bar-rename-tab (project-name (project-current))))
      (quit
       (tab-bar-close-tab))))

  (defun mo-close-project-with-tab ()
    "Kill project buffers and close tab.
Tab is named after the project's name."
    (interactive)
    (call-interactively #'project-kill-buffers)
    (tab-bar-close-tab))

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

  (defun mo-show-welcome-screen ()
    "Show welcome screen."
    (with-current-buffer (get-buffer-create "*Welcome*")
      (setq truncate-lines t)
      (let* ((buffer-read-only)
             (image-path (expand-file-name "emacs.png" user-emacs-directory))
             (image (create-image image-path))
             (size (image-size image))
             (height (cdr size))
             (width (car size))
             (top-margin (floor (/ (- (window-height) height) 2)))
             (left-margin (floor (/ (- (window-width) width) 2)))
             (title "Welcome to Modus Operandi Emacs!")
             (subtitle "In Absentia Lucis, Tenebrae Vincunt."))
        (setq mode-line-format nil)
        (goto-char (point-min))
        (insert (make-string top-margin ?\n ))
        (insert (make-string left-margin ?\ ))
        (insert-image image)
        (insert "\n\n\n")
        (insert (make-string (floor (/ (- (window-width) (string-width title)) 2)) ?\ ))
        (insert (propertize title 'face 'bold))
        (insert "\n")
        (insert (make-string (floor (/ (- (window-width) (string-width subtitle)) 2)) ?\ ))
        (insert (propertize subtitle 'face 'font-lock-comment-face))
        (goto-char (point-min)))
      (switch-to-buffer (current-buffer))
      (read-only-mode +1)))

  (when (< (length command-line-args) 2)
    (add-hook 'emacs-startup-hook (lambda ()
                                    (when (display-graphic-p)
                                      (mo-show-welcome-screen)))))

  ;; When killing a modified buffer, show the changes
  (add-to-list 'kill-buffer-query-functions #'mo-show-modified-buffer-changes t))

;; Add evil key bindings to other, non-default, modes
(use-package evil-collection
  :after ( evil xref magit)
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    ")" #'evil-collection-consult-jump-list)
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
  ( after-enable-theme .
    (lambda()
      ;; Better highlight the leading characters
      (set-face-attribute 'avy-lead-face nil :background "gold2")
      (set-face-attribute 'avy-lead-face-0 nil :background "gold3")
      (set-face-attribute 'avy-lead-face-1 nil :background "gold4")
      (set-face-attribute 'avy-lead-face-2 nil :background "DarkGoldenrod4")))
  :config
  :general
  ( :keymaps 'override
    "C-'" #'avy-goto-char-timer)
  :config
  (setq avy-single-candidate-jump nil)
  (setq avy-all-windows 'all-frames)
  (setq avy-timeout-seconds 0.25))

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
  :bind ( "C-=" . er/expand-region))

;; Init evil-numbers for increasing/decreasing number at point
(use-package evil-numbers
  :general
  ( :states '( normal visual)
    "z i" #'evil-numbers/inc-at-pt
    "z d" #'evil-numbers/dec-at-pt))

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
  :config
  ;; Jump list to work as a stack
  (setq better-jumper-add-jump-behavior 'replace)
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
  ;; Increase xref marker stack length
  (setq xref-marker-ring-length 100))

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
    "C-M-s-c" #'org-store-link
    "C-M-s-S-l" #'org-latex-preview
    "C-M-s-p" #'org-toggle-pretty-entities
    "C-M-s-s" #'org-schedule
    "C-M-s-d" #'org-deadline
    "C-M-s-t" #'org-todo
    "C-M-s-g" #'org-set-tags-command
    "C-M-s-i" #'org-insert-structure-template
    "C-M-s-e" #'org-edit-special
    "C-M-s-S-e" #'org-export-dispatch
    "C-M-s-<return>" #'org-open-at-point)
  ( :keymaps 'org-src-mode-map
    "C-M-s-;" #'org-edit-src-exit)
  ( :keymaps 'org-agenda-mode-map
    "C-M-s-s" #'org-agenda-schedule
    "C-M-s-d" #'org-agenda-deadline
    "C-M-s-g" #'org-agenda-set-tags)
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "c" #'org-capture
    "s" #'org-store-link
    "a" #'mo-org-agenda-and-todo)
  ( :keymaps 'org-mode-map
    :states 'normal
    "TAB" #'org-cycle)
  ;; Close any loaded org buffer when exiting the agenda buffer
  ( :keymaps 'org-agenda-mode-map
    "q" #'org-agenda-exit)
  :hook
  ( after-enable-theme .
    (lambda ()
      ;; Resize org headings and agenda dates
      (dolist (face '( ( org-document-title . 1.2)
                       ( org-level-1 . 1.1)
                       ( org-level-2 . 1.0)
                       ( org-level-3 . 1.0)
                       ( org-level-4 . 1.0)
                       ( org-level-5 . 1.0)
                       ( org-level-6 . 1.0)
                       ( org-level-7 . 1.0)
                       ( org-level-8 . 1.0)
                       ( org-agenda-date . 1.1)
                       ( org-agenda-date-today . 1.1)
                       ( org-agenda-date-weekend . 1.1)
                       ( org-agenda-date-weekend-today . 1.1)))
        (set-face-attribute (car face) nil :height (cdr face)))
      ;; Make sure certain org faces always use the fixed-pitch face
      (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-code nil :inherit '( shadow fixed-pitch))
      (set-face-attribute 'org-verbatim nil :inherit '( shadow fixed-pitch))
      (set-face-attribute 'org-special-keyword nil :inherit '( font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-meta-line nil :inherit '( font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)))
  :config
  (defun mo-org-agenda-and-todo ()
    "Open org agenda with all TODOs"
    (interactive)
    (org-agenda nil "n"))
  ;; Visually indent text under bullets
  (setq org-startup-indented t)
  ;; Allow resizing inline images
  (setq org-image-actual-width nil)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))
  (setq org-directory "~/org")
  (setq org-default-notes-file (concat (file-name-as-directory org-directory) "notes.org"))
  (setq org-capture-templates '( ( "t" "Task" entry (file+headline org-default-notes-file "Tasks")
                                   "** TODO %? \nSCHEDULED: %t")))
  ;; Unfold everything on startup, except for things that set to be hidden
  ;; by default (e.g. drawers)
  (setq org-startup-folded 'showall)
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
  (setq org-agenda-files `( ,org-directory))
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-include-diary t)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-diary-file
        (concat (file-name-as-directory org-directory) "diary.org"))
  (setq org-id-locations-file (mo-cache-path ".org-id-locations"))
  (setq org-src-preserve-indentation t)
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

;; Init org-contrib for org add-ons
(use-package org-contrib)

;; Init ox-confluence for exporting org to confluence
(use-package ox-confluence
  :after org-contrib
  :straight nil)

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

;; Init org-super-agenda for grouping agenda items into sections
(use-package org-super-agenda
  :after org
  :demand t
  :defines org-super-agenda-header-map
  :functions org-super-agenda-mode
  :hook
  ( after-enable-theme .
    (lambda ()
      (set-face-attribute 'org-super-agenda-header nil
                          :foreground "HotPink")))
  :custom
  ;; Enable auto grouping
  ( org-super-agenda-groups '((:auto-group t) (:auto-category t)))
  ( org-super-agenda-final-group-separator "\n")
  :config
  ;; Delete super agenda key map to support evil mode
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode))

;; Init org-roam for Zettelkasten note management
(use-package org-roam
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
    "n" #'org-roam-capture)
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

;; Init consult-org-roam for better searching in org-roam notes
(use-package consult-org-roam
  :demand t
  :after org-roam
  :functions consult-org-roam-mode
  :general
  ( :keymaps 'org-mode-map
    "C-M-s-r C-M-s-b" #'consult-org-roam-backlinks
    "C-M-s-r C-M-s-f" #'consult-org-roam-forward-links)
  :custom
  ;; Disable consult-buffer integration, use this package only for note previewing
  (consult-org-roam-buffer-enabled nil)
  :config
  (consult-org-roam-mode))

;; Init org-modern for a modern org buffer style
(use-package org-modern
  :functions global-org-modern-mode
  :custom
  ;; We disable prettifying tables as currently it is not pixel-aligned
  ( org-modern-table nil)
  ( org-modern-star '( "●" "◉" "◎" "◍" "◌"))
  ( org-modern-list '( ( 43 . "▸") ( 45 . "▹") ( 42 . "◦")))
  ( org-modern-hide-stars nil)
  :config
  (global-org-modern-mode))

;; Init org-appear to show invisible org elements on cursor hover
(use-package org-appear
  :hook ( org-mode . org-appear-mode))

;; Init org-pomodoro for using the Pomodoro technique with org mode
(use-package org-pomodoro
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "p" #'org-pomodoro))

;; Init org-present for creating presentations based on org mode
(use-package org-present
  :after ( visual-fill-column org-modern)
  :general
  ( :keymaps 'org-mode-map
    "C-M-s-;" #'org-present)
  :hook
  ;; Set style of slides
  ( org-present-mode . mo-enter-present-mode)
  ;; Undo style changes
  ( org-present-mode-quit . mo-exit-present-mode)
  :init
  (defun mo-enter-present-mode ()
    "Initialize settings for present mode."
    ;; Disable spellchecking
    (flyspell-mode 0)
    ;; Center presentation and wrap lines
    (setq visual-fill-column-width 150)
    (setq visual-fill-column-center-text t)
    (visual-fill-column-mode 1)
    (visual-line-mode 1)
    ;; Present images
    (org-display-inline-images)
    ;; Make slides not modifiable
    (org-present-read-only)
    ;; Create a space at the beginning of the slide
    (setq header-line-format " ")
    ;; Add space between lines
    (setq line-spacing 0.5)
    ;; Change org level font sizes
    (setq-local face-remapping-alist
                '( ( default ( :height 2.0) default)
                   ( header-line ( :height 3.0) default)
                   ( org-document-title ( :height 1.75) org-document-title)
                   ( org-block ( :height 0.75) org-block))))
  (defun mo-exit-present-mode ()
    "Restore settings for non-present mode."
    (setq-local face-remapping-alist '( ( default default default)))
    (setq line-spacing nil)
    (setq header-line-format nil)
    (org-present-read-write)
    (org-remove-inline-images)
    (visual-line-mode 0)
    (visual-fill-column-mode 0)
    (setq visual-fill-column-center-text nil)
    (setq visual-fill-column-width nil)
    (flyspell-mode 1))
  :config
  ;; Enable heading stars
  (setq org-present-hide-stars-in-headings nil))

;; Init org-download for downloading and embedding images in org mode
(use-package org-download)

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
  :after ( org org-roam)
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "," #'consult-notes-search-in-all-notes
    "n" #'consult-notes)
  :config
  (setq consult-notes-file-dir-sources '( ( "Org" ?o "~/org")))
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

;; Init emacs-async for async processing in Emacs
(use-package emacs-async
  ;; This is not a loadable package
  :no-require t
  :demand t
  :functions dired-async-mode
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "a"
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
  ( orderless-style-dispatchers '( mo-orderless-exclude-dispatcher))
  ( completion-styles '( orderless basic))
  :config
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '( ( file ( styles basic partial-completion))))

  ;; Add exclude pattern style
  (defun mo-orderless-exclude-dispatcher (pattern _index _total)
    "Handle orderless exclude pattern."
    (cond
     ((equal "!" pattern)
      '( orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `( orderless-without-literal . ,(substring pattern 1))))))

;; Init vertico for item list selection
(use-package vertico
  ;; Load extensions
  :straight ( :files ( :defaults "extensions/*"))
  :functions vertico-mode
  :custom
  ( vertico-count 20)
  ( vertico-cycle t)
  :config
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
    ;; We don't want next/previous line to change selection
    "<remap> <next-line>" nil
    "<remap> <previous-line>" nil)
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
  ( eshell-mode . (lambda () (setq-local corfu-auto nil) (corfu-mode)))
  ;; Disable auto mode in shell
  ( shell-mode . (lambda () (setq-local corfu-auto nil) (corfu-mode)))
  ;; Close popup when exiting evil insert state
  ( evil-insert-state-exit . corfu-quit)
  :config
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
  ( :keymaps 'mo-quick-menu-map
    :prefix "RET"
    "w" #'mo-project-save
    "W" #'project-forget-project
    "d" #'project-dired
    "f" #'mo-project-find-file
    "x" #'project-async-shell-command
    "k" #'project-kill-buffers
    "p" #'project-switch-project
    "i" #'project-list-buffers
    "r" #'mo-reload-dir-locals-project)
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "c" #'mo-project-recompile
    "C" #'project-compile)
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
    "TAB" #'consult-bookmark)
  ( :keymaps 'mo-quick-menu-map
    :prefix "RET"
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
    "m" #'consult-minor-mode-menu
    "B" #'consult-recent-file)
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
  ( "C-~" #'consult-register-load)
  ( "M-~" #'consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ( "C-M-~" #'consult-register)
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
  ( :keymaps 'mo-quick-menu-map
    :prefix "RET"
    "b" #'consult-project-buffer)
  ( :keymaps 'mo-quick-menu-map
    "*" #'mo-consult-line-symbol-at-point)
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

  (evil-set-command-property #'mo-consult-line-symbol-at-point :jump t)

  (defun mo-consult-ripgrep-current-dir ()
    "Call consult-ripgrep on buffer's directory."
    (interactive)
    (consult-ripgrep default-directory))

  (defun mo-consult-fd-current-dir ()
    "Call consult-fd on buffer's directory."
    (interactive)
    (consult-fd default-directory)))

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
  :functions marginalia-mode
  :custom
  ( marginalia-field-width 200)
  :config
  (marginalia-mode))

;; Init embark for enabling contextual actions
(use-package embark
  :general
  ( :keymaps 'override
    "C-;" #'embark-act
    "C-:" #'embark-collect
    "C-)" #'embark-export)
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
    "<f5>" #'toggle-debug-on-quit
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
  :straight nil
  :init
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
  :config
  ;; Disable tab bar buttons
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-button-show nil)
  ;; Customize the tab bar
  (setq tab-bar-format '( tab-bar-format-tabs-groups
                          tab-bar-separator))
  (setq tab-bar-tab-hints t)
  ;; Switch to tab by pressing C-<hint num>
  (setq tab-bar-select-tab-modifiers '(control))
  ;; Init tab-bar for supporting multiple window layouts in frame
  (tab-bar-mode)
  ;; Set initial tab name
  (tab-bar-rename-tab "Initial"))

;; Init dired for file management
(use-package dired
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "f"
    "d" #'dired-jump
    "o" #'mo-open-with)
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

;; Init treemacs for a tree-like sidebar file navigator
(use-package treemacs
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "RET"
    ;; We want to present the current project only
    "s" #'treemacs-add-and-display-current-project-exclusively)
  :config
  (setq treemacs-persist-file (mo-cache-path "treemacs-persist"))
  (setq treemacs-width 50)
  (setq treemacs-no-png-images t)
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

;; Init hexl for editing binary files
(use-package hexl
  :straight nil
  :general
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "x" #'hexl-mode))

;; Init calc for Emacs built-in calculator
(use-package calc
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "l" #'calc))

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
  (setq proced-enable-color-flag t))

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
  :hook
  ( git-commit-setup . (lambda () (setq fill-column 72))))

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
    "+" #'diff-buffers))

;; Init ediff for better diff view and commands
(use-package ediff
  :straight nil
  :init
  ;; Ignore space changes
  (setq ediff-diff-options "-b")
  :config
  ;; Open ediff window in the current frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Set ediff to show diff changes in character-level
  (setq ediff-forward-word-function #'forward-char))

;; Init vdiff for vimdiff in Emacs
(use-package vdiff
  :general
  ( :keymaps '( vdiff-mode-map vdiff-3way-mode-map)
    "C-M-s-g" #'vdiff-switch-buffer
    "C-M-s-n" #'vdiff-next-hunk
    "C-M-s-S-n" #'vdiff-next-fold
    "C-M-s-p" #'vdiff-previous-hunk
    "C-M-s-S-p" #'vdiff-previous-fold
    "C-M-s-r" #'vdiff-receive-changes
    "C-M-s-S-r" #'vdiff-receive-changes-and-step
    "C-M-s-s" #'vdiff-send-changes
    "C-M-s-S-s" #'vdiff-send-changes-and-step
    "C-M-s-f" #'vdiff-refine-this-hunk
    "C-M-s-S-f" #'vdiff-refine-all-hunks
    "C-M-s-c" #'vdiff-close-fold
    "C-M-s-S-c" #'vdiff-close-all-folds
    "C-M-s-t" #'vdiff-close-other-folds
    "C-M-s-o" #'vdiff-open-fold
    "C-M-s-S-o" #'vdiff-open-all-folds
    "C-M-s-x" #'vdiff-remove-refinements-in-hunk
    "C-M-s-i C-M-s-c" #'vdiff-toggle-case
    "C-M-s-i C-M-s-w" #'vdiff-toggle-whitespace
    "C-M-s-w" #'vdiff-save-buffers
    "C-M-s-u" #'vdiff-refresh
    "C-M-s-q" #'vdiff-quit)
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "=" #'vdiff-buffers)
  ( :keymaps 'mo-quick-menu-map
    :prefix "f"
    "=" #'vdiff-files)
  :custom
  ( vdiff-auto-refine t)
  ( vdiff-subtraction-fill-char ?‧)
  ;; vdiff has a known bug with setting ignore preferences using minibuffer completions.
  ;; Set ignore space changes manually, instead.
  ( vdiff-diff-algorithm 'git-diff-histogram-no-space-changes)
  :config
  (add-to-list 'vdiff-diff-algorithms
               '( git-diff-histogram-no-space-changes .
                  "git --no-pager diff --histogram --no-index --no-color -b")))

;; Init vdiff-magit for vdiff and magit integration
(use-package vdiff-magit
  :after magit
  :general
  ( :keymaps 'magit-mode-map
    "e" #'vdiff-magit-dwim
    "E" #'vdiff-magit)
  :config
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

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
    "C-M-s-s" #'eval-last-sexp))

;; Init sly for Common Lisp support
(use-package sly
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "l"
    "s" #'sly
    "S" #'sly-quit-lisp
    "c" #'sly-connect
    "C" #'sly-disconnect)
  ( :keymaps 'sly-mode-map
    "C-M-s-e" #'sly-eval-defun
    "C-M-s-s" #'sly-eval-last-expression
    "C-M-s-r" #'sly-eval-region
    "C-M-s-b" #'sly-eval-buffer)
  (mo-quick-menu-definer
    :keymaps 'sly-mode-map
    "h h" #'sly-documentation)
  :custom
  ( inferior-lisp-program "sbcl"))

;; Init sly-quicklisp for quicklisp support in sly
(use-package sly-quicklisp
  :general
  ( :keymaps 'sly-mode-map
    "C-M-s-q" #'sly-quickload))

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
  :config
  (setq treesit-font-lock-level 4))

;; Init treesit-auto for automatically using tree-sitter major modes
(use-package treesit-auto
  :functions
  ( global-treesit-auto-mode
    treesit-auto--build-major-mode-remap-alist)
  :preface
  ;; Exclude modes from treesit-auto
  (defvar mo-treesit-auto-exclude-mode-list '( rustic-mode)
    "List of auto treesit excluded modes.")
  (defun mo-treesit-auto-filter-excluded-mode (remap-alist)
    "Filter out excluded modes enabled by treesit-auto."
    (cl-remove-if (lambda (item)
                    (memq (car item) mo-treesit-auto-exclude-mode-list))
                  remap-alist))
  :config
  (advice-add
   #'treesit-auto--build-major-mode-remap-alist
   :filter-return #'mo-treesit-auto-filter-excluded-mode)
  (global-treesit-auto-mode))

;; Init lsp mode for lsp support
(use-package lsp-mode
  :after orderless
  :hook
  ( after-enable-theme .
    (lambda ()
      ;; Distinguish between var reads and writes by underlining lsp write highlights
      (set-face-attribute 'lsp-face-highlight-write nil :underline t)))
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
    :prefix "h"
    "r" #'lsp-rust-analyzer-open-external-docs)
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "l" #'lsp-org)
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
                                   js2-mode
                                   typescript-mode
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
                                   sh-mode))

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
                          (`( ,(pred (equal major-mode)) ,func) (funcall func) (lsp) t)
                          ((pred (equal major-mode)) (lsp) t)))
                      mo-lsp-enable-for-modes))))))

  ;; Kill language server after the last associated buffer was closed
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-session-file (mo-cache-path "lsp-session-v1"))
  (setq lsp-eslint-library-choices-file (mo-cache-path ".lsp-eslint-choices"))
  ;; Always ask before executing auto actions
  (setq lsp-auto-execute-action nil)
  ;; Force lsp mode to forget the workspace folders for multi root servers
  ;; so the folders are added on demand
  (advice-add 'lsp :before
              (lambda (&rest _args)
                (eval
                 '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
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
  ;; Set completion style to orderless
  (defun mo-lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '( orderless)))
  :hook
  ;; Postpone lsp load for after dir local vars are read
  ;; Do not load lsp if dir local vars are not enabled (e.g. on preview)
  ( hack-local-variables . (lambda ()
                             (when enable-dir-local-variables
                               (mo-maybe-enable-lsp))))

  ;; Setup completion to use orderless
  ( lsp-completion-mode . mo-lsp-mode-setup-completion)
  ;; Temporarily disable the breadcrumb headerline when the buffer is in vdiff-mode.
  ( vdiff-mode . (lambda ()
                   (when (and lsp-mode lsp-headerline-breadcrumb-enable)
                     (if vdiff-mode
                         (lsp-headerline-breadcrumb-mode -1)
                       (lsp-headerline-breadcrumb-mode)))))
  :commands lsp)

;; Init lsp-ui for an interactive lsp interface
(use-package lsp-ui
  :after lsp-mode
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "i" #'lsp-ui-imenu
    "d" #'lsp-ui-doc-glance
    "'" #'lsp-ui-doc-focus-frame)
  :config
  ;; Do not show documentation automatically
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-max-height 60))

;; Init lsp-treemacs for an interactive lsp tree-like interface
(use-package lsp-treemacs
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "t" #'lsp-treemacs-call-hierarchy
    "T" #'lsp-treemacs-type-hierarchy))

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
    "b" #'dape-breakpoint-toggle
    "B" #'dape-breakpoint-expression
    "%" #'dape-breakpoint-log
    "s" #'dape-select-stack
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
  :custom
  ( dape-adapter-dir (mo-cache-path "debug-adapters"))
  ( dape-buffer-window-arrangment 'right)

  :config
  ;; Do not display info and repl buffers on startup
  (remove-hook 'dape-on-start-hooks 'dape-info)
  (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; Display info and repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-on-start-hooks
            (defun mo-dape--save-on-start ()
              (save-some-buffers t t))))

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
    "," #'consult-lsp-symbols)
  :config
  ;; Manual preview key for symbols results
  (consult-customize consult-lsp-symbols :preview-key "M-.")
  ;; Remove initial async separator as we use spaces for search tokenization
  (consult-customize consult-lsp-symbols :initial nil))

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
    "C-M-s-S-e" #'treesit-end-of-defun))

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
    "C-n" (yas-filtered-definition #'yas-next-field-or-maybe-expand)
    "C-p" (yas-filtered-definition #'yas-prev-field))
  :config
  (yas-global-mode 1))

;; Init yasnippet-snippets for common code templates
(use-package yasnippet-snippets)

;; Init rustic for Rust support
(use-package rustic
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
  :custom
  ( rustic-cargo-clippy-trigger-fix 'on-compile))

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

;; Init js2-mode for enhanced JavaScript editing
(use-package js2-mode
  :mode "\\.js\\'")

;; Init typescript-mode for enhanced TypeScript editing
(use-package typescript-mode)

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
    "<f12>" #'mo-toggle-messages-buffer)
  :config
  (defun mo-toggle-messages-buffer ()
    "Toggle *Messages* buffer."
    (interactive)
    (let ((window (get-buffer-window "*Messages*")))
      (if window
          (quit-window nil window)
        (view-echo-area-messages)))))

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
    "h" #'helpful-at-point))

;; Init mode-minder for showing all modes
(use-package mode-minder
  :straight
  ( mode-minder :type git :host github :repo "jdtsmith/mode-minder")
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "M" #'mode-minder))

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

;; Init google-this for quick Google searches from Emacs
(use-package google-this
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "g" #'google-this)
  :config
  (google-this-mode 1))

;; Init chatgpt-shell for ChatGPT support in Emacs
(use-package chatgpt-shell
  :general
  ( :keymaps 'mo-quick-menu-map
    "DEL" #'chatgpt-shell-prompt)
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "g" #'chatgpt-shell
    "G" #'chatgpt-shell-describe-image)
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "g d" #'chatgpt-shell-describe-code
    "g r" #'chatgpt-shell-refactor-code
    "g u" #'chatgpt-shell-generate-unit-test)
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "g p" #'chatgpt-shell-proofread-region
    "g r" #'chatgpt-shell-send-and-review-region)
  :hook
  ;; Disable auto-completion in chat buffer
  ( shell-maker-mode . (lambda () (setq-local corfu-auto nil)))
  :config
  ;; Display buffer according to the display-buffer rules
  (setq chatgpt-shell-display-function #'pop-to-buffer)
  (setq chatgpt-shell-request-timeout 240)
  (setq chatgpt-shell-system-prompt 0)
  (setq chatgpt-shell-welcome-function nil)
  (setq chatgpt-shell-prompt-query-response-style 'shell))

;; Init rainbow-delimiters for highlighting parens by their depth
(use-package rainbow-delimiters
  :hook
  ( prog-mode . rainbow-delimiters-mode))

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
  ( prog-mode . electric-pair-local-mode))

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
  (setq ahs-idle-interval 0.3))

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
  :hook
  ( prog-mode . global-hl-todo-mode))

;; Init ace-window for fast window selection
(use-package ace-window
  :general
  ( :keymaps 'override
    "M-o" #'ace-window
    "M-O" #'mo-ace-window-with-action
    "C-S-o" #'mo-ace-selected-window-prefix)
  :config
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
  ;; Remove treemacs windows from the ignored buffer list
  (setq aw-ignored-buffers (remove 'treemacs-mode aw-ignored-buffers)))

;; Init eww for browsing the web using Emacs
(use-package eww
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "w" #'eww)
  :config
  (setq eww-search-prefix "https://www.google.com/search?q="))

;; Init erc for accessing IRC through Emacs
(use-package erc
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "i" #'erc))

;; Init app-launcher for launching desktop apps
(use-package app-launcher
  :straight ( :type git :host github :repo "SebastienWae/app-launcher")
  :general
  ( :keymaps 'mo-quick-menu-map
    "ESC" #'app-launcher-run-app))

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
    :prefix "f"
    "t t" #'ansi-term)
  ( :keymaps 'mo-quick-menu-map
    :prefix "RET"
    "t t" #'mo-ansi-term-project)
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
    "t" #'tramp-term))

;; Init vterm for terminal emulation
(use-package vterm
  :demand t
  :if (not (eq system-type 'windows-nt))
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "RET"
    "t v" #'mo-vterm-project)
  ( :keymaps 'mo-quick-menu-map
    :prefix "f"
    "t v" #'mo-vterm-file)
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
    "C-n" #'comint-next-input)
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
    :prefix "f"
    "t s" #'shell-new)
  ( :keymaps 'mo-quick-menu-map
    :prefix "RET"
    "t s" #'mo-shell-project)
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
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "RET"
    "t e" #'project-eshell)
  ( :keymaps 'mo-quick-menu-map
    :prefix "f"
    "t e" #'eshell-new)
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "X" #'eshell-command
    "h" #'mo-run-htop)
  ( :keymaps 'eshell-mode-map
    "C-p" #'eshell-previous-matching-input-from-input
    "C-n" #'eshell-next-matching-input-from-input)
  :hook
  ;; Remove pager due to the lack of support for ANSI cursor sequence controls
  ( eshell-mode . (lambda () (setenv "PAGER" "cat")))
  :config
  (defun mo-run-htop ()
    "Run htop command."
    (interactive)
    (eshell-command "htop"))
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
  (setq eshell-directory-name (file-name-as-directory (mo-cache-path "eshell"))))

;; Init xterm-color for better ANSI color control sequence support
(use-package xterm-color
  :after eshell
  :defines
  ( xterm-color-preserve-properties
    eshell-preoutput-filter-functions)
  :hook
  ( eshell-before-prompt . (lambda () (setq xterm-color-preserve-properties t)))
  ;; eshell environment variables are buffer local, thus external commands
  ;; will be kept running with a "dumb" term.
  ( eshell-mode . (lambda () (setenv "TERM" "xterm-256color")))
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  :custom
  ( eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

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
  :config
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
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Init solaire-mode for visually highlighting file backed buffers
(use-package solaire-mode
  :functions solaire-global-mode
  :config
  (solaire-global-mode +1))

;; Init moody for adding tabs and ribbons to the mode line
(use-package moody
  :functions
  ( moody-replace-mode-line-buffer-identification
    moody-replace-vc-mode
    moody-replace-eldoc-minibuffer-message-function)
  :custom
  ;; Set mode line height to be calculated based on content height
  ( moody-mode-line-height nil)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

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
  :config
  (setq flyspell-delay 0)
  :hook
  ;; Enable spell checking
  ( text-mode . flyspell-mode)
  ( prog-mode . flyspell-prog-mode))

;; Init flyspell-correct for spell correction
(use-package flyspell-correct
  :demand t
  :general
  ( :states 'normal "z =" #'flyspell-correct-wrapper))

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
    "C-`" #'popper-toggle-latest
    "C-M-`" #'popper-cycle
    "M-`" #'popper-toggle-type)
  :init
  (defun mo-popper-group-by-tab ()
    "Return an identifier (tab name) to group popups."
    ;; We cannot simply use the current tab, as this function can be called during
    ;; tab switching, and that will incorrectly attach the popup to the newly
    ;; switched tab.
    (let ((tab-name (alist-get 'name (tab-bar-get-buffer-tab nil))))
      ;; Sometimes this function is called after the current buffer is hidden. In this
      ;; case, get the current tab. A null tab-name will not be returned when switching
      ;; tabs, hence there is no risk with just getting the current tab.
      (or tab-name
          (alist-get 'name (tab-bar--current-tab)))))

  (setq popper-reference-buffers
        '( "\\*Messages\\*"
           "\\*Warnings\\*"
           "\\*scratch\\*"
           "Output\\*$"
           "\\*Async Shell Command\\*"
           "\\*ielm\\*"
           "^\\*HTTP Response .*\\*$"
           help-mode
           helpful-mode
           "\\*sly-description\\*"
           world-clock-mode
           devdocs-mode
           compilation-mode
           inferior-python-mode
           "^\\*\\(.+-\\)?eshell\\*.*$" eshell-mode
           "^\\*shell.*\\*.*$" shell-mode
           "^\\*term.*\\*$" term-mode
           "^\\*vterm.*\\*.*$" vterm-mode))
  ;; Group popups by their associated project, with default dir as a fallback
  (setq popper-group-function #'popper-group-by-directory)
  ;; Set fractional height
  (setq popper-window-height 0.33)
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
    "C" #'world-clock)
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
  (setq recentf-save-file (mo-cache-path "recentf"))
  ;; Enlarge the max size of the recent files list
  (setq recentf-max-saved-items 10000)
  ;; Do not save remote files
  (add-to-list 'recentf-exclude #'file-remote-p)
  ;; Do not check readability of remote files.
  ;; This is needed in order to prevent tramp from hanging Emacs when killing a buffer.
  (add-to-list 'recentf-keep #'file-remote-p)
  (recentf-mode t))

;; Init autorevert for updating buffers that were changed on disk
(use-package autorevert
  :straight nil
  :config
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode))

;; Init bookmark for managing bookmarks
(use-package bookmark
  :straight nil
  :hook
  ;; Recenter after jump
  ( bookmark-after-jump . recenter)
  :config
  (setq bookmark-file (mo-cache-path "bookmarks")))

;; Init bookmark-in-project for managing per-project bookmarks
(use-package bookmark-in-project
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "RET"
    "TAB" #'bookmark-in-project-jump)
  :custom
  ( bookmark-in-project-project-root
    (lambda () (project-root (project-current)))))

;; Init tramp for accessing remote files
(use-package tramp
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "a"
    "c" #'tramp-cleanup-all-buffers)
  ( :keymaps 'mo-quick-menu-map
    :prefix "a"
    "C" #'tramp-cleanup-connection)
  :config
  ;; Set default method to ssh as it is faster than scp
  (setq tramp-default-method "ssh")
  ;; Lower verbosity to avoid connection messages in the echo area
  (setq tramp-verbose 2)
  ;; Preserve remote path value
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Init tramp-cache for caching in tramp
(use-package tramp-cache
  :straight nil
  :config
  (setq tramp-persistency-file-name (mo-cache-path "tramp")))

;; Init envrc for direnv integration with Emacs
;; This is here on purpose, so that its hooks will be registered as late as possible
(use-package envrc
  :hook
  ;; envrc kills buffer local env var variables (e.g. process-environment). This
  ;; interferes with other modes that set these vars to be local, such as eshell.
  ;; Until fixed, we should load this package only for specific modes.
  ( python-mode . envrc-mode)
  ( python-ts-mode . envrc-mode))

;; Load customization file
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
