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

;; Disable deferred & auto native compilation temporarily until we set the exec-path var
(setq native-comp-jit-compilation nil)
(defvar straight-disable-native-compile t)

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

;; Before loading other packages, set exec-path to the PATH var under the default shell
;; when executed under a windowing system, using the exec-path-from-shell package.
;; This is needed so libgccjit would be found by native compilation

(straight-use-package 'exec-path-from-shell)
(when (memq window-system '( mac ns x))
  (exec-path-from-shell-initialize))

;; Re-enable auto & deferred native compilation
(setq straight-disable-native-compile nil)
(setq native-comp-jit-compilation t)

;; Install use-package
(straight-use-package 'use-package)
;; Packages should be installed by default using straight
(setq straight-use-package-by-default t)
;; Show calls to use-package in imenu
(setq use-package-enable-imenu-support t)

;; Optionally, load personal settings
(load (concat (file-name-directory load-file-name) "personal.el") t)

;; Add general.el key mapper
(use-package general)

;; Init evil mode for Vim emulation in Emacs
(use-package evil
  :demand t
  :general
  ( :states 'motion
    "C-S-d" #'evil-scroll-up)
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
  :config
  ;; Set word movement to operate on symbol boundaries
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; Start with Emacs mode in rustic-popup-mode buffers
  (evil-set-initial-state 'rustic-popup-mode 'emacs)
  (evil-mode 1))

;; Prefix keys for quick action menu
(defvar mo-quick-menu-prefix "SPC")
(defvar mo-quick-menu-nn-prefix "M-SPC")

;; Create quick menu map and definer
(general-create-definer mo-quick-menu-def :keymaps 'mo-quick-menu-map)

;; Bind the quick menu map to the leader key and the relevant states
(general-define-key
 :states '( normal insert visual motion emacs)
 :keymaps 'override
 :prefix mo-quick-menu-prefix
 :non-normal-prefix mo-quick-menu-nn-prefix
 :prefix-map 'mo-quick-menu-map
 :which-key "Quick menu prefix key"
 "b" '( :which-key "Buffer")
 "f" '( :which-key "File")
 "s" '( :which-key "Search")
 "v" '( :which-key "View")
 "w" '( :which-key "Window")
 "x" '( :which-key "Utils")
 "t" '( :which-key "Tab")
 "h" '( :which-key "Help")
 "p" '( :which-key "Project")
 "c" '( :which-key "Code")
 "g" '( :which-key "Git")
 "r" '( :which-key "Multiple Cursors")
 "n" '( :which-key "Notes"))

;; Add evil shortcuts here, after the initialization of the quick menu map
(when (featurep 'evil)
  (mo-quick-menu-def
    :prefix "w"
    "H" #'evil-window-move-far-left
    "J" #'evil-window-move-very-bottom
    "K" #'evil-window-move-very-top
    "L" #'evil-window-move-far-right
    "v" #'evil-window-vsplit
    "s" #'evil-window-split
    "c" #'evil-window-delete
    "+" #'evil-window-increase-height
    "-" #'evil-window-decrease-height
    ">" #'evil-window-increase-width
    "<" #'evil-window-decrease-width)
  (mo-quick-menu-def
    "ESC" #'evil-ex-nohighlight))

;; Quick save key binding
(mo-quick-menu-def
  "SPC" #'save-buffer)

;; Universal argument key binding
(mo-quick-menu-def
  "u" #'universal-argument)

;; Init window for managing windows
(use-package window
  :straight nil
  :general
  ( :keymaps 'override
    "M-O" #'other-window))

;; Init frame for managing frames
(use-package frame
  :demand t
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "v"
    "f" #'make-frame-command
    "c" #'delete-frame))

;; Init ibuffer for editing buffer lists
(use-package ibuffer
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "i" #'ibuffer))

;; Init bookmark for managing bookmarks
(use-package bookmark
  :straight nil
  :hook
  ;; Recenter after jump
  ( bookmark-after-jump . recenter))

(defun mo-copy-file-path ()
  "Copy the full path of the current buffer's file."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "%s" filepath))))

(mo-quick-menu-def
  :prefix "b"
  "p" #'mo-copy-file-path
  "t" #'toggle-truncate-lines
  "r" #'revert-buffer-quick
  "k" #'kill-current-buffer
  "[" #'previous-buffer
  "]" #'next-buffer)

(mo-quick-menu-def
  :prefix "f"
  "f" #'find-file)

(mo-quick-menu-def
  :prefix "w"
  "=" #'balance-windows
  "C" #'delete-other-windows)

;; Add evil key bindings to other, non-default, modes
(use-package evil-collection
  :after ( evil xref)
  :config
  ;; We have our own find references key binding. Remove evil-collection's one.
  ;; evil-collection's find usages overrides evil-mc key bindings.
  (setq evil-collection-want-find-usages-bindings nil)
  (setq evil-collection-want-unimpaired-p nil)
  (evil-collection-init))

;; Init evil-org for supporting evil key bindings in org-mode
(use-package evil-org
  :after org
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
  ( :keymaps 'evil-mc-cursors-map
    "d" #'evil-mc-make-and-goto-next-match
    "D" #'evil-mc-make-and-goto-prev-match)
  ( :keymaps 'mo-quick-menu-map
    "r" '( :keymap evil-mc-cursors-map :package evil-mc))
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
  :config
  (global-anzu-mode +1))

;; Init evil-anzu for anzu integration with evil search
(use-package evil-anzu
  :after evil)

;; Init avy for text zapping using free text and a timeout
(use-package avy
  :config
  (defun mo-avy-goto-char-timer-action ()
    "Zap to free text search with timeout.
Ask for action even on single candidate jumps."
    (interactive)
    (let ((avy-single-candidate-jump nil))
      (call-interactively #'avy-goto-char-timer)))
  :general
  ( :keymaps 'override
    "C-'" #'avy-goto-char-timer
    "C-\"" #'mo-avy-goto-char-timer-action)
  :config
  ;; Better highlight the leading characters
  (set-face-attribute 'avy-lead-face nil :background "gold1")
  (set-face-attribute 'avy-lead-face-0 nil :background "gold2")
  (set-face-attribute 'avy-lead-face-1 nil :background "gold3")
  (set-face-attribute 'avy-lead-face-2 nil :background "gold4")
  (setq avy-all-windows 'all-frames)
  (setq avy-timeout-seconds 0.25))

;; Init evil-snipe for an improved 1 char evil search experience
(use-package evil-snipe
  :config
  (evil-snipe-override-mode 1))

;; Init expand-region for expanding the selected region by semantic units
(use-package expand-region
  :bind ( "C-=" . er/expand-region))

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
  :hook
  ;; Recenter after returning to a pre-jump location
  ( xref-after-return . recenter)
  :general
  ( :keymaps 'mo-quick-menu-map
    ";" #'xref-find-definitions
    "'" #'xref-find-references)
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
  (setq dumb-jump-selector #'completing-read))

;; Init origami for text and code folding
(use-package origami
  :config
  (global-origami-mode))

;; Init lsp-origami for code folding based on data from language server
(use-package lsp-origami
  :after ( origami lsp)
  :hook
  ( lsp-after-open . lsp-origami-try-enable))

;; Init org mode for editing and managing notes
(use-package org
  :straight ( :type built-in)
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "l" #'org-insert-link
    "o" #'org-open-at-point
    "L" #'org-store-link
    "v" #'org-latex-preview
    "e" #'org-toggle-pretty-entities
    "s" #'org-schedule
    "S" #'org-agenda-schedule
    "d" #'org-deadline
    "D" #'org-agenda-deadline
    "c" #'org-capture
    "t" #'org-todo
    "q" #'org-set-tags-command
    "Q" #'org-agenda-set-tags
    "a" #'mo-org-agenda-and-todo)
  ( :keymaps 'org-mode-map
    :states 'normal
    "TAB" #'org-cycle)
  ;; Close any loaded org buffer when exiting the agenda buffer
  ( :keymaps 'org-agenda-mode-map
    "q" #'org-agenda-exit)
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
  (setq org-ellipsis " ▼")
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
  (setq org-src-preserve-indentation t))

;; Init org-contrib for org add-ons
(use-package org-contrib)

;; Init ox-confluence for exporting org to confluence
(use-package ox-confluence
  :after org-contrib
  :straight nil)

;; Init org-roam for Zettelkasten note management
(use-package org-roam
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "r b" #'org-roam-buffer-toggle
    "r g" #'org-roam-graph
    "r i" #'org-roam-node-insert
    "r r" #'org-roam-node-find
    "r t" #'org-roam-tag-add
    "r c" #'org-roam-capture)
  :custom
  ( org-roam-directory "~/roam")
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-db-location (mo-cache-path "org-roam.db"))
  (org-roam-db-autosync-mode))

;; Init consult-org-roam for better searching in org-roam notes
(use-package consult-org-roam
  :after org-roam
  :config
  (consult-org-roam-mode))

;; Init org-modern for a modern org buffer style
(use-package org-modern
  :config
  ;; We disable prettifying tables as currently it is not pixel-aligned
  (setq org-modern-table nil)
  (global-org-modern-mode))

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
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    ";" #'org-present)
  :hook
  ;; Set style of slides
  ( org-present-mode .
    (lambda ()
      ;; Disable org modern mode
      (org-modern-mode 0)
      ;; Disable spellchecking
      (flyspell-mode 0)
      ;; Center presentation and wrap lines
      (setq visual-fill-column-width 110)
      (setq visual-fill-column-center-text t)
      (visual-fill-column-mode 1)
      (visual-line-mode 1)
      ;; Present images
      (org-display-inline-images)
      ;; Make slides not modifiable
      (org-present-read-only)
      ;; Create a space at the beginning of the slide
      (setq header-line-format " ")
      ;; Change org level font sizes
      (setq-local face-remapping-alist
                  '( ( default ( :height 1.5) default)
                     ( header-line ( :height 4.0) default)
                     ( org-document-title ( :height 1.75) org-document-title)
                     ( org-block ( :height 0.75) org-block)))
      ;; We want variable pitch faces
      (variable-pitch-mode 1)))
  ;; Undo style changes
  ( org-present-mode-quit .
    (lambda ()
      (variable-pitch-mode 0)
      (setq-local face-remapping-alist '( ( default default default)))
      (setq header-line-format nil)
      (org-present-read-write)
      (org-remove-inline-images)
      (visual-line-mode 0)
      (visual-fill-column-mode 0)
      (setq visual-fill-column-center-text nil)
      (setq visual-fill-column-width nil)
      (flyspell-mode 1)
      (org-modern-mode 1)))
  :config
  ;; Per slide actions
  (add-hook 'org-present-after-navigate-functions
            (lambda (buffer-name heading)
              ;; Show only top-level headlines
              (org-overview)
              ;; Unfold the current entry
              (org-show-entry)
              ;; Show only direct subheadings of the slide but don't expand them
              (org-show-children))))

;; Init org-download for downloading and embedding images in org mode
(use-package org-download)

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
    :prefix "n"
    "C" #'calendar))

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
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (pdf-tools-install :no-query))

;; Init orderless for advanced (e.g. fuzzy) completion styles
(use-package orderless
  :demand t
  :config
  ;; Set matching style to regexp and literal
  (setq orderless-matching-styles '( orderless-regexp orderless-literal))
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '( ( file ( styles basic partial-completion))))

  ;; Add exclude pattern style
  (defun mo-orderless-exclude-dispatcher (pattern _index _total)
    "Handle orderless exclude pattern."
    (cond
     ((equal "!" pattern)
      '( orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `( orderless-without-literal . ,(substring pattern 1)))))
  (setq orderless-style-dispatchers '( mo-orderless-exclude-dispatcher))

  :custom ( completion-styles '( orderless basic)))

;; Init vertico for item list selection
(use-package vertico
  ;; Load extensions
  :straight ( :files ( :defaults "extensions/*"))
  :config
  (setq vertico-count 20)
  (setq vertico-cycle t)
  (vertico-mode))

;; Init vertico-repeat for repeating the last minibuffer command
(use-package vertico-repeat
  :after vertico
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    "z" #'vertico-repeat-select)
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

;; Enable recursive minibuffer
(setq enable-recursive-minibuffers t)

;; Init recursion-indicator for indicating minibuffer recursions
(use-package recursion-indicator
  :config
  (recursion-indicator-mode))

;; Init corfu for auto completion
(use-package corfu
  ;; Load extensions
  :straight ( :files ( :defaults "extensions/*"))
  :demand t
  :general
  ( :keymaps 'corfu-map
    "C-<return>" #'corfu-insert-separator)
  :init
  ;; Enable auto completion
  (setq corfu-auto t)
  ;; Keep popup only if there is a match or the separator was inserted
  (setq corfu-quit-no-match 'separator)
  ;; Set auto completion to be more responsive
  (setq corfu-auto-delay 0)
  (setq corfu-auto-prefix 0)
  :hook
  ;; Disable auto mode in eshell
  ( eshell-mode . (lambda () (setq-local corfu-auto nil) (corfu-mode)))
  ;; Close popup when exiting evil insert state
  ( evil-insert-state-exit . corfu-quit)
  :config
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

;; Init cape for completion at point extensions
(use-package cape
  :config
  ;; Add completion functions
  (add-to-list 'completion-at-point-functions (cape-capf-prefix-length #'cape-dabbrev 3))
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Enable indentation and completion using the TAB key
(setq tab-always-indent 'complete)

;; Init dabbrev for the automatic completion of dynamic abbreviations
(use-package dabbrev
  :straight nil
  :config
  (setq dabbrev-ignored-buffer-regexps '( "^\\*.+::stderr\\*$")))

;; Used by project.el for project detection
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

;; Init project for auto project detection
(use-package project
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "p"
    "w" #'mo-project-save
    "d" #'project-dired
    "c" #'project-compile
    "x" #'project-async-shell-command
    "k" #'project-kill-buffers
    "p" #'project-switch-project
    "i" #'project-list-buffers)
  :config
  ;; Enable project detection using .project files
  (add-to-list 'project-find-functions #'mo-project-try-local)
  ;; Set project history file path
  (setq project-list-file (mo-cache-path "projects")))

;; Init consult for enhanced search commands
(use-package consult
  :demand t
  :general
  ;; Quick bindings
  ( :keymaps 'mo-quick-menu-map
    "/" #'consult-line
    "?" #'consult-line-multi
    "." #'consult-fd
    "," #'consult-ripgrep)
  ( :keymaps 'mo-quick-menu-map
    :prefix "g"
    "," #'consult-git-grep)
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "q" #'consult-compile-error)
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "I" #'consult-info
    "m" #'consult-man)
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "b" #'consult-buffer
    "/" #'consult-imenu
    "?" #'consult-imenu-multi
    ";" #'consult-keep-lines
    "m" #'consult-minor-mode-menu
    "B" #'consult-recent-file)
  ( :keymaps 'mo-quick-menu-map
    "RET" #'consult-bookmark)
  ;; C-x bindings (ctl-x-map)
  ( "C-x M-:" #'consult-complex-command)     ;; orig. repeat-complex-command
  ( "C-x b" #'consult-buffer)                ;; orig. switch-to-buffer
  ( "C-x 4 b" #'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ( "C-x 5 b" #'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ;; Custom M-# bindings for fast register access
  ( "M-#" #'consult-register-load)
  ( "M-'" #'consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ( "C-M-#" #'consult-register)
  ;; Other custom bindings
  ( "M-y" #'consult-yank-pop)                ;; orig. yank-pop
  ( "<help> a" #'consult-apropos)            ;; orig. apropos-command
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
    "M-r" #'consult-history)                ;; orig. previous-matching-history-element
  ;; Eshell history
  ( :keymaps 'eshell-hist-mode-map
    "M-s" #'consult-history                 ;; orig. eshell-next-matching-input
    "M-r" #'consult-history)                ;; orig. eshell-previous-matching-input
  ( :keymaps 'mo-quick-menu-map
    :prefix "p"
    "b" #'consult-project-buffer)
  ( :keymaps 'mo-quick-menu-map
    "*" #'mo-consult-line-symbol-at-point)
  ( :keymaps 'mo-quick-menu-map
    :prefix "v"
    "t" #'consult-theme)
  ( :keymaps 'mo-quick-menu-map
    :prefix "n"
    "/" #'consult-org-heading)

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

  ;; Add consult-fd command
  ;; Based on code from consult wiki:
  ;; https://github.com/minad/consult/wiki#find-files-using-fd
  (defvar consult--fd-command nil)
  (defun consult--fd-builder (input dir)
    (unless consult--fd-command
      (setq consult--fd-command
            (if (eq 0 (call-process-shell-command "fdfind"))
                "fdfind"
              "fd")))
    (pcase-let* ((`( ,arg . ,opts) (consult--command-split input))
                 (`( ,re . ,hl) (funcall consult--regexp-compiler
                                         arg 'extended t)))
      (when re
        (cons (append
               (list consult--fd-command
                     "--color=never" "-i" "-p" "-H" "-t" "f"
                     (concat dir ".*" (consult--join-regexps re 'extended)))
               opts)
              hl))))

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

  ;; On project switch, use consult for file and regexp search
  (setq project-switch-commands
        '( (consult-project-buffer "Buffer" ?b)
           (consult-fd "Find file" ?f)
           (consult-ripgrep "Ripgrep" ?r)
	   (magit-status "Magit" ?g))))

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
  :config
  (marginalia-mode))

(defun mo-embark-magit-status (file)
  "Run `magit-status` on repo containing the embark target."
  (interactive "GFile: ")
  (magit-status (locate-dominating-file file ".git")))

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
  ;; Init tab-bar for supporting multiple window layouts in frame
  (tab-bar-mode))

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
  (setq dired-dwim-target t))

;; Init dired+ for additional dired functionality
(use-package dired+
  :after evil-collection
  :init
  (setq diredp-hide-details-initially-flag nil)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

;; Init treemacs for a tree-like sidebar file navigator
(use-package treemacs
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "p"
    ;; We want to present the current project only
    "s" #'treemacs-add-and-display-current-project-exclusively)
  :config
  (setq treemacs-persist-file (mo-cache-path "treemacs-persist"))
  (setq treemacs-width 50)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'simple))

;; Init treemacs-evil for treemacs and evil integration
(use-package treemacs-evil
  :after ( treemacs evil))

;; Init treemacs-icons-dired for having icons in dired mode
(use-package treemacs-icons-dired
  :after ( treemacs dired))

;; Init treemacs-magit for treemacs and magit integration
(use-package treemacs-magit
  :after ( treemacs magit))

;; Init dired-narrow for narrowing dired results using regexp
(use-package dired-narrow
  :general
  ( :keymaps 'dired-mode-map
    :states 'normal
    "/" #'dired-narrow-fuzzy))

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

;; Init vc-git for configuring the git VC backend
(use-package vc-git
  :straight nil
  :config
  (setq vc-git-diff-switches '( "--histogram")))

;; Init magit for a better git user experience
(use-package magit
  ;; Refine diff view to show sub hunk changes
  :general
  ;; Visit files in the other window
  ( :keymaps 'mo-quick-menu-map
    :prefix "g"
    "g" #'magit-status
    "s" #'magit-stage
    "u" #'magit-unstage
    "c" #'magit-commit
    "d" #'magit-dispatch
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
  (setq transient-levels-file (mo-cache-path "transient_levels.el"))
  (setq transient-values-file (mo-cache-path "transient_values.el"))
  (setq transient-history-file (mo-cache-path "transient_history.el"))
  (setq magit-diff-refine-hunk 'all))

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
  :config
  (setq forge-database-file (mo-cache-path "forge-database.sqlite")))

;; Init github-review for helping with code review on github
(use-package github-review
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "g"
    "r a" #'github-review-approve
    "r c" #'github-review-comment
    "r k" #'github-review-reject
    "r r" #'github-review-forge-pr-at-point)
  :config
  (setq github-review-view-comments-in-code-lines t)
  (setq github-review-reply-inline-comments t))

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

;; Init diff-hl for highlighting uncommitted changes
(use-package diff-hl
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "g"
    "R" #'diff-hl-set-reference-rev
    "]" #'diff-hl-next-hunk
    "[" #'diff-hl-previous-hunk)
  :hook
  ( magit-pre-refresh . diff-hl-magit-pre-refresh)
  ( magit-post-refresh . diff-hl-magit-post-refresh)
  ( dired-mode . diff-hl-dired-mode-unless-remote)
  :config
  (global-diff-hl-mode))

;; Init git-link for creating URLs for files in web git services
(use-package git-link
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "g"
    "L" #'git-link)
  :config
  (setq git-link-use-commit t))

;; Init treesit for tree-sitter support in Emacs
(use-package treesit
  :straight nil
  :config
  (setq treesit-font-lock-level 4))

;; Init treesit-auto for automatically using tree-sitter major modes
(use-package treesit-auto
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

;; Init lsp mode for lsp support
(use-package lsp-mode
  :after orderless
  :general
  ;; Set the lsp prefix key
  ( :keymaps 'lsp-mode-map
    "C-c l" '( :keymap lsp-command-map :which-key "lsp"))
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "a" #'lsp-execute-code-action
    "r" #'lsp-rename
    "p" #'lsp-signature-activate
    "R" #'lsp-workspace-restart
    "=" #'lsp-format-region
    "o" #'lsp-clangd-find-other-file)
  ( :keymaps 'mo-quick-menu-map
    "\"" #'lsp-find-implementation)
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "r" #'lsp-rust-analyzer-open-external-docs)
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
                                   js-ts-mode
                                   js2-mode
                                   typescript-mode
                                   typescript-ts-mode
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
                                   terraform-mode
                                   cmake-mode
                                   cmake-ts-mode
                                   sh-mode))

  (defun mo-maybe-enable-lsp (lsp-config)
    "If mode in LSP-CONFIG is equal to the current major-mode,
run the attached function (if exists) and enable lsp"
    (pcase lsp-config
      (`( ,(pred (equal major-mode)) ,func) (funcall func) (lsp) t)
      ((pred (equal major-mode)) (lsp) t)))

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
                               (seq-find #'mo-maybe-enable-lsp
                                         mo-lsp-enable-for-modes))))

  ;; Enable which-key integration
  ( lsp-mode . lsp-enable-which-key-integration)
  ;; Setup completion to use orderless
  ( lsp-completion-mode . mo-lsp-mode-setup-completion)
  :commands lsp)

;; Init lsp-ui for an interactive lsp interface
(use-package lsp-ui
  :after lsp-mode
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "i" #'lsp-ui-imenu
    "d" #'lsp-ui-doc-glance
    "c" #'lsp-ui-doc-focus-frame)
  :config
  ;; Do not show documentation automatically
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-max-height 60))

;; Init lsp-treemacs for an interactive lsp tree-like interface
(use-package lsp-treemacs
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "h" #'lsp-treemacs-call-hierarchy
    "H" #'lsp-treemacs-type-hierarchy))

;; Init dap-mode for interactive debugging
(use-package dap-mode
  :after lsp-mode
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    ":" #'dap-debug
    ";" #'dap-hydra)
  :config
  (setq dap-breakpoints-file (mo-cache-path "dap-breakpoints"))
  (setq dap-utils-extension-path (mo-cache-path ".extension"))
  ;; Disable control buttons on the top of the screen
  (setq dap-auto-configure-features (delq 'controls dap-auto-configure-features))
  (dap-auto-configure-mode)
  ;; Init lldb debugging
  (require 'dap-lldb)
  ;; Init native debugging
  (require 'dap-gdb-lldb))

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

;; Init highlight-defined for highlighting Emacs Lisp symbols
(use-package highlight-defined
  :hook ( emacs-lisp-mode . highlight-defined-mode))

;; Init highlight-quoted for highlighting Emacs Lisp quoted symbols
(use-package highlight-quoted
  :hook ( emacs-lisp-mode . highlight-quoted-mode))

;; Init cc-mode for C/C++/Obj-C support
(use-package cc-mode
  :straight nil
  :config
  ;; Set the default style
  (setf (alist-get 'other c-default-style) "stroustrup"))

;; Init google-c-style for Google's C/C++ style
(use-package google-c-style
  :config
  (c-add-style "Google" google-c-style))

;; Init yasnippets for adding code snippet templates
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Init yasnippet-snippets for common code templates
(use-package yasnippet-snippets)

;; Init rustic for Rust support
(use-package rustic)

;; Associate objc-mode with Objective C files
(add-to-list 'auto-mode-alist '( "\\.mm\\'" . objc-mode))

;; Init swift-mode for Swift support
(use-package swift-mode)

;; Init lsp-sourcekit for SourceKit language server
(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable
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
  :config
  (setq lsp-java-workspace-dir (mo-cache-path "workspace")))

;; Init lsp-pyright for pyright python language server
(use-package lsp-pyright)

;; Init pip-requirements for editing pip requirements files
(use-package pip-requirements)

;; Init pyvenv for activating python virtual environments
(use-package pyvenv)

;; Init pipenv for supporting pipenv projects and commands
(use-package pipenv
  :hook ( python-mode . pipenv-mode)
  :config
  ;; We don't use projectile
  (setq pipenv-with-projectile nil))

;; Init js2-mode for enhanced JavaScript editing
(use-package js2-mode
  :mode "\\.js\\'")

;; Init typescript-mode for enhanced TypeScript editing
(use-package typescript-mode)

;; Init julia-mode for Julia support
(use-package julia-mode)

;; Init lsp-julia for Julia language server
(use-package lsp-julia
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.8"))

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
    :prefix "c"
    "D" #'docker))

;; Init nix-mode for editing nix files
(use-package nix-mode
  :mode "\\.nix\\'")

;; Init terraform-mode for editing terraform files
(use-package terraform-mode)

;; Init protobuf-mode for editing protobuf files
(use-package protobuf-mode)

;; Init markdown-mode for enhanced markdown editing
(use-package markdown-mode)

;; Init cmake-mode for editing CMake files
(use-package cmake-mode)

;; Init bazel for editing Bazel related files
(use-package bazel)

;; Init restclient for sending rest requests from emacs
(use-package restclient)

;; Init formal-all for a universal code formatter
(use-package format-all
  :hook
  ( prog-mode . format-all-mode)
  ( format-all-mode . format-all-ensure-formatter)
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "f" #'format-all-region
    "F" #'format-all-buffer)
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "f" #'format-all-mode)
  :config
  (setq format-all-show-errors 'never))

;; Disable default tab indentation
(setq-default indent-tabs-mode nil)

;; Init dtrt-indent for auto indentation detection
(use-package dtrt-indent
  :hook
  ( prog-mode . dtrt-indent-mode)
  :config
  (setq dtrt-indent-hook-mapping-list
        (append dtrt-indent-hook-mapping-list
                '( ( c-ts-mode c/c++/java c-ts-mode-indent-offset)
                   ( c++-ts-mode c/c++/java c-ts-mode-indent-offset)
                   ( java-ts-mode c/c++/java c-ts-mode-indent-offset)
                   ( json-ts-mode c/c++/java c-ts-mode-indent-offset)))))

;; Init editorconfig for applying EditorConfig settings
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Init which-key for interactively displaying key bindings
(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-add-column-padding 8)
  (which-key-mode))

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
    "F" #'describe-face))

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
    "v" #'helpful-variable
    "k" #'helpful-key
    "h" #'helpful-at-point))

;; Init devdocs for viewing online dev documentation
(use-package devdocs
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "d" #'devdocs-lookup)
  :config
  (setq devdocs-data-dir (mo-cache-path "devdocs")))

;; Init google-this for quick Google searches from Emacs
(use-package google-this
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "h"
    "g" #'google-this)
  :config
  (google-this-mode 1))

;; Init gptel for ChatGPT support in Emacs
(use-package gptel
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "x"
    "g" #'gptel)
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "g" #'gptel-send)
  ( :keymaps 'mo-quick-menu-map
    :prefix "c"
    "g d" #'mo-gpt-describe-code
    "g b" #'mo-gpt-find-bugs
    "g i" #'mo-gpt-improve-code
    "g r" #'mo-gpt-review-code)

  :config
  (defun mo-gpt-region (name prompt)
    "Send selected region as a gpt query in a new session called NAME with PROMPT."
    (let ((text (and (use-region-p)
                     (buffer-substring (region-beginning)
                                       (region-end)))))
      (gptel name gptel-api-key text)
      (with-current-buffer name
        (let ((gptel--system-message prompt))
          (gptel-send)))))

  (defun mo-gpt-describe-code ()
    "Describe code in region using gpt."
    (interactive)
    (mo-gpt-region "*GPT describe code*" "You are a large model living in Emacs and also a helpful assistant. \
Please describe the following code to the best of your ability."))

  (defun mo-gpt-find-bugs ()
    "Find bugs in region using gpt."
    (interactive)
    (mo-gpt-region "*GPT find bugs*" "You are a large model living in Emacs and also a helpful assistant. \
Please find bugs in the following code to the best of your ability."))

  (defun mo-gpt-improve-code ()
    "Improve code in region using gpt."
    (interactive)
    (mo-gpt-region "*GPT improve code*" "You are a large model living in Emacs and also a helpful assistant. \
Please improve the following code to the best of your ability."))

  (defun mo-gpt-review-code ()
    "Review code in region using gpt."
    (interactive)
    (mo-gpt-region "*GPT code review*" "You are a large model living in Emacs and also a helpful assistant. \
Please provide a code review on the following code, to the best of your ability.")))

;; Init rainbow-delimiters for highlighting parens by their depth
(use-package rainbow-delimiters
  :hook
  ( prog-mode . rainbow-delimiters-mode))

;; Show matching parentheses
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Auto insert matching parentheses in code
(add-hook 'prog-mode-hook 'electric-pair-local-mode)

;; Init auto-highlight-symbol for auto highlighting symbols in code.
;; This can be used when no LSP based highlighting is available.
(use-package auto-highlight-symbol
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "h" #'auto-highlight-symbol-mode)
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

;; Init hl-todo for highlighting specific keywords (e.g. TODO)
(use-package hl-todo
  :hook
  ( prog-mode . global-hl-todo-mode))

;; A fast key binding for showing the next command's result in another window.
;; Make sure it also works when the command is using 'switch-to-buffer'.
(setq switch-to-buffer-obey-display-actions t)
(general-define-key "M-[" #'other-window-prefix)
;; Other window key binding
(mo-quick-menu-def
  "o" #'other-window-prefix)

;; Init ace-window for fast window selection
(use-package ace-window
  :general
  ( :keymaps 'override
    "M-o" #'ace-window)
  :config
  (setq aw-keys '( ?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Init vterm for terminal emulation
(use-package vterm
  :demand t
  :if (not (eq system-type 'windows-nt))
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "p"
    "t" #'mo-vterm-project)
  :commands vterm
  :init
  (defun mo-vterm-project ()
    "Create a vterm buffer with current directory set to the active project root.
If project root cannot be found, use the buffer's default directory."
    (interactive)
    (let* ((default-directory (mo-get-buffer-dir)))
      (vterm vterm-buffer-name)))
  ;; Set a low response delay
  (setq vterm-timer-delay 0.07)
  ;; Set longer scrollback history
  (setq vterm-max-scrollback 50000))

;; Init eshell for terminal emulation
(use-package eshell
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "p"
    "e" #'project-eshell)
  ( :keymaps 'mo-quick-menu-map
    :prefix "f"
    "e" #'eshell-new)
  :hook
  ;; Remove pager due to the lack of support for ANSI cursor sequence controls
  ( eshell-mode . (lambda () (setenv "PAGER" "cat")))
  :config
  (defun eshell-new()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N))
  (setq eshell-history-size 100000)
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
                              'face '( :foreground "plum3"))
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
  :hook
  ( eshell-before-prompt . (lambda () (setq xterm-color-preserve-properties t)))
  ;; eshell environment variables are buffer local, thus external commands
  ;; will be kept running with a "dumb" term.
  ( eshell-mode . (lambda () (setenv "TERM" "xterm-256color")))
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

;; Init all-the-icons for icon support
(use-package all-the-icons
  :if (display-graphic-p))

;; Init kind-icon for icon support in auto completion
(use-package kind-icon
  :after corfu
  :config
  ;; Compute blended backgrounds correctly
  (setq kind-icon-default-face 'corfu-default)
  ;; Don't use icons, but text symbols
  (setq kind-icon-use-icons nil)
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

;; Init dashboard for an informative splash screen
(use-package dashboard
  :config
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-banner-logo-title "Welcome to Modus Operandi Emacs!")
  (setq dashboard-init-info "In Absentia Lucis, Tenebrae Vincunt")
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-footer nil)
  (setq dashboard-items '( ( projects . 10)
                           ( recents  . 10)
                           ( bookmarks . 10)))
  (dashboard-setup-startup-hook))

;; Init doom one theme
(use-package doom-themes
  :after treemacs-icons-dired
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Set brighter comments
  (setq doom-one-brighter-comments t)
  (load-theme 'doom-one t)
  ;; Resize org headings
  (dolist (face '( ( org-level-1 . 1.2)
                   ( org-level-2 . 1.1)
                   ( org-level-3 . 1.05)
                   ( org-level-4 . 1.0)
                   ( org-level-5 . 1.0)
                   ( org-level-6 . 1.0)
                   ( org-level-7 . 1.0)
                   ( org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :height (cdr face)))
  ;; Make sure certain org faces always use the fixed-pitch face
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '( shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '( shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '( font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '( font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  ;; Distinguish between var reads and writes by underlining lsp write highlights
  (set-face-attribute 'lsp-face-highlight-write nil :underline t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; Load icons here, so their background will be aligned with the theme
  (treemacs-icons-dired-mode))

;; Init solaire-mode for visually highlighting file backed buffers
(use-package solaire-mode
  :config
  (solaire-global-mode +1))

;; Init minions for collapsing the minor mode indicator in the modeline
(use-package minions
  :config
  (minions-mode 1))

;; Init flyspell for spell checking
(use-package flyspell
  :straight nil
  :config
  (setq flyspell-delay 0))

;; Init flyspell-correct for spell correction
(use-package flyspell-correct
  :demand t
  :general
  ( :states 'normal "z =" #'flyspell-correct-wrapper)
  :hook
  ;; Enable spell checking
  ( text-mode . flyspell-mode)
  ( prog-mode . flyspell-prog-mode))

;; Init consult-flyspell for incorporating flyspell into consult
(use-package consult-flyspell
  :after flyspell-correct
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "s" #'consult-flyspell)
  :config
  (setq consult-flyspell-select-function 'flyspell-correct-at-point))

;; Init jinx for just-in-time spell checking
(use-package jinx
  :hook ( emacs-startup . global-jinx-mode)
  :general ( [remap ispell-word] #'jinx-correct))

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

;; Init face-remap for remapping face properties
(use-package face-remap
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "v"
    "z" #'global-text-scale-adjust))

;; Init writeroom-mode for distraction free writing mode
(use-package writeroom-mode
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "w" #'writeroom-mode))

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
  :demand t
  :general
  ( "C-`" #'popper-toggle-latest
    "M-`" #'popper-cycle
    "C-M-`" #'popper-toggle-type)
  :init
  (setq popper-reference-buffers
        '( "\\*Messages\\*"
           "Output\\*$"
           "\\*Async Shell Command\\*"
           help-mode
           helpful-mode
           devdocs-mode
           compilation-mode
           "^\\*\\(.+-\\)?eshell\\*.*$" eshell-mode
           "^\\*shell.*\\*$" shell-mode
           "^\\*term.*\\*$" term-mode
           "^\\*vterm.*\\*.*$" vterm-mode))
  ;; Group popups by their associated project, with default dir as a fallback
  (setq popper-group-function #'popper-group-by-directory)
  (popper-mode +1)
  (popper-echo-mode +1))

;; Cleanup the frame UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Disable cursor blink
(blink-cursor-mode 0)

;; Inhibit the splash screen
(setq inhibit-splash-screen t)

;; Enable winner-mode for window management
(use-package winner
  :straight nil
  :demand t
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "w"
    "u" #'winner-undo
    "r" #'winner-redo)
  :config
  (setq winner-dont-bind-my-keys t)
  (winner-mode 1))

;; Set the default initial frame size
(add-to-list 'default-frame-alist '( height . 55))
(add-to-list 'default-frame-alist '( width . 210))

;; Don't split windows vertically by default
(setq split-height-threshold nil)

;; Set frame to full screen
(toggle-frame-fullscreen)

;; Scroll incrementally
(setq scroll-step 1)
;; Don't automatically recenter after scrolling
(setq scroll-conservatively 101)

;; Init follow-mode for scrolling buffer on multiple windows
(use-package follow-mode
  :straight nil
  :general
  ( :keymaps 'mo-quick-menu-map
    :prefix "b"
    "F" #'follow-mode))

;; Ask for confirmation before exiting emacs
(setq confirm-kill-emacs #'y-or-n-p)

;; Replace yes or no questions to y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't create backup, autosave and lock files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq auto-save-list-file-prefix (mo-cache-path "auto-save-list/.saves-"))

;; Disable bell audio
(setq ring-bell-function 'ignore)

;; Show cursor's column number
(setq column-number-mode t)

;; Init time for showing time in the modeline
(use-package time
  :straight nil
  :config
  ;; Remove average load time indicator from the modeline
  (setq display-time-default-load-average nil)
  (display-time-mode 1))

;; Truncate lines by default
(setq truncate-lines t)

;; Init whitespace for showing trailing whitespaces in code
(use-package whitespace
  :straight nil
  :config
  (setq whitespace-style '( face trailing))
  :hook
  ( prog-mode . whitespace-mode))

;; Init visual-fill-column for mimicking fill-column in visual-line-mode
(use-package visual-fill-column)

;; Start Emacs server
(server-start)

;; Set url configuration directory
(setq url-configuration-directory (mo-cache-path "url"))

;; Init savehist for minibuffer history persistence over Emacs restarts
(use-package savehist
  :straight nil
  :config
  (setq savehist-file (mo-cache-path "history"))
  (savehist-mode))

;; Init recentf for tracking recently opened files
(use-package recentf
  :straight nil
  :config
  (setq recentf-save-file (mo-cache-path "recentf"))
  ;; Enlarge the max size of the recent files list
  (setq recentf-max-saved-items 10000)
  (recentf-mode t))

;; Init autorevert for updating buffers that were changed on disk
(use-package autorevert
  :straight nil
  :config
  (global-auto-revert-mode))

(setq bookmark-file (mo-cache-path "bookmarks"))
(setq tramp-persistency-file-name (mo-cache-path "tramp"))

;; Init envrc for direnv integration with Emacs
;; This is here on purpose, so that its hooks will be registered as late as possible
(use-package envrc
  :hook
  ;; envrc kills buffer local env var variables (e.g. process-environment). This
  ;; interferes with other modes that set these vars to be local, such as eshell.
  ;; Until fixed, we should load this package only for specific modes.
  ( python-mode . envrc-mode))

;; Set customization file path
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Load customization file
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
