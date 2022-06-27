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
(setq native-comp-deferred-compilation nil)
(defvar straight-disable-native-compile t)

;; Set a directory path to be used for cache files
(defvar mo-cache-dir (expand-file-name ".cache" user-emacs-directory))

(defun mo-cache-path (filename)
  "Return a valid file path for FILENAME under the cache directory."
  (concat (file-name-as-directory mo-cache-dir) filename))

(defvar straight-base-dir mo-cache-dir)

;; Init straight.el for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name ".cache/straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Before loading other packages, set exec-path to the PATH var under the default shell
;; when executed under a windowing system, using the exec-path-from-shell package.
;; This is needed so libgccjit would be found by native compilation

(straight-use-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Re-enable auto & deferred native compilation
(setq straight-disable-native-compile nil)
(setq native-comp-deferred-compilation t)

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

;; Init gcmh for executing GC on idle
(use-package gcmh
  :config
  (gcmh-mode 1))

;; Init evil mode for Vim emulation in Emacs
(use-package evil
  :demand t
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
  :config
  ;; Set word movement to operate on symbol boundaries
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode 1))

;; Prefix keys for quick action menu
(defvar mo-quick-menu-prefix "SPC")
(defvar mo-quick-menu-nn-prefix "M-SPC")

;; Create quick menu map and definer
(general-create-definer mo-quick-menu-def :keymaps 'mo-quick-menu-map)

;; Bind the quick menu map to the leader key and the relevant states
(general-define-key
 :states '(normal insert visual motion emacs)
 :keymaps 'override
 :prefix mo-quick-menu-prefix
 :non-normal-prefix mo-quick-menu-nn-prefix
 :prefix-map 'mo-quick-menu-map
 :which-key "Quick menu prefix key"
 "b" '(:which-key "Buffer")
 "f" '(:which-key "File")
 "s" '(:which-key "Search")
 "v" '(:which-key "View")
 "w" '(:which-key "Window")
 "x" '(:which-key "Utils")
 "t" '(:which-key "Tab")
 "h" '(:which-key "Help")
 "p" '(:which-key "Project")
 "c" '(:which-key "Code")
 "g" '(:which-key "Git")
 "r" '(:which-key "Multiple Cursors")
 "n" '(:which-key "Notes"))

;; Add evil shortcuts here, after the initialization of the quick menu map
(when (featurep 'evil)
  (mo-quick-menu-def
    :prefix "w"
    "v" #'evil-window-vsplit
    "s" #'evil-window-split
    "c" #'evil-window-delete))

;; Quick save key binding
(mo-quick-menu-def
  "SPC" #'save-buffer)

;; Universal argument key binding
(mo-quick-menu-def
  "u" #'universal-argument)

;; Init ibuffer for editing buffer lists
(use-package ibuffer
  :straight nil
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "b"
   "i" #'ibuffer))

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
  :after evil
  :config
  ;; We have our own find references key binding. Remove evil-collection's one.
  ;; evil-collection's find usages overrides evil-mc key bindings.
  (setq evil-collection-want-find-usages-bindings nil)
  (evil-collection-init))

;; Init evil-mc for supporting multiple cursors in evil mode
(use-package evil-mc
  :demand t
  :general
  (:keymaps 'evil-mc-cursors-map
   "d" #'evil-mc-make-and-goto-next-match
   "D" #'evil-mc-make-and-goto-prev-match)
  (:keymaps 'mo-quick-menu-map
   "r" '(:keymap evil-mc-cursors-map :package evil-mc))
  :config
  (global-evil-mc-mode))

;; Init evil-surround for quickly adding paired surrounding characters
(use-package evil-surround
  :demand t
  :general
  (:states 'visual
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

;; Init goggles for highlighting modified regions
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq goggles-pulse-delay 0.05)
  ;; Show a gradual pulse
  (setq-default goggles-pulse t))

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
  (:keymaps 'override
   "C-'" #'avy-goto-char-timer
   "C-\"" #'mo-avy-goto-char-timer-action)
  :config
  ;; Better highlight the leading characters
  (set-face-attribute 'avy-lead-face nil :background "gold1")
  (set-face-attribute 'avy-lead-face-0 nil :background "gold2")
  (set-face-attribute 'avy-lead-face-1 nil :background "gold3")
  (set-face-attribute 'avy-lead-face-2 nil :background "gold4")
  (setq avy-timeout-seconds 0.25))

;; Init evil-snipe for an improved 1 char evil search experience
(use-package evil-snipe
  :config
  (evil-snipe-override-mode 1))

;; Init better-jumper for better controlling the jump list logic
(use-package better-jumper
  :demand t
  :after evil
  :general
  ([remap evil-jump-forward] 'better-jumper-jump-forward)
  ([remap evil-jump-backward] 'better-jumper-jump-backward)
  :config
  ;; Jump list to work as a stack
  (setq better-jumper-add-jump-behavior 'replace)
  (better-jumper-mode +1))

;; Init xref for code reference lookup
(use-package xref
  :straight nil
  :general
  (:keymaps 'mo-quick-menu-map
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
  (:keymaps 'mo-quick-menu-map
   ":" #'dumb-jump-go)
  :init
  (setq dumb-jump-selector #'completing-read))

;; Init origami for text and code folding
(use-package origami
  :config
  (global-origami-mode))

;; Init lsp-origami for code folding based on data from language server
(use-package lsp-origami
  :after (origami lsp)
  :hook
  (lsp-after-open . lsp-origami-try-enable))

;; Init org mode for editing and managing notes
(use-package org
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "n"
   "l" #'org-insert-link
   "o" #'org-open-at-point
   "L" #'org-store-link
   "s" #'org-schedule
   "t" #'org-todo
   "a" #'org-agenda)
  (:keymaps 'org-mode-map
   :states 'normal
   "TAB" #'org-cycle)
  :config
  ;; Visually indent text under bullets
  (setq org-startup-indented t)
  (setq org-cycle-separator-lines 1)
  ;; Customize bullet faces
  (setq org-hidden-keywords '(title))
  (set-face-attribute 'org-level-3 nil :height 1.03)
  (set-face-attribute 'org-level-2 nil :height 1.05)
  (set-face-attribute 'org-level-1 nil :height 1.1)
  (setq org-cycle-level-faces nil)
  (setq org-directory "~/org")
  (setq org-agenda-files `(,org-directory))
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-include-diary t)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-start-on-weekday 0)
  (setq org-agenda-diary-file
        (concat (file-name-as-directory org-directory) "diary.org"))
  (setq org-id-locations-file (mo-cache-path ".org-id-locations"))
  (setq org-src-preserve-indentation t))

;; Init org-superstar for showing org mode bullets as UTF-8 characters
(use-package org-superstar
  :hook
  (org-mode . (lambda () (org-superstar-mode 1)))
  :config
  (setq org-superstar-headline-bullets-list
        '("◉" "◎" "◯" "▶" "▷" "◈" "◇")))

;; Init org-roam for Zettelkasten note management
(use-package org-roam
  :demand t
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "n"
   "b" #'org-roam-buffer-toggle
   "g" #'org-roam-graph
   "i" #'org-roam-node-insert
   "n" #'org-roam-node-find
   "T" #'org-roam-tag-add
   "c" #'org-roam-capture)
  :custom
  (org-roam-directory org-directory)
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-db-location (mo-cache-path "org-roam.db"))
  (org-roam-db-autosync-mode))

;; Init org-pomodoro for using the Pomodoro technique with org mode
(use-package org-pomodoro
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "n"
   "p" #'org-pomodoro))

;; Init calendar for showing a calendar
(use-package calendar
  :straight nil
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "n"
   "C" #'calendar))

;; Init orderless for advanced (e.g. fuzzy) completion styles
(use-package orderless
  :demand t
  :config
  ;; Set matching style to regexp and literal
  (setq orderless-matching-styles '(orderless-regexp orderless-literal))
  :custom (completion-styles '(orderless)))

;; Init vertico for item list selection
(use-package vertico
  ;; Load extensions
  :straight (:files (:defaults "extensions/*"))
  :config
  (setq vertico-count 20)
  (setq vertico-cycle t)
  (vertico-mode))

;; Init vertico-repeat for repeating the last minibuffer command
(use-package vertico-repeat
  :after vertico
  :straight nil
  :general
  (:keymaps 'mo-quick-menu-map
   "z" #'vertico-repeat)
  :hook
  (minibuffer-setup . vertico-repeat-save))

;; Init vertico-directory for directory navigation commands
(use-package vertico-directory
  :after vertico
  :straight nil
  :general
  (:keymaps 'vertico-map
   "M-<backspace>" #'vertico-directory-up))

;; Init vertico-quick for quick result selection
(use-package vertico-quick
  :after vertico
  :straight nil
  :general
  (:keymaps 'vertico-map
   "M-q" #'vertico-quick-jump))

;; Enable recursive minibuffer
(setq enable-recursive-minibuffers t)

;; Init recursion-indicator for indicating minibuffer recursions
(use-package recursion-indicator
  :config
  (recursion-indicator-mode))

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
  (:keymaps 'mo-quick-menu-map
   :prefix "p"
   "w" #'mo-project-save
   "d" #'project-dired
   "c" #'project-compile
   "x" #'project-async-shell-command
   "k" #'project-kill-buffers
   "p" #'project-switch-project)
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
  (:keymaps 'mo-quick-menu-map
   "/" #'consult-line
   "?" #'consult-line-multi
   "." #'consult-fd
   "," #'consult-ripgrep)
  (:keymaps 'mo-quick-menu-map
   :prefix "s"
   "h" #'consult-history
   "m" #'consult-mode-command
   "B" #'consult-bookmark
   "k" #'consult-kmacro
   ":" #'consult-complex-command
   "b" #'consult-buffer
   "O" #'consult-buffer-other-window
   "F" #'consult-buffer-other-frame
   "e" #'consult-compile-error
   "g" #'consult-goto-line
   "o" #'consult-outline
   "M" #'consult-mark
   "K" #'consult-global-mark
   "L" #'consult-locate
   "G" #'consult-git-grep
   "x" #'consult-multi-occur
   "s" #'consult-keep-lines
   "f" #'consult-focus-lines
   "#" #'consult-register-load
   "'" #'consult-register-store
   "r" #'consult-register
   "a" #'consult-apropos)
  (:keymaps 'mo-quick-menu-map
   :prefix "b"
   "b" #'consult-buffer
   "B" #'consult-buffer-other-window)
  ;; C-x bindings (ctl-x-map)
  ("C-x M-:" #'consult-complex-command)     ;; orig. repeat-complex-command
  ("C-x b" #'consult-buffer)                ;; orig. switch-to-buffer
  ("C-x 4 b" #'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" #'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ;; Custom M-# bindings for fast register access
  ("M-#" #'consult-register-load)
  ("M-'" #'consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ("C-M-#" #'consult-register)
  ;; Other custom bindings
  ("M-y" #'consult-yank-pop)                ;; orig. yank-pop
  ("<help> a" #'consult-apropos)            ;; orig. apropos-command
  ;; M-g bindings (goto-map)
  ("M-g g" #'consult-goto-line)             ;; orig. goto-line
  ("M-g M-g" #'consult-goto-line)           ;; orig. goto-line
  (:keymaps 'mo-quick-menu-map
   :prefix "c"
   "i" #'consult-imenu)
  ;; M-s bindings (search-map)
  ("M-s f" #'consult-fd)
  ("M-s L" #'consult-locate)
  ("M-s g" #'consult-grep)
  ("M-s G" #'consult-git-grep)
  ("M-s r" #'consult-ripgrep)
  ("M-s l" #'consult-line)
  ("M-s m" #'consult-multi-occur)
  ("M-s k" #'consult-keep-lines)
  ("M-s u" #'consult-focus-lines)
  ("M-s b" #'consult-line-multi)
  ;; Isearch integration
  ("M-s e" #'consult-isearch)
  (:keymaps 'org-mode-map
   "M-e" #'consult-isearch)                 ;; orig. isearch-edit-string
  ("M-s e" #'consult-isearch)               ;; orig. isearch-edit-string
  ("M-s l" #'consult-line)                  ;; required by consult-line to detect isearch
  (:keymaps 'mo-quick-menu-map
   :prefix "p"
   "b" #'consult-project-buffer)

  :config
  ;; Configure the narrowing key.
  (setq consult-narrow-key ">")

  ;; Use consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

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
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended t)))
      (when re
        (list :command (append
                        (list consult--fd-command
                              "--color=never" "-i" "-p" "-H" "-t" "f"
                              (concat dir ".*" (consult--join-regexps re 'extended)))
                        opts)
              :highlight hl))))

  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (let* ((prompt-dir (consult--directory-prompt "fd" dir))
           (default-directory (cdr prompt-dir)))
      (find-file (consult--find (car prompt-dir)
                                (lambda (input) (consult--fd-builder input default-directory))
                                initial))))

  ;; Do not auto preview ripgrep and recent file results
  (consult-customize
   consult-ripgrep consult--source-recent-file consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Configure project detection using project.el
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))

  ;; On project switch, use consult for file and regexp search
  (setq project-switch-commands
        '((consult-find "Find file" ?f)
          (consult-ripgrep "Ripgrep" ?r)
	  (magit-status "Magit" ?g))))

;; Init consult-flycheck for showing syntax errors with consult
(use-package consult-flycheck
  :general
  (:keymaps 'mo-quick-menu-map
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
  ("C-M-a" #'embark-act)
  (:keymaps 'embark-file-map
   "g" #'mo-embark-magit-status)
  (:keymaps 'mo-quick-menu-map
   :prefix "h"
   "b" #'embark-bindings)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Init embark-consult for enabling embark actions on consult results
(use-package embark-consult
  :demand t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Init wgrep for editing grep-style results across files in grep buffer
(use-package wgrep)

;; Set buffer commands key bindings
(setq mo-binding-next-buffer "C-M-s-j"
      mo-binding-prev-buffer "C-M-s-k")

(general-define-key
 mo-binding-next-buffer #'next-buffer
 mo-binding-prev-buffer #'previous-buffer)

;; Init tab-bar for managing tab views
(use-package tab-bar
  :straight nil
  :init
  ;; Set tab commands key bindings
  (setq mo-binding-next-tab "C-M-s-l"
        mo-binding-prev-tab "C-M-s-h")
  (general-define-key
   mo-binding-next-tab #'tab-next
   mo-binding-prev-tab #'tab-previous)
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "t"
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
  (setq tab-bar-format '(tab-bar-format-tabs-groups
                         tab-bar-separator))
  (setq tab-bar-tab-hints t)
  ;; Init tab-bar for supporting multiple window layouts in frame
  (tab-bar-mode))

;; Init tab-bar-lost-commands for usable tab-bar commands
(use-package tab-bar-lost-commands
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "t"
   "t" #'tab-bar-lost-commands-switch-to-or-create-tab))

;; Init dired for file management
(use-package dired
  :straight nil
  :general
  (:keymaps 'mo-quick-menu-map
   "d" #'dired-jump)
  :config
  (setq dired-dwim-target t))

;; Init dired+ for additional dired functionality
(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag nil)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

;; Init treemacs for a tree-like sidebar file navigator
(use-package treemacs
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "p"
   "s" #'treemacs-select-window
   ;; We want to present the current project only
   "S" #'treemacs-display-current-project-exclusively)
  :config
  (setq treemacs-persist-file (mo-cache-path "treemacs-persist"))
  (setq treemacs-width 50)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'simple))

;; Init treemacs-evil for treemacs and evil integration
(use-package treemacs-evil
  :after (treemacs evil))

;; Init treemacs-icons-dired for having icons in dired mode
(use-package treemacs-icons-dired
  :after (treemacs dired))

;; Init treemacs-magit for treemacs and magit integration
(use-package treemacs-magit
  :after (treemacs magit))

;; Init dired-narrow for narrowing dired results using regexp
(use-package dired-narrow)

;; Init proced for viewing and managing running processes
(use-package proced
  :straight nil
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "x"
   "p" #'proced))

;; Init magit for a better git user experience
(use-package magit
  ;; Refine diff view to show sub hunk changes
  :general
  ;; Visit files in the other window
  (:keymaps 'mo-quick-menu-map
   :prefix "g"
   "g" #'magit-status
   "d" #'magit-dispatch
   "b" #'magit-blame-addition
   "c" #'magit-file-checkout
   "l" #'magit-log-buffer-file
   "f" #'magit-file-dispatch)
  (:keymaps 'magit-diff-section-base-map
   "C-<return>" #'magit-diff-visit-worktree-file-other-window)
  :init
  (setq magit-define-global-key-bindings nil)
  :config
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
  (:keymaps 'mo-quick-menu-map
   "m" '(:keymap smerge-basic-map :package smerge)))

;; Init forge for working with git forges (e.g. Github, Gitlab)
(use-package forge
  :after magit
  :config
  (setq forge-database-file (mo-cache-path "forge-database.sqlite")))

;; Init code-review for helping with code review on git forges
(use-package code-review
  :general
  (:keymaps 'code-review-mode-map
   :states '(normal emacs)
   "r" #'code-review-transient-api
   "RET" #'code-review-comment-add-or-edit)
  (:keymaps 'mo-quick-menu-map
   :prefix "g"
   "r" #'code-review-forge-pr-at-point)
  :config
  (setq code-review-download-dir (mo-cache-path "code-review"))
  (setq code-review-db-database-file (mo-cache-path "code-review-db-file.sqlite")))

;; Init ediff for better diff view and commands
(use-package ediff
  :straight nil
  :config
  ;; Open ediff window in the current frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Set ediff to show diff changes in character-level
  (setq ediff-forward-word-function #'forward-char))

;; Init diff-hl for highlighting uncommitted changes
(use-package diff-hl
  :demand t
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "g"
   "]" #'diff-hl-next-hunk
   "[" #'diff-hl-previous-hunk)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  (dired-mode . diff-hl-dired-mode-unless-remote)
  :config
  (global-diff-hl-mode))

;; Init git-link for creating URLs for files in web git services
(use-package git-link
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "g"
   "L" #'git-link)
  :config
  (setq git-link-use-commit t))

;; Init lsp mode for lsp support
(use-package lsp-mode
  :general
  ;; Set the lsp prefix key
  (:keymaps 'lsp-mode-map
   "C-c l" '(:keymap lsp-command-map :which-key "lsp"))
  (:keymaps 'mo-quick-menu-map
   :prefix "c"
   "a" #'lsp-execute-code-action
   "r" #'lsp-rename
   "R" #'lsp-workspace-restart
   "I" #'lsp-ui-imenu
   "o" #'lsp-clangd-find-other-file)
  (:keymaps 'mo-quick-menu-map
   "\"" #'lsp-find-implementation)
  :init
  ;; Set a high read output max value for handling large language server responses
  (setq read-process-output-max (* 10 1024 1024))
  ;; Reduce the max number of files to watch before showing warning
  (setq lsp-file-watch-threshold 500)
  ;; Set a short delay for refreshing state after moving the cursor
  (setq lsp-idle-delay 0.2)
  ;; Enable which-key help on the lsp prefix key
  (setq lsp-keymap-prefix "C-c l")
  ;; Enable for the following modes
  (setq mo-lsp-enable-for-modes '(c-mode
                                  c++-mode
                                  objc-mode
                                  swift-mode
                                  haskell-mode
                                  haskell-literate-mode
                                  go-mode
                                  csharp-mode
                                  java-mode
                                  (python-mode (lambda () (require 'lsp-pyright)))
                                  js2-mode
                                  typescript-mode
                                  groovy-mode
                                  web-mode
                                  json-mode
                                  yaml-mode
                                  dockerfile-mode
                                  terraform-mode
                                  cmake-mode
                                  sh-mode))

  (defun mo-maybe-enable-lsp (lsp-config)
    "If mode in LSP-CONFIG is equal to the current major-mode,
run the attached function (if exists) and enable lsp"
    (pcase lsp-config
      (`(,(pred (equal major-mode)) ,func) (funcall func) (lsp) t)
      ((pred (equal major-mode)) (lsp) t)))

  ;; Kill language server after the last associated buffer was closed
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-session-file (mo-cache-path "lsp-session-v1"))
  (setq lsp-eslint-library-choices-file (mo-cache-path ".lsp-eslint-choices"))
  ;; Force lsp mode to forget the workspace folders for multi root servers
  ;; so the folders are added on demand
  (advice-add 'lsp :before
              (lambda (&rest _args)
                (eval
                 '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
  ;; Enable semantic token highlighting
  (setq lsp-semantic-tokens-enable t)
  ;; Set clangd default parameters
  (setq lsp-clients-clangd-args '("--header-insertion-decorators=0"
                                  "--completion-style=detailed"))
  :hook
  ;; Postpone lsp load for after dir local vars are read
  ;; Do not load lsp if dir local vars are not enabled (e.g. on preview)
  (hack-local-variables . (lambda ()
                            (when enable-dir-local-variables
                              (seq-find #'mo-maybe-enable-lsp
                                        mo-lsp-enable-for-modes))))

  ;; Enable which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

;; Init lsp-ui for an interactive lsp interface
(use-package lsp-ui
  :after lsp-mode
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "c"
   "d" #'mo-toggle-lsp-doc-show)
  :config
  (defun mo-toggle-lsp-doc-show ()
    "Toggle showing documentation for things under the cursor using lsp."
    (interactive)
    (setq lsp-ui-doc-show-with-cursor (not lsp-ui-doc-show-with-cursor)))
  ;; Do not show documentation automatically
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-position 'top))

;; Init lsp-treemacs for an interactive lsp tree-like interface
(use-package lsp-treemacs
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "c"
   "h" #'lsp-treemacs-call-hierarchy
   "H" #'lsp-treemacs-type-hierarchy
   "S" #'lsp-treemacs-symbols
   "F" #'lsp-treemacs-errors-list))

;; Init dap-mode for interactive debugging
(use-package dap-mode
  :after lsp-mode
  :demand t
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "c"
   "." #'dap-debug
   "," #'dap-hydra)
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
  :init
  (global-flycheck-mode))

;; Init company mode for auto completion everywhere
(use-package company
  :general
  ("M-<tab>" #'company-complete)
  :init
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  (global-company-mode))

;; Init company-prescient for sorting auto completions
(use-package company-prescient
  :after company
  :config
  (setq prescient-save-file (mo-cache-path "persp-state"))
  (setq prescient-sort-full-matches-first t)
  (prescient-persist-mode +1)
  (company-prescient-mode))

;; Init consult-lsp for additional interacitve lsp commands
(use-package consult-lsp
  :after (lsp-mode consult)
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "c"
   "s" #'consult-lsp-symbols)
  :config
  ;; Manual preview key for symbols results
  (consult-customize consult-lsp-symbols :preview-key (kbd "M-."))
  ;; Remove initial async separator as we use spaces for search tokenization
  (consult-customize consult-lsp-symbols :initial nil))

;; Init cc-mode for C/C++/Obj-C support
(use-package cc-mode
  :straight nil
  :config
  ;; Set the default style
  (setf (alist-get 'other c-default-style) "stroustrup"))

;; Init yasnippets for adding code snippet templates
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Init yasnippet-snippets for common code templates
(use-package yasnippet-snippets)

;; Init rustic for Rust support
(use-package rustic)

;; Associate objc-mode with Objective C files
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

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

;; Init csharp-mode for C# support
(use-package csharp-mode)

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

;; Init pipenv for supporting pipenv projects and commands
(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :config
  ;; We don't use projectile
  (setq pipenv-with-projectile nil))

;; Init js2-mode for enhanced JavaScript editing
(use-package js2-mode
  :mode "\\.js\\'")

;; Init typescript-mode for enhanced TypeScript editing
(use-package typescript-mode)

;; Init groovy-mode for Groovy support
(use-package groovy-mode)

;; Init jenkinsfile-mode for editing Jenkins files
(use-package jenkinsfile-mode)

;; Init web-mode for enhanced web files editing
(use-package web-mode
  :mode "\\.html?\\'" "\\.tsx\\'")

;; Init json mode for enhanced JSON editing
(use-package json-mode)

;; Init yaml-mode for enhanced YAML editing
(use-package yaml-mode)

;; Init dockerfile-mode for editing docker files
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; Init docker for managing docker from Emacs
(use-package docker
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "c"
   "D" #'docker))

;; Init docker-tramp for supporting TRAMP in containers
(use-package docker-tramp)

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
(use-package format-all)

;; Disable default tab indentation
(setq-default indent-tabs-mode nil)

;; Init dtrt-indent for auto indentation detection
(use-package dtrt-indent
  :hook
  (prog-mode . dtrt-indent-mode))

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
  (:keymaps 'mo-quick-menu-map
   :prefix "h"
   "i" #'info))

;; Init helpful for better lisp help
(use-package helpful
  :general
  ("C-h f" #'helpful-callable)
  ("C-h v" #'helpful-variable)
  ("C-h k" #'helpful-key)
  ("C-h F" #'helpful-function)
  ("C-h C" #'helpful-command)
  (:keymaps 'mo-quick-menu-map
   :prefix "h"
   "f" #'helpful-callable
   "v" #'helpful-variable
   "k" #'helpful-key
   "h" #'helpful-at-point))

;; Init devdocs for viewing online dev documentation
(use-package devdocs
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "h"
   "d" #'devdocs-lookup)
  :config
  (setq devdocs-data-dir (mo-cache-path "devdocs")))

;; Init google-this for quick Google searches from Emacs
(use-package google-this
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "h"
   "g" #'google-this)
  :config
  (google-this-mode 1))

;; Init rainbow-delimiters for highlighting parens by their depth
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Show matching parentheses
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Auto insert matching parentheses in code
(add-hook 'prog-mode-hook 'electric-pair-local-mode)

;; Init vi-tilde-fringe for marking empty lines on the margin column
(use-package vi-tilde-fringe
  :hook
  (prog-mode . vi-tilde-fringe-mode))

;; Init evil-nerd-commenter for comment editing
(use-package evil-nerd-commenter
  :after evil
  :general
  (:states '(normal, visual) "gc" #'evilnc-comment-operator))

;; Init hl-todo for highlighting specific keywords (e.g. TODO)
(use-package hl-todo
  :hook
  (prog-mode . global-hl-todo-mode))

;; A fast key binding for showing the next command's result in another window.
;; Make sure it also works when the command is using 'switch-to-buffer'.
(setq switch-to-buffer-obey-display-actions t)
(general-define-key "M-[" #'other-window-prefix)

;; Init ace-window for fast window selection
(use-package ace-window
  :general
  ("M-o" #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Init vterm for terminal emulation
(use-package vterm
  :demand t
  :if (not (eq system-type 'windows-nt))
  :general
  (:keymaps 'mo-quick-menu-map
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
  ;; Exclude next/previous tab/buffer key bindings (incl. original excludes)
  (setq vterm-keymap-exceptions
        (append '("C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-y" "M-y")
                `(,mo-binding-next-buffer
                  ,mo-binding-prev-buffer
                  ,mo-binding-next-tab
                  ,mo-binding-prev-tab))))

;; Set eshell cache directory
(setq eshell-directory-name (file-name-as-directory (mo-cache-path "eshell")))

;; Init all-the-icons for icon support
(use-package all-the-icons
  :if (display-graphic-p))

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
  (setq dashboard-items '((projects . 10)
                          (recents  . 10)
                          (bookmarks . 10)))
  (dashboard-setup-startup-hook))

;; Init doom one theme
(use-package doom-themes
  :after treemacs-icons-dired
  :custom-face
  (org-document-title ((nil (:height 1.2))))
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Set brighter comments
  (setq doom-one-brighter-comments t)
  (load-theme 'doom-one t)
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

;; Init flyspell-correct for spell correction
(use-package flyspell-correct
  :demand t
  :general
  (:states 'normal "z =" #'flyspell-correct-wrapper)
  :hook
  ;; Enable spell checking
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

;; Init desktop+ for saving session configuration
(use-package desktop+
  :general
  (:keymaps 'mo-quick-menu-map
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
    t)

  ;; Before exiting Emacs, ask the user to save the current session
  (add-to-list 'kill-emacs-query-functions #'mo-ask-save-desktop t))

;; Init zoom-window for toggling window zoom
(use-package zoom-window
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "w"
   "z" #'zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color "#3b404e")
  (minions-mode 1))

;; Init zoom-frm to scale text in frame
(use-package zoom-frm
  :general
  ("s--" #'zoom-in/out)
  ("s-+" #'zoom-in/out)
  ("s-=" #'zoom-in/out))

;; Init popper for managing popup windows
(use-package popper
  :demand t
  :general
  ("C-`" #'popper-toggle-latest
   "M-`" #'popper-cycle
   "C-M-`" #'popper-toggle-type)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
          devdocs-mode
          compilation-mode
          "^\\*eshell.*\\*$" eshell-mode
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
(toggle-scroll-bar -1)

;; Disable cursor blink
(blink-cursor-mode 0)

;; Inhibit the splash screen
(setq inhibit-splash-screen t)

;; Enable winner-mode for window management
(use-package winner
  :straight nil
  :demand t
  :general
  (:keymaps 'mo-quick-menu-map
   :prefix "w"
   "<left>" #'winner-undo
   "<right>" #'winner-redo)
  :config
  (setq winner-dont-bind-my-keys t)
  (winner-mode 1))

;; Set the default initial frame size
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 210))

;; Scroll incrementally
(setq scroll-step 1)
;; Don't automatically recenter after scrolling
(setq scroll-conservatively 101)

;; Ask for confirmation before exiting emacs
(setq confirm-kill-emacs #'y-or-n-p)

;; Replace yes or no questions to y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't create backup, autosave and lock files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

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
  (setq whitespace-style '(face trailing))
  :hook
  (prog-mode . whitespace-mode))

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

;; Set customization file path
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Load customization file
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
