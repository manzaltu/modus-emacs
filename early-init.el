;;; early-init.el --- Modus Operandi Emacs Configuration -*- lexical-binding: t -*-

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

;; Set GC threshold to a high number. Should help with lsp-mode memory demands.
(setq gc-cons-threshold 100000000)

;; We use straight.el to manage our packages. Disable package.el.
(setq package-enable-at-startup nil)

;; Configure the initial frame shape here, before the frame is created, to
;; avoid a visible reflow when these would otherwise be applied post-creation.
;; Inhibit the splash screen
(setq inhibit-splash-screen t)
;; Don't display startup screen
(setq inhibit-startup-screen t)
;; Remove frame decoration
(setq default-frame-alist '( ( undecorated . t)))
;; Enable pixelwise resizing
(setq frame-resize-pixelwise t)
;; Don't let implied resizes negotiate with the WM during early frame setup
(setq frame-inhibit-implied-resize t)
;; Set the default initial frame size
(add-to-list 'default-frame-alist '( height . 55))
(add-to-list 'default-frame-alist '( width . 210))

;;; early-init.el ends here
