;;; init.el


;;;; startup customization -- run first to reduce flicker etc

;; remove GUI stuff
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; disable bell
(setq ring-bell-function 'ignore)

;; show blank *scratch* buffer at start
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)


;;;; package setup

(require 'cl)
(require 'package)

;; add extra ELPA archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)

;; initialize package system
(package-initialize)

;; get list of packages if not present
(unless package-archive-contents
  (package-refresh-contents))

;; make sure use-package is installed (we'll use it to install other packages)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; make use-package available
(require 'use-package)


;; set up individual packages

;; monokai theme
(use-package monokai-theme
  :ensure monokai-theme
  :config
  (progn (load-theme 'monokai t)))

;; evil-mode
(use-package evil
  :ensure evil
  :config
  (progn (evil-mode 1)))

;; markdown-mode
(use-package markdown-mode
  :ensure markdown-mode
  :commands markdown-mode)

;; org-mode
(use-package org
  :ensure org
  :commands org-mode
  :config
  (progn
    (setq org-agenda-files (list "~/Wiki/index.org"))
    (setq org-log-done t)
    (setq org-agenda-span 'month)
    (define-key global-map (kbd "C-c a") 'org-agenda)))


;;;; behavior customization

;;  disable auto-save and backup files
(setq auto-save-default nil)
(setq make-backup-files nil)


;;;; keybindings

;;  for Windows
(global-set-key [M-kanji] 'ignore)
(global-set-key [kanji] 'ignore)

;;  map C-h to backspace
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "M-h") 'help-command)


;;; end of init.el
