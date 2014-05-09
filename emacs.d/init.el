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
;; (use-package monokai-theme
;;   :ensure t
;;   :config
;;   (progn (load-theme 'monokai t)))

;; zenburn theme
(use-package zenburn-theme
  :ensure t
  :config
  (progn (load-theme 'zenburn t)))

;; evil-mode
(use-package evil
  :ensure t
  :config
  (progn
    (evil-mode 1)
    (define-key evil-normal-state-map (kbd "C-w q") 'evil-quit)
    (define-key evil-normal-state-map "gol" 'whitespace-mode)
    (define-key evil-normal-state-map "gow" 'toggle-truncate-lines)

    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "C-p") nil)
    (define-key evil-insert-state-map (kbd "C-n") nil)
    ))

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :commands markdown-mode)

;; org-mode
(use-package org
  :ensure t
  :commands org-mode
  :config
  (progn
    ;; configure general org-mode behavior
    (setq org-startup-indented t)
    ;(setq org-cycle-separator-lines 1)

    ;; configure org-agenda
    (setq org-agenda-files (list "~/Org/"))
    (setq org-log-done t)
    (setq org-agenda-span 'month)
    (define-key global-map (kbd "C-c a") 'org-agenda)

    ;; org-babel for literate programming
    (org-babel-do-load-languages 'org-babel-load-languages
				 '((emacs-lisp . t)
				   (clojure . t)
				   (python . t)))
    (setq org-src-fontify-natively t)
    (setq org-src-preserve-indentation t)
    ))

;;julia-mode
(use-package julia-mode
  :load-path "~/.emacs.d/elisp/"
  :mode "\\.jl\\'")

(use-package clojure-mode
  :ensure t
  :commands clojure-mode)

(use-package magit
  :ensure t
  :commands magit-status)


;;;; behavior customization

;;  disable auto-save and backup files
(setq auto-save-default nil)
(setq make-backup-files nil)

;; whitespace
;; TODO: combine this with whitespace-mode and make faces nicer
;; (see http://stackoverflow.com/questions/14636786/how-to-unset-the-foreground-color-of-whitespace-mode-for-emacs)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq show-trailing-whitespace t)))

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;; keybindings

;;  for Windows
(global-set-key [M-kanji] 'ignore)
(global-set-key [kanji] 'ignore)

;;  map C-h to backspace
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "M-h") 'help-command)

;;;; keep separate custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
