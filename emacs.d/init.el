
;;;;
;;
;; This file was tangled from `init.org' using Org Babel -- see that
;; file for a literate version of this configuration.
;;
;;;;

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq ring-bell-function 'ignore)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(setq auto-save-default nil)
(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))

(setq whitespace-display-mappings
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))

(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

(define-key key-translation-map [?\C-h] [?\C-?])

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq sentence-end-double-space nil)

(electric-indent-mode 1)

(defun windows-p ()
  (eq system-type 'windows-nt))

(defun add-to-path-string (dir path)
  (if (string-equal "" (or path ""))
      dir
    (concat dir path-separator path)))

(defun add-to-exec-path (dir)
  (setenv "PATH"
          (add-to-path-string
           (expand-file-name dir "~")
           (getenv "PATH")))
  (add-to-list 'exec-path dir))

(unless (windows-p)
  (add-to-exec-path (expand-file-name ".local/bin" "~")))

(defconst dir/emacsd (file-name-as-directory
                      (expand-file-name ".emacs.d" (getenv "HOME")))
  "Emacs configuration directory.")

(defconst dir/home (file-name-as-directory
                    (if (windows-p)
                        (getenv "USERPROFILE")
                      (expand-file-name "~")))
  "User's home directory.")

(defconst dir/org (file-name-as-directory
                   (expand-file-name
                    (if (windows-p)
                        "Documents/Org"
                      "Org")
                    dir/home))
  "Org file directory.")

(setq toggle/prefix "C-x t")
(setq toggle/keymap (make-sparse-keymap))

(defun toggle/bind-key (key command)
  (define-key toggle/keymap key command))

(global-set-key (kbd toggle/prefix) toggle/keymap)

(toggle/bind-key "l" 'whitespace-mode)
(toggle/bind-key "w" 'toggle-truncate-lines)

(winner-mode 1)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package evil
  :ensure t
  :demand t
  :init
  (progn
    ; from https://github.com/syl20bnr/spacemacs/blob/master/spacemacs/packages.el
    (setq evil-mode-line-format 'before)
    (setq evil-emacs-state-cursor  '("red" box))
    (setq evil-normal-state-cursor '("orange" box))
    (setq evil-visual-state-cursor '("black" box))
    (setq evil-insert-state-cursor '("green3" bar))
    (setq evil-motion-state-cursor '("purple" box))
    )
  :config
  (progn

    ; load evil-leader before (evil-mode 1)
    (use-package evil-leader
      :ensure t
      :init
      (progn
        (evil-leader/set-leader ",")
        (global-evil-leader-mode))
      :config
      (progn
        (evil-leader/set-key "t" toggle/keymap)
        ))

    (evil-mode 1)

    (use-package evil-surround
      :ensure t
      :init (global-evil-surround-mode 1))

    ;; add useful keys
    (define-key evil-normal-state-map (kbd "C-w q") 'evil-quit)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

    ;; unbind keys
    ;; insert
    (define-key evil-insert-state-map (kbd "C-e") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil)
    (define-key evil-insert-state-map (kbd "C-n") nil)
    ;; normal
    (define-key evil-normal-state-map (kbd "C-p") nil)
    (define-key evil-normal-state-map (kbd "C-n") nil)
    (define-key evil-normal-state-map (kbd "M-.") nil)
    ))

(use-package powerline
  :ensure t
  :demand t
  :config
  (progn
    (toggle/bind-key "m" 'powerline-minor-modes-toggle)

    (defun propertize-evil-mode-line-tag ()
      (propertize evil-mode-line-tag 'font-lock-face
                  ;; Don't propertize if we're not in the selected buffer
                  (cond ((not (eq (current-buffer) (car (buffer-list)))) '())
                        ((evil-insert-state-p) '(:background "green3" :foreground "black"))
                        ((evil-emacs-state-p)  '(:background "red" :foreground "black"))
                        ((evil-motion-state-p) '(:background "purple" :foreground "black"))
                        ((evil-visual-state-p) '(:background "gray" :foreground "black"))
                        ((evil-normal-state-p)  '(:background "orange" :foreground "black"))
                        (t '()))))

    (defpowerline powerline-evil-mode (propertize-evil-mode-line-tag))

    (defvar powerline-minor-modesp nil)
    (defun powerline-minor-modes-toggle ()
      "Toggle display of minor modes."
      (interactive)
      (if powerline-minor-modesp
          (setq powerline-minor-modesp nil)
        (setq powerline-minor-modesp t)))

    (defun my-powerline-theme ()
      "Set up my powerline theme with evil mode etc."
      (interactive)
      (setq-default mode-line-format
                    '("%e"
                      (:eval
                       (let* ((active (eq (frame-selected-window) (selected-window)))
                              (face1 (if active 'powerline-active1 'powerline-inactive1))
                              (face2 (if active 'powerline-active2 'powerline-inactive2))
                              (lhs (append (list
                                            ;; (powerline-window-number face1 'l)
                                            (powerline-evil-mode face1 'l)

                                            (powerline-raw "%*" nil 'l)
                                            (powerline-buffer-size nil 'l)
                                            (powerline-buffer-id nil 'l)
                                            (powerline-raw " " nil)

                                            (powerline-arrow-right nil face1)
                                            (powerline-major-mode face1 'l)
                                            (powerline-raw " " face1))

                                           (if powerline-minor-modesp
                                               (list (powerline-arrow-right face1 nil)
                                                     (powerline-minor-modes nil 'l)
                                                     (powerline-raw mode-line-process nil 'l)
                                                     (powerline-raw " " nil)
                                                     (powerline-arrow-right nil face2))
                                             (list (powerline-raw " " face1)
                                                   (powerline-arrow-right face1 face2)))

                                           (list (powerline-vc face2))))
                              (rhs (list
                                    (powerline-raw global-mode-string face2 'r)
                                    (powerline-raw " " face2)

                                    (powerline-arrow-left face2 face1)
                                    (powerline-raw " " face1)
                                    (powerline-raw "%l:%2c" face1 'r)
                                    (powerline-arrow-left face1 nil)
                                    (powerline-raw " " nil)
                                    (powerline-raw "%p" nil 'r)

                                    (powerline-hud face2 face1))))
                         (concat
                          (powerline-render lhs)
                          (powerline-fill face2 (powerline-width rhs))
                          (powerline-render rhs)))))))
    ))

(use-package moe-theme
  :ensure t
  :config
  (progn
    ;; Resize titles
    ;; (setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0))
    ;; (setq moe-theme-resize-org-title '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
    ;; (setq moe-theme-resize-rst-title '(2.0 1.7 1.5 1.3 1.1 1.0))

    (setq moe-theme-mode-line-color 'green)

    (moe-dark)
    (powerline-moe-theme)
    ))

(my-powerline-theme)

(use-package org
  :ensure t
  :commands org-mode
  :config
  (progn
    ;; configure general org-mode behavior
    (setq org-startup-indented t)
    (setq org-startup-folded 'content)
    (setq org-directory dir/org)

    ;; configure org-agenda
    (setq org-agenda-files (list "agenda.org"))
    (setq org-log-done t)
    (setq org-agenda-span 'month)
    (setq org-agenda-window-setup 'current-window)
    (define-key global-map (kbd "C-c a") 'org-agenda)

    ;; configure latex export
    (setq org-latex-pdf-process
          '("latexmk -c" "latexmk -g -xelatex %f"))

    ;; org-babel for literate programming
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((emacs-lisp . t)
                                   (clojure . t)
                                   (python . t)
                                   (sh . t)
                                   (gnuplot . t)))
    (setq org-src-fontify-natively t)
    (setq org-src-preserve-indentation t)

    (setq org-export-html-coding-system 'utf-8)
    (setq org-export-babel-evaluate nil)

    (use-package htmlize :ensure t)

    (use-package org-indent :diminish "")
    ))

(use-package magit
  :ensure t
  :commands magit-status
  :diminish magit-auto-revert-mode
  :bind ("C-x g" . magit-status))

(use-package helm
  :ensure t
  :diminish ""
  :init
  (progn

    (setq helm-command-prefix-key "C-c h")

    (require 'helm-config)
    (require 'helm-eshell)
    (require 'helm-files)
    (require 'helm-grep)

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z")  'helm-select-action)

    (setq helm-candidate-number-limit 10)

    (setq helm-idle-delay 0.1
          helm-input-idle-delay 0.01
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
          helm-buffers-fuzzy-matching t
          helm-ff-file-name-history-use-recentf t
          helm-move-to-line-cycle-in-source t
          helm-split-window-default-side 'other
          helm-split-window-in-side-p t
          )

    (evil-leader/set-key ":" 'helm-M-x
                         "b" 'helm-mini)


    (global-set-key (kbd "C-c h o") 'helm-occur)
    (global-set-key (kbd "C-c h g") 'helm-do-grep)

    (helm-mode)

    (use-package helm-gtags
      :ensure t
      :defer t
      :diminish ""
      :init
      (progn
        (setq helm-gtags-prefix-key "\C-cg"
              helm-gtags-suggested-key-mapping nil
              helm-gtags-ignore-case t
              helm-gtags-auto-update t
              helm-gtags-use-input-at-cursor t
              helm-gtags-pulse-at-cursor t
              )

        (add-hook 'dired-mode-hook 'helm-gtags-mode)
        (add-hook 'eshell-mode-hook 'helm-gtags-mode)
        (add-hook 'prog-mode-hook 'helm-gtags-mode)
        )
      :config
      (progn
        (global-set-key (kbd "M-.") 'helm-gtags-dwim)
        ;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
        ;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
        ;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
        ))
    ))

(use-package company
  :ensure t
  :diminish ""
  :config
  (progn

    (use-package semantic
      :diminish abbrev-mode
      :config
      (progn
        (global-semanticdb-minor-mode 1)
        (global-semantic-idle-scheduler-mode 1)
        (global-semantic-stickyfunc-mode 1)

        (semantic-mode 1)
        ))

    (global-company-mode)
    ))

(use-package tex
  :mode ("\\.tex\\'" . latex-mode)
  :ensure auctex)
(use-package reftex
  :ensure t)

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'")

(use-package cider
  :defer t
  :ensure t)

(use-package gnuplot
  :ensure t
  :commands gnuplot-mode)

(use-package julia-mode
  :load-path "~/.emacs.d/elisp/"
  :mode "\\.jl\\'")

(use-package python
  :commands python-mode
  :mode ("wscript\\'" . python-mode))

(use-package markdown-mode
  :ensure t
  :commands markdown-mode)

(use-package unicycle-mode
  :load-path "~/.emacs.d/elisp/"
  :commands unicycle-mode
  :diminish " ¶"
  :init (toggle/bind-key "u" 'unicycle-mode))

(use-package undo-tree
  :diminish "")
