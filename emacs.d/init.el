
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

(add-hook 'prog-mode-hook
          (lambda ()
            (show-paren-mode)))

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
(toggle/bind-key "e" 'electric-indent-mode)

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

    (defvar powerline-minor-modes-p t)
    (defun powerline-minor-modes-toggle ()
      "Toggle display of minor modes."
      (interactive)
      (if powerline-minor-modes-p
          (setq powerline-minor-modes-p nil)
        (setq powerline-minor-modes-p t)))

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

                                           (if powerline-minor-modes-p
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

    ;; setting the powerline mode line in 'after-init-hook solves problems with other packages screwing it up (looking at you, workgroups2!)
    (add-hook 'after-init-hook (lambda () (my-powerline-theme)))
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

(use-package org
  :defer t
  :commands org-agenda
  :init
  (progn
    ;; general org-mode behavior
    (setq org-directory dir/org
          org-startup-indented t
          org-startup-folded 'content
          org-use-speed-commands t
          org-catch-invisible-edits 'error
          )

    ;; org-agenda
    (define-key global-map (kbd "C-c a") 'org-agenda)
    (setq org-agenda-files (list (expand-file-name "agenda.org" dir/org))
          org-agenda-span 'month
          org-agenda-window-setup 'current-window
          org-log-done t
          )

    ;; enable org-export backends
    (setq org-export-backends '(ascii html latex beamer))
    )
  :config
  (progn

    ;; configure sub packages and other packages used with org-mode
    (use-package org-indent :diminish "")
    (use-package htmlize :ensure t)


    ;; do more intensive customization of org export:
    ;; - latex
    (setq org-latex-pdf-process '("latexmk -g -xelatex %f")
          org-latex-default-packages-alist nil
          org-latex-packages-alist '(("" "graphicx" nil) ("" "float" nil))
          org-latex-default-class "default"
          )
    (setq org-latex-classes ; this really calls for a macro
          '(
            ("default"
             "\\documentclass[11pt]{article}
[PACKAGES]
\\usepackage{amssymb}
\\usepackage{fontspec}
\\usepackage{xunicode}
\\usepackage{url}
\\usepackage{hyperref}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

            ("beamer"
             "\\documentclass{beamer}
[PACKAGES]
\\usepackage{amssymb}
\\usepackage{fontspec}
\\usepackage{xunicode}
\\usepackage{url}
\\usepackage{hyperref}
\\setbeamertemplate{navigation symbols}{}
"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
            ))


    ;; org-babel for literate programming
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (clojure . t)
       (emacs-lisp . t)
       (gnuplot . t)
       (python . t)
       (sh . t)
      )
    )
    (setq org-src-fontify-natively t)
    (setq org-src-preserve-indentation t)
    ))

(use-package magit
  :ensure t
  :commands magit-status
  :diminish magit-auto-revert-mode
  :bind ("C-x g" . magit-status)
  :config
  (define-key magit-status-mode-map (kbd "k") nil)
  (define-key magit-status-mode-map (kbd "K") 'magit-discard-item)
  )

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

    (setq helm-idle-delay 0.1
          helm-input-idle-delay 0.01
          helm-quick-update t
          helm-buffers-fuzzy-matching t
          helm-ff-skip-boring-files t
          helm-ff-file-name-history-use-recentf t
          helm-split-window-in-side-p t
          )

    ;; key bindings within helm buffer
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z")  'helm-select-action)

    ;; key bindings starting with `helm-command-prefix-key'
    (define-key helm-command-map (kbd "o") 'helm-occur)
    (define-key helm-command-map (kbd "g") 'helm-do-grep)

    ;; key bindings using evil-leader
    (evil-leader/set-key ":" 'helm-M-x
                         "f" 'helm-for-files
                         "b" 'helm-mini)

    ;; enable helm in built-in commands
    (helm-mode 1)
    ;; replace some built-in commands with helm commands
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-x C-b") 'helm-buffers-list)

    (use-package helm-gtags
      :ensure t
      :if (not (windows-p))
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

        ;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
        ;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
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
    (setq company-selection-wrap-around t)

    (use-package semantic
      :diminish abbrev-mode
      :disabled t
      :config
      (progn
        (global-semanticdb-minor-mode 1)
        (global-semantic-idle-scheduler-mode 1)
        (global-semantic-stickyfunc-mode 1)

        (semantic-mode 1)
        ))

    (use-package company-irony
      :ensure t
      :if (not (windows-p))
      :diminish ""
      :config
      (use-package irony-mode
        :commands irony-mode
        :config
        (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
        )
      (add-to-list 'company-backends 'company-irony)
      )

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

(use-package lua-mode
  :ensure t
  :commands lua-mode
  :mode "\\.lua\\'")

(use-package cc-mode
  :commands (c-mode c++-mode)
  :mode ("\\.h\\(pp|h\\)?\\'" . c++-mode)
  :config
  (use-package clang-format
    :if (file-exists-p "/usr/share/clang/clang-format.el")
    :load-path "/usr/share/clang/"
    :commands clang-format-region)

  (defun my-c-mode-common-hook ()
    ;; http://programmers.stackexchange.com/q/87250
    (font-lock-add-keywords nil
                            '(("\\<\\(assert\(.*\);\\)" 1 '(:foreground "#444444") t)
                              ("\\<assert\\(\(.*\);\\)" 1 '(:foreground "#666666") t)))
    )

  (defun my-c++-mode-hook ()
    (define-key c++-mode-map (kbd "M-q") 'clang-format-region)
    (irony-mode)
    )
  :init
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
  (add-hook 'c++-mode-hook 'my-c++-mode-hook)
  )

(use-package markdown-mode
  :ensure t
  :commands markdown-mode)

(use-package tup-mode
  :ensure t
  :commands tup-mode
  :mode "Tupfile.ini")

(use-package unicycle-mode
  :load-path "~/.emacs.d/elisp/"
  :commands unicycle-mode
  :diminish (unicycle-mode . " ¶")
  :init (toggle/bind-key "u" 'unicycle-mode))

(use-package undo-tree
  :diminish "")

(use-package workgroups2
  :ensure t
  :diminish (workgroups-mode . "")
  :config
  (setq wg-mode-line-display-on nil)
  (setq wg-prefix-key (kbd "C-c z"))
  (setq wg-session-file "~/.emacs.d/workgroups")
  :init
  (workgroups-mode 1)
  )
