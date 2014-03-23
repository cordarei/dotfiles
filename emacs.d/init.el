;; setup for requiring and installing packages

(require 'cl)
(require 'package)

;; add extra ELPA archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

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

;; evil-mode
(use-package evil
  :ensure evil
  :config
  (progn (evil-mode 1)))

;; markdown-mode
(use-package markdown-mode
  :ensure markdown-mode
  :commands markdown-mode)

;; monokai theme
(use-package monokai-theme
  :ensure monokai-theme
  :config
  (progn (load-theme 'monokai t)))


;; set up interface customizations

;; behavior:

(setq make-backup-files nil)

;; keybindings:

;; for Windows
(global-set-key [M-kanji] 'ignore)
(global-set-key [kanji] 'ignore)

(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "M-h") 'help-command)

;; GUI settings:

(tool-bar-mode -1)
(menu-bar-mode -1)
