(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(setq packages '(evil markdown-mode monokai-theme))

(defun ensure-package (package)
    (unless (package-installed-p package)
      (package-refresh-contents)
      (package-install package)))

(mapcar 'ensure-package packages)


(require 'evil)
(evil-mode t)


(global-set-key [M-kanji] 'ignore)
(global-set-key [kanji] 'ignore)

(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "M-h") 'help-command)

(setq make-backup-files nil)

(tool-bar-mode -1)
(menu-bar-mode -1)


(load-theme 'monokai t)
