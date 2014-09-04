;;; unicycle-mode.el — Cycle Unicode punctuation marks

;; Copyright © 2014 Joseph Irwin

;; This is a port of UniCycle.vim (https://github.com/cordarei/UniCycle) to Emacs
;;
;; Probably requires Emacs 24, assumes unicode encoding, etc.
;; It Works For Me™

(defun unicycle/hyphen ()
  (interactive)
  (let ((prev (char-before)))
    (cond ((eq prev ?-)
           (delete-char -1)
           (insert #x2013)) ; EN-DASH (–)
          ((eq prev #x2013)
           (delete-char -1)
           (insert #x2014)) ; EM-DASH (—)
          ((eq prev #x2014)
           (delete-char -1)
           (insert ?-))
          (t (insert ?-)))))

;; TODO: select initial character (quote) more intelligently
(defun unicycle/apostrophe ()
  (interactive)
  (let ((prev (char-before)))
    (cond ((eq prev ?')
           (delete-char -1)
           (insert #x2019)) ; RIGHT SINGLE QUOTATION MARK (’)
          ((eq prev #x2019)
           (delete-char -1)
           (insert #x2018)) ; LEFT SINGLE QUOTATION MARK (‘)
          ((eq prev #x2018)
           (delete-char -1)
           (insert ?'))
          (t (insert #x2019))))) ; RIGHT SINGLE QUOTATION MARK (’)

;; TODO: select initial character (quote) more intelligently
(defun unicycle/double-quote ()
  (interactive)
  (let ((prev (char-before)))
    (cond ((eq prev ?\")
           (delete-char -1)
           (insert #x201C)) ; LEFT DOUBLE QUOTATION MARK (“)
          ((eq prev #x201C)
           (delete-char -1)
           (insert #x201D)) ; RIGHT DOUBLE QUOTATION MARK (”)
          ((eq prev #x201D)
           (delete-char -1)
           (insert ?\"))
          (t (insert ?\")))))

(defun unicycle/period ()
  (interactive)
  (let ((prev (char-before))
        (prev1 (char-before (- (point) 1))))
    (cond ((and (eq prev ?.) (eq prev1 ?.))
           (delete-char -2)
           (insert #x2026)) ; HORIZONTAL ELLIPSIS (…)
          (t (insert ?.)))))

(defun unicycle/greater-than ()
  "translate <> to ◊"
  (interactive)
  (let ((prev (char-before)))
    (cond ((eq prev ?<)
           (delete-char -1)
           (insert #x25CA)) ; LOZENGE (◊)
          ((eq prev ?>)
           (delete-char -1)
           (insert #xBB)) ; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK (»)
          (t (insert ?>)))))


(define-minor-mode unicycle-mode
  "Cycle through typographical punctuation when pressing keys like ‘-’"
  :lighter " UniCycle"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [?-] 'unicycle/hyphen)
            (define-key map [?'] 'unicycle/apostrophe)
            (define-key map [?\"] 'unicycle/double-quote)
            (define-key map [?.] 'unicycle/period)
            (define-key map [?>] 'unicycle/greater-than)
            map))


(provide 'unicycle-mode)
