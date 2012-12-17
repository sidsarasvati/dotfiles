(setq default-tab-width 4)              ; Tab width = 4 
(setq-default indent-tabs-mode nil)     ; Tab inserts spaces only
(setq mac-command-modifier 'meta)       ; Sets the command (Apple) key as Meta

;;Navigation Functions 

(defun scroll-in-place (scroll-up)
  "Scroll window up (or down) without moving point (if possible).

SCROLL-Up is non-nil to scroll up one line, nil to scroll down."
  (interactive)
  (let ((pos (point))
                (col (current-column))
                (up-or-down (if scroll-up 1 -1)))
        (scroll-up up-or-down)
        (if (pos-visible-in-window-p pos)
                (goto-char pos)
          (if (or (eq last-command 'next-line)
                          (eq last-command 'previous-line))
                  (move-to-column temporary-goal-column)
                (move-to-column col)
                (setq temporary-goal-column col))
          (setq this-command 'next-line))))

;;;; ------------------------------------------------------------------------
(defun scroll-up-in-place ()
  "Scroll window up without moving point (if possible)."
  (interactive)
  (scroll-in-place t))

;;;; ------------------------------------------------------------------------
(defun scroll-down-in-place ()
  "Scroll window up without moving point (if possible)."
  (interactive)
  (scroll-in-place nil))


(global-set-key "\C-\M-n" 'scroll-up-in-place)
(global-set-key "\C-\M-p" 'scroll-down-in-place)


(progn
  (load "cc-mode")
  (c-add-style "Kenstir"
               '((c-basic-offset . 4)
                 (c-tab-always-indent . nil)
                 (tab-width . 8)
                 (c-comment-only-line-offset . 0)
                 (c-offsets-alist . ((statement-block-intro . +)
                                     (knr-argdecl-intro . +)
                                     (substatement-open . 0)
                                     (label . 0)
                                     (statement-cont . +)
                                     ))
                 ))

  ;; See also kc-c-mode-common-hook above.
  (defun kc-c-mode-hook ()
    (c-set-style "Kenstir"))
  (add-hook 'c-mode-hook 'kc-c-mode-hook)

  ;; See also kc-c-mode-common-hook above.
  (defun kc-c++-mode-hook ()
    (c-set-style "Kenstir")
    ;; Make ':' a symbol constituent char so that find-tag gets the right
    ;; default value.
    (modify-syntax-entry ?: "_"))
  (add-hook 'c++-mode-hook 'kc-c++-mode-hook)
