(setq default-tab-width 4)              ; Tab width = 4 
(setq-default indent-tabs-mode nil)     ; Tab inserts spaces only

(setq mac-command-modifier 'meta)       ; Sets the command (Apple) key as Meta

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