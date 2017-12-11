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



;; key bidings
(global-set-key "\C-\M-n" 'scroll-up-in-place)
(global-set-key "\C-\M-p" 'scroll-down-in-place)
(global-set-key "\C-\M-o" 'scroll-down-in-place)
