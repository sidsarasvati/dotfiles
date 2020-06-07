;; ------- productivuty features ------------

;; multiple cursors
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;duplicate binding for my convenience
(global-set-key "\C-x\C-g" 'find-file)
(global-set-key "\M-k" 'kill-buffer)
(global-set-key "\M-o" 'other-window)

;;keybinding macros
(global-set-key "\C-\M-y" "\C-a\C- \C-n\M-w\C-y\C-p")
;;other useful stuff
;;(global-set-key "\M-g" 'grep)

;;buffer management 
(global-set-key "\M--" 'previous-buffer)
(global-set-key "\M-=" 'next-buffer)

;;window management
;;using meta as modifier for windmove
(windmove-default-keybindings 'meta)
(global-set-key "\M-p" 'windmove-up)
(global-set-key "\M-n" 'windmove-down)
(global-set-key "\M-1" 'windmove-left)
(global-set-key "\M-0" 'windmove-right)

;; ------- MODE SPECIFIC ----------

;;semantic 
;;TODO
;;cmake
(eval-after-load 'cmake
                     '(define-key CMAKE-mode-map (kbd "\C-c-h") 'cmake-help-command))

;; clang format
(global-set-key [C-M-tab] 'clang-format-region)


;; avy jump

(bind-key (kbd "C-:") 'avy-goto-char)

;; ------- functions to make it happen  -----

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
