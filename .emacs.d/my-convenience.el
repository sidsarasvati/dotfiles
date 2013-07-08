;; productivuty features 

;;duplicate binding for my convenience
(global-set-key "\C-x\C-g" 'find-file)
(global-set-key "\M-k" 'kill-buffer)

;;other useful stuff
;;(global-set-key "\M-g" 'grep)

;;buffer management 
(global-set-key "\M-1" 'delete-other-windows)
(global-set-key "\M-`" 'previous-buffer)

;;window management
;;using meta as modifier for windmove
(windmove-default-keybindings 'meta)
(global-set-key "\M-\C-w" 'windmove-up)
(global-set-key "\M-\C-s" 'windmove-down)
(global-set-key "\M-\C-a" 'windmove-left)
(global-set-key "\M-\C-d" 'windmove-right)

;;MODE SPECIFIC

;;semantic 
;;TODO
;;cmake
(eval-after-load 'cmake
                     '(define-key CMAKE-mode-map (kbd "\C-c-h") 'cmake-help-command))
