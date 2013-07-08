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
(windmove-default-keybindings 'shift)
(global-set-key "\S-W" 'windmove-up)
(global-set-key "\S-S" 'windmove-down)
(global-set-key "\S-A" 'windmove-left)
(global-set-key "\S-D" 'windmove-right)

;;MODE SPECIFIC

;;semantic 
;;TODO
;;cmake
(eval-after-load 'cmake
                     '(define-key CMAKE-mode-map (kbd "\C-c-h") 'cmake-help-command))
