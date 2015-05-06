;; productivuty features 

;;duplicate binding for my convenience
(global-set-key "\C-x\C-g" 'find-file)
(global-set-key "\M-k" 'kill-buffer)

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

;;MODE SPECIFIC

;;semantic 
;;TODO
;;cmake
(eval-after-load 'cmake
                     '(define-key CMAKE-mode-map (kbd "\C-c-h") 'cmake-help-command))
