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
(global-set-key "\M-`" 'previous-buffer) ;; dup for convenience and past habit hangover :) 
(global-set-key "\M-=" 'next-buffer)

;;window management
;;using meta as modifier for windmove
(windmove-default-keybindings 'meta)
;;need to unset keys
(global-unset-key kbd("\C-1"))
;;(global-unset-key "\C-2")
;;(global-unset-key "\C-3")
;;(global-unset-key "\C-4")
;;(global-set-key "\C-\M-2" 'windmove-up)
(global-set-key "\M-3" 'windmove-down)
(global-set-key "\M-1" 'windmove-left)
(global-set-key "\M-4" 'windmove-right)

;;MODE SPECIFIC

;;semantic 
;;TODO
;;cmake
(eval-after-load 'cmake
                     '(define-key CMAKE-mode-map (kbd "\C-c-h") 'cmake-help-command))
