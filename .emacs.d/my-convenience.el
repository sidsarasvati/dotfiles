;; productivuty features 

;;rebinding some defaults for my convenience

(global-set-key "\C-x\C-g" 'find-file)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\M-1" 'delete-other-windows)
(global-set-key "\M-`" 'previous-buffer)


;;MODE SPECIFIC

;;semantic 
;;TODO
;;cmake
(eval-after-load 'cmake
                     '(define-key CMAKE-mode-map (kbd "\C-c-h") 'cmake-help-command))
