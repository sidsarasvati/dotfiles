;;TODO - enable this only for programming modes
;;http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
(load-file "~/.emacs.d/cedet-1.1/common/cedet.el")      ; Loading CDET 

;;enable semantic
(semantic-load-enable-gaudy-code-helpers)

;;imenu integration
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
(add-hook 'c-mode-common-hook 'my-cedet-hook)

(defun my-c-mode-cedet-hook ()
 (local-set-key "." 'semantic-complete-self-insert)
 (local-set-key ">" 'semantic-complete-self-insert))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;;settings
(global-set-key "\C-c " 'cedet-m3-menu-kbd)

(global-set-key "\C-ci" 'semantic-ia-fast-jump)
(global-set-key "\C-co" 'sematic-mrub-switch-tag)

(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
