;;Load Paths
(add-to-list 'load-path "~/.emacs.d/")

(setq default-tab-width 4)              ; Tab width = 4 
(setq-default indent-tabs-mode nil)     ; Tab inserts spaces only
(setq mac-command-modifier 'meta)       ; Sets the command (Apple) key as Meta

;;Loading Custom Scripts

;;CEDET
;;(load "cedet.el") 
;;Navigation fucntions and key binding
(load "scroll.el")
(load "my-convenience.el")  

;;package management 
;;; Emacs is not a package manager, and here we load its package manager!
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ;; TODO: Maybe, use this after emacs24 is released
                  ;; (development versions of packages)
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
(package-initialize)

;;maybe in future 
;;; Required packages
;;; everytime emacs starts, it will automatically check if those packages are
;;; missing, it will install them automatically
;;(when (not package-archive-contents)
;;  (package-refresh-contents))
;;(defvar tmtxt/packages
;;  '(package1 package2 package3 package4 package5))
;;(dolist (p tmtxt/packages)
;;  (when (not (package-installed-p p))
;;    (package-install p)))

;;Replace package1 package2 package3 package4 package5 with the packages that you want.

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;cmake-mode
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))
;;dos mode
(require 'dos)
(add-to-list 'auto-mode-alist '("\\.bat\\'" . dos-mode))

;;end Loading custom scripts 

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
									 (innamespce . 0)
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
  (add-hook 'c++-mode-hook 'kc-c++-mode-hook))

;; experimenting with cc coding style
;; writing below the original code so that I only override and not omit
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; no indent under namespaces 
(defconst my-cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))
(c-add-style "my-cc-mode" my-cc-style)

(defconst my-cc-style
  '("gnu"
    (c-offsets-alist . ((innamespace . [4])))))
(c-add-style "my-cc-style" my-cc-style)

(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; someday might want to rotate windows if more than 2 of them
(defun swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (first (window-list)))
	 (w2 (second (window-list)))
	 (b1 (window-buffer w1))
	 (b2 (window-buffer w2))
	 (s1 (window-start w1))
	 (s2 (window-start w2)))
 (set-window-buffer w1 b2)
 (set-window-buffer w2 b1)
 (set-window-start w1 s2)
 (set-window-start w2 s1)))))

;;(global-set-key (kbd "M-2") 'swap-windows)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'diff-mode-hook '(lambda () (require 'ansi-color)(ansi-color-apply-on-region (point-min) (point-max))))

;;get rid of DOS EOL i.e annoying ^M
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'diff-mode-hook 'remove-dos-eol)
