;;------------------------------------------------------------------------------
;; Set Load Path (Should be first)
;;------------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/")

(setq default-tab-width 4)              ; Tab width = 4
(setq-default indent-tabs-mode nil)     ; Tab inserts spaces only
(setq mac-command-modifier 'meta)       ; Sets the command (Apple) key as Meta

;;------------------------------------------------------------------------------
;; Loading Custom Scripts
;;------------------------------------------------------------------------------

;;CEDET
;;(load "cedet.el")
(load "scroll.el") ;; scrolling 
(load "my-convenience.el") ;; key binding
(load "gud.el") ;; lldb mode

;; Preferences                          *** ----------------------------
(setq make-backup-files nil)            ;;; Don't make backup files
(setq column-number-mode t)             ;;; Put column number in display
                                        ;;; -----------------------------
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

;;Replace package1 package2.. package5 with the packages that you want.

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

;;yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;;
;;end Loading custom scripts
;;

;;------------------------------------------------------------------------------
;; CC Mode
;;------------------------------------------------------------------------------
(progn
  (load "cc-mode")
  (c-add-style "sid-cxx-style"
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
    (c-set-style "sid-cxx-style"))
  (add-hook 'c-mode-hook 'kc-c-mode-hook)

  ;; See also kc-c-mode-common-hook above.
  (defun kc-c++-mode-hook ()
    (c-set-style "sid-cxx-style")
    ;; Make ':' a symbol constituent char so that find-tag gets the right
    ;; default value.
    (modify-syntax-entry ?: "_"))
  (add-hook 'c++-mode-hook 'kc-c++-mode-hook))

;; experimenting with cc coding style
;; writing below the original code so that I only override and not omit
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; auto pair braces, quotes, brackets
(require 'autopair)
;; disabling it, as this is more for pain then beign usefulc
;; (autopair-global-mode 1) 
(setq autopair-autowrap t)

;; whitespace-mode
;; highlight lines more than 80 chars
;; free of trailing whitespace and to use 80-column width, standard indentation
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 80)
(global-whitespace-mode 1)


;; turn line numbers on (left margin) - globally
(global-linum-mode t)

;; set highlight color 10% darker default background
(require 'color)
(defun set-hl-line-color-based-on-theme ()
  "Sets the hl-line face to have no foregorund and a background
    that is 10% darker than the default face's background."
  (set-face-attribute 'hl-line nil
                      :foreground nil
                      :background (color-darken-name (face-background 'default) 10)))

(add-hook 'global-hl-line-mode-hook 'set-hl-line-color-based-on-theme)

(global-hl-line-mode t)

;; someday might want to rotate windows if more than 2 of them
(defun swap-windows () "If you have 2 windows, it swaps them."
  (interactive) (cond ((not (= (count-windows) 2))
                       (message "You need exactly 2 windows to do this."))
                      (t
                       (let* ((w1 (first (window-list)))
                              (W2 (second (window-list)))
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

;;------------------------------------------------------------------------------
;; Hooks
;;------------------------------------------------------------------------------

(add-hook 'diff-mode-hook '(lambda ()
                             (require 'ansi-color)
                             (ansi-color-apply-on-region
                              (point-min) (point-max))))

;;get rid of DOS EOL i.e annoying ^M
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'diff-mode-hook 'remove-dos-eol)

(add-hook 'yaml-mode-hook
		  (lambda ()
			 (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
;; enable upcase-region command (this uppercases the selection)
(put 'upcase-region 'disabled nil)
;; enable narrow-to-region command (I forgot why/how I would use is
(put 'narrow-to-region 'disabled nil)

