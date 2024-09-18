;; Emacs Configuration File
;; This file contains the configuration settings for Emacs, including package management, load paths, global settings, and mode-specific configurations.

;;------------------------------------------------------------------------------
;; Package Management: Package Listing and Auto Installation
;;-----------------------------------------------------------------------------

;; List the packages you want
(setq package-list '(yaml-mode cmake-mode))

;; List the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;------------------------------------------------------------------------------
;; Set Load Path (Should be first)
;;------------------------------------------------------------------------------

;; Add custom load paths
(add-to-list 'load-path "~/.emacs.d/elisp")

(let ((default-directory  "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Set default tab width and indentation settings
(setq default-tab-width 2)              ; Tab width = 2
(setq-default indent-tabs-mode nil)     ; Tab inserts spaces only
(setq mac-command-modifier 'meta)       ; Sets the command (Apple) key as Meta

;;------------------------------------------------
;; Global Emacs Config {
;;------------------------------------------------

;; Load custom scripts
(load "scroll.el") ;; scrolling
(load "my-convenience.el") ;; key binding
(load "gud.el") ;; lldb mode

;; Preferences
(setq make-backup-files nil)            ; Disable backup files
(setq column-number-mode t)             ; Put column number in display
(setq-default fill-column 80)           ; M-q to fill col; width set to 80

;; Change “yes or no” to “y or n”
(fset 'yes-or-no-p 'y-or-n-p)

;; Initialize smart-mode-line with powerline theme
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'automatic)
(sml/setup)

;; Load dark mode theme: dracula
(load-theme 'dracula t)

;;------------------------------------------------
;; Global Emacs Config }
;;------------------------------------------------

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

;; Experimenting with cc coding style
;; Writing below the original code so that I only override and not omit
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; Auto pair braces, quotes, brackets
(require 'autopair)
;; Disabling it, as this is more for pain than being useful
;; (autopair-global-mode 1)
(setq autopair-autowrap t)

;; Whitespace-mode
;; Highlight lines more than 80 chars
;; Free of trailing whitespace and to use 80-column width, standard indentation
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 80)
(global-whitespace-mode 1)

;; Disable toolbar from GUI (THIS IS EMACS!)
(tool-bar-mode -1)

;; Turn line numbers on (left margin) - globally
(global-linum-mode t)

;; Enable git-gutter https://github.com/syohex/emacs-git-gutter-fringe
;; (global-git-gutter-mode +1)

;; Set highlight color 10% darker default background
(require 'color)
(defun set-hl-line-color-based-on-theme ()
  "Sets the hl-line face to have no foreground and a background
    that is 10% darker than the default face's background."
  (set-face-attribute 'hl-line nil
                      :foreground nil
                      :background (color-darken-name (face-background 'default) 10)))

(add-hook 'global-hl-line-mode-hook 'set-hl-line-color-based-on-theme)

(global-hl-line-mode t)

;; Someday might want to rotate windows if more than 2 of them
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

;; (global-set-key (kbd "M-2") 'swap-windows)

;;------------------------------------------------------------------------------
;; Hooks
;;------------------------------------------------------------------------------

(add-hook 'diff-mode-hook '(lambda ()
                             (require 'ansi-color)
                             (ansi-color-apply-on-region
                              (point-min) (point-max))))

;; Get rid of DOS EOL i.e annoying ^M
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'diff-mode-hook 'remove-dos-eol)

(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Enable upcase-region command (this uppercases the selection)
(put 'upcase-region 'disabled nil)
;; Enable narrow-to-region command (I forgot why/how I would use it)
(put 'narrow-to-region 'disabled nil)

;; Treat CamelCase words the same special way it treats lisp-case and
;; snake_case words.
(add-hook 'prog-mode-hook 'subword-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "d0fe9efeaf9bbb6f42ce08cd55be3f63d4dfcb87601a55e36c3421f2b5dc70f3" default))
 '(google-this-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(counsel-jq json-mode typescript typescript-mode avy bind-key org-cliplink htmlize multiple-cursors restclient dracula-theme google-this go-autocomplete auto-complete govet go-mode markdown-mode kotlin-mode swift-mode command-log-mode eshell-bookmark docker-tramp docker-compose-mode yaml-mode web-mode smart-mode-line git-gutter-fringe flycheck exec-path-from-shell cmake-mode))
 '(send-mail-function 'mailclient-send-it))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
