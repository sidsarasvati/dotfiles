;;(setq debug-on-error t) ; get backtrace or errors

;;------------------------------------------------------------------------------
;; Package Management: Package Listing and Auto Installation
;;-----------------------------------------------------------------------------
; list the packages you want
(setq package-list '(yaml-mode cmake-mode))

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages - DOESN'T WORK!!
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;------------------------------------------------------------------------------
;; Set Load Path (Should be first)
;;------------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elisp")

(let ((default-directory  "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(setq default-tab-width 2)              ; Tab width = 2
(setq-default indent-tabs-mode nil)     ; Tab inserts spaces only
(setq mac-command-modifier 'meta)       ; Sets the command (Apple) key as Meta

;;------------------------------------------------
;; Global Emacs Config {
;;------------------------------------------------

;; load custom scripts 
(load "scroll.el") ;; scrolling 
(load "my-convenience.el") ;; key binding
(load "gud.el") ;; lldb mode

;; Preferences                          *** ----------------------------
(setq make-backup-files nil)            ;;; Don't make backup files
(setq column-number-mode t)             ;;; Put column number in display
                                        ;;; -----------------------------
;; clang format
;; (require 'clang-format)

;; init smart-mode-line with powerline theme
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'automatic)
(sml/setup)

;; my dark mode theme: dracula 
(load-theme 'dracula t)
;;------------------------------------------------
;; Global Emacs Config }
;;------------------------------------------------

;;package management
;;; Emacs is not a package manager, and here we load its package manager!
;; (require 'package)
;; (dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
;;                   ("elpa" . "http://tromey.com/elpa/")
;;                   ("melpa-stable" . "https://stable.melpa.org/packages/")
;;                   ;; (development versions of packages)
;;                   ("melpa" . "http://melpa.milkbox.net/packages/")
;;                   ))
;;   (add-to-list 'package-archives source t))
;; (package-initialize)

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

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;----------------------------------------------------------
;; GLOBAL DEV MODE SETUP {
;;----------------------------------------------------------

;; flycheck and linting settings (copied from web-mode setup)
;; ---------------------------------------------------------
(require 'flycheck) ;; http://www.flycheck.org/manual/latest/index.html

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;;----------------------------------------------------------
;; } END GLOBAL DEV MODE ----------------------------------
;;----------------------------------------------------------

;;----------------------------------------------------------
;; ORG MODE  {
;;----------------------------------------------------------

(global-set-key (kbd "C-x p i") 'org-cliplink)
;;----------------------------------------------------------
;; ORG MODE  }
;;----------------------------------------------------------

;;----------------------------------------------------------
;; GOLANG DEVELOPMENT {
;;----------------------------------------------------------

;; from: https://github.com/dominikh/go-mode.el/blob/master/go-guru.el
(require 'go-guru)

;; To enable identifier highlighting mode in a Go source buffer, use:
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

;; from
;; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; autocomplete for go
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

;;----------------------------------------------------------
;; } END GOLANG DEV ----------------------------------------
;;----------------------------------------------------------

;;----------------------------------------------------------
;; WEB DEVELOPMENT
;;----------------------------------------------------------
(require 'web-mode)
(setq js-indent-level 2)

(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))


;; see http://web-mode.org/
(defun my-web-mode-hook ()
  "Hooks for Web mode."

  ;; indents 
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)

  ;; smartparens
  (setq web-mode-enable-auto-pairing nil)

  ;; key bindings
  (local-set-key (kbd "C-c C-v") 'browse-url-of-buffer)

  (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
              nil))
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)


;;---------------------------------------------------------
;; } End Web Development Settings--------------------------
;;---------------------------------------------------------

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


;; disable toolbar from GUI (THIS IS EMACS!)
(tool-bar-mode -1)

;; turn line numbers on (left margin) - globally
(global-linum-mode t)

;; enable git-gutter https://github.com/syohex/emacs-git-gutter-fringe
;; (global-git-gutter-mode +1)

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

;; Treat CamelCase words the same special way it treats lisp-case and
;; snake_case words.
(add-hook 'prog-mode-hook 'subword-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "d0fe9efeaf9bbb6f42ce08cd55be3f63d4dfcb87601a55e36c3421f2b5dc70f3" default)))
 '(google-this-mode t)
 '(package-selected-packages
   (quote
    (org-cliplink htmlize multiple-cursors restclient dracula-theme google-this go-autocomplete auto-complete govet go-mode markdown-mode kotlin-mode swift-mode command-log-mode eshell-bookmark docker-tramp docker-compose-mode yaml-mode web-mode smart-mode-line git-gutter-fringe flycheck exec-path-from-shell cmake-mode)))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
