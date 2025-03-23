;; Bootstrap config.org - Sid's Literate Emacs Configuration
;;
;; This minimal bootstrap file loads the actual configuration from config.org.
;; All configuration is maintained in that file in a literate programming style.

;; Set up package system
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Ensure dracula theme is installed and loaded early
(unless (package-installed-p 'dracula-theme)
  (package-refresh-contents)
  (package-install 'dracula-theme))

;; Consider all themes safe to prevent prompting
(setq custom-safe-themes t)

;; Load the theme immediately
(load-theme 'dracula t)

;; Ensure org is installed
(use-package org
  :ensure t)

;; Load the literate configuration from config.org
(let* ((this-file (file-truename load-file-name))
       (this-dir (file-name-directory this-file))
       (config-file (expand-file-name "config.org" this-dir)))
  (when (file-exists-p config-file)
    (org-babel-load-file config-file)))
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
