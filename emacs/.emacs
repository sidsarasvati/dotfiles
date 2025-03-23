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