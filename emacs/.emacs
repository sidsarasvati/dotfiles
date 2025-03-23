;; Bootstrap config.org - Sid's Literate Emacs Configuration

;; This is just a minimal bootstrap file that loads the actual configuration
;; from config.org using org-babel-load-file. All actual configuration
;; is maintained in that file in a literate programming style.

;; When first installing:
;; 1. Install org-mode if not already available
;; 2. Load the literate config file

;; Set up package system
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Ensure org is installed
(use-package org
  :ensure t)

;; Load the org config file
(org-babel-load-file
 (expand-file-name "config.org"
                   (file-name-directory (or load-file-name buffer-file-name))))

;; If there are issues loading the org file, we can fall back to a simple configuration
;; that just loads the necessary packages. This is useful when first setting up the
;; configuration on a new system.
(when (not (boundp 'org-babel-load-file))
  (message "Failed to load org-babel-load-file, falling back to basic configuration")
  ;; Basic fallback configuration goes here
  (load-theme 'tango-dark t))