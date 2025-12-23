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

;; Ensure doom-themes is installed and loaded early
;; Using doom-monokai-pro: red keywords, warm non-black background
;; Chosen Dec 2025 after testing dracula, doom-one, doom-dracula
;; Key: Magenta/red for keywords (classic Java/C++ feel)
(unless (package-installed-p 'doom-themes)
  (package-refresh-contents)
  (package-install 'doom-themes))

;; Consider all themes safe to prevent prompting
(setq custom-safe-themes t)

;; Load the theme immediately (before config.org for instant visual)
(load-theme 'doom-monokai-pro t)

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
   '("088cd6f894494ac3d4ff67b794467c2aa1e3713453805b93a8bcb2d72a0d1b53"
     "3061706fa92759264751c64950df09b285e3a2d3a9db771e99bcbb2f9b470037"
     "19d62171e83f2d4d6f7c31fc0a6f437e8cec4543234f0548bad5d49be8e344cd"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "d0fe9efeaf9bbb6f42ce08cd55be3f63d4dfcb87601a55e36c3421f2b5dc70f3" default))
 '(google-this-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(;; Core editing
     avy multiple-cursors magit
     ;; Completion stack (Dec 2025)
     corfu cape kind-icon copilot
     ;; AI integration
     claude-code-ide vterm
     ;; Theme + modeline (Dec 2025)
     doom-themes doom-modeline nerd-icons
     ;; Language modes (keep for future)
     go-mode kotlin-mode swift-mode cmake-mode json-mode markdown-mode web-mode
     ;; Utilities
     exec-path-from-shell org-cliplink htmlize))
 '(package-vc-selected-packages
   '((copilot :vc-backend Git :url "https://github.com/copilot-emacs/copilot.el")))
 '(send-mail-function 'mailclient-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
