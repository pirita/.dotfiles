;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(setq-default tab-width 4)
(setq inhibit-startup-screen t)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq use-package-always-ensure t)
(setq sentence-end-double-space nil)
(setq ensime-startup-notification nil)

;; MELPA and Marmalade repos for packages.
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 0)
							  ("melpa" . 20)
							  ("gnu" . 10)))
(package-initialize)

;; Bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(use-package diminish)

;;;;
;;UI
;;;;
(use-package doom-themes
  :config
  (progn
	(setq doom-themes-enable-bold t
		  doom-themes-enable-italic t)
	(load-theme 'doom-molokai t)
	(doom-themes-org-config)))

;;;;;;
;;Dependencies
;;;;;

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

;; Mwin
(use-package mwim
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

;; Common Lisp
(use-package cl
  :defer t)

;; Idris mode
(use-package idris-mode)

;; Rainbow mode
(use-package rainbow-mode
  :defer t)

;; Rainbow delimiters mode
(use-package rainbow-delimiters
  :config (progn (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)))

;; Neo-tree
(use-package neotree
  :defer t
  :bind ("<f8>" . neotree-toggle))

(use-package nlinum
  :config (add-hook 'prog-mode-hook '(lambda () (nlinum-mode t))))

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)

(use-package darkokai-theme
  :config (load-theme 'darkokai t))

(add-to-list 'exec-path "/usr/local/bin")

(use-package auto-package-update
  :config (setq auto-package-update-delete-old-versions t))

;;;;;;;
;; Configuration
;;;;;;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(toggle-frame-maximized)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
