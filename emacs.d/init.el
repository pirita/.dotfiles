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

(add-to-list 'custom-theme-load-path "~/.emacs.d/etc/themes")

;; MELPA and Marmalade repos for packages.
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))
(package-initialize)

;; Bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)


;;;;;;
;;Dependencies
;;;;;

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

(use-package ensime
  :pin melpa)

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)

(use-package darkokai-theme
  :config (load-theme 'darkokai t))

(add-to-list 'exec-path "/usr/local/bin")

;;;;;;;
;; Configuration
;;;;;;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(toggle-frame-maximized)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lua-mode adoc-mode ansible darkokai-theme nlinum doom-themes neotree rainbow-delimiters rainbow-mode asciidoc use-package ascii-art-to-unicode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;(use-package doom-themes
;  :ensure t
;  :config
;  (progn
;    (setq doom-one-brighter-comments t)
;    (load-theme 'doom-one t)
;    (add-hook 'find-file-hook 'doom-buffer-mode)
;    (add-hook 'minibuffer-setup-hook 'doom-buffer-mode)))

;;(doom-themes-neotree-config)
;;(doom-themes-nlinum-config)

