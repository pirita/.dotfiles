;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

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
;;;;;;

;; Common Lisp
(use-package cl
  :ensure t
  :defer t)

;; Ascii-doc
(use-package asciidoc
  :ensure t)

;; Idris mode
(use-package idris-mode
  :ensure t)

;; Rainbow mode
(use-package rainbow-mode
  :ensure t
  :defer t)

;; Rainbow delimiters mode
(use-package rainbow-delimiters
  :ensure t
  :config (progn (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)))

;; Neo-tree
(use-package neotree
  :ensure t
  :defer t
  :bind ("<f8>" . neotree-toggle))

(use-package doom-themes
  :ensure t
  :config
  (progn
    (setq doom-one-brighter-comments t)
    (load-theme 'doom-one t)
    (add-hook 'find-file-hook 'doom-buffer-mode)
    (add-hook 'minibuffer-setup-hook 'doom-buffer-mode)))

;;(doom-themes-neotree-config)
(doom-themes-nlinum-config)

(use-package nlinum
  :ensure t
  :config (add-hook 'prog-mode-hook '(lambda () (nlinum-mode t))))

(use-package ensime
  :ensure t
  :pin melpa)

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)

(add-to-list 'exec-path "/usr/local/bin")
;;;;;;;
;; Configuration
;;;;;;;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (nlinum doom-themes neotree rainbow-delimiters rainbow-mode asciidoc use-package ascii-art-to-unicode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
