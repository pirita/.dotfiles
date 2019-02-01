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

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode))

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)

(use-package all-the-icons)

(add-to-list 'exec-path "/usr/local/bin")

(use-package auto-package-update
  :config (setq auto-package-update-delete-old-versions t))

(use-package projectile
  :demand
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-enable-caching nil
        projectile-indexing-method 'alien
		projectile-project-root-files '(".git" ".project" "setup.py" "build.sbt" "pom.xml")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".class")
        projectile-globally-ignored-files '(".DS_Store" "Icon")))

(use-package neotree
  :defer t
  :functions (neotree-resize-window neotree-project-dir)
  :commands neotree-project-dir
  :hook ((neo-enter . neotree-resize-window))
  :bind ("<f8>" . 'neotree-project-dir)
  :config

  (defun neotree-resize-window (&rest _args)
    "Resize neotree window."
    (neo-global--when-window
      (let ((fit-window-to-buffer-horizontally t))
        (neo-buffer--unlock-width)
        (fit-window-to-buffer)
        (neo-buffer--lock-width))))

  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name))))))

  (setq neo-theme 'nerd
        neo-window-width 40
        neo-create-file-auto-open t
        neo-show-updir-line nil
        neo-mode-line-type 'neotree
        neo-smart-open t
        neo-autorefresh t
        neo-auto-indent-point t
        neo-show-hidden-files t
        neo-window-fixed-size nil))

;;;;;;;
;; Configuration
;;;;;;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(toggle-frame-maximized)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
