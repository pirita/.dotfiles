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
(global-auto-revert-mode t)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

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

;; Global temp files
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs-saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

;;;;
;;UI
;;;;
(use-package doom-themes
  :config
  (progn
	(setq doom-themes-enable-bold t
		  doom-themes-enable-italic t)
	(load-theme 'doom-city-lights t)
	(doom-themes-org-config)))

;;;;;;
;;Dependencies
;;;;;

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

;; Yaml
(use-package yaml-mode)

;; Mwin
(use-package mwim
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

;; Smooth scrolling
(setq redisplay-dont-pause t
      scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Common Lisp
(use-package cl
  :defer t)

;; Idris mode
(use-package idris-mode)

;; Rainbow mode
(use-package rainbow-mode)

;; Rainbow delimiters mode
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode))

;; Helm
(use-package helm
  :bind (("C-c h"   . helm-command-prefix)
         ("C-x C-f" . helm-find-files)
         ("C-x b"   . helm-buffers-list)
         ("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring))
  :config (progn (require 'helm-config)
                 (setq helm-split-window-in-side-p t
                       helm-move-to-line-cycle-in-source t ; Circle when using helm-next/previous-line
                       helm-ff-search-library-in-sexp t
                       helm-scroll-amount 8
                       helm-ff-file-name-history-use-recentf t
                       helm-autoresize-mode t;
                       helm-autoresize-max-height 20
                       helm-mode-fuzzy-match t
                       helm-buffers-fuzzy-matching t
                       helm-recentf-fuzzy-match t
                       helm-completion-in-region-fuzzy-match t
                       helm-M-x-fuzzy-match t)
                 (helm-mode t)
                 (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
                 (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
                 (define-key helm-map (kbd "C-z") 'helm-select-action)))  ; list actions using C-z

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)

(use-package all-the-icons)

(add-to-list 'exec-path "/usr/local/bin")

(use-package auto-package-update
  :config (setq auto-package-update-delete-old-versions t))

(use-package winum
  :demand
  :functions winum-assign-0-to-neotree
  :bind (:map winum-keymap
              ("M-0" . 'winum-select-window-0-or-10)
              ("M-1" . 'winum-select-window-1)
              ("M-2" . 'winum-select-window-2)
              ("M-3" . 'winum-select-window-3)
              ("M-4" . 'winum-select-window-4)
              ("M-5" . 'winum-select-window-5)
              ("M-6" . 'winum-select-window-6)
              ("M-7" . 'winum-select-window-7)
              ("M-8" . 'winum-select-window-8)
              ("M-9" . 'winum-select-window-9))
  :config
  (progn
    (defun winum-assign-0-to-neotree ()
      (when (string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 10))
    (add-to-list 'winum-assign-functions #'winum-assign-0-to-neotree)
    (setq winum-auto-assign-0-to-minibuffer nil
          winum-ignored-buffers '("*which-key*"))
    (winum-mode)))

(use-package projectile
  :demand
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-enable-caching nil
        projectile-indexing-method 'alien
		projectile-project-root-files '(".git" ".project" "setup.py" "build.sbt" "pom.xml")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".class")
        projectile-globally-ignored-files '(".DS_Store" "Icon"))
  (projectile-mode t))

(use-package helm-projectile
  :ensure t
  :hook (projectile-mode . helm-projectile-on))

(use-package which-key
  :ensure t
  :init (which-key-mode))

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
