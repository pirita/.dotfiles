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
(global-unset-key (kbd "M-/"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

; Mac emacs
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

(provide 'module-misc)

;; MELPA and Marmalade repos for packages.
(require 'package)
(setq
 package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                    ("melpa"        . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/"))
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
  (load-theme 'doom-spacegrey t)
  (doom-themes-org-config)))

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-height 20)
  (doom-modeline-major-mode-color-icon t))

;;;;;;
;;Dependencies
;;;;;

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

;; Flymd
(use-package pandoc-mode
  :config (add-hook 'markdown-mode-hook 'pandoc-mode)
  )

;; Yaml
(use-package yaml-mode)

;; Haskell
(use-package haskell-mode)

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
(use-package cl-lib)

;; Idris mode
(use-package idris-mode)

;; Rainbow mode
(use-package rainbow-mode)

;; Weather
(use-package wttrin
  :custom
  (wttrin-default-cities '("Murcia" "Madrid" "Leganes" "Tokyo" "London")))

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
(use-package helm-ag)

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
  :bind (:map projectile-command-map
              ("a" . projectile-add-known-project))
  :config
  (setq projectile-enable-caching nil
        projectile-indexing-method 'alien
    projectile-completion-system 'helm
    projectile-project-root-files '(".git" ".project" "setup.py" "build.sbt" "pom.xml")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".class")
        projectile-globally-ignored-files '(".DS_Store" "Icon"))
  (projectile-mode t))

(use-package helm-projectile
  :demand
  :init (setq helm-projectile-fuzzy-match t)
  :custom
  (helm-projectile-fuzzy-match t)
  :config
  (helm-projectile-on))

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

  (setq neo-theme (if (display-graphic-p) 'icons 'nerd)
        neo-window-width 40
        neo-create-file-auto-open t
        neo-show-updir-line nil
        neo-mode-line-type 'neotree
        neo-smart-open t
        neo-autorefresh t
        neo-auto-indent-point t
        neo-show-hidden-files t
        neo-window-fixed-size nil))

(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :functions (my-company-yasnippet)
  :bind (("M-/" . company-complete)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 0)
  (company-tooltip-limit 12)
  (company-tooltip-align-annotations t)
  (company-show-numbers t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  :config
  (defun my-company-yasnippet ()
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet)))
;;;;;
;;; Scala metals
;;;;;

;; Enable scala-mode and sbt-mode
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :defer t
  :config
  (setq lsp-metals-server-command "~/.local/bin/metals-emacs"))

(use-package sbt-mode
  :defer t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; Enable nice rendering of diagnostics like compile errors.
;(use-package flycheck
;  :init (global-flycheck-mode))

(use-package lsp-mode
  :defer t
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         ((scala-mode haskell-mode) . lsp))
  :custom
  (lsp-auto-guess-root t)
  (lsp-keep-workspace-alive nil)
  (lsp-enable-folding nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-semantic-highlighting nil)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-idle-delay 0.1)
  (lsp-eldoc-enable-hoover t)
  (lsp-eldoc-render-all t)
  (lsp-diagnostic-package :flymake)
  (lsp-signature-auto-activate t)
  (lsp-signature-render-documentation t)
  (lsp-enable-text-document-color t))

(use-package lsp-ui
  :defer t
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-include-signture t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable nil))

(use-package lsp-haskell
  :after haskell-mode
  :demand t
  :custom
  ;; (lsp-log-io t)
  (lsp-haskell-process-path-hie "/Users/inavarro/.local/bin/ghcide")
  (lsp-haskell-process-args-hie '()))

(use-package company-lsp
  :defer t
  :after company-mode
  :custom
  (company-lsp-cache-candidates nil)
  (company-lsp-async t)
  (company-lsp-enable-recompletion t))

;;;;;;;
;; Configuration
;;;;;;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(toggle-frame-maximized)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
