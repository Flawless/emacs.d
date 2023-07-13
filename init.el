; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Alexander Ushanov
;;
;; Author: Alexander Ushanov <https://github.com/flawless>
;; Maintainer: Alexander Ushanov <alushanov92@gmail.com>
;; Created: April 24, 2021
;; Modified: April 24, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/flawless/init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'package)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(customize-set-variable 'package-archives
                        `(,@package-archives
                          ("melpa" . "https://melpa.org/packages/")
                          ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package use-package-core
  :custom
  (use-package-enable-imenu-support t))

(use-package system-packages
  :ensure t
  :custom
  (system-packages-noconfirm t))

(use-package use-package-ensure-system-package :ensure t)

(use-package quelpa
  :ensure t
  :defer t
  :custom
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

(use-package quelpa-use-package
  :ensure t
  :init
  (setq quelpa-use-package-inhibit-loading-quelpa t))

(use-package general
  :demand t
  :ensure t
  :commands (general-define-key general-override-mode general-evil-setup general--simulate-keys)
  :custom
  (general-override-states '(insert emacs hybrid normal visual motion operator replace))
  :config
  (defvar flawless-leader-key "SPC")
  (defvar flawless-leader-secondary-key "M-SPC")
  (defvar flawless-mode-leader-key "SPC m")
  (defvar flawless-mode-leader-secondary-key "M-SPC m")

  (general-create-definer flawless-def
    :states '(normal visual)
    :prefix flawless-leader-key
    :non-normal-prefix flawless-leader-secondary-key)

  (general-create-definer flawless-mode-def
    :states '(normal visual)
    :prefix flawless-mode-leader-key
    :non-normal-prefix flawless-mode-leader-secondary-key)

  (general-override-mode)
  (general-evil-setup))

(use-package emacs
  :delight
  (eldoc-mode)
  (auto-fill-function)
  (auto-revert-mode)
  :custom
  (indent-tabs-mode nil)
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (make-backup-file-name-function 'lt/backup-file-name)
  (mac-command-modifier 'meta)
  (fill-column 120)

  :config
  (defun file-notify-rm-all-watches ()
    "Remove all existing file notification watches from Emacs."
    (interactive)
    (maphash
     (lambda (key _value)
       (file-notify-rm-watch key))
     file-notify-descriptors))
  ;; hotfix error with image, remove after 29.1 release
  ;; overriding image.el function image-type-available-p
  (add-to-list 'image-types 'svg)
  ;; end of hotfix

  (defcustom telega-database-dir-base (expand-file-name "~/.telega")
    "telega base dir")
  ;; make fullscreen
  (modify-frame-parameters nil `((fullscreen . fullboth) (fullscreen-restore . ,(frame-parameter nil 'fullscreen))))

  (defun disable-all-themes ()
    "disable all active themes."
    (dolist (i custom-enabled-themes)
      (disable-theme i)))

  (defun lt:whiteboard ()
    (interactive)
    (disable-all-themes)
    (load-theme 'almost-mono-white t)
    (setq hl-sexp-background-color "#eceff4")
    (hl-sexp-delete-overlay)
    (hl-sexp-create-overlay)
    (setq rainbow-identifiers-choose-face-function
          'rainbow-identifiers-cie-l*a*b*-choose-face)
    (setq rainbow-identifiers-cie-l*a*b*-lightness 00)
    (setq rainbow-identifiers-cie-l*a*b*-saturation 00))

  (defun lt/backup-file-name (fpath)
    "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
    (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
           (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath)) ; remove Windows driver letter in path, ➢ for example: “C:”
           (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~"))))
      (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
      backupFilePath))

  (define-minor-mode ansi-color-mode
    "..."
    :init-value nil
    :lighter nil
    :keymap nil
    (ansi-color-apply-on-region 1 (buffer-size)))

  (defun my-mode-line-visual-bell ()
    (setq visible-bell nil)
    (setq ring-bell-function 'my-mode-line-visual-bell--flash))

  (defun my-mode-line-visual-bell--flash ()
    (let ((frame (selected-frame)))
      (run-with-timer
       0.1 nil
       #'(lambda (frame)
           (let ((inhibit-quit)
                 (inhibit-redisplay t))
             (invert-face 'header-line frame)
             (invert-face 'header-line-highlight frame)
             (invert-face 'mode-line frame)
             (invert-face 'mode-line-inactive frame)))
       frame)
      (let ((inhibit-quit)
            (inhibit-redisplay t))
        (invert-face 'header-line frame)
        (invert-face 'header-line-highlight frame)
        (invert-face 'mode-line frame)
        (invert-face 'mode-line-inactive frame))))

  (my-mode-line-visual-bell)

  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Prevent flickering issues
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

  (cond
   ((find-font (font-spec :name "Input Mono"))
    (set-frame-font "Input Mono-12" nil t))
   ((find-font (font-spec :name "Jetbrains Mono"))
    (set-frame-font "Jetbrains Mono-08")))


  (add-to-list 'exec-path "/usr/local/bin")
  (defun lt:reload-dir-locals-for-current-buffer ()
    "reload dir locals for the current buffer"
    (interactive)
    (let ((enable-local-variables :all))
      (hack-dir-local-variables-non-file-buffer)))
  (put 'use-package 'lisp-indent-function 1)
  (put ':states 'lisp-indent-function 1)
  (customize-set-variable 'package-enable-at-startup nil)

  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)

  (defun find-config-file ()
    (interactive)
    (find-file user-init-file))
  :general
  (:states '(normal visual)
    :keymaps 'override
    "SPC u" 'universal-argument
    "SPC fC" 'find-config-file
    "SPC ff" 'find-file
    "SPC fs" 'save-buffer
    "SPC fS" 'write-file
    "SPC fr" 'recover-this-file
    "SPC fR" 'recover-file

    "SPC bC" 'clean-buffer-list
    "SPC bb" 'counsel-switch-buffer
    "SPC bi" 'ibuffer
    "SPC br" 'revert-buffer
    "SPC bd" 'kill-current-buffer
    "SPC bs" 'save-buffer
    "SPC bS" 'save-some-buffers

    "SPC hk" 'describe-key
    "SPC hf" 'describe-function
    "SPC hF" 'describe-face
    "SPC hm" 'describe-mode
    "SPC ho" 'describe-symbol
    "SPC hv" 'describe-variable

    "SPC wv" 'split-window-right
    "SPC ws" 'split-window-vertically
    "SPC wd" 'delete-window
    "SPC ww" 'ace-window
    "SPC wm" 'maximize-window

    "SPC tF" 'toggle-frame-fullscreen
    "SPC tt" 'toggle-truncate-lines

    "SPC qq" 'save-buffers-kill-emacs))

(use-package path-helper
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (path-helper-setenv-all))

(use-package files
    :hook
    (before-save . whitespace-cleanup)
    :custom
    (require-final-newline t)
    (backup-by-copying t)
    (delete-old-versions t)
    (version-control t)
    (kept-new-versions 50)
    (kept-old-versions 20)
    (create-lockfiles nil))

(use-package dired
  :after evil
  :init (evil-collection-init 'dired)
  :hook (dired-mode . dired-omit-mode))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :init
    (exec-path-from-shell-initialize)))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package vterm
  :ensure t
  :after evil
  :init (evil-collection-init 'vterm)
  :config
  (use-package multi-vterm :ensure t)
  :general
  (:states '(normal visual)
    :prefix "SPC"
    "V" 'multi-vterm)
  (:states '(normal visual)
    :keymaps 'vterm-mode-map
    "C-c C-c" 'vterm-send-C-c
    "C-p" 'vterm-yank-pop
    "p" 'vterm-yank)
  (:states '(normal visual)
    :keymaps 'vterm-mode-map
    :prefix "SPC"
    "k" 'vterm-send-next-key))

(use-package vlf
  :init
  (require 'vlf-setup)
  :ensure t)

(use-package winner
  :config
  (winner-mode 1)
  :general
  (:states '(normal visual)
    :keymaps 'override
    "SPC wz" 'winner-undo
    "SPC wx" 'winner-redo))

(use-package tramp
  :defer t
  :config
  (put 'temporary-file-directory 'standard-value `(,temporary-file-directory))
  :custom
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil))

(use-package goto-last-change
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :custom
  (evil-want-C-u-scroll t)
  :config
  (evil-mode t))

(use-package evil-collection
  :ensure t
  :after evil
  :delight evil-collection-unimpaired-mode
  :custom (evil-collection-want-find-usages-bindings t))

(use-package evil-multiedit
  :ensure t
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-surround
  :ensure t
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :delight
  :ensure t)

(use-package evil-lispy
  :delight
  :ensure t
  :hook
  (lisp-mode . evil-lispy-mode)
  (emacs-lisp-mode . evil-lispy-mode)
  (clojure-mode . evil-lispy-mode)
  :general
  (:states '(normal visual) :keymaps '(lsp-mode-map)
           "SPC mjs" 'lispy-split))

(use-package evil-org
  :delight
  :ensure t
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :init (counsel-projectile-mode t)
  :after (counsel projectile))

(use-package default-text-scale
  :ensure t
  :custom
  (default-text-scale-amount 20)
  :general
  (:states '(normal visual)
    :prefix "SPC"
    :infix "t"
    "+" 'default-text-scale-increase
    "-" 'default-text-scale-decrease
    "R" 'default-text-scale-reset))

(use-package memory-usage
  :ensure t)

(use-package ace-window :ensure t)

(use-package ivy
  :ensure t
  :custom
  (ivy-initial-inputs-alist nil)
  :config
  (defun lt:swiper-org-section ()
    "Pre-fill swiper input with region."
    (interactive)
    (swiper "^\\* "))
  :general
  (:states '(normal visual)
    "SPC sS" 'swiper-all
    "SPC ss" 'swiper
    "SPC so" 'lt:swiper-org-section))

(use-package yasnippet
  :ensure t
  :delight yas-minor-mode
  :custom
  (yas-verbosity 1)
  (yas-wrap-around-region t)
  (yas-snippet-dirs '("~/.emacs.d/snippets/"))
  :config
  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs '(yasnippet-snippets-dir)))

  (yas-reload-all)
  (yas-global-mode))

(use-package centered-cursor-mode
  :ensure t
  :delight
  :hook prog-mode
  :custom
  (ccm-vps-init (round (* 21 (window-text-height)) 34)))

(use-package yasnippet-snippets
  :ensure t)

(use-package counsel
  :ensure t
  :custom
  (ivy-on-del-error-function #'ignore)
  :general
  (:states '(normal visual)
    :keymaps 'override
    "SPC SPC" 'counsel-M-x))

(use-package which-key
  :ensure t
  :delight
  :commands (which-key-mode)
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.2 "Reduce the time before which-key pops up")
  (which-key-allow-evil-operators t "Show evil keybindings")
  (which-key-sort-order 'which-key-key-order-alpha  "Sort things properly alphabetical"))

(use-package projectile
  :ensure t
  :delight '(:eval (concat " P:" (projectile-project-name)))
  :init (projectile-mode t)
  :custom
  ;; Use lsp-clojure-create-test instead
  ;; (projectile-create-missing-test-files t)
  (projectile-project-search-path '("~/projects/"))
  (projectile-sort-order 'recently-active)
  (projectile-enable-caching t)
  :config
  (add-to-list 'projectile-globally-ignored-directories "^\\.shadow-cljs$")
  (use-package counsel-projectile
    :ensure t
    :init (counsel-projectile-mode t)
    :after (counsel projectile))
  :general
  (flawless-def
    :infix "p"
    "c" 'projectile-kill-buffers
    "C" 'projectile-invalidate-cache
    "t" 'projectile-toggle-between-implementation-and-test
    "s" 'projectile-save-project-buffers
    "g" 'counsel-git-grep
    "r" 'counsel-rg
    "p" 'counsel-projectile-switch-project
    "b" 'counsel-projectile-switch-to-buffer
    "f" 'counsel-projectile-find-file))

;; Visual
(use-package delight
  :ensure t)

(use-package almost-mono-themes
  :config
  ;; (load-theme 'almost-mono-black t)
  ;; (load-theme 'almost-mono-gray t)
  ;; (load-theme 'almost-mono-cream t)
  ;; (load-theme 'almost-mono-white t)
  )

(use-package nord-theme
  :after highlight-sexp
    ;; nano-theme
  ;; darktooth-theme
  ;; :quelpa (nano-theme
  ;;	   :fetcher github
  ;;	   :repo "rougier/nano-theme")
  :custom
  (hl-sexp-background-color "#3b4252")

  (hl-sexp-foreground-color nil)
  :ensure t
  :load-path "themes"
  ;; :init
  ;; (setq darktooth-theme-kit t)
  :config
  (defun lt:nord ()
    (interactive)
    (load-theme 'nord t)
    (setq hl-sexp-background-color "#3b4252")
    (hl-sexp-delete-overlay)
    (hl-sexp-create-overlay)
    (setq rainbow-identifiers-cie-l*a*b*-lightness 80)
    (setq rainbow-identifiers-cie-l*a*b*-saturation 50)
    (setq rainbow-identifiers-choose-face-function
   #'rainbow-identifiers-cie-l*a*b*-choose-face))
  (lt:nord))

(use-package highlight-sexp
  :delight
  :quelpa
  (highlight-sexp :repo "daimrod/highlight-sexp" :fetcher github :version original)
  :hook
  (lisp-mode . highlight-sexp-mode)
  (emacs-lisp-mode . highlight-sexp-mode)
  (clojure-mode . highlight-sexp-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :ensure t
  :hook
  (prog-mode . rainbow-identifiers-mode))

(use-package rainbow-mode
  :ensure t
  :delight
  :hook '(prog-mode help-mode))

;; (use-package diff-hl
;;   :ensure t
;;   :quelpa
;;   (diff-hl :repo "dgutov/diff-hl" :fetcher github :version original :files ("diff-hl.el"))
;;   :custom-face
;;   (diff-hl-insert ((t (:background "#a6e22c" :foreground "#a6e22c"))))
;;   (diff-hl-delete ((t (:background "#f83535" :foreground "#f83535"))))
;;   (diff-hl-change ((t (:background "#e7db74" :foreground "#e7db74"))))
;;   :hook (find-file . (lambda () (when (vc-backend (buffer-file-name)) (diff-hl-mode)))))

(use-package shackle
  :ensure t
  :config
  (shackle-mode)
  :custom
  ;; (shackle-default-rule '(:popup t))
  (shackle-rules
   '(("\\*cider-repl.*\\*"
      :regexp t
      :other t
      :size 0.25
      :align bottom)
     ("\\*vterm\\*"
      :regexp t
      :other t
      :size 0.25
      :popup t
      :align bottom)
     ("\\*vterminal.*\\*"
      :regexp t
      :other t
      :size 0.25
      :popup t
      :align bottom))))

(use-package idle-highlight-mode
  :ensure t
  :hook prog-mode)

;; Text editing
(use-package display-line-numbers-mode
  :hook (prog-mode org-mode beancount-mode yaml-mode text-mode))

(use-package display-fill-column-indicator-mode
  :hook (text-mode prog-mode))

(use-package undo-fu
  :after evil
  :ensure t
  :delight
  :custom
  (evil-undo-system 'undo-fu))

;; Programming
;;; Git
(use-package smerge
  :general
  (:states '(normal visual) :keymaps 'smerge-mode-map
          "gj" 'smerge-prev
          "gk" 'smerge-next))

(use-package git-timemachine :ensure t)

(use-package magit
  :ensure t
  :after evil
  :init (evil-collection-init 'magit)
  :custom
  (cond
   ((eq system-type 'darwin)
    (magit-git-executable "/usr/local/bin/git")))
  (magit-diff-paint-whitespace-lines 'all)
  (magit-display-buffer-function
   (lambda (buffer)
     (display-buffer buffer '(display-buffer-same-window))))
  :config
  (evil-set-initial-state 'magit-mode 'normal)
  :general
  (:states '(normal visual) :keymaps '(magit-mode-map)
           "SPC" nil)
  (:states '(normal visual) :prefix "SPC" :infix "g"
           "b" 'magit-checkout
           "B" 'magit-blame
           "g" 'magit-status
           "f" 'magit-find-file
           "l" 'magit-log-buffer-file))

;;; Basic programming (not basic lang!!)
(use-package company
  :ensure t
  :delight
  :pin melpa-stable
  :init (global-company-mode))

(use-package flycheck
  :ensure t
  :delight
  :hook
  (prog-mode . flycheck-mode))

(use-package flycheck-projectile
  :ensure t
  :general
  (:states '(normal visual) :prefix "SPC" :infix "f"
    "p" 'flycheck-projectile-list-errors))

(use-package flycheck-clj-kondo :ensure t)

(use-package highlight :ensure t)

(use-package highlight-symbol :ensure t)

(use-package lsp-mode
  :delight
  (lsp-mode "LSP")
  (lsp-lens-mode "")
  :ensure t
  :custom
  (read-process-output-max (* 1024 1024))
  (lsp-auto-guess-root t)
  (lsp-headerline-breadcrumb-enable nil)
  :general
  (:states '(normal visual) :keymaps '(lsp-mode-map)
           "gr" 'lsp-find-references
           ;; "gi" 'lsp-find-implementation
           "gd" 'lsp-find-definition
           "gD" 'evil-goto-definition
           "SPC mjrs" 'lsp-rename))

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t)

(use-package lsp-ui
  :after (lsp-mode)
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package dap-mode
  :after (lsp-mode)
  :defer
  :ensure t)

(use-package prog-mode
  :custom
  (display-line-numbers-type 'relative)
  (evil-commentary-mode t)
  :general
  (:states '(normal visual)
    "SPC mCa" 'mc/mark-all-dwim))

;;; Lisps

(use-package lisp-mode
  :defer 1
  :general
  (:states '(normal visual)
    :keymaps 'emacs-lisp-mode-map
    :prefix "SPC m"
    :infix "e"
    "e" 'eval-last-sexp
    "b" 'eval-buffer
    "d" 'eval-defun
    "l" 'load-library
    "r" 'eval-region)
  :config
  (use-package lispy :ensure t)

  (use-package lispyville
    :ensure t
    :delight
    :hook
    (lisp-mode . lispyville-mode)
    (emacs-lisp-mode . lispyville-mode)
    (clojure-mode . lispyville-mode))

  (use-package paredit :ensure t)

  (use-package smex :ensure t))

;;; Clojure
(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.bb\\'" . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'" . clojure-mode))
  :config
  (use-package clj-refactor
    :ensure t
    :delight)
  (use-package clojure-snippets :ensure t :defer t)
  (use-package anakondo :ensure t)

  (use-package cider
    :after evil
    :init (evil-collection-init 'cider)
    :ensure t
    :defer t
    :delight
    (clojurescript-mode "")
    (cider-auto-test-mode " t")
    (cider-enlighten-mode " e")

    :config
    (add-to-list 'display-buffer-alist '("\\*cider-error\\*"
                                         (display-buffer-in-side-window)
                                         (side . right)
                                         (slot . 3)
                                         (window-height . shrink-window-if-larger-than-buffer)
                                         (dedicated . t)))

    :custom
    (cider-print-fn 'fipp)
    (cider-merge-sessions 'project)
    (cider-save-file-on-load nil)
    (cider-repl-pop-to-buffer-on-connect t)
    (cider-repl-result-prefix "\n;; => ")
    (cider-repl-buffer-size-limit 10000)
    (nrepl-log-messages t)
    (nrepl-hide-special-buffers t)
    (nrepl-use-ssh-fallback-for-remote-hosts t)
    (cider-repl-display-in-current-window t)
    (cider-repl-use-clojure-font-lock t)
    (cider-prompt-save-file-on-load 'always-save)
    (cider-font-lock-dynamically '(macro core deprecated))
    (cider-overlays-use-font-lock t)
    (cider-repl-use-pretty-printing t)
    (cljr-magic-requires nil)
    (cljr-insert-newline-after-require nil)

    :hook
    (cider-mode . clj-refactor-mode)
    :delight)

  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2)
    (re-frame.core/reg-event-fx 1)
    (re-frame.core/reg-fx 1)
    (re-frame.core/reg-cofx 1)
    (rf/reg-event-fx 1)
    (rf/reg-fx 1)
    (re-frame.core/reg-event-db 1)
    (re-frame.core/reg-db 1)
    (rf/reg-event-db 1)
    (rf/reg-db 1)
    (re-frame.core/reg-sub 1)
    (rf/reg-sub 1)
    (component-style-def 1)
    (reg-view 1)
    (reg-modal 1)
    (attempt-all 1)
    (try-all 1))

  (defun +/insert-random-uuid ()
    "Insert a random UUID.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

WARNING: this is a simple implementation. The chance of generating the same UUID is much higher than a robust algorithm.."
    (interactive)
    (insert
     (format "#uuid \"%04x%04x-%04x-%04x-%04x-%06x%06x\""
             (random (expt 16 4))
             (random (expt 16 4))
             (random (expt 16 4))
             (random (expt 16 4))
             (random (expt 16 4))
             (random (expt 16 6))
             (random (expt 16 6)))))
  (require 'flycheck-clj-kondo)

  :hook
  (clojure-mode . lsp)
  (clojure-mode . lsp-ui-mode)
  (clojure-mode . yas-minor-mode)
  (clojure-mode . subword-mode)
  (clojure-mode . eldoc-mode)
  (clojure-mode . (lambda ()
                    (auto-fill-mode 1)
                    (set (make-local-variable 'fill-nobreak-predicate)
                         (lambda ()
                           (not (or (eq (get-text-property (point) 'face)
                                        'font-lock-comment-face)
                                    (eq (get-text-property (point) 'face)
                                        'font-lock-string-face (point) 'face)))))))

  :general
  (:states '(normal visual) :prefix "SPC mc" :keymaps 'clojure-mode-map
           "j" 'cider-jack-in
           "s" 'cider-jack-in-cljs
           "J" 'cider-jack-in-clj&cljs
           "c" 'cider-connect-clj
           "s" 'cider-connect-cljs
           "C" 'cider-connect-clj&cljs
           "S" 'cider-connect-sibling-cljs)
  (:states '(normal visual) :prefix "SPC ms" :keymaps 'clojure-mode-map
           "b" 'sesman-link-with-buffer
           "s" 'sesman-link-session)
  (:states '(normal visual) :prefix "SPC mm" :keymaps 'clojure-mode-map
           "e" 'cider-enlighten-mode)
  (flawless-mode-def
    :infix "d"
    :keymaps 'clojure-mode-map
    "e" 'cider-debug-defun-at-point)
  (flawless-mode-def
    :infix "j"
    :keymaps 'clojure-mode-map
    "ml" 'cljr-move-to-let
    "xl" 'cljr-expand-let
    "rs" 'cljr-rename-symbol
    "uw" 'cljr-unwind
    "uW" 'cljr-unwind-all
    "tf" 'cljr-thread-first-all
    "tl" 'cljr-thread-last-all
    "tt" 'transpose-sexps
    "aM" 'lsp-clojure-add-missing-libspec
    "am" 'cljr-add-missing-libspec
    "nc" 'cljr-clean-ns)
  (flawless-mode-def
    :infix "i"
    :keymaps 'clojure-mode-map
    "l" 'cider-inspect-last-result
    "e" 'cider-inspect-last-sexp)
  (flawless-mode-def
    :keymaps 'cider-repl-mode-map
    "q" 'cider-quit
    "c" 'cider-repl-clear-buffer)
  (flawless-mode-def
    :infix "r"
    :keymaps 'clojure-mode-map
    "b" 'cider-switch-to-repl-buffer
    "B" 'cider-switch-to-repl-on-insert)
  (flawless-mode-def
    :infix "h"
    :keymaps 'clojure-mode-map
    "d" 'cider-doc)
  (flawless-mode-def
    :infix "e"
    :keymaps 'clojure-mode-map
    "c" 'cider-pprint-eval-last-sexp-to-comment
    "e" 'cider-eval-last-sexp
    "d" 'cider-eval-defun-at-point
    "b" 'cider-eval-buffer)
  (flawless-mode-def
    :infix "t"
    :keymaps 'clojure-mode-map
    "P" 'cider-test-run-project-tests
    "t" 'cider-test-run-test
    "f" 'cider-test-rerun-failed-tests
    "n" 'cider-test-run-ns-tests)
  (flawless-mode-def
    :infix "P"
    :keymaps 'clojure-mode-map
    "t" 'cider-profile-toggle
    "T" 'cider-profile-ns-toggle
    "v" 'cider-profile-var-summary
    "C" 'cider-profile-clear)
  (flawless-mode-def
    :infix "m"
    :keymaps 'clojure-mode-map
    "m" 'cider-macroexpand-1
    "M" 'cider-macroexpand-all)
  (flawless-mode-def
    :infix "i"
    :keymaps 'clojure-mode-map
    "t" 'transpose-sexps
    "s" 'indent-sexp
    "r" 'indent-region
    "B" 'cider-format-buffer
    "u" '+/insert-random-uid))

(use-package lsp-java
  :after (lsp-mode)
  :ensure t)

;;; YML

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

;;; Beancount
;; (use-package beancount
;;   :ensure t
;;   :quelpa (beancount :fetcher github :repo "beancount/beancount-mode" :files ("beancount.el" "COPYING"))
;;   :mode ("\\.bean\\'" . beancount-mode)
;;   :general
;;   (:states '(normal visual) :keymaps 'beancount-mode-map
;;	   "SPC mq" 'beancount-query
;;	   "SPC mc" 'beancount-check))

;;; Org
(use-package org
  :ensure org-contrib
  :general
 (:states '(normal visual) :keymap 'outline-mode-map
    ;; "gh" 'org-next-visible-heading
    ;; "gl" 'org-previous-visible-heading
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level)
 (:states '(normal visual)
    :prefix "SPC"
    :keymaps 'org-mode-map
    "mx" 'org-export-dispatch
    "mst" 'org-set-tags-command
    "msp" 'org-set-property
    "msa" 'org-attach)
  (:states '(normal visual)
    :prefix "SPC"
    :infix "o"
    "a" 'org-agenda
    "c" 'org-capture
    "g" 'org-caputre-goto-last-stored)
  (:states '(normal visual)
    :prefix "SPC m"
    :keymaps 'org-mode-map
    :infix "d"
    "d" 'org-deadline
    "s" 'org-schedule)
  (:states '(normal visual)
    :prefix "SPC m"
    :keymaps 'org-mode-map
    "t" 'org-todo)
  (:states '(normal visual)
    :prefix "SPC m"
    :keymaps 'org-mode-map
    "C" 'org-columns)
  (:states '(normal visual)
    :prefix "SPC m"
    :keymaps 'org-mode-map
    :infix "c"
    "r" 'org-evaluate-time-range
    "o" 'org-open-at-point
    "i" 'org-clock-in
    "p" 'org-pomodoro
    "e" 'org-set-effort)
  (:states '(normal visual)
    :prefix "SPC"
    :infix "n"
    "n" 'counsel-projectile-switch-to-org
    "p" 'org-pomodoro
    "i" 'org-clock-in-last
    "o" 'org-clock-goto
    "r" 'org-resolve-clocks
    "C" 'org-clock-cancel
    "c" 'org-clock-out)
  (:states '(normal visual)
    :keymaps 'org-mode-map
    :prefix "SPC m"
    :infix "l"
    "l" 'org-insert-link
    "C" ''counsel-org-link)
  :custom
  (org-attach-store-link-p 'attached)
  (org-log-reschedule 'time)

  (org-log-redeadline 'time)

  (org-startup-folded "OVERVIEW")
  (org-directory "~/org/")
  (org-agenda-files (directory-files-recursively org-directory "\\.org$"))
  (org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))
  (org-todo-keywords
   '((sequence "TODO(t!)" "WAIT(w@/!)" "RVIW(r)" "STRT(s!)" "CTRL(c!)" "HOLD(h!)" "TEST(q!)"
               "|" "DONE(d!)" "KILL(k@)")))
  (org-tag-alist '(("important" . ?i)
                   ("urgent" . ?u)
                   ("buy" . ?b)))
  ;; (org-agenda-category-icon-alist
  ;;  '(("WORK" "~/.emacs.d/icons/person-digging-solid.svg" nil nil :ascent center :mask heuristic)))
  (org-agenda-custom-commands
   '(("u" "Undated tasks" ((todo "TODO")
                           (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                       'deadline 'scheduled 'timestamp))))
     ("s" "Sprint" ((todo "TODO")
                    (org-agenda-span (lt/days-to-next-sunday))))
     ("b" "Backlog" ((todo "TODO")
                     (tags-todo "-expense")))
     ("d" "Daily" ((org-agenda-ndays 60)))
     ("e" "Planned Expenses" tags-todo "+expense")
     ("i" "Inbox" (search ((org-agenda-files '("~/inbox.org")))))
     ("d" "Upcoming deadlines" agenda ""
      ((org-agenda-entry-types '(:deadline))
       ;; a slower way to do the same thing
       ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
       (org-agenda-span 1)
       (org-deadline-warning-days 60)
       (org-agenda-time-grid nil)))))

  (org-capture-templates
   `(("a" "ARVL")

     ("am" "Meeting" entry (id "7DE08A4E-86F5-4D6A-A378-5BF7237BD8C4")
      "*** %^{TITLE}\n**** Participants\n**** Agenda %i%?\n**** Resolution")

     ("aj" "YT task" plain (file ,(lt:capture-issue "~/org/arvl/tyrell/tasks" :arvl-short))
      "#+TITLE: %^{TITLE}\n#+CATEGORY: ARVL\n* TODO RIGEL-%^{ID1} %^1\n** TODO RIGEL-%^{ID2} %^{TYPE|DEVELOP} %^{DESCR2}"
      :jump-to-captured t)

     ("ab" "Bug" entry (file "arvl/bugs.org")
      "* %^{TITLE}\n %x" :jump-to-captured t)

     ("ar" "Review" entry (id "606B1037-48A0-41F0-9348-249FAA0FEF59")
      "*** TODO RIGEL-%^{ID} D%^{DIFF} %^{ANNOTATION}
:PROPERTIES:
:YT:         RIGEL-%^1
:PHBR:       D%^2
:AUTHOR:     %^{AUTHOR}
:END:"
      :clock-in t)

     ("b" "Book" entry (file "evolution/books.org")
      "* %^{TITLE}\n:PROPERTIES:\n:ADDED: %<[%Y-%02m-%02d]>\n:END:%^{AUTHOR}p\n%?" :empty-lines 1)

     ("q" "Quick Inbox")
     ("qe" "Expence"         entry (file+headline "~/org/inbox.org" "Expenses")
      "* TODO %? :expense:\n")
     ("qt" "Todo")
     ("qtl" "Todo with link" entry (file+headline "~/org/inbox.org" "Tasks")
      "* TODO %?\n %i\n %a")
     ("qtt" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
      "* TODO %?\n")

     ("n" "Notes")

     ("nc" "Current task note" item (clock))
     ("nl" "Current task note + link to line" item (clock) "%a")
     ("no" "Current task link to comment" item (clock) "%(lt:capture-comment-line \"%i\")\n  %a")))
  (org-clock-persist 'history)
  (org-clock-idle-time 15)
  (org-columns-default-format "%80ITEM(Task) %TODO %Effort(Estimated Effort){:} %CLOCKSUM(Clocked){:}")

  :after (evil-org org-pomodoro)
  :hook
  ((org-mode . auto-fill-mode)
   (org-mode . evil-org-mode)
   ((org-clock-in org-clock-out org-clock-cancel) . save-buffer))

  :config
  (defun lt/days-to-next-sunday()
    (let ((dayspan 0)
          (today (string-to-number (format-time-string "%u"))))
      (cond
       ((> today 0) ; from today till sunday, today included
        (setq dayspan (- 8 today)))
       ((= today 0) ; sunday to sunday
        (setq dayspan 8)))))
  ;; ox-extra
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines))
  ;; ox-latex
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author..} to latex export
    ;; (setq org-latex-prefer-user-labels t)

    ;; deleted unwanted file extensions after latexMK
  (setq org-latex-logfiles-extensions
        '("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist"))

  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))
  ;; other
  (defun lt:org-clock-todays-total ()
    "Visit each file in `org-agenda-files' and return the total time of today's
clocked tasks in minutes."
    (interactive)
    (let ((files (org-agenda-files))
          (total 0))
      (org-agenda-prepare-buffers files)
      (dolist (file files)
        (with-current-buffer (find-buffer-visiting file)
          (setq total (+ total (org-clock-sum-today)))))
      (format " Today's total: %s " (org-minutes-to-clocksum-string total))))
 (evil-set-initial-state 'org-agenda-mode 'normal)
  (org-clock-persistence-insinuate)
  (defun lt:capture-comment-line (&optional line)
    (let ((c
           (save-excursion
             (save-window-excursion
               (switch-to-buffer (plist-get org-capture-plist :original-buffer))
               comment-start))))
      (while (string-prefix-p c line)
        (setq line (string-remove-prefix c line)))
      (comment-string-strip line t t)))
  (defun lt:capture-issue (path issue-type)
    #'(lambda ()
        (cl-case issue-type
          (:arvl-short
           (let ((issue-id (read-string "ID: " "RIGEL-")))
             (expand-file-name (format "%s.org" issue-id)path)))
          (:arvl
           (let ((issue-id (read-string "ID: " "RIGEL-"))
                 (name (read-string "Name (camelCase prefix): ")))
             (expand-file-name (format "%s_%s.org"
                                       issue-id
                                       name) path))))))
  (defun lt:yank-org-link (text)
    (if (derived-mode-p 'org-mode)
        (insert text)
      (string-match org-link-bracket-re text)
      (insert (substring text (match-beginning 1) (match-end 1)))))

  (defun lt:org-retrieve-url-from-point ()
    (let* ((link-info (assoc :link (org-context)))
           (text (when link-info
                   ;; org-context seems to return nil if the current element
                   ;; starts at buffer-start or ends at buffer-end
                   (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                   (or (caddr link-info) (point-max))))))
      (if (not text)
          (error "Not in org link")
        (add-text-properties 0 (length text) '(yank-handler (my-yank-org-link)) text)
        (kill-new text))))

  (defun lt:smarter-kill-ring-save ()
    (interactive)
    (if (region-active-p)
        (call-interactively #'kill-ring-save)
      (when (eq major-mode 'org-mode)
        (call-interactively #'lt:org-retrieve-url-from-point))))

  (defun lt:sum-direct-children-org (level children)
    "Update the time LEVEL nodes recursively to be the sum of the times of its children.
Used in `my-org-clocktable-formatter' to go from net times back to tatal times."
    (let ((subtrees (-partition-before-pred (lambda (it) (= level (car it))) children)))
      (-flatten-n 1
                  (--map (let ((it-children (lt:sum-direct-children-org (1+ level)
                                                                        (cdr it))))
                           (cons (--update-at
                                  4 (+ it
                                       (-sum
                                        (--map (nth 4 it)
                                               (--filter (= (1+ level)
                                                            (car it))
                                                         it-children))))
                                  (car it))
                                 it-children))
                         subtrees))))
  (defun lt:org-clocktable-formatter (ipos tables params)
    "Custom formatter for org-mode clocktables which groups by category rather than file.
It uses `org-clock-clocktable-formatter' for the insertion of the
table after sorting the items into tables based on an items
category property. Thus all parameters supported by
`org-clock-clocktable-formatter' are supported. To use this to
sort a clocktable add `:properties (\"CATEGORY\") :formatter
my-org-clocktable-formatter' to that clocktable's arguments."
    (let* ((tt (-flatten-n 1 (-map #'-last-item tables)))
           (formatter (or org-clock-clocktable-formatter
                          'org-clocktable-write-default))
           (newprops (remove "CATEGORY" (plist-get params :properties)))
           (newparams (plist-put (plist-put params :multifile t) :properties newprops))
           newtables)

      ;; Compute net clocked time for each item
      (setq tt
            (--map-indexed
             (let* ((it-level (car it))
                    (it-time (nth 4 it))
                    (it-subtree (--take-while (< it-level (car it))
                                              (-drop (1+ it-index) tt)))
                    (it-children (--filter (= (1+ it-level) (car it))
                                           it-subtree)))
               (-replace-at 4 (- it-time (-sum (--map (nth 4 it) it-children)))
                            it))
             tt))

      ;; Add index (ie id) and indexes of parents (these are needed in the
      ;; sorting step). This can probably be written more functionally using --reduce?
      ;; At least without having to modify hist.
      (setq tt
            (let (hist)
              (--map-indexed (let* ((it-level (car it))
                                    (it-hist (-drop (- (length hist)
                                                       it-level -1)
                                                    hist)))
                               (setq hist (cons it-index it-hist))
                               (cons it-index (cons it-hist it)))
                             tt)))

      ;; Now comes the important phase: sorting, where we copy items with >0 net time
      ;; into newtables based on their category, and we copy their parents when
      ;; appropriate.
      (--each tt (let* ((it-hist (nth 1 it))
                        (it-time (nth 6 it))
                        (it-prop (-last-item it))
                        (it-cat (alist-get "CATEGORY" it-prop nil nil #'string=))
                        ;; Find the index of the table for category: it-cat or if
                        ;; it doesn't yet exist add it to the start of newtables.
                        (cat-pos (or
                                  (--find-index (string= (car it) it-cat) newtables)
                                  (progn (push (list it-cat nil) newtables) 0)))
                        (cat-members (-map #'car (-last-item (nth cat-pos newtables))))
                        (it-parent
                         (or (--find-index (member it
                                                   cat-members)
                                           it-hist)
                             (length it-hist)))
                        (hist-to-add
                         ;; replace the time of copied parents with 0 since if a
                         ;; parents is being copied and has time >0 then it has
                         ;; already been placed in the table for a different
                         ;; category. ie. We don't want time double counted.
                         (--map (-replace-at 6 0 (nth it tt))
                                (-take it-parent it-hist))))

                   (when (not (= 0 it-time))
                     (setf (-last-item (nth cat-pos newtables))
                           (append (cons it hist-to-add)
                                   (-last-item (nth cat-pos newtables)))))))

      (--each newtables (setf (-last-item it) (reverse (-last-item it))))
      ;; Cleanup, remove ids and list of parents, as they are no longer needed.
      (setq newtables
            (--map (list (car it) 0 (--map (-drop 2 it) (-last-item it))) newtables))

      ;; Recompute the total times for each node.
      ;; (replace this with --each and setf?)
      (setq newtables
            (--map (let* ((it-children (lt:sum-direct-children-org 1 (-last-item it)))
                          (it-total-time (-sum
                                          (--map (nth 4 it)
                                                 (--filter (= 1 (car it))
                                                           it-children)))))
                     (list (car it) it-total-time it-children))
                   newtables))
      ;; Actually insert the clocktable now.
      (funcall formatter ipos newtables newparams)
      ;; Replace "File" with "Category" in the "file" column and "*File time*" with "*
      ;; Category time*" in the table.
      (org-table-goto-line 1)
      (org-table-blank-field)
      (insert "Category")
      (org-table-align)
      (let ((n 2))
        (while (org-table-goto-line n)
          (org-table-next-field)
          ;; This won't work if there are addition columns eg. Property column.
          ;; Instead look forward along each line to see if that regexp is matched?
          (when (looking-at "\\*File time\\* .*\| *\\*.*[0-9]:[0-9][0-9]\\*")
            (org-table-blank-field)
            (insert "*Category time*")
            (org-table-align))
          (cl-incf n)))))
  (defun lt:counsel-projectile-swith-to-org ()
    (interactive)
    (counsel-projectile-switch-project "~/org/")))

(use-package outshine
  :ensure t
  :config
  (defun lt:outshine-next-slide ()
    (interactive)
    (widen)
    (outline-next-heading)
    (outshine-narrow-to-subtree))

  (defun lt:outshine-prev-slide ()
    (interactive)
    (widen)
    (outline-previous-heading)
    (outshine-narrow-to-subtree))
  :general
  (:states '(normal visual :keymap 'outshine-mode-map)
    "SPC moj" 'lt:outshine-next-slide
    "SPC mok" 'lt:outshine-prev-slide
    "SPC mon" 'outshine-narrow-to-subtree
    "SPC mow" 'widen))

;; (use-package all-the-icons
;;   :if (display-graphic-p))

(use-package org-clock-today
  :after org
  :ensure t)

(use-package org-agenda
  :after org
  :custom
  (org-agenda-window-setup 'current-window)
  :general
  (:state 'motion :keymaps 'org-agenda-mode-map
          "SPC" nil))

(use-package org-fancy-priorities
  :after org
  :delight
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⬆" " " "⬇")))

(use-package ox-clip
  :ensure t)

(use-package org-duration
  :config
  (setq org-duration-units `(("min" . 1)
                             ("h" . 60)
                             ("d" . ,(* 60 8))
                             ("w" . ,(* 60 8 5))
                             ("m" . ,(* 60 8 5 4))
                             ("y" . ,(* 60 8 5 4 11))))
  (org-duration-set-regexps))

(use-package org-pomodoro
  :ensure t
  :custom
  (org-pomodoro-clock-break t)
  (org-pomodoro-finished-sound-args "-volume 0.3")
  (org-pomodoro-long-break-sound-args "-volume 0.3")
  (org-pomodoro-short-break-sound-args "-volume 0.3")
  (org-pomodoro-manual-break t)
  (org-pomodoro-format "%s")
  (org-pomodoro-short-break-format "%s")
  (org-pomodoro-long-break-format "%s"))

;;; Web
(use-package css-mode
  :custom
  (css-indent-offset 4))

;; TS
(use-package tide
  :ensure t)

;;; TeX
(use-package tex
  :defer t
  :ensure auctex)

;;; protobuf
(use-package protobuf-mode
  :ensure t
  :mode
  (("\\.proto$" . protobuf-mode)))

;; Network
;;; Mail
(use-package notmuch
  :ensure t
  :after evil
  :init (evil-collection-init 'notmuch))

;;; Telega
(use-package telega
  :ensure t
  :quelpa (telega :fetcher github
                  :repo "zevlg/telega.el"
                  :branch "master"
                  :files (:defaults "contrib" "etc" "server" "Makefile"))
  :after evil
  :init
  (telega-mode-line-mode t)
  (setq telega-use-images t)
  (evil-collection-init 'telega)
  :custom
  (telega-chat-fill-column 80)
  (telega-accounts
   (list
    (list "AlexanderUshanov" 'telega-database-dir telega-database-dir-base)
    (list "flaw1322" 'telega-database-dir
          (expand-file-name "flaw1322" telega-database-dir-base))
    (list "C11H26NO2PS" 'telega-database-dir
          (expand-file-name "c11h26no2ps" telega-database-dir-base))))
  :config
  (when (eq system-type 'gnu/linux)
    (setq telega-server-libs-prefix "/usr"))
  :general
  (:states '(normal visual) :prefix "SPC" :infix "c"
           "w" 'telega-chat-with
           "g" 'telega
           "A" 'telega-account-switch))

(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save t)
  :general
  (:states '(normal visual) :keymaps 'rustic-mode-map
           "SPC mhd" 'lsp-describe-thing-at-point
           "SPC mcC" 'rustic-compile
           "SPC mcc" 'rustic-cargo-comp
           "SPC mer" 'rustic-cargo-run))

(use-package web-mode
  :ensure t
  :mode (("\\.jsx?$" . web-mode)
         ("\\.mdx$" . web-mode))
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))
;; init.el ends here
