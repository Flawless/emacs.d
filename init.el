;;; init.el --- Description -*- lexical-binding: t; -*-
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
(load custom-file)

(customize-set-variable 'package-archives
			`(,@package-archives
			  ("melpa" . "https://melpa.org/packages/")
			  ("melpa-stable" . "https://stable-melpa.org/packages/")

			  ("org" . "https://orgmode.org/elpa/")
			  ("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")))

(add-hook 'before-save-hook 'whitespace-cleanup)

(customize-set-variable 'package-enable-at-startup nil)
(package-initialize)
(setq mac-command-modifier 'meta)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(put 'use-package 'lisp-indent-function 1)

(use-package use-package-core
  :custom
  ;; (use-package-verbose t)
  ;; (use-package-minimum-reported-time 0.005)
  (use-package-enable-imenu-support t))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package gcmh
  :ensure t
  :init
  (gcmh-mode 1))

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

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (evil-mode))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t)) ;; Prevent flickering issues

(use-package general
  :demand t
  :ensure t
  :commands (general-define-key general-override-mode general-evil-setup general--simulate-keys)
  :custom
  (general-override-states '(insert emacs hybrid normal visual motion operator replace))
  :config
  (general-override-mode)
  (general-evil-setup))

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

(flawless-def
  :infix "s"
  "S" 'swiper-all
  "s" 'swiper)

(flawless-def
  :infix "f"
  "f" 'find-file
  "s" 'save-buffer
  "S" 'write-file)

(flawless-def
  :infix "b"
  "b" 'counsel-switch-buffer
  "r" 'revert-buffer
  "d" 'kill-current-buffer
  "s" 'save-buffer
  "S" 'save-some-buffers)

(flawless-def
  :infix "h"
  "k" 'describe-key
  "f" 'describe-function
  "F" 'describe-face
  "m" 'describe-mode
  "o" 'describe-symbol
  "v" 'describe-variable)

(flawless-def
    :infix "w"
  "s" 'split-window-right
  "v" 'split-window-vertically
  "d" 'delete-window
  "w" 'ace-window)

(flawless-def
  :infix "t"
  "F" 'toggle-frame-fullscreen
  "t" 'toggle-truncate-lines
  "+" 'text-scale-increase
  "-" 'text-scale-decrease)

(flawless-def
 :infix "q"
 "q" 'save-buffers-kill-emacs)

(use-package gcmh
  :ensure t
  :init
  (gcmh-mode 1))

(use-package ace-window :ensure t)
(use-package ivy :ensure t)
(use-package paredit :ensure t)
(use-package smex :ensure t)

(use-package prog-mode
  :custom
  (display-line-numbers-type 'relative)
  (evil-commentary-mode t)
  :config
(rainbow-delimiters-mode)
  (rainbow-identifiers-mode))

(use-package lisp-mode
  :defer 1
  :general
  (flawless-mode-def
   :keymaps 'emacs-lisp-mode-map
   :infix "e"
   "e" 'eval-last-sexp
   "b" 'eval-buffer
   "d" 'eval-defun
   "l" 'load-library
   "r" 'eval-region))

(use-package counsel
  :ensure t
  :general
  (flawless-def
    "SPC" 'counsel-M-x))

(use-package which-key
  :ensure t
  :commands (which-key-mode)
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.2 "Reduce the time before which-key pops up")
  (which-key-allow-evil-operators t "Show evil keybindings")
  (which-key-sort-order 'which-key-key-order-alpha  "Sort things properly alphabetical"))

(use-package darktooth-theme
  :ensure t
  :load-path "themes"
  :init
  (setq darktooth-theme-kit t)
  :config
  (load-theme 'darktooth t))

(use-package evil-commentary
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :ensure t
  :custom
  (rainbow-identifiers-cie-l*a*b*-lightness 80)
  (rainbow-identifiers-cie-l*a*b*-saturation 50)
  (rainbow-identifiers-choose-face-function
   #'rainbow-identifiers-cie-l*a*b*-choose-face)
  :hook
  (prog-mode . rainbow-identifiers-mode))

(use-package rainbow-mode
  :ensure t
  :hook '(prog-mode help-mode))

(use-package display-line-numbers-mode
  :hook (prog-mode org-mode beancount-mode))

(use-package magit
  :ensure t
  :init (evil-collection-init 'magit)
  :general
  (flawless-def
    :infix "g"
    "g" 'magit-status))

(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init))

(defun tyrell-styles ()
  (put-clojure-indent 're-frame.core/reg-event-fx 1)
  (put-clojure-indent 're-frame.core/reg-fx 1)
  (put-clojure-indent 'rf/reg-event-fx 1)
  (put-clojure-indent 'rf/reg-fx 1)
  (put-clojure-indent 're-frame.core/reg-event-db 1)
  (put-clojure-indent 're-frame.core/reg-db 1)
  (put-clojure-indent 'rf/reg-event-db 1)
  (put-clojure-indent 'rf/reg-db 1)
  (put-clojure-indent 're-frame.core/reg-sub 1)
  (put-clojure-indent 'rf/reg-sub 1)
  (put-clojure-indent 'component-style-def 1)
  (put-clojure-indent 'reg-view 1)
  (put-clojure-indent 'reg-modal 1)

  (put-clojure-indent 'attempt-all 1)
  (put-clojure-indent 'try-all 1))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.cljc\\'" . clojurec-mode)
	 ("\\.cljs\\'" . clojurescript-mode)
	 ("\\.edn\\'" . clojure-mode))
  :config
  (require 'flycheck-clj-kondo)
  (tyrell-styles)
  :hook
  (clojure-mode . yas-minor-mode)
  (clojure-mode . subword-mode)
  (clojure-mode . eldoc-mode)
  (clojure-mode . idle-highlight-mode)
  :general
  (flawless-mode-def
    :infix "i"
    :keymaps 'clojure-mode-map
    "t" 'transpose-sexps
    "s" 'indent-sexp
    "r" 'indent-region
    "B" 'cider-format-buffer))

(use-package flycheck-projectile
  :ensure t
  :general
  (flawless-mode-def
    :infix "f"
    "p" 'flycheck-projectile-list-errors))

(use-package lispy :ensure t)

(use-package evil-lispy
  :ensure t
  :hook
  (lisp-mode . evil-lispy-mode)
  (emacs-lisp-mode . evil-lispy-mode)
  (clojure-mode . evil-lispy-mode))

(use-package lispyville
  :ensure t
  :hook
  (lisp-mode . lispyville-mode)
  (emacs-lisp-mode . lispyville-mode)
  (clojure-mode . lispyville-mode))

(use-package clj-refactor :ensure t)
(use-package idle-highlight-mode :ensure t)

(use-package anakondo
  :ensure t
  :hook
  (clojure-mode . anakondo-minor-mode))

(use-package cider
  :ensure t
  :defer t
  :custom
  (cider-save-file-on-load nil)
  :init
  (evil-collection-init 'cider)
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
	cider-repl-display-in-current-window t
	cider-repl-use-clojure-font-lock t
	cider-prompt-save-file-on-load 'always-save
	cider-font-lock-dynamically '(macro core function var)
	nrepl-hide-special-buffers t
	cider-overlays-use-font-lock t
	cider-repl-use-pretty-printing t)
  :general
  (flawless-mode-def
    "q" 'cider-quit)
  (flawless-mode-def
    :infix "r"
    :keymaps 'clojure-mode-map
    "b" 'cider-switch-to-repl-buffer
    "B" 'cider-switch-to-repl-on-insert)
  (flawless-mode-def
    :infix "c"
    :keymaps 'clojure-mode-map
    "c" 'cider-connect-clj
    "s" 'cider-connect-cljs
    "C" 'cider-connect-clj&cljs
    "S" 'cider-connect-sibling-cljs)
  (flawless-mode-def
    :infix "h"
    :keymaps 'clojure-mode-map
    "d" 'cider-doc)
  (flawless-mode-def
    :infix "e"
    :keymaps 'clojure-mode-map
    "e" 'cider-eval-last-sexp
    "d" 'cider-eval-defun-at-point
    "b" 'cider-eval-buffer)
  (flawless-mode-def
    :infix "t"
    :keymaps 'clojure-mode-map
    "P" 'cider-test-run-project-tests
    "t" 'cider-test-run-test
    "n" 'cider-test-run-ns-tests)
  (flawless-mode-def
    :infix "P"
    :keymaps 'clojure-mode-map
    "t" 'cider-profile-toggle
    "T" 'cider-profile-ns-toggle
    "v" 'cider-profile-var-summary
    "C" 'cider-profile-clear))

(use-package centered-cursor-mode
  :ensure t
  :hook prog-mode
  :custom
  (ccm-vps-init (round (* 21 (window-text-height)) 34))
  ;; :general
  ;; ("zj" 'ccm-vpos-up)
  ;; ("zh" 'ccm-vpos-down)
  ;; ("zz" 'ccm-vpos-recenter)
  )

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

(use-package lsp-mode :ensure t)
(use-package clojure-snippets
  :ensure t
  :defer t)
(use-package company
  :ensure t
  :pin melpa-stable
  :init (global-company-mode))

(use-package flycheck
  :ensure t
  :hook
  (prog-mode . flycheck-mode))

(use-package flycheck-clj-kondo :ensure t)

(use-package highlight-sexp
  :quelpa
  (highlight-sexp :repo "daimrod/highlight-sexp" :fetcher github :version original)
  :hook
  (lisp-mode . highlight-sexp-mode)
  (emacs-lisp-mode . highlight-sexp-mode)
  (clojure-mode . highlight-sexp-mode))

(use-package projectile
  :ensure t
  :init
  (projectile-mode t)
  :custom
  (projectile-project-search-path '("~/projects/"))
  :general
  (flawless-def
    :infix "p"
    "g" 'counsel-git-grep
    "p" 'counsel-projectile-switch-project
    "b" 'counsel-projectile-switch-to-buffer
    "f" 'counsel-projectile-find-file))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile))

(defalias 'yes-or-no-p 'y-or-n-p)

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

(use-package beancount
  :ensure t
  :quelpa (beancount :fetcher github :repo "beancount/beancount-mode" :files ("beancount.el" "COPYING"))
  :hook (beancount-mode . outline-minor-mode)
  :mode ("\\.bean\\'" . beancount-mode))

(use-package org
  :general
  (flawless-mode-def
    :keymaps 'org-mode-map
    :infix "c"
    "i" 'org-clock-clock-in)
  (flawless-mode-def
    :infix "n"
    "c" 'org-clock-cl))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package mood-line
  :ensure t
  ;; :custom-face
  ;; (mode-line ((t (:inherit default (:box (:line-width -1 :style released-button))))))
  :hook
  (after-init . mood-line-mode))

(provide 'init)
;;; init.el ends here
