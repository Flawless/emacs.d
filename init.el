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
(customize-set-variable 'package-archives
                        `(,@package-archives
                          ("melpa" . "https://melpa.org/packages/")
                          ("melpa-stable" . "https://stable-melpa.org/packages/")

                          ("org" . "https://orgmode.org/elpa/")
                          ("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")))

(customize-set-variable 'package-enable-at-startup nil)
(package-initialize)

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
    :states '(normal)
    :prefix flawless-leader-key
    :non-normal-prefix flawless-leader-secondary-key)

(general-create-definer flawless-mode-def
    :states '(normal)
    :prefix flawless-mode-leader-key
    :non-normal-prefix flawless-mode-leader-secondary-key)

(flawless-def
  :infix "s"
  "s" 'swiper)

(flawless-def
  :infix "f"
  "f" 'find-file
  "s" 'save-buffer)

(flawless-def
  :infix "b"
  "b" 'counsel-switch-buffer
  "r" 'revert-buffer
  "d" 'kill-current-buffer)

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
  "F" 'toggle-frame-fullscreen)

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
  :config
  (setq display-line-numbers-type 'relative))

(use-package lisp-mode
  :defer 1
  :config
  (rainbow-delimiters-mode)
  (rainbow-identifiers-mode)
  (paredit-mode)
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
  (emacs-lisp-mode . rainbow-identifiers-mode) ; actually, turn it off
  (prog-mode . rainbow-identifiers-mode))

(use-package rainbow-mode
  :ensure t
  :hook '(prog-mode help-mode))

(use-package magit
  :ensure t
  :init (evil-collection-init 'magit))

(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.cljc\\'" . clojurec-mode)
	 ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'display-line-numbers-mode)
  (add-hook 'clojure-mode-hook #'programming-defaults)
  (add-hook 'clojure-mode-hook #'yas-minor-mode)         
  (add-hook 'clojure-mode-hook #'linum-mode)             
  (add-hook 'clojure-mode-hook #'subword-mode)           
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-identifiers-mode)
  (add-hook 'clojure-mode-hook #'eldoc-mode)             
  (add-hook 'clojure-mode-hook #'idle-highlight-mode)
  :general
  (flawless-mode-def
    :infix "i"
    :keymaps 'clojure-mode-map
    "s" 'indent-sexp
    "r" 'indent-region
    "B" 'cider-format-buffer))

(use-package lispy :ensure t)
(use-package lispyville :ensure t)
(use-package clj-refactor :ensure t)
(use-package idle-highlight-mode :ensure t)

(use-package anakondo
  :ensure t
  :hook
  (clojure-mode . anakondo-minor-mode)
  (clojurescript-mode . anakondo-minor-mode)
  (clojurec-mode . anakondo-minor-mode))

(use-package cider
  :ensure t
  :defer t
  :init
  (evil-collection-init 'cider)
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :hook
  (repl-mode . paredit-mode)
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
    "n" 'cider-test-run-ns-tests))

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

(use-package flycheck :ensure t)
(use-package flycheck-clj-kondo :ensure t)
;; (use-package highlight-sexp :ensure t)
(use-package projectile
  :ensure t
  :general
  (flawless-def
    :infix "p"
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

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by custom.
 ;; if you edit it by hand, you could mess it up, so be careful.
 ;; your init file should contain only one such instance.
 ;; if there is more than one, they won't work right.
 '(line-number-mode nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable-melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")
     ("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")
     ("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable-melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")
     ("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")
     ("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable-melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")
     ("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")))
 '(package-enable-at-startup nil)
 '(package-selected-packages
   '(highlight-sexp flycheck-clj-kondo clojure-snippets lispyville lispy anakondo counsel-projectile smex quelpa-use-package flycheck lsp-mode projectile company idle-highlight-mode clj-refactor clojure-mode evil-collection magit rainbow-mode rainbow-identifiers rainbow-delimiters evil-commentary darktooth-theme which-key counsel general use-package-ensure-system-package quelpa gcmh evil)))
(custom-set-faces
 ;; custom-set-faces was added by custom.
 ;; if you edit it by hand, you could mess it up, so be careful.
 ;; your init file should contain only one such instance.
 ;; if there is more than one, they won't work right.
 )
