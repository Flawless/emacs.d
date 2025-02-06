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

;; bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use use-package
(straight-use-package 'use-package)

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

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))

(require 'quelpa-use-package)

(use-package paradox
  :ensure t
  :custom
  (paradox-github-token t)
  (paradox-execute-asynchronously t)
  (paradox-automatically-star t))

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
  (use-dialog-box nil)
  (tooltip-mode nil)
  (savehist-mode 1)
  (warning-minimum-level :error)
  (indent-tabs-mode nil)
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (make-backup-file-name-function 'lt/backup-file-name)
  (mac-command-modifier 'meta)
  (fill-column 100)

  :init
  (set-face-attribute 'mode-line nil  :height 100)
  (set-face-attribute 'mode-line-inactive nil  :height 100)

  :config
  (auth-source-pass-enable)
  (defun lt:file-notify-rm-all-watches ()
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
   ((find-font (font-spec :name "Jetbrains Mono"))
    (custom-set-faces
     '(default ((t (:inherit nil :font "Jetbrains Mono" :size 16))))))
   ((find-font (font-spec :name "Monaspace Neon"))
    (custom-set-faces
     '(default ((t (:inherit nil :font "Monaspace Neon" :size 16))))))
   ((find-font (font-spec :name "Input Mono"))
    (custom-set-faces
     '(default ((t (:inherit nil :font "Input Mono" :size 16)))))))


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
    "SPC xp" 'projectile-run-async-shell-command-in-root
    "SPC xs" 'async-shell-command
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

    "SPC w=" 'balance-windows

    "SPC tF" 'toggle-frame-fullscreen
    "SPC tt" 'toggle-truncate-lines

    "SPC qq" 'save-buffers-kill-emacs))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))  ; Ensure it runs only on graphical Emacs
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package info
  :init (evil-collection-init 'info))

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

(use-package pass
  :ensure t)

(use-package babashka
  :ensure t
  :general
  (:states '(normal visual)
    "SPC xb" 'babashka-tasks)
  :config
  ;; override internal fn to support short task syntax
  (defun babashka--tasks-to-annotated-names (tasks)
    "Convert TASKS to annotated alist."
    (let (results)
      (maphash (lambda (key value)
                 (let ((task-name (symbol-name key)))
                   (unless (string-prefix-p ":" task-name)
                     (push (if (hash-table-p value)
                               (cons task-name (gethash :doc value))
                             task-name)
                           results))))
               tasks)
      results)))

(use-package tramp
  :defer t
  :config
  ;; Workaround a tramp-MacOS bug that dramatically slows completion
  (put 'temporary-file-directory 'standard-value
       (list temporary-file-directory))
  :custom
  (remote-file-name-inhibit-cache nil)
  (tramp-chunksize 2000)
  (tramp-use-ssh-controlmaster-options nil)
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil))

(use-package vagrant-tramp
  :ensure t)

(use-package goto-last-change
  :ensure t)

(setq evil-want-keybinding nil)
(use-package evil
  :ensure t
  :custom
  (evil-want-C-u-scroll t)
  :config
  (evil-mode t)
  :general
  (:keymaps '(evil-motion-state-map)
            "SPC" nil
            "RET" nil
            "TAB" nil))

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
  :hook ((text-mode prog-mode) . evil-surround-mode)
  :config
  (add-to-list 'evil-surround-pairs-alist
               '(?c . ("<!-- " . " -->")))

  (defun evil-surround-get-active-pair (beg end)
    "Get the active surround pair between BEG and END."
    (let ((text (buffer-substring-no-properties beg end)))
      (cl-loop for (key . (open . close)) in evil-surround-pairs-alist
               when (and (string-prefix-p open text)
                         (string-suffix-p close text))
               return (cons open close))))

  (defun toggle-comment-on-html-tag ()
    "Toggle HTML comment on the current HTML tag."
    (interactive)
    (require 'web-mode)
    (save-excursion
      (when (derived-mode-p 'web-mode)
        (web-mode-element-select)
        (let ((beg (region-beginning))
              (end (region-end))
              (surround-pair (evil-surround-get-active-pair beg end)))
          (if (and surround-pair
                   (string= (car surround-pair) "<!-- ")
                   (string= (cdr surround-pair) " -->"))
              ;; Remove the surround
              (evil-surround-delete beg end 'inclusive ?c)
            ;; Add the surround
            (evil-surround-region beg end 'inclusive ?c))))))

  (evil-define-key 'normal web-mode-map
    (kbd "g c") 'toggle-comment-on-html-tag))

(use-package evil-commentary
  :delight
  :ensure t)

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
  :general
  (:states '(normal visual)
    "SPC myi" 'yas-insert-snippet)
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

(use-package smex :ensure t)

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
  :delight " P"
  :init (projectile-mode t)
  :custom
  ;; Use lsp-clojure-create-test instead
  ;; (projectile-create-missing-test-files t)
  ;; (projectile-auto-update-cache nil) why?
  ;; (projectile-dynamic-mode-line nil) why?
  (projectile-auto-update-cache t)      ; i'm tired manually update its cache
  (projectile-dynamic-mode-line t)      ; just interesting, whats that - feel free to disable
  (projectile-project-search-path '("~/projects/"))
  (projectile-sort-order 'recently-active)
  (projectile-enable-caching t)
  :config
  (add-to-list 'projectile-globally-ignored-directories "^\\.shadow-cljs$")
  (use-package counsel-projectile
    :ensure t
    :init (counsel-projectile-mode t)
    :after (counsel projectile))
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))
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

;; Visual Appearance
(use-package delight
  :ensure t)

(use-package almost-mono-themes
  :ensure t
  :load-path "themes"
  :config
  ;; (load-theme 'almost-mono-black t)
  ;; (load-theme 'almost-mono-gray t)
  ;; (load-theme 'almost-mono-cream t)
  ;; (load-theme 'almost-mono-white t)

  (defun lt:apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'almost-mono-white t))
      ('dark (load-theme 'almost-mono-black t))))

  (add-hook 'ns-system-appearance-change-functions #'lt:apply-theme)
  (lt:apply-theme ns-system-appearance))

;; (use-package nord-theme
;;   :after highlight-sexp
;;   ;; nano-theme
;;   ;; darktooth-theme
;;   ;; :quelpa (nano-theme
;;   ;;	   :fetcher github
;;   ;;	   :repo "rougier/nano-theme")
;;   :custom
;;   (hl-sexp-background-color "#3b4252")

;;   (hl-sexp-foreground-color nil)
;;   :ensure t
;;   :load-path "themes"
;;   ;; :init
;;   ;; (setq darktooth-theme-kit t)
;;   :config
;;   (defun lt:nord ()
;;     (interactive)
;;     (load-theme 'nord t)
;;     (setq hl-sexp-background-color "#3b4252")
;;     (hl-sexp-delete-overlay)
;;     (hl-sexp-create-overlay)
;;     (setq rainbow-identifiers-cie-l*a*b*-lightness 80)
;;     (setq rainbow-identifiers-cie-l*a*b*-saturation 50)
;;     (setq rainbow-identifiers-choose-face-function
;;           #'rainbow-identifiers-cie-l*a*b*-choose-face))
;;   (lt:nord))

;; (use-package highlight-sexp
;;   :delight
;;   :quelpa
;;   (highlight-sexp :repo "daimrod/highlight-sexp" :fetcher github :version original)
;;   :hook
;;   (lisp-mode . highlight-sexp-mode)
;;   (emacs-lisp-mode . highlight-sexp-mode)
;;   (clojure-mode . highlight-sexp-mode))

;; (use-package rainbow-delimiters
;;   :ensure t
;;   :hook
;;   (prog-mode . rainbow-delimiters-mode))

;; (use-package rainbow-identifiers
;;   :ensure t
;;   :hook
;;   (prog-mode . rainbow-identifiers-mode))

;; (use-package rainbow-mode
;;   :ensure t
;;   :delight
;;   :hook '(prog-mode help-mode))

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

(use-package langtool
  :custom
  (langtool-language-tool-jar "~/LanguageTool-6.3/languagetool-commandline.jar")
  :ensure t)

(use-package flycheck-languagetool
  :ensure t
  :hook (text-mode . flycheck-languagetool-setup)
  :custom
  (flycheck-languagetool-server-jar
   "~/LanguageTool-6.3/languagetool-commandline.jar"))

(use-package compilation-mode
  :init (evil-collection-init 'compile)
  :hook ((compilation-filter . ansi-color-compilation-filter)))

;; Programming
;;; Tree sitter
(use-package tree-sitter
  :straight (tree-sitter :type git
                         :host github
                         :repo "ubolonton/emacs-tree-sitter"
                         :files ("lisp/*.el")))

(use-package tree-sitter-langs
  :straight (tree-sitter-langs :type git
                               :host github
                               :repo "ubolonton/emacs-tree-sitter"
                               :files ("langs/*.el" "langs/queries"))
  :after tree-sitter)

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

;; (use-package forge
;;   :ensure t)
;; (use-package code-review
;;   :ensure t
;;   :quelpa (code-review :fetcher github
;;                        :repo "phelrine/code-review"
;;                        :branch "fix/closql-update"))

;;; Basic programming stuff

;; (use-package aggressive-indent
;;   :ensure t
;;   :hook (prog-mode . aggressive-indent-mode))

(use-package company
  :ensure t
  :delight
  :pin melpa-stable
  :init (global-company-mode))

(use-package flycheck
  :ensure t
  :delight
  :hook
  ((flycheck-mode . lt/disable-flycheck-for-remote-files)
   (prog-mode . flycheck-mode))
  :config
  (defun lt/disable-flycheck-for-remote-files ()
    "Disable flycheck-mode for remote files opened via TRAMP."
    (when (file-remote-p default-directory)
      (flycheck-mode -1))))

(use-package flycheck-projectile
  :ensure t
  :general
  (:states '(normal visual) :prefix "SPC" :infix "f"
           "p" 'flycheck-projectile-list-errors))

;; (use-package flycheck-clj-kondo :ensure t)

(use-package highlight :ensure t)

(use-package highlight-symbol :ensure t)

(use-package lsp-mode
  :delight
  (lsp-mode "⎈")
  (lsp-lens-mode "")
  :ensure t
  :custom
  (lsp-prefer-flymake nil)
  (read-process-output-max (* 1024 1024))
  (lsp-enable-symbol-highlighting nil)
  (lsp-auto-guess-root t)
  (lsp-headerline-breadcrumb-enable nil)
  :general
  (:states '(normal visual) :keymaps '(lsp-mode-map)
           "gr" 'lsp-find-references
           ;; "gi" 'lsp-find-implementation
           "gd" 'lsp-find-definition
           "gD" 'evil-goto-definition
           "gb" 'lsp-format-buffer
           "SPC mjrs" 'lsp-rename
           "SPC mhd" 'lsp-describe-thing-at-point)
  :init
  (let ((lsp-session-file (expand-file-name ".lsp-session-v1" user-emacs-directory)))
    (when (file-exists-p lsp-session-file)
      (delete-file lsp-session-file)))
  :config
  (defun lsp-tramp-connection-over-ssh-port-forwarding (command)
    "Like lsp-tcp-connection, but uses SSH portforwarding."
    (list
     :connect (lambda (filter sentinel name environment-fn)
                (let* ((host "localhost")
                       (lsp-port (lsp--find-available-port host (cl-incf lsp--tcp-port)))
                       (command (with-parsed-tramp-file-name buffer-file-name nil
                                  (message "[tcp/ssh hack] running LSP %s on %s / %s" command host localname)
                                  (let* ((unix-socket (format "/tmp/lsp-ssh-portforward-%s.sock" lsp-port))
                                         (command (list
                                                   "ssh"
                                                   ;; "-vvv"
                                                   "-L" (format "%s:%s" lsp-port unix-socket)
                                                   host
                                                   "socat"
                                                   (format "unix-listen:%s" unix-socket)
                                                   (format "system:'\"cd %s && %s\"'" (file-name-directory localname) command)
                                                   )))
                                    (message "using local command %s" command)
                                    command)))
                       (final-command (if (consp command) command (list command)))
                       (_ (unless (executable-find (cl-first final-command))
                            (user-error (format "Couldn't find executable %s" (cl-first final-command)))))
                       (process-environment
                        (lsp--compute-process-environment environment-fn))
                       (proc (make-process :name name :connection-type 'pipe :coding 'no-conversion
                                           :command final-command :sentinel sentinel :stderr (format "*%s::stderr*" name) :noquery t))
                       (tcp-proc (progn
                                   (sleep-for 1) ; prevent a connection before SSH has run socat. Ugh.
                                   (lsp--open-network-stream host lsp-port (concat name "::tcp")))))

                  ;; TODO: Same :noquery issue (see above)
                  (set-process-query-on-exit-flag proc nil)
                  (set-process-query-on-exit-flag tcp-proc nil)
                  (set-process-filter tcp-proc filter)
                  (cons tcp-proc proc)))
     :test? (lambda () t))))

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t)

(use-package treemacs-nerd-icons
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package lsp-ui
  :after (lsp-mode)
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover nil)
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
    "g==" 'align
    "g=r" 'align-regexp
    "SPC mCa" 'mc/mark-all-dwim))

;;; Lisps

(use-package lisp-mode
  :defer 1
  :general
  (:states '(normal visual)
    :keymaps 'emacs-lisp-mode-map
    "SPC mee" 'eval-last-sexp
    "SPC meb" 'eval-buffer
    "SPC med" 'eval-defun
    "SPC mel" 'load-library
    "SPC mer" 'eval-region))

;; evil paredit alternative
(use-package lispy
  :ensure t
  :delight
  :general
  (:states '(normal visual)
    :keymaps 'lispy-mode-map
    "SPC mjs" 'lispy-split))

(use-package evil-lispy
  :ensure t
  :hook
  ((elisp-mode clojure-mode clojure-ts-mode) . evil-lispy-mode)
  :delight)

(use-package lispyville
  :ensure t
  :hook
  ((elisp-mode clojure-mode clojure-ts-mode) . lispyville-mode)
  :delight)

;;; Clojure
(use-package clojure-ts-mode
  :delight
  (clojure-ts-mode "CLJ:TS ")
  (clojure-ts-clojurescript-mode "CLJS:TS ")
  (clojure-ts-clojurec-mode "CLJC:TS ")
  :ensure t
  :mode (("\\.clj\\'" . clojure-ts-mode)
         ("\\.bb\\'" . clojure-ts-mode)
         ("\\.cljc\\'" . clojure-ts-clojurec-mode)
         ("\\.cljs\\'" . clojure-ts-clojurescript-mode)
         ("\\.edn\\'" . clojure-ts-mode))
  :custom
  (clojure-ts-indent-style 'fixed)
  :config
  (define-minor-mode clj-auto-reload-mode
    "Toggle automatic reload of Clojure code after save.
When enabled, runs reload function after saving Clojure files."
    :init-value nil
    :lighter "⟳"
    :global t
    :keymap nil
    (if clj-auto-reload-mode
        (add-hook 'after-save-hook #'lt/clj-reload nil t)
      (remove-hook 'after-save-hook #'lt/clj-reload t)) )

  (defun lt/clj-reload ()
    (interactive)
    (cider-interactive-eval
     "
    (try
      (require ' [clj-reload.core :refer [reload]])
      ((eval 'reload))
      (catch Exception _ (println :clj-reload-is-not-available)))
    "))
  :preface
  (defun lt/clts-lsp-start()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (lsp-deferred))
  :hook
  (clojure-ts-mode . cider-mode)
  (clojure-ts-mode . lt/clts-lsp-start)
  :general
  (:states '(normal visual) :keymaps 'clojure-ts-mode-map
           "SPC mjrR" 'clj-auto-reload-mode
           "SPC mjrr" 'lt/clj-reload))

(use-package clojure-mode
  :ensure t
  :delight
  (clojure-mode "CLJ ")
  (clojurescript-mode "CLJS ")
  ;; :mode (("\\.clj\\'" . clojure-mode)
  ;;        ("\\.bb\\'" . clojure-mode)
  ;;        ("\\.cljc\\'" . clojurec-mode)
  ;;        ("\\.cljs\\'" . clojurescript-mode)
  ;;        ("\\.edn\\'" . clojure-mode))
  :custom
  (clojure-indent-style 'always-indent)

  :preface
  (defun lt/clerk-show ()
    (interactive)
    (when-let
        ((filename
          (buffer-file-name)))
      (save-buffer)
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")"))))

  :config
  (defun lt/clj-lsp-start()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (lsp-deferred))
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

  ;; (require 'flycheck-clj-kondo)

  :hook
  (clojure-mode . cider-mode)
  (clojure-mode . lt/clj-lsp-start)
  ;; (after-save . lt/clj-reload)
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
  (:states '(normal visual) :keymaps 'clojure-mode-map
           "SPC mjR" 'lt/clj-reload
           "SPC mCs" 'lt/clerk-show))

(use-package cider
  :ensure t :defer t
  :delight
  (cider-auto-test-mode " t")
  (cider-enlighten-mode " e")
  :after evil
  :init (evil-collection-init 'cider)
  :config
  (add-to-list 'display-buffer-alist '("\\*cider-error\\*"
                                       (display-buffer-in-side-window)
                                       (side . right)
                                       (slot . 3)
                                       (window-height . shrink-window-if-larger-than-buffer)
                                       (dedicated . t)))

  (defun polylith-project-p ()
    (let* ((root (projectile-project-root))
           (deps (expand-file-name "deps.edn" root))
           (workspace (expand-file-name "workspace.edn" root)))
      (and (projectile-project-p)
           (file-exists-p deps)
           (file-exists-p workspace))))

  (defcustom my-cider-last-selected-aliases nil
    "List of last selected CIDER aliases.
This variable is used to remember the user's alias selections across Emacs sessions."
    :type '(repeat string)
    :group 'my-cider)

  (defun my-extract-poly-profiles (deps-file)
    "Extract aliases starting with :+ from the deps.edn file.
Returns a list of alias strings without the leading colon, e.g., '+default'."
    (with-temp-buffer
      (insert-file-contents deps-file)
      (goto-char (point-min))
      (let (aliases)
        ;; Regex explanation:
        ;; :\+       => Matches the literal characters :+
        ;; \([-a-zA-Z0-9_]+\) => Captures one or more allowed characters in the alias name
        (while (re-search-forward ":\\+\\([-a-zA-Z0-9_]+\\)" nil t)
          (push (concat "+" (match-string 1)) aliases))
        (delete-dups aliases))))

  (defun my-toggle-selection (choices &optional initial-selected)
    "Allow toggling of multiple CHOICES interactively.
INITIAL-SELECTED is a list of initially selected items."
    (let ((selected (or initial-selected '())))
      (catch 'finish-selection
        (while t
          (let ((choice (completing-read
                         (format "Selected: [%s] Toggle (RET to finish): "
                                 (string-join selected ", "))
                         choices nil nil nil nil ""))) ;; Allow empty input to finish
            (if (string-empty-p choice)
                (throw 'finish-selection selected)
              (if (member choice selected)
                  (setq selected (remove choice selected))
                (push choice selected))))))))

  (defun my-poly-profiles-prompt (root)
    "Prompt for aliases to use when starting CIDER. Only works for Polylith projects.
Returns a list of selected aliases or nil."
    (let* ((deps (expand-file-name "deps.edn" root))
           (aliases (my-extract-poly-profiles deps)))
      (if (and aliases (> (length aliases) 0))
          (let* ((default-alias (if (member "+default" aliases) "+default" nil))
                 (last-selected-aliases (cl-remove-if (lambda (x)
                                                        (not (member x aliases)))
                                                      my-cider-last-selected-aliases))
                 (initial-selected (or last-selected-aliases
                                       (and default-alias (list "+default"))))
                 (chosen (my-toggle-selection aliases initial-selected)))
            (setq my-cider-last-selected-aliases chosen)
            chosen) ;; Return the list of chosen aliases
        (progn
          (message "No :+aliases found in deps.edn.")
          nil))))

  (defun my-cider-jack-in-with-aliases (orig-fun args)
    "Advice around `cider-jack-in` to set aliases and start REPL in main root."
    (if (polylith-project-p)
        (let* ((poly-root (projectile-project-root))
               (aliases (my-poly-profiles-prompt poly-root))
               (alias-string (when aliases (string-join aliases ":")))
               (existing-aliases cider-clojure-cli-aliases)
               (cider-clojure-cli-aliases (if existing-aliases
                                              (concat alias-string ":" existing-aliases)
                                            alias-string)))
          (when alias-string
            (setq cider-session-name-template (format "%%J:%%h:%s:%%p" alias-string)))
          ;; Temporarily set `default-directory` to poly-root
          (progn
            (message poly-root)
            (funcall orig-fun (plist-put args :project-dir poly-root))))
      ;; Non-Polylith projects proceed normally
      (setq cider-session-name-template "%J:%h:%p")
      (funcall orig-fun args)))

  (advice-add 'cider-jack-in :around #'my-cider-jack-in-with-aliases)

  :custom
  (cider-clojure-cli-aliases ":user")
  ;; lsp
  (cider-print-fn 'fipp)
  ;; (cider-merge-sessions 'project)
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

  :hook
  ((clojure-mode clojure-ts-mode) . cider-mode)
  (cider-mode . clj-refactor-mode)
  (cider-mode . clj-refactor-mode)
  :general
  (:states '(normal visual) :keymaps 'cider-repl-mode-map
           "SPC mq" 'cider-quit
           "SPC mc" 'cider-repl-clear-buffer)

  (:states '(normal visual) :keymap 'cider-mode-map
           "SPC mil" 'cider-inspect-last-result
           "SPC mie" 'cider-inspect-last-sexp

           "SPC mme" 'cider-enlighten-mode

           "SPC msb" 'sesman-link-with-buffer
           "SPC mss" 'sesman-link-session

           "SPC mcj" 'cider-jack-in
           "SPC mcs" 'cider-jack-in-cljs
           "SPC mcJ" 'cider-jack-in-clj&cljs
           "SPC mcc" 'cider-connect-clj
           "SPC mcs" 'cider-connect-cljs
           "SPC mcC" 'cider-connect-clj&cljs
           "SPC mcS" 'cider-connect-sibling-cljs

           "SPC mde" 'cider-debug-defun-at-point

           "SPC mrb" 'cider-switch-to-repl-buffer
           "SPC mrB" 'cider-switch-to-repl-on-insert
           ;; "SPC mhd" 'cider-doc

           "SPC mec" 'cider-pprint-eval-last-sexp-to-comment
           "SPC mee" 'cider-eval-last-sexp
           "SPC med" 'cider-eval-defun-at-point
           "SPC meb" 'cider-eval-buffer

           "SPC mtP" 'cider-test-run-project-tests
           "SPC mtt" 'cider-test-run-test
           "SPC mtf" 'cider-test-rerun-failed-tests
           "SPC mtr" 'cider-test-rerun-test
           "SPC mtn" 'cider-test-run-ns-tests

           "SPC mPt" 'cider-profile-toggle
           "SPC mPT" 'cider-profile-ns-toggle
           "SPC mPv" 'cider-profile-var-summary
           "SPC mPC" 'cider-profile-clear

           "SPC mmm" 'cider-macroexpand-1
           "SPC mmM" 'cider-macroexpand-all

           "SPC mit" 'transpose-sexps
           "SPC mis" 'cider-format-edn-last-sexp
           "SPC mir" 'cider-format-region
           "SPC miB" 'cider-format-buffer
           "SPC mif" 'cider-format-defun
           "SPC miu" '+/insert-random-uid))

(use-package clj-refactor
  :ensure t :delight
  :custom
  (cljr-magic-requires nil)
  (cljr-insert-newline-after-require nil)
  :general
  (:states '(normal visual) :keymaps 'clj-refactor-map
           "SPC mjml" 'cljr-move-to-let
           "SPC mjxl" 'cljr-expand-let
           "SPC mjuw" 'cljr-unwind
           "SPC mjuW" 'cljr-unwind-all
           "SPC mjtf" 'cljr-thread-first-all
           "SPC mjtl" 'cljr-thread-last-all
           "SPC mjtt" 'transpose-sexps
           "SPC mjaM" 'lsp-clojure-add-missing-libspec
           "SPC mjam" 'cljr-add-missing-libspec
           "SPC mjnc" 'cljr-clean-ns))

(use-package clojure-snippets :ensure t :defer t)

(use-package anakondo :ensure t)

(use-package clj-decompiler :ensure t)

(use-package lsp-java
  :after (lsp-mode)
  :ensure t)

;;; java
(use-package java-mode
  :defer t
  :hook ((java-mode . lsp)
         (java-mode . tree-sitter-hl-mode))
  :general
  (:states '(normal visual) :keymaps 'java-mode-map :prefix "SPC"
           "mjam" 'lsp-java-add-import))

;;; YML

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'"
  :hook (yaml-mode . tree-sitter-hl-mode))

;;; Beancount
(use-package beancount
  :ensure t
  :quelpa (beancount :fetcher github :repo "beancount/beancount-mode" :files ("beancount.el" "COPYING"))
  :mode ("\\.bean\\'" . beancount-mode)
  :general
  (:states '(normal visual) :keymaps 'beancount-mode-map
           "gs" 'counsel-org-goto
           "SPC mq" 'beancount-query
           "SPC mc" 'beancount-check))

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
  (:states '(normal visual)
    :keymaps 'org-mode-map
    "gs" 'counsel-org-goto)
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

(use-package elisp-mode
  :defer t
  :hook ((emacs-lisp-mode . flycheck-mode) ; Enable Flycheck for linting
         (emacs-lisp-mode . lispy-mode) ; Enable Lispy for structured editing
         (emacs-lisp-mode . lispyville-mode) ; Enable Lispyville for Evil users
         (emacs-lisp-mode . eldoc-mode) ; Enable Eldoc for inline help
         (emacs-lisp-mode . subword-mode) ; Navigate through subwords
         (emacs-lisp-mode . (lambda () ; Automatically indent the buffer before saving
                              (add-hook 'before-save-hook #'lt/elisp-indent-buffer nil t))))
  :config
  (defun lt/elisp-indent-buffer ()
    "Indent the entire buffer when saving Emacs Lisp code."
    (when (eq major-mode 'emacs-lisp-mode)
      (indent-region (point-min) (point-max)))))

(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save t)
  :hook (rustic-mode . tree-sitter-hl-mode)
  :general
  (:states '(normal visual) :keymaps 'rustic-mode-map
           "SPC mhd" 'lsp-describe-thing-at-point
           "SPC mcC" 'rustic-compile
           "SPC mcc" 'rustic-cargo-comp
           "SPC mer" 'rustic-cargo-run
           "SPC mtt" 'rustic-cargo-test-run))

(use-package web-mode
  :ensure t
  :mode ("\\.jsx?$" "\\.mdx$" "\\.j2$" "\\.html$")
  :hook (web-mode . tree-sitter-hl-mode)
  :custom
  (web-mode-enable-engine-detection t)
  (web-mode-engines-alist '(("jinja2" . "\\.j2$\\'")))
  (web-mode-markup-indent-offset 2)
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package typescript-mode
  :ensure t
  :hook (typescript-mode . tree-sitter-hl-mode))

(use-package lua-mode
  :ensure t
  :defer 1)

(use-package vue-mode
  :ensure t
  :defer 1)

(use-package dockerfile-mode
  :ensure t
  :defer 1)

(use-package eww
  :after elfeed-org
  :general
  (:states '(normal visual)
    "SPC eww" 'eww))

(use-package elfeed
  :ensure t
  :after elfeed-org
  :init (evil-collection-init 'elfeed)
  :general
  (:states '(normal visual)
    "SPC elf" 'elfeed)
  :hook
  (elfeed-show-mode . lt:ajust-to-read)
  :custom
  (elfeed-db-directory "~/.emacs.d")
  :config
  (elfeed-org)
  (defun lt:ajust-to-read ()
    (buffer-face-set 'variable-pitch nil
                     :family "American Typewriter"
                     :height 1.3)))

(use-package elfeed-org
  :after org
  :ensure t
  :custom
  (rmh-elfeed-org-files
   (list (concat org-directory "feed.org")))
  :config
  (elfeed-org))

(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pass-get 'secret "openai-key")))))

(use-package terraform-mode
  :ensure t
  :defer 1
  :hook (terraform-mode . lsp-deferred)
  :custom
  (terraform-format-on-save t))

(use-package esxml
  :straight (esxml :type git :host nil :repo "https://github.com/tali713/esxml.git"))

(use-package nov
  :after esxml elfeed
  :mode (("\\.epub\\'" . nov-mode))
  :hook
  (nov-mode . lt:ajust-to-read)
  :custom
  (nov-text-width 80)
  :config
  :straight (nov :type git :host nil :repo "https://depp.brause.cc/nov.el.git"))


(use-package kubernetes :ensure t)

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-ts-mode)
  :preface
  (defun lt/go-lsp-start()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (lsp-deferred))
  :hook
  ((go-ts-mode . lt/go-lsp-start)
   (go-ts-mode . tree-sitter-hl-mode))
  :config
  (add-to-list 'exec-path "~/.local/bin")
  (setq lsp-go-analyses '((nilness . t)
                          (shadow . t)
                          (unusedwrite . t)
                          (fieldalignment . t))
        lsp-go-codelenses '((test . t)
                            (tidy . t)
                            (upgrade_dependency . t)
                            (vendor . t)
                            (run_govulncheck . t))))

(use-package go-tag
  :ensure t)

(use-package godoctor
  :ensure t)

(use-package nginx-mode
  :ensure t
  :commands nginx-mode)

;; Python
(use-package python
  :ensure t
  :hook
  ((python-mode . lsp-deferred)
   (python-mode . lt/disable-venv-checks-for-remote-files)
   ;; (python-mode . lt/python-flycheck)
   )
  :config
  (defun lt/disable-venv-checks-for-remote-files ()
    "Disable virtual environment checks for remote files."
    (when (file-remote-p default-directory)
      (setq-local pet-use-poetry-p nil)
      (setq-local pet-use-pipenv-p nil)
      (setq-local pet-use-conda-p nil)))

  (defun lt/python-flycheck ()
    (flycheck-select-checker 'python-ruff)
    (flycheck-mode 1)))

(use-package python-pytest :ensure t)

(use-package conda
  :straight t
  :after python
  :config
  ;; taken from doom
  ;; The location of your anaconda home will be guessed from a list of common
  ;; possibilities, starting with `conda-anaconda-home''s default value (which
  ;; will consult a ANACONDA_HOME envvar, if it exists).
  ;;
  ;; If none of these work for you, `conda-anaconda-home' must be set
  ;; explicitly. Afterwards, run M-x `conda-env-activate' to switch between
  ;; environments
  (or (cl-loop for dir in (list conda-anaconda-home
                                "~/.anaconda"
                                "~/.miniconda"
                                "~/.miniconda3"
                                "~/.miniforge3"
                                "~/anaconda3"
                                "~/miniconda3"
                                "~/miniforge3"
                                "~/opt/miniconda3"
                                "/usr/bin/anaconda3"
                                "/usr/local/anaconda3"
                                "/usr/local/miniconda3"
                                "/usr/local/Caskroom/miniconda/base"
                                "~/.conda")
               if (file-directory-p dir)
               return (setq conda-anaconda-home (expand-file-name dir)
                            conda-env-home-directory (expand-file-name dir)))
      (message "Cannot find Anaconda installation"))
  ;; integration with term/eshell
  (conda-env-initialize-interactive-shells)
  (add-to-list 'global-mode-string
               '(conda-env-current-name (" conda:" conda-env-current-name " "))
               'append))

;;; pyvenv
;; To hopefully work better with virtual environments over tramp
;; to test if works with conda (and tramp really)
(use-package pyvenv
  :straight t
  :after python
  :config
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)
  (add-to-list 'global-mode-string
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
               'append))

;;; numpydoc
(use-package numpydoc
  ;; use main repo and not emacsmirror as is a bit behind
  ;; as of 07/10/23
  :straight (numpydoc
             :type git
             :host github
             :repo "douglasdavis/numpydoc.el")
  :defer t
  :config
  (setq numpydoc-insertion-style 'yas ;; use yasnippet style prompt
        numpydoc-insert-examples-block nil ;; no examples block
        numpydoc-insert-return-without-typehint nil ;; as it says
        numpydoc-auto-fill-paragraphs t)) ;; autofill my docs

(use-package lsp-pyright
  :straight t
  :custom
  (lsp-pyright-langserver-command "pyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; (use-package lsp-pyright
;;   :straight t
;;   :init (setq lsp-pyright-multi-root nil)
;;   :config
;;   (lsp-register-custom-settings
;;    '(("python.pythonPath" ljosa-lsp-pyright-locate-python)))
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp-deferred))))

;;; apheleia for formatting
(use-package apheleia
  :straight (apheleia
             :type git
             :host github
             :repo "radian-software/apheleia"))

(use-package python-black
  :ensure t
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(use-package python-isort
  :ensure t
  :demand t
  :after python
  :hook (python-mode . python-isort-on-save-mode))

(use-package ruff-format
  :ensure t
  :demand t
  :after python
  :hook
  ((python-mode . ruff-format-on-save-mode)
   ;; (python-mode . lt/flycheck-python)
   )
  ;; :config
  ;;   (flycheck-define-checker python-ruff
  ;;     "A Python syntax and style checker using the ruff utility.
  ;; To override the path to the ruff executable, set
  ;; `flycheck-python-ruff-executable'.
  ;; See URL `http://pypi.python.org/pypi/ruff'."
  ;;     :command ("ruff"
  ;;               "check"
  ;;               "--output-format=full"
  ;;               (eval (when buffer-file-name
  ;;                       (concat "--stdin-filename=" buffer-file-name)))
  ;;               "-")
  ;;     :standard-input t
  ;;     :error-filter (lambda (errors)
  ;;                     (let ((errors (flycheck-sanitize-errors errors)))
  ;;                       (seq-map #'flycheck-flake8-fix-error-level errors)))
  ;;     :error-patterns
  ;;     ((warning line-start
  ;;               (file-name) ":" line ":" (optional column ":") " "
  ;;               (id (one-or-more (any alpha)) (one-or-more digit)) " "
  ;;               (message (one-or-more not-newline))
  ;;               line-end))
  ;;     :modes python-mode)

  ;; (defun lt/flycheck-python()
  ;;   (setq-local flycheck-checkers '(python-ruff))
  ;;   (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  ;;   (add-to-list 'flycheck-disabled-checkers 'lsp)
  ;;   (flycheck-add-next-checker 'lsp 'python-ruff))
  )

(use-package elpy
  :ensure t
  :after poetry lsp
  :hook
  ((python-mode . elpy-mode)
   (elpy-mode . flycheck-mode)
   (elpy-mode . poetry-tracking-mode))
  :config
  (flycheck-add-next-checker 'lsp '(t . python-ruff))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote))
  :init
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (elpy-enable))

;; (use-package flycheck-pycheckers
;;   :after flycheck
;;   :ensure t
;;   :custom
;;   (flycheck-pycheckers-checkers '(mypy3 pyflakes))
;;   :init
;;   (with-eval-after-load 'flycheck
;;     (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

(use-package poetry
  :ensure t)

(use-package pet
  :straight (pet
             :type git
             :host github
             :repo "wyuenho/emacs-pet")
  :ensure t
  :hook ((pet-mode . lt/disable-pet-mode-for-remote-files))
  :config
  (defun lt/disable-pet-mode-for-remote-files ()
    "Disable pet-mode's heavy operations for remote files."
    (when (file-remote-p default-directory)
      (setq-local pet-mode nil)))
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; Copilot
(use-package gptel
  :straight t
  :ensure t
  :custom
  (gptel-model "gpt-4o")
  (gptel-default-mode 'org-mode)
  (gptel-fill-column 80) ;; Set the desired fill column
  (gptel-directives
   '((default
      . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
     (programming
      . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
     (programming-apply
      . "You are a large language model and a careful programmer. Apply changes from context related to given code in git merge conflicts format so user can review it easyly with standard tools and apply. Do not add anything not directly related to code like notes, text, markup languages quotation or other syntax and keep code that you were given since your reply will replace original content. Use code pieces from chat as basis if provided.")
     (writing . "You are a large language model and a writing assistant. Respond concisely.")
     (chat . "You are a large language model and a conversation partner. Respond concisely.")) )
  :config
  (defun enable-visual-line-mode-in-chatgpt ()
    "Enable visual-line-mode in *ChatGPT* buffer."
    (when (string= (buffer-name) "*ChatGPT*")
      (visual-line-mode 1)))
  (defun clean-gpt-buffer (&rest _)
    (with-current-buffer "*ChatGPT*"
      (fill-region (point-min) (point-max))))
  (defun gptel-api-key ()
    (string-trim (shell-command-to-string "pass /flawless/gpt-copilot")))
  :general
  (:states '(normal visual)
    "SPC lG" 'gptel
    "SPC lg" 'gptel-menu
    "SPC la" 'gptel-add)
  :init
  ;; (add-hook 'gptel-post-response-functions 'clean-gpt-buffer)
  (add-hook 'gptel-post-response-functions 'enable-visual-line-mode-in-chatgpt)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

;; GraphQL
(use-package graphql-mode
  :ensure t
  :mode "\\.graphql\\'"
  :hook
  (graphql-mode . lsp)
  (graphql-mode . (lambda ()
                    (add-hook 'before-save-hook #'prettier-prettify nil t)))
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("graphql-lsp" "server" "--method" "stream"))
    :major-modes '(graphql-mode)
    :server-id 'graphql-lsp))
  (setq graphql-indent-level 2))

(use-package prettier
  :config
  (setenv "NODE_PATH" "/usr/local/lib/node_modules")
  :ensure t)


;;; init.el ends here
