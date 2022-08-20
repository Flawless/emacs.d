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
(load custom-file t)

(customize-set-variable 'package-archives
			`(,@package-archives
			  ("melpa" . "https://melpa.org/packages/")
			  ("melpa-stable" . "https://stable.melpa.org/packages/")

			  ;; ("org" . "https://orgmode.org/elpa/")
			  ;; ("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")
			  ))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

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
  :custom
  (mac-command-modifier 'meta)
  (fill-column 120)

  :config
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

  (set-frame-font "Input Mono-14" nil t)

  (add-to-list 'exec-path "/usr/local/bin")
  (defun lt:reload-dir-locals-for-current-buffer ()
    "reload dir locals for the current buffer"
    (interactive)
    (let ((enable-local-variables :all))
      (hack-dir-local-variables-non-file-buffer)))
  (put 'use-package 'lisp-indent-function 1)
  (put ':states 'lisp-indent-function 1)
  (customize-set-variable 'package-enable-at-startup nil)
  (package-initialize)

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

    "SPC tF" 'toggle-frame-fullscreen
    "SPC tt" 'toggle-truncate-lines

    "SPC qq" 'save-buffers-kill-emacs))

(use-package vlf
  :init
  (require 'vlf-setup)
  :ensure t)

(use-package balanced-windows
  :config
  (balanced-windows-mode))

(use-package neotree
  :ensure t
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow)))

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

(use-package use-package-core
  :custom
  ;; (use-package-verbose t)
  ;; (use-package-minimum-reported-time 0.005)
  (use-package-enable-imenu-support t))

(use-package auto-package-update
  :ensure quelpa
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t))

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
  :ensure t
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil)
  :config (evil-mode t))

(use-package default-text-scale
  :ensure t
  :custom
  (default-text-scale-amount 10)
  :general
  (:states '(normal visual)
    :prefix "SPC"
    :infix "t"
    "+" 'default-text-scale-increase
    "-" 'default-text-scale-decrease
    "R" 'default-text-scale-reset))

(use-package gcmh
  :ensure t
  :custom
  (gcmh-high-cons-threshold #x800000000)
  :init
  (gcmh-mode 1))

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

(use-package paredit :ensure t)
(use-package smex :ensure t)

(use-package prog-mode
  :custom
  (display-line-numbers-type 'relative)
  (evil-commentary-mode t)
  :general
  (:states '(normal visual)
    "SPC mCa" 'mc/mark-all-dwim))

(use-package yasnippet
  :ensure t
  :custom
  (yas-verbosity 1)
  (yas-wrap-around-region t)
  (yas-snippet-dirs '("~/.emacs.d/snippets/"))
  :config
  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs '(yasnippet-snippets-dir)))

  (yas-reload-all)
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package clojure-snippets
  :ensure t)

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
   "r" 'eval-region))

(use-package counsel
  :ensure t
  :custom
  (ivy-on-del-error-function #'ignore)
  :general
  (:states '(normal visual)
    :keymap 'override
    "SPC SPC" 'counsel-M-x))

(use-package which-key
  :ensure t
  :commands (which-key-mode)
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.2 "Reduce the time before which-key pops up")
  (which-key-allow-evil-operators t "Show evil keybindings")
  (which-key-sort-order 'which-key-key-order-alpha  "Sort things properly alphabetical"))

;; (use-package darktooth-theme
;;   :ensure t
;;   :load-path "themes"
;;   :init
;;   (setq darktooth-theme-kit t)
;;   :config
;;   (use-package highlight-sexp
;;     :quelpa
;;     (highlight-sexp :repo "daimrod/highlight-sexp" :fetcher github :version original)
;;     :hook
;;     (lisp-mode . highlight-sexp-mode)
;;     (emacs-lisp-mode . highlight-sexp-mode)
;;     (clojure-mode . highlight-sexp-mode))

;;   (use-package rainbow-delimiters
;;     :ensure t
;;     :hook
;;     (prog-mode . rainbow-delimiters-mode))

;;   (use-package rainbow-identifiers
;;     :ensure t
;;     :custom
;;     (rainbow-identifiers-cie-l*a*b*-lightness 80)
;;     (rainbow-identifiers-cie-l*a*b*-saturation 50)
;;     (rainbow-identifiers-choose-face-function
;;      #'rainbow-identifiers-cie-l*a*b*-choose-face)
;;     :hook
;;     (prog-mode . rainbow-identifiers-mode))

;;   (use-package rainbow-mode
;;     :ensure t
;;     :hook '(prog-mode help-mode))

;;   (load-theme 'darktooth t))

(use-package theme-changer
  :custom
  (calendar-location-name "Saint-Petersburg, Russia")
  (calendar-latitude 59.85)
  (calendar-longitude 30.18)
  :ensure t
  :config
  (use-package almost-mono-themes
    :ensure t
    :load-path "themes")
  (change-theme 'almost-mono-white 'almost-mono-gray))

(use-package evil-commentary
  :ensure t)

(use-package display-line-numbers-mode
  :hook (prog-mode org-mode beancount-mode yaml-mode text-mode))

(use-package display-fill-column-indicator-mode
  :hook (text-mode prog-mode))

(use-package git-timemachine :ensure t)

(use-package magit
  :ensure t
  :init (evil-collection-init 'magit)
  :hook
  (magit-pre-refresh diff-hl-magit-pre-refresh)
  (magit-post-refresh diff-hl-magit-post-refresh)
  :custom
  (magit-git-executable "/usr/local/bin/git")
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

(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init))

(use-package evil-multiedit
  :ensure t
  :config
  (evil-multiedit-default-keybinds))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package flycheck-projectile
  :ensure t
  :general
  (:states '(normal visual) :prefix "SPC" :infix "f"
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
  :custom
  (evil-undo-system 'undo-tree)
  (ad-return-value (concat ad-return-value ".gz"))
  (undo-tree-auto-save-history t)
  :init (global-undo-tree-mode)
  :general
  (flawless-mode-def
    "u" 'undo-tree-visualize))

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

(use-package highlight-symbol
  :ensure t)

(use-package highlight
  :ensure t)

(use-package projectile
  :ensure t
  :init (projectile-mode t)
  :custom
  (projectile-project-search-path '("~/projects/"))
  (projectile-sort-order 'recently-active)
  (projectile-enable-caching t)
  :general
  (flawless-def
    :infix "p"
    "c" 'projectile-kill-buffers
    "C" 'projectile-invalidate-cache
    "t" 'projectile-toggle-between-implementation-and-test
    "s" 'projectile-save-project-buffers
    "g" 'counsel-git-grep
    "p" 'counsel-projectile-switch-project
    "b" 'counsel-projectile-switch-to-buffer
    "f" 'counsel-projectile-find-file))

(use-package counsel-projectile
  :ensure t
  :init (counsel-projectile-mode t)
  :after (counsel projectile))

(use-package beancount
  :ensure t
  :quelpa (beancount :fetcher github :repo "beancount/beancount-mode" :files ("beancount.el" "COPYING"))
  :mode ("\\.bean\\'" . beancount-mode)
  :general
  (:states '(normal visual) :keymaps 'beancount-mode-map
	  "SPC mq" 'beancount-query
	  "SPC mc" 'beancount-check))

(use-package org-agenda
  :custom
  (org-agenda-window-setup 'current-window)
  :general
  (:state 'motion :keymaps 'org-agenda-mode-map
	  "SPC" nil))

(use-package smerge
  :general
  (:states '(normal visual) :keymaps 'smerge-mode-map
	  "gj" 'smerge-prev
	  "gk" 'smerge-next))

(use-package org
  :general
  (:states '(normal visual)
    :prefix "SPC"
    "at" 'org-set-tags-command)
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
    "r" 'org-evaluate-time-range
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
    "o" 'org-open-at-point
    "r" 'org-clock-report
    "i" 'org-clock-in
    "p" 'org-pomodoro
    "e" 'org-set-effort)
  (flawless-def
    :states '(normal visual)
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
    :prefix "SPC m"
    :infix "l"
    "l" 'org-insert-link
    "C" ''counsel-org-link)
  :custom
  (org-startup-folded "OVERVIEW")
  (org-directory "~/org/")
  (org-agenda-files (directory-files-recursively org-directory "\\.org$"))
  (org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))
  (org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w@/!)" "RVIW(r)" "STRT(s!)" "CTRL(c!)" "HOLD(h!)" "TEST(q!)"
		    "|" "DONE(d!)" "KILL(k@)")))
  (org-tag-alist '(("important" . ?i)
		   ("urgent" . ?u)
		   ("arvl" . ?a)))
  (org-agenda-custom-commands '(("1" "Q1" tags-todo "+important+urgent")
				("2" "Q2" tags-todo "+important-urgent")
				("3" "Q3" tags-todo "-important+urgent")
				("4" "Q4" tags-todo "-important-urgent")
				("d" "Daily" ((org-agenda-ndays 60)))))

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

     ("b" "Book" entry (file org-books-file)
      "* %^{TITLE}\n:PROPERTIES:\n:ADDED: %<[%Y-%02m-%02d]>\n:END:%^{AUTHOR}p\n%?" :empty-lines 1)

     ("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
      "* TODO %?\n %i\n %a")

     ("n" "Notes")

     ("nc" "Current task note" item (clock))))
  (org-clock-persist 'history)
  (org-clock-idle-time 15)
  (org-columns-default-format "%80ITEM(Task) %TODO %Effort(Estimated Effort){:} %CLOCKSUM(Clocked){:}")

  :after (evil-org org-pomodoro)
  :hook
  ((org-mode . auto-fill-mode)
   (org-mode . evil-org-mode)
   ((org-clock-in org-clock-out org-clock-cancel) . save-buffer))

  :config
  (use-package org-fancy-priorities
    :ensure t
    :hook
    (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '("⬆" " " "⬇")))
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (org-clock-persistence-insinuate)
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

(use-package org-duration
  :config
  (setq org-duration-units `(("min" . 1)
			     ("h" . 60)
			     ("d" . ,(* 60 8))
			     ("w" . ,(* 60 8 5))
			     ("m" . ,(* 60 8 5 4))
			     ("y" . ,(* 60 8 5 4 11))))
  (org-duration-set-regexps))

(use-package org-pomodoro :ensure t)

(use-package evil-org
  :ensure t
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package doom-modeline
  :custom
  (column-number-mode t)
  (line-number-mode t)
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package shackle
  :ensure t
  :custom
  ;; (shackle-default-rule (:popup t))
  (shackle-rules
   '(("*cider-repl.*\\*"
      :regexp t
      :other t
      :size 0.25
      :align bottom))))

(use-package diff-hl
  :ensure t
  :custom-face
  (diff-hl-insert ((t (:background "#a6e22c" :foreground "#a6e22c"))))
  (diff-hl-delete ((t (:background "#f83535" :foreground "#f83535"))))
  (diff-hl-change ((t (:background "#e7db74" :foreground "#e7db74"))))
  :hook (find-file . (lambda () (when (vc-backend (buffer-file-name)) (diff-hl-mode)))))


;; Programming languages
;;; Clojure
(use-package clojure-mode
  :ensure t
  :requires
  (evil-lispy-mode lispyville-mode cider-mode clj-refactor anakondo)
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.bb\\'" . clojure-mode)
	 ("\\.cljc\\'" . clojurec-mode)
	 ("\\.cljs\\'" . clojurescript-mode)
	 ("\\.edn\\'" . clojure-mode))
  :config
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
  (flawless-mode-def
    :infix "i"
    :keymaps 'clojure-mode-map
    "t" 'transpose-sexps
    "s" 'indent-sexp
    "r" 'indent-region
    "B" 'cider-format-buffer
    "u" '+/insert-random-uid))

(use-package clj-refactor :ensure t)
(use-package idle-highlight-mode
  :ensure t
  :hook prog-mode)

;; (use-package anakondo
;;   :ensure t
;;   :hook
;;   (clojure-mode . anakondo-minor-mode))

(use-package cider
  :ensure t
  :defer t
  :config
  (cider-add-to-alist 'cider-jack-in-dependencies "djblue/portal" "0.24.0")
  :custom
  (cider-save-file-on-load nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-result-prefix "\n;; => ")
  (cider-repl-buffer-size-limit 10000)
  (nrepl-log-messages t)
  (cider-repl-display-in-current-window t)
  (cider-repl-use-clojure-font-lock t)
  (cider-prompt-save-file-on-load 'always-save)
  (cider-font-lock-dynamically '(macro core function var))
  (nrepl-hide-special-buffers t)
  (cider-overlays-use-font-lock t)
  (cider-repl-use-pretty-printing t)
  (cider-clojure-cli-global-options "-A:portal")
  :init
  (evil-collection-init 'cider)
  (defun portal.api/open ()
    (interactive)
    (cider-nrepl-sync-request:eval
     "(require 'portal.api) (portal.api/tap) (portal.api/open)"))

  (defun portal.api/clear ()
    (interactive)
    (cider-nrepl-sync-request:eval "(portal.api/clear)"))

  (defun portal.api/close ()
    (interactive)
    (cider-nrepl-sync-request:eval "(portal.api/close)"))
  :hook
  (cider-mode . clj-refactor-mode)
  :diminish subword-mode
  :general
  (flawless-mode-def
    :infix "p"
    :keymaps 'clojure-mode-map
    "o" 'portal.api/open
    "c" 'portal.api/clear)
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
    :infix "c"
    :keymaps 'clojure-mode-map
    "j" 'cider-jack-in
    "s" 'cider-jack-in-cljs
    "J" 'cider-jack-in-clj&cljs
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
    "c" 'cider-pprint-eval-last-sexp-to-comment
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
    "C" 'cider-profile-clear)
  (flawless-mode-def
    :infix "m"
    :keymaps 'clojure-mode-map
    "m" 'cider-macroexpand-1
    "M" 'cider-macroexpand-all))

;;; Elixir
(use-package elixir-mode
  :ensure t
  :hook alchemist)

(use-package alchemist
  :ensure t
  :general
  (flawless-mode-def
    :infix "e"
    :keymaps 'alchemist-mode-map
    "b" 'alchemist-eval-buffer
    "e" 'alchemist-eval-current-line))

(provide 'init)
;;; Web
(use-package css-mode
  :custom
  (css-indent-offset 2))


;;; JS
;; (use-package js2-mode
;;   :ensure t
;;   :mode
;;   (("\\.js$" . js2-mode))

;;   (js-indent-level 2))

;; TeX

(use-package tex
  :defer t
  :ensure auctex)

;; protobuf

(use-package protobuf-mode
  :ensure t
  :mode
  (("\\.proto$" . protobuf-mode)))

(use-package telega
  :ensure t
  :quelpa (telega :fetcher github
		  :repo "zevlg/telega.el"
		  :branch "master"
		  :files (:defaults "contrib" "etc" "server" "Makefile"))
  :init
  (evil-collection-init 'telega)
  :custom
  (telega-use-docker t)
  :general
  (:states '(normal visual) :prefix "SPC" :infix "c"
	   "w" 'telega-chat-with
	   "g" 'telega))

;;; init.el ends here
