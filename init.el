;;; init.el --- My flawless emacs config -*- lexical-binding: t -*-
;; Copyright (C) 2021-2025 Alexander Ushanov
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
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(defvar bootstrap-version)
(defvar straight-repository-user "flawless")

(let
  (
    (bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir) user-emacs-directory)))
    (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/flawless/straight.el/develop/install.el"
        'silent
        'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
(straight-use-package 'use-package)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; (use-package straight
;;   :straight (:type git :host github :repo "flawless/straight.el"))

;;; here goes config modules
(use-package
  core
  :straight (:type built-in)
  :load-path "config"
  :config (message "Core loaded"))

(use-package
  editing
  :after core
  :straight (:type built-in)
  :load-path "config"
  :config (message "Editing loaded"))

(use-package
  langs
  :after editing
  :straight (:type built-in)
  :load-path "config"
  :after core
  :config (message "Langs loaded"))


(provide 'init)
;;; init.el ends here
(put 'magit-clean 'disabled nil)
