;;; flawless-clojure.el --- My flawless emacs config for Clojure -*- lexical-binding: t -*-
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(use-package
  clojure-mode
  :ensure nil ;; Don't install if not present
  :defer t ;; Defer loading
  :init
  ;; Remove conflicting auto-mode-alist entries for clojure-mode
  (setq auto-mode-alist
    (seq-remove
      (lambda (pair)
        (and (stringp (car pair))
          (string-match-p "\\.clj\\|\\.edn" (car pair))
          (memq (cdr pair) '(clojure-mode clojurescript-mode clojurec-mode))))
      auto-mode-alist)))

(use-package
  clojure-ts-mode
  :delight
  (clojure-ts-mode "CLJ:TS ")
  (clojure-ts-clojurescript-mode "CLJS:TS ")
  (clojure-ts-clojurec-mode "CLJC:TS ")
  :ensure t
  :mode
  (("\\.clj\\'" . clojure-ts-mode)
    ("\\.bb\\'" . clojure-ts-mode)
    ("\\.cljc\\'" . clojure-ts-clojurec-mode)
    ("\\.cljs\\'" . clojure-ts-clojurescript-mode)
    ("\\.edn\\'" . clojure-ts-mode))
  :custom (clojure-ts-indent-style 'fixed)
  :config
  (define-minor-mode lt/clj-auto-reload-mode
    "Toggle automatic reload of Clojure code after save.
When enabled, runs reload function after saving Clojure files."
    :init-value nil
    :lighter "⟳"
    :global t
    :keymap
    nil
    (if lt/clj-auto-reload-mode
      (add-hook 'after-save-hook #'lt/clj-reload nil t)
      (remove-hook 'after-save-hook #'lt/clj-reload t)))

  (defun lt/clj-reload-loaded ()
    (interactive)
    (cider-interactive-eval "
    (try
      (require ' [clj-reload.core :refer [reload]])
      (reload {:only :loaded}))
      (catch Exception _ (println :clj-reload-is-not-available)))
    "))
  (defun lt/clj-reload ()
    (interactive)
    (cider-interactive-eval "
    (try
      (require ' [clj-reload.core :refer [reload]])
      ((eval 'reload))
      (catch Exception _ (println :clj-reload-is-not-available)))
    "))
  :preface
  (defun lt/clts-lsp-start ()
    (lsp-deferred))
  :hook
  (clojure-ts-mode . cider-mode)
  (clojure-ts-mode . lt/clts-lsp-start)
  :general
  (:states
    '(normal visual)
    :keymaps
    'clojure-ts-mode-map
    "SPC mjtR"
    'lt/clj-auto-reload-mode
    "SPC mjrR"
    'lt/clj-reload-loaded
    "SPC mjrr"
    'lt/clj-reload))

(use-package
  cider
  :delight
  (cider-auto-test-mode " t")
  (cider-enlighten-mode " e")
  :after evil
  :init (evil-collection-init 'cider)
  :config
  (add-to-list
    'display-buffer-alist
    '
    ("\\*cider-error\\*"
      (display-buffer-in-side-window)
      (side . right)
      (slot . 3)
      (window-height . shrink-window-if-larger-than-buffer)
      (dedicated . t)))

  (defun polylith-project-p ()
    (let*
      (
        (root (projectile-project-root))
        (deps (expand-file-name "deps.edn" root))
        (workspace (expand-file-name "workspace.edn" root)))
      (and (projectile-project-p) (file-exists-p deps) (file-exists-p workspace))))

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
          (let
            (
              (choice
                (completing-read
                  (format "Selected: [%s] Toggle (RET to finish): " (string-join selected ", "))
                  choices
                  nil
                  nil
                  nil
                  nil
                  ""))) ;; Allow empty input to finish
            (if (string-empty-p choice)
              (throw 'finish-selection selected)
              (if (member choice selected)
                (setq selected (remove choice selected))
                (push choice selected))))))))

  (defun my-poly-profiles-prompt (root)
    "Prompt for aliases to use when starting CIDER. Only works for Polylith projects.
Returns a list of selected aliases or nil."
    (let*
      (
        (deps (expand-file-name "deps.edn" root))
        (aliases (my-extract-poly-profiles deps)))
      (if (and aliases (> (length aliases) 0))
        (let*
          (
            (default-alias
              (if (member "+default" aliases)
                "+default"
                nil))
            (last-selected-aliases
              (cl-remove-if
                (lambda (x) (not (member x aliases)))
                my-cider-last-selected-aliases))
            (initial-selected (or last-selected-aliases (and default-alias (list "+default"))))
            (chosen (my-toggle-selection aliases initial-selected)))
          (setq my-cider-last-selected-aliases chosen)
          chosen) ;; Return the list of chosen aliases
        (progn
          (message "No :+aliases found in deps.edn.")
          nil))))

  (defun my-cider-jack-in-with-aliases (orig-fun args)
    "Advice around `cider-jack-in` to set aliases and start REPL in main root."
    (if (polylith-project-p)
      (let*
        (
          (poly-root (projectile-project-root))
          (aliases (my-poly-profiles-prompt poly-root))
          (alias-string
            (when aliases
              (string-join aliases ":")))
          (existing-aliases cider-clojure-cli-aliases)
          (cider-clojure-cli-aliases
            (if existing-aliases
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

  :custom (cider-clojure-cli-aliases ":user")
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
  (clojure-ts-mode . cider-mode)
  (cider-mode . clj-refactor-mode)
  (cider-mode . clj-refactor-mode)
  :general
  (:states
    '(normal visual)
    :keymaps
    'cider-repl-mode-map
    "SPC mq"
    'cider-quit
    "SPC mc"
    'cider-repl-clear-buffer)

  (:states
    '(normal visual)
    :keymap
    'cider-mode-map
    "SPC mil"
    'cider-inspect-last-result
    "SPC mie"
    'cider-inspect-last-sexp

    "SPC mme"
    'cider-enlighten-mode

    "SPC msb"
    'sesman-link-with-buffer
    "SPC mss"
    'sesman-link-session

    "SPC mcj"
    'cider-jack-in
    "SPC mcs"
    'cider-jack-in-cljs
    "SPC mcJ"
    'cider-jack-in-clj&cljs
    "SPC mcc"
    'cider-connect-clj
    "SPC mcs"
    'cider-connect-cljs
    "SPC mcC"
    'cider-connect-clj&cljs
    "SPC mcS"
    'cider-connect-sibling-cljs

    "SPC mde"
    'cider-debug-defun-at-point

    "SPC mrb"
    'cider-switch-to-repl-buffer
    "SPC mrB"
    'cider-switch-to-repl-on-insert
    ;; "SPC mhd" 'cider-doc

    "SPC mec"
    'cider-pprint-eval-last-sexp-to-comment
    "SPC mee"
    'cider-eval-last-sexp
    "SPC med"
    'cider-eval-defun-at-point
    "SPC meb"
    'cider-eval-buffer

    "SPC mtP"
    'cider-test-run-project-tests
    "SPC mtt"
    'cider-test-run-test
    "SPC mtf"
    'cider-test-rerun-failed-tests
    "SPC mtr"
    'cider-test-rerun-test
    "SPC mtn"
    'cider-test-run-ns-tests

    "SPC mPt"
    'cider-profile-toggle
    "SPC mPT"
    'cider-profile-ns-toggle
    "SPC mPv"
    'cider-profile-var-summary
    "SPC mPC"
    'cider-profile-clear

    "SPC mmm"
    'cider-macroexpand-1
    "SPC mmM"
    'cider-macroexpand-all

    "SPC mit"
    'transpose-sexps
    "SPC mis"
    'cider-format-edn-last-sexp
    "SPC mir"
    'cider-format-region
    "SPC miB"
    'cider-format-buffer
    "SPC mif"
    'cider-format-defun
    "SPC miu"
    '+/insert-random-uid))

(use-package
  clj-refactor
  :delight
  :custom
  (cljr-magic-requires nil)
  (cljr-insert-newline-after-require nil)
  :general
  (:states
    '(normal visual)
    :keymaps
    'clj-refactor-map
    "SPC mjml"
    'cljr-move-to-let
    "SPC mjxl"
    'cljr-expand-let
    "SPC mjuw"
    'cljr-unwind
    "SPC mjuW"
    'cljr-unwind-all
    "SPC mjtf"
    'cljr-thread-first-all
    "SPC mjtl"
    'cljr-thread-last-all
    "SPC mjtt"
    'transpose-sexps
    "SPC mjaM"
    'lsp-clojure-add-missing-libspec
    "SPC mjam"
    'cljr-add-missing-libspec
    "SPC mjnc"
    'cljr-clean-ns))

(use-package clojure-snippets :defer t)

(use-package anakondo)

(use-package clj-decompiler)

(use-package zprint-mode)

(use-package lsp-java :after (lsp-mode))

(provide 'flawless-clojure)
;;; flawless-clojure.el ends here
