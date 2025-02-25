;;; editing.el --- My flawless emacs config for editor behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package
    shell-command-x
  :config (shell-command-x-mode 1)
  :custom (async-shell-command-display-buffer nil))

(use-package info
  :init (evil-collection-init 'info))

(use-package path-helper :if (memq window-system '(mac ns)) :config (path-helper-setenv-all))

(use-package
    files
  :straight (:type built-in)
  :hook (before-save . whitespace-cleanup)
  :custom
  (require-final-newline t)
  (backup-by-copying t)
  (delete-old-versions t)
  (version-control t)
  (kept-new-versions 50)
  (kept-old-versions 20)
  (create-lockfiles nil))

(use-package dired
  :straight (:type built-in)
  :init (evil-collection-init 'dired) :hook (dired-mode . dired-omit-mode))

(use-package
    exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init (exec-path-from-shell-initialize))

(use-package direnv :config (direnv-mode))

(use-package
    vterm
  :init (evil-collection-init 'vterm)
  :config (use-package multi-vterm :ensure t)
  :general
  (:states '(normal visual) :prefix "SPC" "V" 'multi-vterm)
  (:states
      '(normal visual)
    :keymaps
    'vterm-mode-map
    "C-c C-c"
    'vterm-send-C-c
    "C-p"
    'vterm-yank-pop
    "p"
    'vterm-yank)
  (:states '(normal visual) :keymaps 'vterm-mode-map :prefix "SPC" "k" 'vterm-send-next-key))

(use-package vlf :init (require 'vlf-setup))

(use-package
    winner
  :config (winner-mode 1)
  :general (:states '(normal visual) :keymaps 'override "SPC wz" 'winner-undo "SPC wx" 'winner-redo))

(use-package pass)

(use-package
    tramp
  :defer t
  :config
  ;; Workaround a tramp-MacOS bug that dramatically slows completion
  (put 'temporary-file-directory 'standard-value (list temporary-file-directory))
  :custom
  (remote-file-name-inhibit-cache nil)
  (tramp-chunksize 2000)
  (tramp-use-ssh-controlmaster-options nil)
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil))

(use-package vagrant-tramp)
(use-package goto-last-change)

(setq evil-want-keybinding nil)

(use-package evil-multiedit :config (evil-multiedit-default-keybinds))

(use-package
    evil-surround
  :hook ((text-mode prog-mode) . evil-surround-mode)
  :config (add-to-list 'evil-surround-pairs-alist '(?c . ("<!-- " . " -->")))

  (defun evil-surround-get-active-pair (beg end)
    "Get the active surround pair between BEG and END."
    (let ((text (buffer-substring-no-properties beg end)))
      (cl-loop
       for
       (key . (open . close))
       in
       evil-surround-pairs-alist
       when
       (and (string-prefix-p open text) (string-suffix-p close text))
       return
       (cons open close))))

  (defun toggle-comment-on-html-tag ()
    "Toggle HTML comment on the current HTML tag."
    (interactive)
    (require 'web-mode)
    (save-excursion
      (when (derived-mode-p 'web-mode)
        (web-mode-element-select)
        (let
            (
             (beg (region-beginning))
             (end (region-end))
             (surround-pair (evil-surround-get-active-pair beg end)))
          (if
              (and surround-pair
                   (string= (car surround-pair) "<!-- ")
                   (string= (cdr surround-pair) " -->"))
              ;; Remove the surround
              (evil-surround-delete beg end 'inclusive ?c)
            ;; Add the surround
            (evil-surround-region beg end 'inclusive ?c))))))

  (evil-define-key 'normal web-mode-map (kbd "g c") 'toggle-comment-on-html-tag))

(use-package evil-commentary :delight)

(use-package evil-org :delight :config (require 'evil-org-agenda) (evil-org-agenda-set-keys))

(use-package counsel-projectile :init (counsel-projectile-mode t) :after (counsel projectile))

(use-package
    default-text-scale
  :custom (default-text-scale-amount 20)
  :general
  (:states
      '(normal visual)
    :prefix "SPC"
    :infix
    "t"
    "+"
    'default-text-scale-increase
    "-"
    'default-text-scale-decrease
    "R"
    'default-text-scale-reset))

(use-package memory-usage)
(use-package ace-window)
(use-package
    ivy
  :custom (ivy-initial-inputs-alist nil)
  :config
  (defun lt:swiper-org-section ()
    "Pre-fill swiper input with region."
    (interactive)
    (swiper "^\\* "))
  :general (:states '(normal visual) "SPC sS" 'swiper-all "SPC ss" 'swiper "SPC so" 'lt:swiper-org-section))

;; (use-package
;;     yasnippet
;;   :delight yas-minor-mode
;;   :general (:states '(normal visual) "SPC myi" 'yas-insert-snippet)
;;   :custom (yas-verbosity 1) (yas-wrap-around-region t) (yas-snippet-dirs '("~/.emacs.d/snippets/"))
;;   :config
;;   (with-eval-after-load 'yasnippet
;;     (setq yas-snippet-dirs '(yasnippet-snippets-dir)))

;;   (yas-reload-all) (yas-global-mode))

(use-package
    centered-cursor-mode
  :delight
  :hook prog-mode
  :custom (ccm-vps-init (round (* 21 (window-text-height)) 34)))

(use-package yasnippet-snippets)

(use-package
    counsel
  :custom (ivy-on-del-error-function #'ignore)
  :general (:states '(normal visual) :keymaps 'override "SPC SPC" 'counsel-M-x))


(use-package smex)

(use-package
    which-key
  :delight
  :commands (which-key-mode)
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.2 "Reduce the time before which-key pops up")
  (which-key-allow-evil-operators t "Show evil keybindings")
  (which-key-sort-order 'which-key-key-order-alpha "Sort things properly alphabetical"))

(use-package projectile
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
  (:states '(normal visual)
    :keymaps 'override
    "SPC xp" 'projectile-run-async-shell-command-in-root
    "SPC pc" 'projectile-kill-buffers
    "SPC pC" 'projectile-invalidate-cache
    "SPC pt" 'projectile-toggle-between-implementation-and-test
    "SPC ps" 'projectile-save-project-buffers
    "SPC pg" 'counsel-git-grep
    "SPC pr" 'counsel-rg
    "SPC pp" 'counsel-projectile-switch-project
    "SPC pb" 'counsel-projectile-switch-to-buffer
    "SPC pf" 'counsel-projectile-find-file))

(use-package company
  :delight
  ;; :pin melpa-stable
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

(use-package lsp-ui
  :after (lsp-mode)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil))

(use-package dap-mode
  :defer)

(use-package delight)

(use-package almost-mono-themes
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

(use-package idle-highlight-mode
  :hook prog-mode)

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

(provide 'editing)
;;; editing.el ends here
