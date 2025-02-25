;;; core.el --- My flawless emacs config core -*- lexical-binding: t -*-
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(use-package system-packages :custom (system-packages-noconfirm t))

(use-package
    general
  :demand t
  :commands (general-define-key general-override-mode general-evil-setup general--simulate-keys)
  :custom
  (use-package-enable-imenu-support t)
  (general-override-states '(insert emacs hybrid normal visual motion operator replace))
  :config
  (general-override-mode)
  (general-evil-setup))

(setq evil-want-keybinding nil)
(use-package
    evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-search-module 'evil-search)
  :config
  (evil-mode t)
  ;; :general (:keymaps '(evil-motion-state-map) "SPC" nil "RET" nil "TAB" nil)
  )

(use-package
    evil-collection
  :after evil
  :delight evil-collection-unimpaired-mode
  :custom (evil-collection-want-find-usages-bindings t))

(use-package
    emacs
  :ensure nil
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
  (fill-column 96)

  :init
  (set-face-attribute 'mode-line nil :height 96)
  (set-face-attribute 'mode-line-inactive nil :height 96)

  :config
  (auth-source-pass-enable)
  (defun lt:file-notify-rm-all-watches ()
    "Remove all existing file notification watches from Emacs."
    (interactive)
    (maphash (lambda (key _value) (file-notify-rm-watch key)) file-notify-descriptors))
  ;; hotfix error with image, remove after 29.1 release
  ;; overriding image.el function image-type-available-p
  (add-to-list 'image-types 'svg)
  ;; end of hotfix

  (defcustom telega-database-dir-base (expand-file-name "~/.telega")
    "telega base dir")

  ;; make fullscreen
  (modify-frame-parameters
   nil
   `((fullscreen . fullboth) (fullscreen-restore . ,(frame-parameter nil 'fullscreen))))

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
    (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face)
    (setq rainbow-identifiers-cie-l*a*b*-lightness 00)
    (setq rainbow-identifiers-cie-l*a*b*-saturation 00))

  (defun lt/backup-file-name (fpath)
    "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
    (let*
        (
         (backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath)) ; remove Windows driver letter in path, ➢ for example: “C:”
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~"))))
      (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
      backupFilePath))

  (define-minor-mode ansi-color-mode
    "..."
    :init-value nil
    :lighter nil
    :keymap
    nil
    (ansi-color-apply-on-region 1 (buffer-size)))

  (defun my-mode-line-visual-bell ()
    (setq visible-bell nil)
    (setq ring-bell-function 'my-mode-line-visual-bell--flash))

  (defun my-mode-line-visual-bell--flash ()
    (let ((frame (selected-frame)))
      (run-with-timer 0.1 nil
                      #'
                      (lambda (frame)
                        (let
                            (
                             (inhibit-quit)
                             (inhibit-redisplay t))
                          (invert-face 'header-line frame)
                          (invert-face 'header-line-highlight frame)
                          (invert-face 'mode-line frame)
                          (invert-face 'mode-line-inactive frame)))
                      frame)
      (let
          (
           (inhibit-quit)
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
    (custom-set-faces '(default ((t (:inherit nil :font "Jetbrains Mono" :size 16))))))
   ((find-font (font-spec :name "Monaspace Neon"))
    (custom-set-faces '(default ((t (:inherit nil :font "Monaspace Neon" :size 16))))))
   ((find-font (font-spec :name "Input Mono"))
    (custom-set-faces '(default ((t (:inherit nil :font "Input Mono" :size 16)))))))


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
  (:states
      '(normal visual)
    :keymaps
    'override
    "SPC xs"
    'async-shell-command
    "SPC u"
    'universal-argument
    "SPC fC"
    'find-config-file
    "SPC ff"
    'find-file
    "SPC fs"
    'save-buffer
    "SPC fS"
    'write-file
    "SPC fr"
    'recover-this-file
    "SPC fR"
    'recover-file

    "SPC bC"
    'clean-buffer-list
    "SPC bb"
    'counsel-switch-buffer
    "SPC bi"
    'ibuffer
    "SPC br"
    'revert-buffer
    "SPC bd"
    'kill-current-buffer
    "SPC bs"
    'save-buffer
    "SPC bS"
    'save-some-buffers

    "SPC hk"
    'describe-key
    "SPC hf"
    'describe-function
    "SPC hF"
    'describe-face
    "SPC hm"
    'describe-mode
    "SPC ho"
    'describe-symbol
    "SPC hv"
    'describe-variable

    "SPC wv"
    'split-window-right
    "SPC ws"
    'split-window-vertically
    "SPC wd"
    'delete-window
    "SPC ww"
    'ace-window
    "SPC wm"
    'maximize-window

    "SPC w="
    'balance-windows

    "SPC tF"
    'toggle-frame-fullscreen
    "SPC tt"
    'toggle-truncate-lines

    "SPC qq"
    'save-buffers-kill-emacs))

(provide 'core)
;;; core.el ends here
