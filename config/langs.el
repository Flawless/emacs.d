;;; langs.el --- My flawless emacs config for various langs -*- lexical-binding: t -*-
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(use-package
  display-line-numbers-mode
  :straight (:type built-in)
  :hook (prog-mode org-mode beancount-mode yaml-mode text-mode))

(use-package
  display-fill-column-indicator-mode
  :straight (:type built-in)
  :hook (text-mode prog-mode))

(use-package undo-fu :after evil :delight :custom (evil-undo-system 'undo-fu))

(use-package
  aider
  :straight (:host github :repo "tninja/aider.el")
  :custom (aider-args '("--model" "o4-mini"))
  :config (setenv "OPENAI_API_KEY" openai-token)
  :general (:states '(normal visual) "SPC z" 'aider-transient-menu))

(use-package
  aidermacs
  :config (setenv "OPENAI_API_KEY" openai-token)
  :custom (aidermacs-default-model "gpt-4o-mini")
  :general (:states '(normal visual) "SPC a" 'aider-transient-menu))

(use-package
  langtool
  :custom (langtool-language-tool-jar "~/LanguageTool-6.3/languagetool-commandline.jar"))

(use-package
  flycheck-languagetool
  :hook (text-mode . flycheck-languagetool-setup)
  :custom (flycheck-languagetool-server-jar "~/LanguageTool-6.3/languagetool-commandline.jar"))

(use-package
  compilation-mode
  :straight (:type built-in)
  :init (evil-collection-init 'compile)
  :hook ((compilation-filter . ansi-color-compilation-filter)))

(use-package
  tree-sitter
  :straight (tree-sitter :type git :host github :repo "ubolonton/emacs-tree-sitter" :files ("lisp/*.el")))

(use-package
  tree-sitter-langs
  :straight
  (tree-sitter-langs
    :type git
    :host github
    :repo "ubolonton/emacs-tree-sitter"
    :files ("langs/*.el" "langs/queries"))
  :after tree-sitter)

(use-package smerge-mode :init (evil-collection-init 'smerge-mode))

(use-package git-timemachine)

;;(use-package why-this
;;  :straight (why-this :type git :host codeberg
;;            :repo "akib/emacs-why-this.git"))

(use-package
  magit
  :after evil
  :init (evil-collection-init 'magit)
  :custom
  (cond
    ((eq system-type 'darwin)
      (magit-git-executable "/usr/local/bin/git")))
  (magit-diff-paint-whitespace-lines 'all)
  (magit-display-buffer-function
    (lambda (buffer) (display-buffer buffer '(display-buffer-same-window))))
  :config (evil-set-initial-state 'magit-mode 'normal)
  :general (:states '(normal visual) :keymaps '(magit-mode-map) "SPC" nil)
  (:states
    '(normal visual)
    :prefix "SPC"
    :infix
    "g"
    "b"
    'magit-checkout
    "B"
    'magit-blame
    "g"
    'magit-status
    "f"
    'magit-find-file
    "l"
    'magit-log-buffer-file))

(use-package
  magit-gptcommit
  :demand t
  :after magit
  :bind (:map git-commit-mode-map ("C-c C-g" . magit-gptcommit-commit-accept))
  :custom
  (magit-gptcommit-llm-provider
    (make-llm-openai :key (getenv "OPENAI_TOKEN"))
    :chat-model "gpt-4.1-mini")
  (magit-gptcommit-prompt
    "You are an expert programmer writing a commit message.
You went over every file diff that was changed in it.

First Determine the best label for the diffs.
Here are the labels you can choose from:
- build: Changes that affect the build system or external dependencies (example scopes: gulp, broccoli, npm)
- chore: Updating libraries, copyrights or other repo setting, includes updating dependencies.
- ci: Changes to our CI configuration files and scripts (example scopes: Travis, Circle, GitHub Actions)
- docs: Non-code changes, such as fixing typos or adding new documentation
- feat: a commit of the type feat introduces a new feature to the codebase
- fix: A commit of the type fix patches a bug in your codebase
- perf: A code change that improves performance
- refactor: A code change that neither fixes a bug nor adds a feature
- style: Changes that do not affect the meaning of the code (white-space, formatting, missing semi-colons, etc)
- test: Adding missing tests or correcting existing tests

Then summarize the commit into a single specific and cohesive theme.
Remember to write header in only one line, no more than 80 characters.
Any additional details may go after an empty line if needed (omit if not)
Write your response using the imperative tense following the kernel git commit style guide.
Write a high level title but keep it specific, avoid abstract phrases like \"code improved\",
\"things restructured\" and so on, as an expert programmer you know how annoyed they could be,
in other words don't write anything if it has no meaning.
 If you feel you can't keep it in one line, add commit details as described below.

THE FILE DIFFS:
```
%s
```
Now write Commit message in follow template, avoid any special character or special syntax like
quotes and so on since your reply would be used as is:
[label]:[one line of summary]

[extra lines with details if needed no longer then 100 chars each line]:
")

  :config
  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  ;; (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))

(use-package
  prog-mode
  :straight (:type built-in)
  :custom
  (display-line-numbers-type 'relative)
  (evil-commentary-mode t)
  :general (:states '(normal visual) "g==" 'align "g=r" 'align-regexp "SPC mCa" 'mc/mark-all-dwim))

(use-package
  babashka
  :general (:states '(normal visual) "SPC xb" 'babashka-tasks)
  :config
  ;; override internal fn to support short task syntax
  (defun babashka--tasks-to-annotated-names (tasks)
    "Convert TASKS to annotated alist."
    (let (results)
      (maphash
        (lambda (key value)
          (let ((task-name (symbol-name key)))
            (unless (string-prefix-p ":" task-name)
              (push
                (if (hash-table-p value)
                  (cons task-name (gethash :doc value))
                  task-name)
                results))))
        tasks)
      results)))

(use-package
  lisp-mode
  :defer 1
  :straight (:type built-in)
  :general
  (:states
    '(normal visual)
    :keymaps
    'emacs-lisp-mode-map
    "SPC mee"
    'eval-last-sexp
    "SPC meb"
    'eval-buffer
    "SPC med"
    'eval-defun
    "SPC mel"
    'load-library
    "SPC mer"
    'eval-region))

(use-package
  lispy
  :delight
  :general (:states '(normal visual) :keymaps 'lispy-mode-map "SPC mjs" 'lispy-split))

(use-package evil-lispy :hook ((elisp-mode clojure-ts-mode) . evil-lispy-mode) :delight)

(use-package lispyville :hook ((elisp-mode clojure-ts-mode) . lispyville-mode) :delight)

(use-package flawless-clojure :straight (:type built-in) :load-path "config/langs")

(use-package
  java-mode
  :straight (:type built-in)
  :defer t
  :hook ((java-mode . lsp) (java-mode . tree-sitter-hl-mode))
  :general (:states '(normal visual) :keymaps 'java-mode-map :prefix "SPC" "mjam" 'lsp-java-add-import))

(use-package
  yaml-mode
  :mode "\\.ya?ml\\'"
  :hook ((yaml-mode . lsp) (yaml-mode . font-lock-mode) (yaml-mode . tree-sitter-hl-mode)))

;; (use-package
;;   beancount
;;   :quelpa (beancount :fetcher github :repo "beancount/beancount-mode" :files ("beancount.el" "COPYING"))
;;   :mode ("\\.bean\\'" . beancount-mode)
;;   :general
;;   (:states
;;       '(normal visual)
;;     :keymaps
;;     'beancount-mode-map
;;     "gs"
;;     'counsel-org-goto
;;     "SPC mq"
;;     'beancount-query
;;     "SPC mc"
;;     'beancount-check))

(use-package
  org
  :straight (:type built-in)
  :ensure org-contrib
  :general
  (:states
    '(normal visual)
    :keymap 'outline-mode-map
    ;; "gh" 'org-next-visible-heading
    ;; "gl" 'org-previous-visible-heading
    "gj" 'outline-forward-same-level "gk" 'outline-backward-same-level)
  (:states
    '(normal visual)
    :prefix "SPC"
    :keymaps
    'org-mode-map
    "mx"
    'org-export-dispatch
    "mst"
    'org-set-tags-command
    "msp"
    'org-set-property
    "msa"
    'org-attach)
  (:states
    '(normal visual)
    :prefix "SPC"
    :infix
    "o"
    "a"
    'org-agenda
    "c"
    'org-capture
    "g"
    'org-caputre-goto-last-stored)
  (:states
    '(normal visual)
    :prefix "SPC m"
    :keymaps 'org-mode-map
    :infix
    "d"
    "d"
    'org-deadline
    "s"
    'org-schedule)
  (:states '(normal visual) :prefix "SPC m" :keymaps 'org-mode-map "t" 'org-todo)
  (:states '(normal visual) :prefix "SPC m" :keymaps 'org-mode-map "C" 'org-columns)
  (:states
    '(normal visual)
    :prefix "SPC m"
    :keymaps 'org-mode-map
    :infix
    "c"
    "r"
    'org-evaluate-time-range
    "o"
    'org-open-at-point
    "i"
    'org-clock-in
    "p"
    'org-pomodoro
    "e"
    'org-set-effort)
  (:states
    '(normal visual)
    :prefix "SPC"
    :infix
    "n"
    "n"
    'counsel-projectile-switch-to-org
    "p"
    'org-pomodoro
    "i"
    'org-clock-in-last
    "o"
    'org-clock-goto
    "r"
    'org-resolve-clocks
    "C"
    'org-clock-cancel
    "c"
    'org-clock-out)
  (:states
    '(normal visual)
    :keymaps 'org-mode-map
    :prefix "SPC m"
    :infix
    "l"
    "l"
    'org-insert-link
    "C"
    ''counsel-org-link)
  (:states '(normal visual) :keymaps 'org-mode-map "gs" 'counsel-org-goto)
  :custom
  (org-attach-store-link-p 'attached)
  (org-log-reschedule 'time)

  (org-log-redeadline 'time)

  (org-startup-folded "OVERVIEW")
  (org-directory "~/org/")
  (org-agenda-files (directory-files-recursively org-directory "\\.org$"))
  (org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))
  (org-todo-keywords
    '
    (
      (sequence
        "TODO(t!)"
        "WAIT(w@/!)"
        "RVIW(r)"
        "STRT(s!)"
        "CTRL(c!)"
        "HOLD(h!)"
        "TEST(q!)"
        "|"
        "DONE(d!)"
        "KILL(k@)")))
  (org-tag-alist '(("important" . ?i) ("urgent" . ?u) ("buy" . ?b)))
  ;; (org-agenda-category-icon-alist
  ;;  '(("WORK" "~/.emacs.d/icons/person-digging-solid.svg" nil nil :ascent center :mask heuristic)))
  (org-agenda-custom-commands
    '
    (
      ("u" "Undated tasks"
        ((todo "TODO")
          (org-agenda-skip-function
            '(org-agenda-skip-entry-if 'deadline 'scheduled 'timestamp))))
      ("s" "Sprint" ((todo "TODO") (org-agenda-span (lt/days-to-next-sunday))))
      ("b" "Backlog" ((todo "TODO") (tags-todo "-expense")))
      ("d" "Daily" ((org-agenda-ndays 60)))
      ("e" "Planned Expenses" tags-todo "+expense")
      ("i" "Inbox" (search ((org-agenda-files '("~/inbox.org")))))
      ("d" "Upcoming deadlines" agenda ""
        ((org-agenda-entry-types '(:deadline))
          ;; a slower way to do the same thing
          ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
          (org-agenda-span 1) (org-deadline-warning-days 60) (org-agenda-time-grid nil)))))

  (org-capture-templates
    `
    (("a" "ARVL")

      ("am"
        "Meeting"
        entry
        (id "7DE08A4E-86F5-4D6A-A378-5BF7237BD8C4")
        "*** %^{TITLE}\n**** Participants\n**** Agenda %i%?\n**** Resolution")

      ("aj"
        "YT task"
        plain
        (file ,(lt:capture-issue "~/org/arvl/tyrell/tasks" :arvl-short))
        "#+TITLE: %^{TITLE}\n#+CATEGORY: ARVL\n* TODO RIGEL-%^{ID1} %^1\n** TODO RIGEL-%^{ID2} %^{TYPE|DEVELOP} %^{DESCR2}"
        :jump-to-captured t)

      ("ab" "Bug" entry (file "arvl/bugs.org") "* %^{TITLE}\n %x" :jump-to-captured t)

      ("ar" "Review" entry (id "606B1037-48A0-41F0-9348-249FAA0FEF59")
        "*** TODO RIGEL-%^{ID} D%^{DIFF} %^{ANNOTATION}
:PROPERTIES:
:YT:         RIGEL-%^1
:PHBR:       D%^2
:AUTHOR:     %^{AUTHOR}
:END:"
        :clock-in t)

      ("b"
        "Book"
        entry
        (file "evolution/books.org")
        "* %^{TITLE}\n:PROPERTIES:\n:ADDED: %<[%Y-%02m-%02d]>\n:END:%^{AUTHOR}p\n%?"
        :empty-lines 1)

      ("q" "Quick Inbox")
      ("qe"
        "Expence"
        entry
        (file+headline "~/org/inbox.org" "Expenses")
        "* TODO %? :expense:\n")
      ("qt" "Todo")
      ("qtl"
        "Todo with link"
        entry
        (file+headline "~/org/inbox.org" "Tasks")
        "* TODO %?\n %i\n %a")
      ("qtt" "Todo" entry (file+headline "~/org/inbox.org" "Tasks") "* TODO %?\n")

      ("n" "Notes")

      ("nc" "Current task note" item (clock))
      ("nl" "Current task note + link to line" item (clock) "%a")
      ("no"
        "Current task link to comment"
        item
        (clock)
        "%(lt:capture-comment-line \"%i\")\n  %a")))
  (org-clock-persist 'history)
  (org-clock-idle-time 15)
  (org-columns-default-format
    "%80ITEM(Task) %TODO %Effort(Estimated Effort){:} %CLOCKSUM(Clocked){:}")

  :after (evil-org org-pomodoro)
  :hook
  ((org-mode . auto-fill-mode)
    (org-mode . evil-org-mode)
    ((org-clock-in org-clock-out org-clock-cancel) . save-buffer))

  :config
  (defun extract-ticket-number (s)
    "Extract number from string like 'PROJECT-123' or return nil if not found."
    (when (string-match "\\([A-Za-z]+\\)-\\([0-9]+\\)" s)
      (cons
        (match-string 1 s) ; project prefix
        (string-to-number (match-string 2 s))))) ; ticket number

  (defun compare-ticket-numbers (a b)
    "Compare two ticket numbers, first by project, then by number."
    (let
      (
        (ticket-a (extract-ticket-number a))
        (ticket-b (extract-ticket-number b)))
      (cond
        ;; If both are ticket numbers
        ((and ticket-a ticket-b)
          (if (string= (car ticket-a) (car ticket-b))
            (< (cdr ticket-a) (cdr ticket-b)) ; same project, compare numbers
            (string< (car ticket-a) (car ticket-b)))) ; different projects
        ;; If only one is a ticket number, put it first
        (ticket-a
          t)
        (ticket-b
          nil)
        ;; If neither is a ticket number, sort alphabetically
        (t
          (string< a b)))))

  (defun org-sort-by-ticket-number ()
    "Sort entries by ticket number (e.g., WISOFT-249, PROJ-123)."
    (interactive)
    (org-sort-entries
      nil ?f
      (lambda ()
        (let ((title (or (match-string 4) ""))) ; Ensure string
          (when (string-match "\\([A-Za-z]+\\)-\\([0-9]+\\)" title)
            (string-to-number (match-string 2 title)))))))
  ; Return heading for comparison

  (defun lt/days-to-next-sunday ()
    (let
      (
        (dayspan 0)
        (today (string-to-number (format-time-string "%u"))))
      (cond
        ((> today 0) ; from today till sunday, today included
          (setq dayspan (- 8 today)))
        ((= today 0) ; sunday to sunday
          (setq dayspan 8)))))
  ;; ox-extra
  ;; (require 'ox-extra)
  ;; (ox-extras-activate '(latex-header-blocks ignore-headlines))
  ;; ox-latex
  (setq org-latex-pdf-process
    '
    ("pdflatex -interaction nonstopmode -output-directory %o %f"
      "bibtex %b"
      "pdflatex -interaction nonstopmode -output-directory %o %f"
      "pdflatex -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author..} to latex export
  ;; (setq org-latex-prefer-user-labels t)

  ;; deleted unwanted file extensions after latexMK
  (setq org-latex-logfiles-extensions
    '
    ("lof"
      "lot"
      "tex~"
      "aux"
      "idx"
      "log"
      "out"
      "toc"
      "nav"
      "snm"
      "vrb"
      "dvi"
      "fdb_latexmk"
      "blg"
      "brf"
      "fls"
      "entoc"
      "ps"
      "spl"
      "bbl"
      "xmpi"
      "run.xml"
      "bcf"
      "acn"
      "acr"
      "alg"
      "glg"
      "gls"
      "ist"))

  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))
  ;; other
  (defun lt:org-clock-todays-total ()
    "Visit each file in `org-agenda-files' and return the total time of today's
clocked tasks in minutes."
    (interactive)
    (let
      (
        (files (org-agenda-files))
        (total 0))
      (org-agenda-prepare-buffers files)
      (dolist (file files)
        (with-current-buffer (find-buffer-visiting file)
          (setq total (+ total (org-clock-sum-today)))))
      (format " Today's total: %s " (org-minutes-to-clocksum-string total))))
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (org-clock-persistence-insinuate)
  (defun lt:capture-comment-line (&optional line)
    (let
      (
        (c
          (save-excursion
            (save-window-excursion
              (switch-to-buffer (plist-get org-capture-plist :original-buffer))
              comment-start))))
      (while (string-prefix-p c line)
        (setq line (string-remove-prefix c line)))
      (comment-string-strip line t t)))
  (defun lt:capture-issue (path issue-type)
    #'
    (lambda ()
      (cl-case
        issue-type
        (:arvl-short
          (let ((issue-id (read-string "ID: " "RIGEL-")))
            (expand-file-name (format "%s.org" issue-id) path)))
        (:arvl
          (let
            (
              (issue-id (read-string "ID: " "RIGEL-"))
              (name (read-string "Name (camelCase prefix): ")))
            (expand-file-name (format "%s_%s.org" issue-id name) path))))))
  (defun lt:yank-org-link (text)
    (if (derived-mode-p 'org-mode)
      (insert text)
      (string-match org-link-bracket-re text)
      (insert (substring text (match-beginning 1) (match-end 1)))))

  (defun lt:org-retrieve-url-from-point ()
    (let*
      (
        (link-info (assoc :link (org-context)))
        (text
          (when link-info
            ;; org-context seems to return nil if the current element
            ;; starts at buffer-start or ends at buffer-end
            (buffer-substring-no-properties
              (or (cadr link-info) (point-min))
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
      (-flatten-n
        1
        (--map
          (let ((it-children (lt:sum-direct-children-org (1+ level) (cdr it))))
            (cons
              (--update-at
                4
                (+ it (-sum (--map (nth 4 it) (--filter (= (1+ level) (car it)) it-children))))
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
    (let*
      (
        (tt (-flatten-n 1 (-map #'-last-item tables)))
        (formatter (or org-clock-clocktable-formatter 'org-clocktable-write-default))
        (newprops (remove "CATEGORY" (plist-get params :properties)))
        (newparams (plist-put (plist-put params :multifile t) :properties newprops))
        newtables)

      ;; Compute net clocked time for each item
      (setq tt
        (--map-indexed
          (let*
            (
              (it-level (car it))
              (it-time (nth 4 it))
              (it-subtree (--take-while (< it-level (car it)) (-drop (1+ it-index) tt)))
              (it-children (--filter (= (1+ it-level) (car it)) it-subtree)))
            (-replace-at 4 (- it-time (-sum (--map (nth 4 it) it-children))) it))
          tt))

      ;; Add index (ie id) and indexes of parents (these are needed in the
      ;; sorting step). This can probably be written more functionally using --reduce?
      ;; At least without having to modify hist.
      (setq tt
        (let (hist)
          (--map-indexed
            (let*
              (
                (it-level (car it))
                (it-hist (-drop (- (length hist) it-level -1) hist)))
              (setq hist (cons it-index it-hist))
              (cons it-index (cons it-hist it)))
            tt)))

      ;; Now comes the important phase: sorting, where we copy items with >0 net time
      ;; into newtables based on their category, and we copy their parents when
      ;; appropriate.
      (--each
        tt
        (let*
          (
            (it-hist (nth 1 it))
            (it-time (nth 6 it))
            (it-prop (-last-item it))
            (it-cat (alist-get "CATEGORY" it-prop nil nil #'string=))
            ;; Find the index of the table for category: it-cat or if
            ;; it doesn't yet exist add it to the start of newtables.
            (cat-pos
              (or (--find-index (string= (car it) it-cat) newtables)
                (progn
                  (push (list it-cat nil) newtables)
                  0)))
            (cat-members (-map #'car (-last-item (nth cat-pos newtables))))
            (it-parent (or (--find-index (member it cat-members) it-hist) (length it-hist)))
            (hist-to-add
              ;; replace the time of copied parents with 0 since if a
              ;; parents is being copied and has time >0 then it has
              ;; already been placed in the table for a different
              ;; category. ie. We don't want time double counted.
              (--map (-replace-at 6 0 (nth it tt)) (-take it-parent it-hist))))

          (when (not (= 0 it-time))
            (setf (-last-item (nth cat-pos newtables))
              (append (cons it hist-to-add) (-last-item (nth cat-pos newtables)))))))

      (--each newtables (setf (-last-item it) (reverse (-last-item it))))
      ;; Cleanup, remove ids and list of parents, as they are no longer needed.
      (setq newtables (--map (list (car it) 0 (--map (-drop 2 it) (-last-item it))) newtables))

      ;; Recompute the total times for each node.
      ;; (replace this with --each and setf?)
      (setq newtables
        (--map
          (let*
            (
              (it-children (lt:sum-direct-children-org 1 (-last-item it)))
              (it-total-time (-sum (--map (nth 4 it) (--filter (= 1 (car it)) it-children)))))
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

(use-package
  outshine

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
  (:states
    '(normal visual :keymap 'outshine-mode-map)
    "SPC moj"
    'lt:outshine-next-slide
    "SPC mok"
    'lt:outshine-prev-slide
    "SPC mon"
    'outshine-narrow-to-subtree
    "SPC mow"
    'widen))


(use-package org-clock-today :after org)

(use-package
  org-agenda
  :straight (:type built-in)
  :after org
  :custom (org-agenda-window-setup 'current-window)
  :general (:state 'motion :keymaps 'org-agenda-mode-map "SPC" nil))

(use-package
  org-fancy-priorities
  :after org
  :delight

  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("⬆" " " "⬇")))

(use-package ox-clip)

(use-package
  org-duration
  :straight (:type built-in)
  :config
  (setq org-duration-units
    `
    (("min" . 1)
      ("h" . 60)
      ("d" . ,(* 60 8))
      ("w" . ,(* 60 8 5))
      ("m" . ,(* 60 8 5 4))
      ("y" . ,(* 60 8 5 4 11))))
  (org-duration-set-regexps))

(use-package
  org-pomodoro

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
(use-package css-mode :custom (css-indent-offset 4))

;; TS
(use-package tide)

;;; TeX
(use-package tex :straight (:type built-in) :defer t :ensure auctex)

;;; protobuf
(use-package
  protobuf-mode

  :mode (("\\.proto$" . protobuf-mode)))

;; Network
;;; Mail
(use-package
  notmuch

  :after evil
  :init (evil-collection-init 'notmuch))

;;; Telega
(use-package
  telega

  :straight (telega :type git :repo "zevlg/telega.el" :branch "master")
  ;; :quelpa
  ;; (telega
  ;;  :fetcher github
  ;;  :repo "zevlg/telega.el"
  ;;  :branch "master"
  ;;  :files (:defaults "contrib" "etc" "server" "Makefile"))
  :after evil
  :init (telega-mode-line-mode t) (setq telega-use-images t) (evil-collection-init 'telega)
  :custom (telega-chat-fill-column 80)
  (telega-accounts
    (list
      (list "AlexanderUshanov" 'telega-database-dir telega-database-dir-base)
      (list
        "flaw1322"
        'telega-database-dir
        (expand-file-name "flaw1322" telega-database-dir-base))
      (list
        "C11H26NO2PS"
        'telega-database-dir
        (expand-file-name "c11h26no2ps" telega-database-dir-base))))
  :config
  (when (eq system-type 'gnu/linux)
    (setq telega-server-libs-prefix "/usr"))
  :general
  (:states
    '(normal visual)
    :prefix "SPC"
    :infix
    "c"
    "w"
    'telega-chat-with
    "g"
    'telega
    "A"
    'telega-account-switch))

(use-package
  elisp-mode
  :straight (:type built-in)
  :hook
  ((emacs-lisp-mode . flycheck-mode) ; Enable Flycheck for linting
    (emacs-lisp-mode . lispy-mode) ; Enable Lispy for structured editing
    (emacs-lisp-mode . lispyville-mode) ; Enable Lispyville for Evil users
    (emacs-lisp-mode . eldoc-mode) ; Enable Eldoc for inline help
    (emacs-lisp-mode . subword-mode) ; Navigate through subwords
    ;; (emacs-lisp-mode
    ;;  .
    ;;  (lambda ()                          ; Automatically indent the buffer before saving
    ;;    (add-hook 'before-save-hook #'lt/elisp-indent-buffer nil t)))
    )
  :config
  (defun lt/elisp-indent-buffer ()
    "Indent the entire buffer when saving Emacs Lisp code."
    (when (eq major-mode 'emacs-lisp-mode)
      (indent-region (point-min) (point-max)))))

(use-package
  rustic

  :config (setq rustic-format-on-save t)
  :hook (rustic-mode . tree-sitter-hl-mode)
  :general
  (:states
    '(normal visual)
    :keymaps
    'rustic-mode-map
    "SPC mhd"
    'lsp-describe-thing-at-point
    "SPC mcC"
    'rustic-compile
    "SPC mcc"
    'rustic-cargo-comp
    "SPC mer"
    'rustic-cargo-run
    "SPC mtt"
    'rustic-cargo-test-run))

(use-package
  web-mode

  :mode ("\\.jsx?$" "\\.mdx$" "\\.j2$" "\\.html$")
  :hook (web-mode . tree-sitter-hl-mode)
  :custom
  (web-mode-enable-engine-detection t)
  (web-mode-engines-alist '(("jinja2" . "\\.j2$\\'")))
  (web-mode-markup-indent-offset 2)
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package
  typescript-mode

  :hook (typescript-mode . tree-sitter-hl-mode))

(use-package
  lua-mode

  :defer 1)

(use-package
  vue-mode

  :defer 1)

(use-package
  dockerfile-mode

  :defer 1)

(use-package eww :after elfeed-org :general (:states '(normal visual) "SPC eww" 'eww))

(use-package
  elfeed

  :after elfeed-org
  :init (evil-collection-init 'elfeed)
  :general (:states '(normal visual) "SPC elf" 'elfeed)
  :hook (elfeed-show-mode . lt:ajust-to-read)
  :custom (elfeed-db-directory "~/.emacs.d")
  :config (elfeed-org)
  (defun lt:ajust-to-read ()
    (buffer-face-set 'variable-pitch nil :family "American Typewriter" :height 1.3)))

(use-package
  elfeed-org
  :after org

  :custom (rmh-elfeed-org-files (list (concat org-directory "feed.org")))
  :config (elfeed-org))

(use-package
  chatgpt-shell
  :custom ((chatgpt-shell-openai-key (lambda () (auth-source-pass-get 'secret "openai-key")))))

(use-package
  terraform-mode

  :defer 1
  :hook (terraform-mode . lsp-deferred)
  :custom (terraform-command "tofu") (terraform-format-on-save t))

(use-package
  esxml
  :straight (esxml :type git :host nil :repo "https://github.com/tali713/esxml.git"))

(use-package
  nov
  :after
  esxml
  elfeed
  :mode (("\\.epub\\'" . nov-mode))
  :hook (nov-mode . lt:ajust-to-read)
  :custom (nov-text-width 80)
  :config
  :straight (nov :type git :host nil :repo "https://depp.brause.cc/nov.el.git"))


(use-package kubernetes)

(use-package
  go-ts-mode

  :mode (("\\.go\\'" . go-ts-mode) ("/go\\.mod\\'" . go-mod-ts-mode))
  :hook
  (go-ts-mode . lsp-deferred)
  (go-ts-mode . lsp-go-install-save-hooks)
  :custom
  (require 'dap-dlv-go)
  (tab-width 2)
  (treesit-font-lock-level 4)
  (lsp-gopls-use-placeholders nil)
  (lsp-completion-provider :capf)
  :init
  (add-to-list
    'treesit-language-source-alist
    '(go "https://github.com/tree-sitter/tree-sitter-go"))
  (add-to-list
    'treesit-language-source-alist
    '(gomod "https://github.com/camdencheek/tree-sitter-go-mod"))
  :config
  (defun lsp-go-install-save-hooks ()
    (when (bound-and-true-p lsp-mode)
      (add-hook 'before-save-hook #'lsp-format-buffer t t)
      (add-hook 'before-save-hook #'lsp-organize-imports t t)))

  ;; Ensure grammars are installed
  (dolist (lang '(go gomod))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang)))

  ;; Experimental gopls settings
  (lsp-register-custom-settings '(("gopls.completeUnimported" t t) ("gopls.staticcheck" t t))))

(use-package gotest)

(use-package go-tag)

(use-package godoctor)

(use-package realgud)

(use-package dape)

;; (use-package realgud-dlv)

(use-package
    nginx-mode

  :commands nginx-mode)

;; Python
(use-package
  python

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

(use-package python-pytest)

(use-package
  conda
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
  (or
    (cl-loop
      for dir in
      (list
        conda-anaconda-home
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
      if (file-directory-p dir) return
      (setq
        conda-anaconda-home (expand-file-name dir)
        conda-env-home-directory (expand-file-name dir)))
    (message "Cannot find Anaconda installation"))
  ;; integration with term/eshell
  (conda-env-initialize-interactive-shells)
  (add-to-list
    'global-mode-string
    '(conda-env-current-name (" conda:" conda-env-current-name " "))
    'append))

;;; pyvenv
;; To hopefully work better with virtual environments over tramp
;; to test if works with conda (and tramp really)
(use-package
  pyvenv
  :straight t
  :after python
  :config
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)
  (add-to-list
    'global-mode-string
    '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
    'append))

;;; numpydoc
(use-package
  numpydoc
  ;; use main repo and not emacsmirror as is a bit behind
  ;; as of 07/10/23
  :straight (numpydoc :type git :host github :repo "douglasdavis/numpydoc.el")
  :defer t
  :config
  (setq
    numpydoc-insertion-style 'yas ;; use yasnippet style prompt
    numpydoc-insert-examples-block nil ;; no examples block
    numpydoc-insert-return-without-typehint nil ;; as it says
    numpydoc-auto-fill-paragraphs t)) ;; autofill my docs

(use-package
  lsp-pyright
  :straight t
  :custom (lsp-pyright-langserver-command "pyright")
  :hook
  (python-mode
    .
    (lambda ()
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
(use-package
  apheleia
  :straight (apheleia :type git :host github :repo "radian-software/apheleia"))

(use-package
  python-black

  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(use-package
  python-isort

  :demand t
  :after python
  :hook (python-mode . python-isort-on-save-mode))

(use-package
  ruff-format

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

(use-package
  elpy

  :after
  poetry
  lsp
  :hook ((python-mode . elpy-mode) (elpy-mode . flycheck-mode) (elpy-mode . poetry-tracking-mode))
  :config
  (flycheck-add-next-checker 'lsp '(t . python-ruff))
  (lsp-register-client
    (make-lsp-client
      :new-connection (lsp-tramp-connection "pyls")
      :major-modes '(python-mode)
      :remote? t
      :server-id 'pyls-remote))
  :init
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (elpy-enable))

;; (use-package flycheck-pycheckers
;;   :after flycheck
;;
;;   :custom
;;   (flycheck-pycheckers-checkers '(mypy3 pyflakes))
;;   :init
;;   (with-eval-after-load 'flycheck
;;     (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

(use-package poetry)

(use-package
  pet
  :straight (pet :type git :host github :repo "wyuenho/emacs-pet")

  :hook ((pet-mode . lt/disable-pet-mode-for-remote-files))
  :config
  (defun lt/disable-pet-mode-for-remote-files ()
    "Disable pet-mode's heavy operations for remote files."
    (when (file-remote-p default-directory)
      (setq-local pet-mode nil)))
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; Copilot
(use-package
  gptel
  :straight t

  :custom
  (gptel-model "gpt-4o")
  (gptel-default-mode 'org-mode)
  (gptel-fill-column 80) ;; Set the desired fill column
  (gptel-directives
    '
    (
      (default
        .
        "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
      (programming
        .
        "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
      (programming-apply
        .
        "You are a large language model and a careful programmer. Apply changes from context related to given code in git merge conflicts format so user can review it easyly with standard tools and apply. Do not add anything not directly related to code like notes, text, markup languages quotation or other syntax and keep code that you were given since your reply will replace original content. Use code pieces from chat as basis if provided.")
      (writing . "You are a large language model and a writing assistant. Respond concisely.")
      (chat . "You are a large language model and a conversation partner. Respond concisely.")))
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
  :general (:states '(normal visual) "SPC lG" 'gptel "SPC lg" 'gptel-menu "SPC la" 'gptel-add)
  :init
  ;; (add-hook 'gptel-post-response-functions 'clean-gpt-buffer)
  (add-hook 'gptel-post-response-functions 'enable-visual-line-mode-in-chatgpt)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

;; GraphQL
(use-package
  graphql-mode

  :mode "\\.graphql\\'"
  :hook
  (graphql-mode . lsp)
  (graphql-mode . (lambda () (add-hook 'before-save-hook #'prettier-prettify nil t)))
  :config
  (lsp-register-client
    (make-lsp-client
      :new-connection (lsp-stdio-connection '("graphql-lsp" "server" "--method" "stream"))
      :major-modes '(graphql-mode)
      :server-id 'graphql-lsp))
  (setq graphql-indent-level 2))

(use-package prettier :config (setenv "NODE_PATH" "/usr/local/lib/node_modules"))

(use-package
  elisp-autofmt
  :custom (elisp-autofmt-style 'fixed)
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(use-package
  gdscript-mode
  :straight (gdscript-mode :type git :host github :repo "godotengine/emacs-gdscript-mode"))

(use-package jq-mode)

(provide 'langs)
;;; langs.el ends here
