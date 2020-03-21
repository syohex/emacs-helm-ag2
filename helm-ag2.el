;;; helm-ag2.el --- the silver searcher with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-ag2
;; Version: 0.58
;; Package-Requires: ((emacs "26.1") (helm "2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; helm-ag2 provides interfaces of the silver searcher(Other search programs can be used
;; such as the platinum searcher, ack). And helm-ag2 provides wgrep like features which
;; users can edit from searched result.

;;; Code:

(eval-when-compile
  (require 'grep)
  (defvar helm-help-message))

(require 'cl-lib)
(require 'helm)
(require 'helm-grep)
(require 'helm-utils)
(require 'compile)
(require 'subr-x)

(declare-function helm-read-file-name "helm-mode")
(declare-function helm-grep-get-file-extensions "helm-grep")
(declare-function helm-help "helm-help")

(defgroup helm-ag2 nil
  "the silver searcher with helm interface"
  :group 'helm)

(defsubst helm-ag2--windows-p ()
  (memq system-type '(ms-dos windows-nt)))

(defcustom helm-ag2-base-command
  (if (helm-ag2--windows-p)
      "ag --vimgrep"
    "ag --nocolor --nogroup")
  "Base command of `ag'"
  :type 'string)

(defcustom helm-ag2-command-option nil
  "Command line option of `ag'. This is appended after `helm-ag2-base-command'"
  :type 'string)

(defcustom helm-ag2-insert-at-point nil
  "Insert thing at point as search pattern.
   You can set value same as `thing-at-point'"
  :type 'symbol)

(defcustom helm-ag2-ignore-patterns nil
  "Ignore patterns for `ag'. This parameters are specified as --ignore"
  :type '(repeat string))

(defcustom helm-ag2-use-grep-ignore-list nil
  "Use `grep-find-ignored-files' and `grep-find-ignored-directories' as ignore pattern.
They are specified to `--ignore' options."
  :type 'boolean)

(defcustom helm-ag2-always-set-extra-option nil
  "Always set `ag' options of `helm-do-ag'."
  :type 'boolean)

(defcustom helm-ag2-fuzzy-match nil
  "Enable fuzzy match"
  :type 'boolean)

(defcustom helm-ag2-edit-save t
  "Save buffers you edit at completed."
  :type 'boolean)

(defcustom helm-ag2-use-emacs-lisp-regexp nil
  "[Experimental] Use Emacs Lisp regexp instead of PCRE."
  :type 'boolean)

(defcustom helm-ag2-use-agignore nil
  "Use .agignore where is at project root if it exists."
  :type 'boolean)

(defcustom helm-ag2-use-temp-buffer nil
  "Use temporary buffer for persistent action."
  :type 'boolean)

(defcustom helm-ag2-ignore-buffer-patterns nil
  "Use temporary buffer for persistent action."
  :type '(repeat regexp))

(defcustom helm-ag2-show-status-function 'helm-ag2-show-status-default-mode-line
  "Function called after that `ag' process is finished after `helm-do-ag'.
Default behaviour shows finish and result in mode-line."
  :type 'function)

(defface helm-ag2-edit-deleted-line
  '((t (:inherit font-lock-comment-face :strike-through t)))
  "Face of deleted line in edit mode.")

(defvar helm-ag2--command-history '())
(defvar helm-ag2--helm-history '())
(defvar helm-ag2--context-stack nil)
(defvar helm-ag2--default-directory nil)
(defvar helm-ag2--last-default-directory nil)
(defvar helm-ag2--last-query nil)
(defvar helm-ag2--last-command nil)
(defvar helm-ag2--elisp-regexp-query nil)
(defvar helm-ag2--valid-regexp-for-emacs nil)
(defvar helm-ag2--extra-options nil)
(defvar helm-ag2--extra-options-history nil)
(defvar helm-ag2--original-window nil)
(defvar helm-ag2--search-this-file-p nil)
(defvar helm-ag2--default-target nil)
(defvar helm-ag2--buffer-search nil)
(defvar helm-ag2--command-features '())
(defvar helm-ag2--ignore-case nil)
(defvar helm-do-ag--extensions nil)
(defvar helm-do-ag--commands nil)

(defun helm-ag2--ignore-case-p (cmds input)
  (cl-loop for cmd in cmds
           when (member cmd '("-i" "--ignore-case"))
           return t

           when (member cmd '("-s" "--case-sensitive"))
           return nil

           finally
           return (let ((case-fold-search nil))
                    (not (string-match-p "[A-Z]" input)))))

(defun helm-ag2--save-current-context ()
  (let ((curpoint (with-helm-current-buffer
                    (point))))
    (helm-aif (buffer-file-name helm-current-buffer)
        (push (list :file it :point curpoint) helm-ag2--context-stack)
      (push (list :buffer helm-current-buffer :point curpoint) helm-ag2--context-stack))))

(defun helm-ag2--insert-thing-at-point (thing)
  (helm-aif (thing-at-point thing)
      (substring-no-properties it)
    ""))

(defun helm-ag2--searched-word ()
  (if helm-ag2-insert-at-point
      (helm-ag2--insert-thing-at-point helm-ag2-insert-at-point)
    ""))

(defun helm-ag2--construct-ignore-option (pattern)
  (concat "--ignore=" pattern))

(defun helm-ag2--grep-ignore-list-to-options ()
  (require 'grep)
  (cl-loop for ignore in (append grep-find-ignored-files
                                 grep-find-ignored-directories)
           collect (helm-ag2--construct-ignore-option ignore)))

(defun helm-ag2--parse-options-and-query (input)
  (with-temp-buffer
    (insert input)
    (let (end options)
      (goto-char (point-min))
      (when (re-search-forward "\\s-*--\\s-+" nil t)
        (setq end (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward "\\(?:^\\|\\s-+\\)\\(-\\S-+\\)\\(?:\\s-+\\|$\\)" end t)
        (push (match-string-no-properties 1) options)
        (when end
          (cl-decf end (- (match-end 0) (match-beginning 0))))
        (replace-match ""))
      (cons options (buffer-string)))))

(defun helm-ag2--parse-query (input)
  (let* ((parsed (helm-ag2--parse-options-and-query input))
         (options (car parsed))
         (query (cdr parsed)))
    (when helm-ag2-use-emacs-lisp-regexp
      (setq query (helm-ag2--elisp-regexp-to-pcre query)))
    (setq helm-ag2--last-query query
          helm-ag2--elisp-regexp-query (helm-ag2--pcre-to-elisp-regexp query))
    (setq helm-ag2--valid-regexp-for-emacs
          (helm-ag2--validate-regexp helm-ag2--elisp-regexp-query))
    (if (not options)
        (list query)
      (nconc (nreverse options) (list query)))))

(defsubst helm-ag2--search-buffer-p (bufname)
  (cl-loop for regexp in helm-ag2-ignore-buffer-patterns
           never (string-match-p regexp bufname)))

(defun helm-ag2--file-visited-buffers ()
  (let ((bufs (cl-loop for buf in (buffer-list)
                       when (buffer-file-name buf)
                       collect it)))
    (if (not helm-ag2-ignore-buffer-patterns)
        bufs
      (cl-loop for buf in bufs
               when (helm-ag2--search-buffer-p buf)
               collect buf))))

(defun helm-ag2--construct-targets (targets)
  (let ((default-directory helm-ag2--default-directory))
    (cl-loop for target in targets
             collect (file-relative-name target))))

(defun helm-ag2--root-agignore ()
  (let ((root (helm-ag2--project-root)))
    (when root
      (let ((default-directory root))
        (when (file-exists-p ".agignore")
          (expand-file-name (concat default-directory ".agignore")))))))

(defun helm-ag2--construct-command (this-file)
  (let* ((commands (split-string helm-ag2-base-command nil t))
         (command (car commands))
         (args (cdr commands)))
    (when helm-ag2-command-option
      (let ((ag-options (split-string helm-ag2-command-option nil t)))
        (setq args (append args ag-options))))
    (when helm-ag2-use-agignore
      (helm-aif (helm-ag2--root-agignore)
          (setq args (append args (list "-p" it)))))
    (when helm-ag2-ignore-patterns
      (setq args (append args (mapcar 'helm-ag2--construct-ignore-option
                                      helm-ag2-ignore-patterns))))
    (when helm-ag2-use-grep-ignore-list
      (setq args (append args (helm-ag2--grep-ignore-list-to-options))))
    (setq args (append args (helm-ag2--parse-query helm-ag2--last-query)))
    (when this-file
      (setq args (append args (list this-file))))
    (when helm-ag2--buffer-search
      (setq args (append args (helm-ag2--file-visited-buffers))))
    (when helm-ag2--default-target
      (setq args (append args (helm-ag2--construct-targets helm-ag2--default-target))))
    (cons command args)))

(defun helm-ag2--remove-carrige-returns ()
  (when (helm-ag2--windows-p)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\xd" nil t)
        (replace-match "")))))

(defun helm-ag2--abbreviate-file-name ()
  (unless (helm-ag2--windows-p)
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (while (re-search-forward "^\\([^:]+\\)" nil t)
        (replace-match (abbreviate-file-name (match-string-no-properties 1)))))))

(defun helm-ag2--init ()
  (let ((buf-coding buffer-file-coding-system))
    (helm-attrset 'recenter t)
    (with-current-buffer (helm-candidate-buffer 'global)
      (let* ((default-directory (or helm-ag2--default-directory
                                    default-directory))
             (cmds (helm-ag2--construct-command (helm-attr 'search-this-file)))
             (coding-system-for-read buf-coding)
             (coding-system-for-write buf-coding))
        (setq helm-ag2--ignore-case (helm-ag2--ignore-case-p cmds helm-ag2--last-query)
              helm-ag2--last-command cmds)
        (let ((ret (apply #'process-file (car cmds) nil t nil (cdr cmds))))
          (if (zerop (length (buffer-string)))
              (error "No ag output: '%s'" helm-ag2--last-query)
            (unless (zerop ret)
              (unless (executable-find (car cmds))
                (error "'ag' is not installed."))
              (error "Failed: '%s'" helm-ag2--last-query))))
        (when helm-ag2--buffer-search
          (helm-ag2--abbreviate-file-name))
        (helm-ag2--remove-carrige-returns)
        (helm-ag2--save-current-context)))))

(add-to-list 'debug-ignored-errors "^No ag output: ")

(defun helm-ag2--search-only-one-file-p ()
  (when (and helm-ag2--default-target (= (length helm-ag2--default-target) 1))
    (let ((target (car helm-ag2--default-target)))
      (unless (file-directory-p target)
        target))))

(defun helm-ag2--find-file-action (candidate find-func this-file &optional persistent)
  (when (memq 'pt helm-ag2--command-features)
    ;; 'pt' always show filename if matched file is only one.
    (setq this-file nil))
  (let* ((file-line (helm-grep-split-line candidate))
         (filename (or this-file (cl-first file-line) candidate))
         (line (if this-file
                   (cl-first (split-string candidate ":"))
                 (cl-second file-line)))
         (default-directory (or helm-ag2--default-directory
                                helm-ag2--last-default-directory
                                default-directory)))
    (unless persistent
      (setq helm-ag2--last-default-directory default-directory))
    (funcall find-func filename)
    (goto-char (point-min))
    (when line
      (forward-line (1- (string-to-number line))))
    (ignore-errors
      (and (re-search-forward helm-ag2--last-query (line-end-position) t)
           (goto-char (match-beginning 0))))))

(defun helm-ag2--open-file-with-temp-buffer (filename)
  (let ((search-directory default-directory))
    (switch-to-buffer (get-buffer-create " *helm-ag2 persistent*"))
    (setq default-directory search-directory)
    (fundamental-mode)
    (erase-buffer)
    (insert-file-contents filename)
    (let ((buffer-file-name filename))
      (set-auto-mode)
      (font-lock-fontify-region (point-min) (point-max)))))

(defsubst helm-ag2--vimgrep-option ()
  (member "--vimgrep" helm-ag2--last-command))

(defun helm-ag2--search-this-file-p ()
  (unless (helm-ag2--vimgrep-option)
    (if (eq (helm-get-current-source) 'helm-source-do-ag)
        (helm-ag2--search-only-one-file-p)
      (helm-attr 'search-this-file))))

(defun helm-ag2--persistent-action (candidate)
  (let ((find-func (if helm-ag2-use-temp-buffer
                       #'helm-ag2--open-file-with-temp-buffer
                     #'find-file)))
    (helm-ag2--find-file-action candidate find-func (helm-ag2--search-this-file-p) t)
    (helm-highlight-current-line)))

(defun helm-ag2--validate-regexp (regexp)
  (condition-case nil
      (progn
        (string-match-p regexp "")
        t)
    (invalid-regexp nil)))

(defun helm-ag2--pcre-to-elisp-regexp (pcre)
  ;; This is very simple conversion
  (with-temp-buffer
    (insert pcre)
    (goto-char (point-min))
    ;; convert (, ), {, }, |
    (while (re-search-forward "[(){}|]" nil t)
      (backward-char 1)
      (cond ((looking-back "\\\\\\\\" nil))
            ((looking-back "\\\\" nil)
             (delete-char -1))
            (t
             (insert "\\")))
      (forward-char 1))
    ;; convert \s and \S -> \s- \S-
    (goto-char (point-min))
    (while (re-search-forward "\\(\\\\s\\)" nil t)
      (unless (looking-back "\\\\\\\\s" nil)
        (insert "-")))
    (buffer-string)))

(defun helm-ag2--elisp-regexp-to-pcre (regexp)
  (with-temp-buffer
    (insert regexp)
    (goto-char (point-min))
    (while (re-search-forward "[(){}|]" nil t)
      (backward-char 1)
      (cond ((looking-back "\\\\\\\\" nil))
            ((looking-back "\\\\" nil)
             (delete-char -1))
            (t
             (insert "\\")))
      (forward-char 1))
    (buffer-string)))

(defun helm-ag2--highlight-candidate (candidate)
  (let ((limit (1- (length candidate)))
        (last-pos 0)
        (case-fold-search helm-ag2--ignore-case))
    (when helm-ag2--valid-regexp-for-emacs
      (while (and (< last-pos limit)
                  (string-match helm-ag2--elisp-regexp-query candidate last-pos))
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (if (= start end)
              (cl-incf last-pos)
            (put-text-property start end 'face 'helm-match candidate)
            (setq last-pos (1+ (match-end 0)))))))
    candidate))

(defun helm-ag2--candidate-transform-for-this-file (candidate)
  (when (string-match "\\`\\([^:]+\\):\\(.*\\)" candidate)
    (format "%s:%s"
            (propertize (match-string 1 candidate) 'face 'helm-grep-lineno)
            (helm-ag2--highlight-candidate (match-string 2 candidate)))))

(defun helm-ag2--candidate-transform-for-files (candidate)
  (helm-aif (helm-grep-split-line candidate)
      (format "%s:%s:%s"
              (propertize (cl-first it) 'face 'helm-moccur-buffer)
              (propertize (cl-second it) 'face 'helm-grep-lineno)
              (helm-ag2--highlight-candidate (cl-third it)))))

(defun helm-ag2--candidate-transformer (candidate)
  (or (if (helm-attr 'search-this-file)
          (helm-ag2--candidate-transform-for-this-file candidate)
        (helm-ag2--candidate-transform-for-files candidate))
      candidate))

(defun helm-ag2--action-find-file (candidate)
  (helm-ag2--find-file-action candidate 'find-file (helm-ag2--search-this-file-p)))

(defun helm-ag2--action-find-file-other-window (candidate)
  (helm-ag2--find-file-action candidate 'find-file-other-window (helm-ag2--search-this-file-p)))

(defvar helm-ag2--actions
  (helm-make-actions
   "Open file"              #'helm-ag2--action-find-file
   "Open file other window" #'helm-ag2--action-find-file-other-window
   "Save results in buffer" #'helm-ag2--action-save-buffer
   "Edit search results"    #'helm-ag2--edit))

(defvar helm-ag2-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-ag2--run-other-window-action)
    (define-key map (kbd "C-l") 'helm-ag2--up-one-level)
    (define-key map (kbd "C-c C-e") 'helm-ag2-edit)
    (define-key map (kbd "C-x C-s") 'helm-ag2--run-save-buffer)
    (define-key map (kbd "C-c ?") 'helm-ag2-help)
    (define-key map (kbd "C-c >") 'helm-ag2--next-file)
    (define-key map (kbd "<right>") 'helm-ag2--next-file)
    (define-key map (kbd "C-c <") 'helm-ag2--previous-file)
    (define-key map (kbd "<left>") 'helm-ag2--previous-file)
    map)
  "Keymap for `helm-ag2'.")

(defvar helm-ag2-source
  (helm-build-in-buffer-source "The Silver Searcher"
    :init 'helm-ag2--init
    :real-to-display 'helm-ag2--candidate-transformer
    :persistent-action 'helm-ag2--persistent-action
    :fuzzy-match helm-ag2-fuzzy-match
    :action helm-ag2--actions
    :candidate-number-limit 9999
    :keymap helm-ag2-map
    :follow (and helm-follow-mode-persistent 1)))

;;;###autoload
(defun helm-ag2-pop-stack ()
  (interactive)
  (let ((context (pop helm-ag2--context-stack)))
    (unless context
      (error "Context stack is empty !"))
    (helm-aif (plist-get context :file)
        (find-file it)
      (let ((buf (plist-get context :buffer)))
        (if (buffer-live-p buf)
            (switch-to-buffer buf)
          (error "The buffer is already killed."))))
    (goto-char (plist-get context :point))))

;;;###autoload
(defun helm-ag2-clear-stack ()
  (interactive)
  (setq helm-ag2--context-stack nil))

(defun helm-ag2--marked-input (escape)
  (when (use-region-p)
    (let ((input (buffer-substring-no-properties (region-beginning) (region-end))))
      (deactivate-mark)
      (if (not escape)
          input
        (replace-regexp-in-string " " "\\\\ " input)))))

(defun helm-ag2--query ()
  (let* ((searched-word (helm-ag2--searched-word))
         (marked-word (helm-ag2--marked-input nil))
         (query (read-from-minibuffer "Pattern: "
                                      (or marked-word searched-word)
                                      nil
                                      nil
                                      'helm-ag2--command-history
                                      (helm-aif (symbol-at-point)
                                          (symbol-name it)))))
    (when (string-empty-p query)
      (error "Input is empty!!"))
    (setq helm-ag2--last-query query)))

(defsubst helm-ag2--init-state ()
  (setq helm-ag2--original-window (selected-window)
        helm-ag2--last-default-directory nil))

(defun helm-ag2--get-default-directory ()
  (let ((prefix-val (and current-prefix-arg (abs (prefix-numeric-value current-prefix-arg)))))
    (cond ((not prefix-val) default-directory)
          ((= prefix-val 4)
           (file-name-as-directory
            (read-directory-name "Search directory: " nil nil t)))
          ((= prefix-val 16)
           (let ((dirs (list (read-directory-name "Search directory: " nil nil t))))
             (while (y-or-n-p "More directories ?")
               (push (read-directory-name "Search directory: " nil nil t) dirs))
             (reverse dirs))))))

(defsubst helm-ag2--helm-header (dir)
  (if helm-ag2--buffer-search
      "Search Buffers"
    (concat "Search at " (abbreviate-file-name dir))))

(defun helm-ag2--run-other-window-action ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-ag2--action-find-file-other-window)))

(defun helm-ag2--exit-from-edit-mode ()
  (when (window-live-p helm-ag2--original-window)
    (select-window helm-ag2--original-window))
  (kill-buffer (get-buffer "*helm-ag2-edit*")))

(defun helm-ag2--match-line-regexp ()
  ;; $1: file name
  ;; $2: line
  ;; $3: match body
  ;; $4: file attributes part(filename, line, column)
  (cond ((helm-ag2--vimgrep-option)
         "^\\(?4:\\(?1:[^:]+\\):\\(?2:[1-9][0-9]*\\):[^:]+:\\)\\(?3:.*\\)$")
        (helm-ag2--search-this-file-p
         "^\\(?4:\\(?2:[1-9][0-9]*\\)[:-]\\)\\(?3:.*\\)$")
        (t
         "^\\(?4:\\(?1:[^:]+\\):\\(?2:[1-9][0-9]*\\)[:-]\\)\\(?3:.*\\)$")))

(defun helm-ag2--edit-commit ()
  (interactive)
  (goto-char (point-min))
  (let ((read-only-files 0)
        (saved-buffers nil)
        (regexp (helm-ag2--match-line-regexp))
        (default-directory helm-ag2--default-directory)
        (line-deletes (make-hash-table :test #'equal))
        (kept-buffers (buffer-list))
        open-buffers)
    (while (re-search-forward regexp nil t)
      (let* ((file (or (match-string-no-properties 1) helm-ag2--search-this-file-p))
             (line (string-to-number (match-string-no-properties 2)))
             (body (match-string-no-properties 3))
             (ovs (overlays-at (line-beginning-position))))
        (with-current-buffer (find-file-noselect file)
          (cl-pushnew (current-buffer) open-buffers)
          (if buffer-read-only
              (cl-incf read-only-files)
            (goto-char (point-min))
            (let ((deleted-lines (gethash file line-deletes 0))
                  (deleted (and ovs (overlay-get (car ovs) 'helm-ag2-deleted))))
              (forward-line (- line 1 deleted-lines))
              (delete-region (line-beginning-position) (line-end-position))
              (if (not deleted)
                  (insert body)
                (let ((beg (point)))
                  (forward-line 1)
                  (delete-region beg (point))
                  (puthash file (1+ deleted-lines) line-deletes)))
              (cl-pushnew (current-buffer) saved-buffers))))))
    (when helm-ag2-edit-save
      (dolist (buf saved-buffers)
        (with-current-buffer buf
          (save-buffer))))
    (dolist (buf open-buffers)
      (unless (memq buf kept-buffers)
        (kill-buffer buf)))
    (helm-ag2--exit-from-edit-mode)
    (if (not (zerop read-only-files))
        (message "%d files are read-only and not editable." read-only-files)
      (message "Success update"))))

(defun helm-ag2--edit-abort ()
  (interactive)
  (when (y-or-n-p "Discard changes ?")
    (helm-ag2--exit-from-edit-mode)
    (message "Abort edit")))

(defun helm-ag2--mark-line-deleted ()
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (ov (make-overlay beg end)))
    (overlay-put ov 'face 'helm-ag2-edit-deleted-line)
    (overlay-put ov 'helm-ag2-deleted t)))

(defun helm-ag2--unmark ()
  (interactive)
  (dolist (ov (overlays-in (line-beginning-position) (line-end-position)))
    (when (overlay-get ov 'helm-ag2-deleted)
      (delete-overlay ov))))

(defvar helm-ag2-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'helm-ag2--edit-commit)
    (define-key map (kbd "C-c C-k") 'helm-ag2--edit-abort)
    (define-key map (kbd "C-c C-d") 'helm-ag2--mark-line-deleted)
    (define-key map (kbd "C-c C-u") 'helm-ag2--unmark)
    map))

(defsubst helm-ag2--edit-func-to-keys (func)
  (key-description (car-safe (where-is-internal func helm-ag2-edit-map))))

(defun helm-ag2--edit (_candidate)
  (let* ((helm-buf-dir (or helm-ag2--default-directory
                           helm-ag2--last-default-directory
                           default-directory))
         (default-directory helm-buf-dir))
    (with-current-buffer (get-buffer-create "*helm-ag2-edit*")
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq-local helm-ag2--default-directory helm-buf-dir)
      (unless (helm-ag2--vimgrep-option)
        (setq-local helm-ag2--search-this-file-p
                    (assoc-default 'search-this-file (helm-get-current-source))))
      (let (buf-content)
        (with-current-buffer (get-buffer "*helm-ag2*")
          (goto-char (point-min))
          (forward-line 1)
          (let* ((body-start (point))
                 (marked-lines (cl-loop for ov in (overlays-in body-start (point-max))
                                        when (eq 'helm-visible-mark (overlay-get ov 'face))
                                        return (helm-marked-candidates))))
            (if (not marked-lines)
                (setq buf-content (buffer-substring-no-properties
                                   body-start (point-max)))
              (setq buf-content (concat (string-join marked-lines "\n") "\n")))))
        (insert buf-content)
        (add-text-properties (point-min) (point-max)
                             '(read-only t rear-nonsticky t front-sticky t))
        (let ((inhibit-read-only t)
              (regexp (helm-ag2--match-line-regexp)))
          (setq header-line-format
                (format "[%s] %s: Commit, %s: Abort"
                        (abbreviate-file-name helm-ag2--default-directory)
                        (helm-ag2--edit-func-to-keys #'helm-ag2--edit-commit)
                        (helm-ag2--edit-func-to-keys #'helm-ag2--edit-abort)))
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (let ((file-line-begin (match-beginning 4))
                  (file-line-end (match-end 4))
                  (body-begin (match-beginning 3))
                  (body-end (match-end 3)))
              (add-text-properties file-line-begin file-line-end
                                   '(face font-lock-function-name-face
                                          intangible t))
              (remove-text-properties body-begin body-end '(read-only t))
              (set-text-properties body-end (1+ body-end)
                                   '(read-only t rear-nonsticky t))))))))
  (other-window 1)
  (switch-to-buffer (get-buffer "*helm-ag2-edit*"))
  (goto-char (point-min))
  (setq next-error-function 'compilation-next-error-function)
  (setq-local compilation-locs (make-hash-table :test 'equal :weakness 'value))
  (use-local-map helm-ag2-edit-map))

(defun helm-ag2-edit ()
  (interactive)
  (helm-exit-and-execute-action 'helm-ag2--edit))

(defconst helm-ag2--help-message
  "\n* Helm Ag\n

\n** Specific commands for Helm Ag:\n
\\<helm-ag2-map>
\\[helm-ag2--run-other-window-action]\t\t-> Open result in other buffer
\\[helm-ag2--up-one-level]\t\t-> Search in parent directory.
\\[helm-ag2-edit]\t\t-> Edit search results.
\\[helm-ag2-help]\t\t-> Show this help.
\n** Helm Ag Map\n
\\{helm-map}")

(defun helm-ag2-help ()
  (interactive)
  (let ((helm-help-message helm-ag2--help-message))
    (helm-help)))

(defun helm-ag2-mode-jump ()
  (interactive)
  (let ((line (helm-current-line-contents)))
    (helm-ag2--find-file-action line 'find-file helm-ag2--search-this-file-p)))

(defun helm-ag2-mode-jump-other-window ()
  (interactive)
  (let ((line (helm-current-line-contents)))
    (helm-ag2--find-file-action line 'find-file-other-window helm-ag2--search-this-file-p)))

(defvar helm-ag2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'helm-ag2-mode-jump)
    (define-key map (kbd "C-o") 'helm-ag2-mode-jump-other-window)
    (define-key map (kbd "g") 'helm-ag2--update-save-results)
    map))

(define-derived-mode helm-ag2-mode special-mode "helm-ag2"
  "Major mode to provide actions in helm grep saved buffer.

Special commands:
\\{helm-ag2-mode-map}")

(defun helm-ag2--put-result-in-save-buffer (result search-this-file-p)
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "-*- mode: helm-ag2 -*-\n\n"
            (format "Ag Results for `%s':\n\n" helm-ag2--last-query))
    (save-excursion
      (insert result)))
  (helm-ag2-mode)
  (unless (helm-ag2--vimgrep-option)
    (setq-local helm-ag2--search-this-file-p search-this-file-p))
  (setq-local helm-ag2--default-directory default-directory))

(defun helm-ag2--save-results (use-other-buf)
  (let* ((search-this-file-p nil)
         (result (with-current-buffer helm-buffer
                   (goto-char (point-min))
                   (forward-line 1)
                   (buffer-substring (point) (point-max))))
         (default-directory helm-ag2--default-directory)
         (buf (if use-other-buf
                  (read-string "Results buffer name: "
                               (format "*helm ag results for '%s'*" helm-ag2--last-query))
                "*helm ag results*")))
    (when (buffer-live-p (get-buffer buf))
      (kill-buffer buf))
    (with-current-buffer (get-buffer-create buf)
      (helm-ag2--put-result-in-save-buffer result search-this-file-p)
      (pop-to-buffer buf)
      (message "Helm Ag Results saved in `%s' buffer" buf))))

(defun helm-ag2--update-save-results ()
  (interactive)
  (let* ((default-directory helm-ag2--default-directory)
         (result (with-temp-buffer
                   (apply #'process-file (car helm-ag2--last-command) nil t nil
                          (cdr helm-ag2--last-command))
                   (helm-ag2--remove-carrige-returns)
                   (when helm-ag2--buffer-search
                     (helm-ag2--abbreviate-file-name))
                   (helm-ag2--propertize-candidates helm-ag2--last-query)
                   (buffer-string))))
    (helm-ag2--put-result-in-save-buffer result helm-ag2--search-this-file-p)
    (message "Update Results")))

(defun helm-ag2--action-save-buffer (_arg)
  (helm-ag2--save-results nil))

(defun helm-ag2--run-save-buffer ()
  (interactive)
  (let ((use-other-buf-p current-prefix-arg))
    (with-helm-alive-p
      (helm-exit-and-execute-action
       (lambda (_arg)
         (helm-ag2--save-results use-other-buf-p))))))

(defun helm-ag2--file-of-current-file ()
  (let ((line (helm-current-line-contents)))
    (when (string-match helm-grep-split-line-regexp line)
      (match-string-no-properties 1 line))))

(defun helm-ag2--move-file-common (pred move-fn wrap-fn)
  (with-helm-window
    (let ((file (helm-ag2--file-of-current-file)))
      (funcall move-fn)
      (while (and (not (funcall pred)) (string= file (helm-ag2--file-of-current-file)))
        (funcall move-fn))
      (when (funcall pred)
        (funcall wrap-fn)))))

(defun helm-ag2--previous-file ()
  (interactive)
  (helm-ag2--move-file-common
   #'helm-beginning-of-source-p #'helm-previous-line #'helm-end-of-buffer))

(defun helm-ag2--next-file ()
  (interactive)
  (helm-ag2--move-file-common
   #'helm-end-of-source-p #'helm-next-line #'helm-beginning-of-buffer))

(defsubst helm-ag2--root-directory-p ()
  (cl-loop for dir in '(".git/" ".hg/")
           thereis (file-directory-p dir)))

(defun helm-ag2--up-one-level ()
  (interactive)
  (if (or (not (helm-ag2--root-directory-p))
          (y-or-n-p "Current directory might be the project root. \
Continue searching the parent directory? "))
      (let ((parent (file-name-directory (directory-file-name default-directory))))
        (helm-run-after-exit
         (lambda ()
           (let* ((default-directory parent)
                  (helm-ag2--default-directory parent))
             (setq helm-ag2--last-default-directory default-directory)
             (helm-attrset 'name (helm-ag2--helm-header default-directory) helm-ag2-source)
             (helm :sources '(helm-ag2-source) :buffer "*helm-ag2*" :keymap helm-ag2-map
                   :history 'helm-ag2--helm-history)))))
    (message nil)))

;;;###autoload
(defun helm-ag2-this-file ()
  (interactive)
  (helm-ag2--init-state)
  (let ((filename (file-name-nondirectory (buffer-file-name)))
        (helm-ag2--default-directory default-directory))
    (helm-ag2--query)
    (helm-ag2--set-command-features)
    (helm-attrset 'search-this-file (file-relative-name (buffer-file-name))
                  helm-ag2-source)
    (helm-attrset 'name (format "Search at %s" filename) helm-ag2-source)
    (helm :sources '(helm-ag2-source) :buffer "*helm-ag2*" :keymap helm-ag2-map
          :history 'helm-ag2--helm-history)))

;;;###autoload
(defun helm-ag2 (&optional basedir)
  (interactive)
  (helm-ag2--init-state)
  (let ((dir (helm-ag2--get-default-directory))
        targets)
    (when (listp dir)
      (setq basedir default-directory
            targets dir))
    (let ((helm-ag2--default-directory (or basedir dir))
          (helm-ag2--default-target targets))
      (helm-ag2--query)
      (helm-attrset 'search-this-file nil helm-ag2-source)
      (helm-attrset 'name (helm-ag2--helm-header helm-ag2--default-directory) helm-ag2-source)
      (helm :sources '(helm-ag2-source) :buffer "*helm-ag2*" :keymap helm-ag2-map
            :history 'helm-ag2--helm-history))))

(defun helm-ag2--split-string (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((prev (point))
          patterns)
      (while (search-forward " " nil 'move)
        (cond ((looking-back "\\\\\\\\ " nil)
               (push (buffer-substring-no-properties prev (1- (point))) patterns)
               (skip-chars-forward " ")
               (setq prev (point)))
              ((looking-back "\\\\ " nil)
               (replace-match " "))
              (t (push (buffer-substring-no-properties prev (1- (point))) patterns)
                 (skip-chars-forward " ")
                 (setq prev (point)))))
      (push (buffer-substring-no-properties prev (point)) patterns)
      (reverse (cl-loop for p in patterns unless (string= p "") collect p)))))

(defsubst helm-ag2--convert-invert-pattern (pattern)
  (when (and (memq 'pcre helm-ag2--command-features)
             (string-prefix-p "!" pattern) (> (length pattern) 1))
    (concat "^(?!.*" (substring pattern 1) ").+$")))

(defun helm-ag2--join-patterns (input)
  (let ((patterns (helm-ag2--split-string input)))
    (if (= (length patterns) 1)
        (or (helm-ag2--convert-invert-pattern (car patterns))
            (car patterns))
      (cond ((memq 'pcre helm-ag2--command-features)
             (cl-loop for s in patterns
                      if (helm-ag2--convert-invert-pattern s)
                      concat (concat "(?=" it ")")
                      else
                      concat (concat "(?=.*" s ".*)")))
            ((memq 're2 helm-ag2--command-features)
             (string-join patterns ".*"))
            ;; we don't know anything about this pattern
            (t input)))))

(defun helm-ag2--do-ag-highlight-patterns (input)
  (if (memq 'pcre helm-ag2--command-features)
      (cl-loop with regexp = (helm-ag2--pcre-to-elisp-regexp input)
               for pattern in (helm-ag2--split-string regexp)
               when (helm-ag2--validate-regexp pattern)
               collect pattern)
    (list (helm-ag2--join-patterns input))))

(defun helm-ag2--propertize-candidates (input)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((patterns (helm-ag2--do-ag-highlight-patterns input)))
      (cl-loop with one-file-p = (and (not (helm-ag2--vimgrep-option))
                                      (helm-ag2--search-only-one-file-p))
               while (not (eobp))
               for num = 1 then (1+ num)
               do
               (progn
                 (let ((start (point))
                       (bound (line-end-position)))
                   (if (and one-file-p (search-forward ":" bound t))
                       (set-text-properties (line-beginning-position) (1- (point))
                                            '(face helm-grep-lineno))
                     (when (re-search-forward helm-grep-split-line-regexp bound t)
                       (set-text-properties (match-beginning 1) (match-end 1) '(face helm-moccur-buffer))
                       (set-text-properties (match-beginning 2) (match-end 2) '(face helm-grep-lineno))
                       (goto-char (match-beginning 3))))
                   (let ((curpoint (point))
                         (case-fold-search helm-ag2--ignore-case))
                     (dolist (pattern patterns)
                       (let ((last-point (point)))
                         (while (re-search-forward pattern bound t)
                           (set-text-properties (match-beginning 0) (match-end 0)
                                                '(face helm-match))
                           (when (= last-point (point))
                             (forward-char 1))
                           (setq last-point (point)))
                         (goto-char curpoint))))
                   (put-text-property start bound 'helm-cand-num num))
                 (forward-line 1))))))

(defun helm-ag2-show-status-default-mode-line ()
  (setq mode-line-format
        '(" " mode-line-buffer-identification " "
          (:eval (propertize
                  (format
                   "[AG process finished - (%s results)] "
                   (helm-get-candidate-number))
                  'face 'helm-grep-finish)))))

(defun helm-ag2--do-ag-propertize (input)
  (with-helm-window
    (helm-ag2--remove-carrige-returns)
    (when helm-ag2--buffer-search
      (helm-ag2--abbreviate-file-name))
    (helm-ag2--propertize-candidates input)
    (when helm-ag2-show-status-function
      (funcall helm-ag2-show-status-function)
      (force-mode-line-update))))

(defun helm-ag2--construct-extension-options ()
  (cl-loop for ext in helm-do-ag--extensions
           unless (string= ext "*")
           collect
           (concat "-G" (replace-regexp-in-string
                         "\\*" ""
                         (replace-regexp-in-string "\\." "\\\\." ext)))))

(defun helm-ag2--show-result-p (options has-query)
  (or has-query
      (cl-loop for opt in options
               thereis (string-prefix-p "-g" opt))))

(defun helm-ag2--construct-do-ag-command (pattern)
  (let* ((opt-query (helm-ag2--parse-options-and-query pattern))
         (options (car opt-query))
         (query (cdr opt-query))
         (has-query (not (string= query ""))))
    (when helm-ag2-use-emacs-lisp-regexp
      (setq query (helm-ag2--elisp-regexp-to-pcre query)))
    (when (helm-ag2--show-result-p options has-query)
      (append (car helm-do-ag--commands)
              options
              (and has-query (list (helm-ag2--join-patterns query)))
              (cdr helm-do-ag--commands)))))

(defun helm-ag2--do-ag-set-command ()
  (let ((cmd-opts (split-string helm-ag2-base-command nil t)))
    (when helm-ag2-command-option
      (setq cmd-opts (append cmd-opts (split-string helm-ag2-command-option nil t))))
    (when helm-ag2--extra-options
      (setq cmd-opts (append cmd-opts (split-string helm-ag2--extra-options))))
    (when helm-ag2-ignore-patterns
      (setq cmd-opts
            (append cmd-opts
                    (mapcar #'helm-ag2--construct-ignore-option
                            helm-ag2-ignore-patterns))))
    (when helm-ag2-use-agignore
      (helm-aif (helm-ag2--root-agignore)
          (setq cmd-opts (append cmd-opts (list "-p" it)))))
    (when helm-do-ag--extensions
      (setq cmd-opts (append cmd-opts (helm-ag2--construct-extension-options))))
    (let (targets)
      (when helm-ag2--buffer-search
        (setq targets (helm-ag2--file-visited-buffers)))
      (setq helm-do-ag--commands
            (cons cmd-opts
                  (if helm-ag2--default-target
                      (append targets (helm-ag2--construct-targets helm-ag2--default-target))
                    targets))))))

(defun helm-ag2--do-ag-candidate-process ()
  (let* ((non-essential nil)
         (default-directory (or helm-ag2--default-directory
                                helm-ag2--last-default-directory
                                default-directory))
         (cmd-args (helm-ag2--construct-do-ag-command helm-pattern)))
    (when cmd-args
      (let ((proc (apply #'start-file-process "helm-do-ag" nil cmd-args)))
        (setq helm-ag2--last-query helm-pattern
              helm-ag2--last-command cmd-args
              helm-ag2--ignore-case (helm-ag2--ignore-case-p cmd-args helm-pattern)
              helm-ag2--last-default-directory default-directory)
        (prog1 proc
          (set-process-sentinel
           proc
           (lambda (process event)
             (helm-process-deferred-sentinel-hook
              process event (helm-default-directory))
             (when (string= event "finished\n")
               (helm-ag2--do-ag-propertize helm-input)))))))))

(defconst helm-do-ag--help-message
  "\n* Helm Do Ag\n

\n** Specific commands for Helm Ag:\n
\\<helm-do-ag-map>
\\[helm-ag2--run-other-window-action]\t\t-> Open result in other buffer
\\[helm-ag2--do-ag-up-one-level]\t\t-> Search in parent directory.
\\[helm-ag2-edit]\t\t-> Edit search results.
\\[helm-ag2--do-ag-help]\t\t-> Show this help.
\n** Helm Ag Map\n
\\{helm-map}")

(defun helm-ag2--do-ag-help ()
  (interactive)
  (let ((helm-help-message helm-do-ag--help-message))
    (helm-help)))

(defvar helm-do-ag-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-ag2-map)
    (define-key map (kbd "C-l") 'helm-ag2--do-ag-up-one-level)
    (define-key map (kbd "C-c ?") 'helm-ag2--do-ag-help)
    map)
  "Keymap for `helm-do-ag'.")

(defvar helm-source-do-ag
  (helm-build-async-source "The Silver Searcher"
    :init 'helm-ag2--do-ag-set-command
    :candidates-process 'helm-ag2--do-ag-candidate-process
    :persistent-action  'helm-ag2--persistent-action
    :action helm-ag2--actions
    :nohighlight t
    :requires-pattern 3
    :candidate-number-limit 9999
    :keymap helm-do-ag-map
    :follow (and helm-follow-mode-persistent 1)))

(defun helm-ag2--do-ag-up-one-level ()
  (interactive)
  (if (or (not (helm-ag2--root-directory-p))
          (y-or-n-p "Current directory might be the project root. \
Continue searching the parent directory? "))
      (let ((parent (file-name-directory (directory-file-name default-directory)))
            (initial-input helm-input))
        (helm-run-after-exit
         (lambda ()
           (let ((default-directory parent)
                 (helm-ag2--default-directory parent))
             (setq helm-ag2--last-default-directory default-directory)
             (helm-attrset 'name (helm-ag2--helm-header parent)
                           helm-source-do-ag)
             (helm :sources '(helm-source-do-ag) :buffer "*helm-ag2*"
                   :keymap helm-do-ag-map :input initial-input
                   :history 'helm-ag2--helm-history)))))
    (message nil)))

(defun helm-ag2--set-do-ag-option ()
  (when (or (< (prefix-numeric-value current-prefix-arg) 0)
            helm-ag2-always-set-extra-option)
    (let ((option (read-string "Extra options: " (or helm-ag2--extra-options "")
                               'helm-ag2--extra-options-history)))
      (setq helm-ag2--extra-options option))))

(defun helm-ag2--set-command-features ()
  (let ((cmd (intern (car (split-string helm-ag2-base-command)))))
    (setq helm-ag2--command-features (list cmd))
    (cl-case cmd
      (ack (add-to-list 'helm-ag2--command-features
                        (if (string-match-p "-\\(?:Q\\|-literal\\)\\>" helm-ag2-base-command)
                            'fixed
                          'pcre)))
      (ag (add-to-list 'helm-ag2--command-features
                       (if (string-match-p "-\\(?:[QF]\\|-literal\\|-fixed-strings\\)\\>" helm-ag2-base-command)
                           'fixed
                         'pcre)))
      (pt (add-to-list 'helm-ag2--command-features
                       (if (string-match-p "-e\\>" helm-ag2-base-command)
                           're2
                         'fixed)))
      (rg (add-to-list 'helm-ag2--command-features
                       (if (string-match-p "-\\(?:F\\|-fixed-strings\\)\\>" helm-ag2-base-command)
                           'fixed
                         're2))))))

(defun helm-ag2--do-ag-searched-extensions ()
  (when (and current-prefix-arg (= (abs (prefix-numeric-value current-prefix-arg)) 4))
    (helm-grep-get-file-extensions helm-ag2--default-target)))

(defsubst helm-do-ag--target-one-directory-p (targets)
  (and (listp targets) (= (length targets) 1) (file-directory-p (car targets))))

(defun helm-do-ag--helm ()
  (let ((search-dir (if (not (helm-ag2--windows-p))
                        helm-ag2--default-directory
                      (if (helm-do-ag--target-one-directory-p helm-ag2--default-target)
                          (car helm-ag2--default-target)
                        helm-ag2--default-directory))))
    (helm-attrset 'name (helm-ag2--helm-header search-dir)
                  helm-source-do-ag)
    (helm :sources '(helm-source-do-ag) :buffer "*helm-ag2*" :keymap helm-do-ag-map
          :input (or (helm-ag2--marked-input t)
                     (helm-ag2--insert-thing-at-point helm-ag2-insert-at-point))
          :history 'helm-ag2--helm-history)))

;;;###autoload
(defun helm-do-ag-this-file ()
  (interactive)
  (helm-aif (buffer-file-name)
      (helm-do-ag default-directory (list it))
    (error "Error: This buffer is not visited file.")))

;;;###autoload
(defun helm-do-ag (&optional basedir targets)
  (interactive)
  (require 'helm-mode)
  (helm-ag2--init-state)
  (let* ((helm-ag2--default-directory (or basedir default-directory))
         (helm-ag2--default-target (cond (targets targets)
                                         ((and (helm-ag2--windows-p) basedir) (list basedir))
                                         (t
                                          (when (and (not basedir) (not helm-ag2--buffer-search))
                                            (helm-read-file-name
                                             "Search in file(s): "
                                             :default default-directory
                                             :marked-candidates t :must-match t)))))
         (helm-do-ag--extensions (when helm-ag2--default-target
                                   (helm-ag2--do-ag-searched-extensions)))
         (one-directory-p (helm-do-ag--target-one-directory-p
                           helm-ag2--default-target)))
    (helm-ag2--set-do-ag-option)
    (helm-ag2--set-command-features)
    (helm-ag2--save-current-context)
    (helm-attrset 'search-this-file
                  (and (= (length helm-ag2--default-target) 1)
                       (not (file-directory-p (car helm-ag2--default-target)))
                       (car helm-ag2--default-target))
                  helm-source-do-ag)
    (if (or (helm-ag2--windows-p) (not one-directory-p)) ;; Path argument must be specified on Windows
        (helm-do-ag--helm)
      (let* ((helm-ag2--default-directory
              (file-name-as-directory (car helm-ag2--default-target)))
             (helm-ag2--default-target nil))
        (helm-do-ag--helm)))))

(defun helm-ag2--project-root ()
  (cl-loop for dir in '(".git/" ".hg/" ".svn/" ".git")
           when (locate-dominating-file default-directory dir)
           return it))

;;;###autoload
(defun helm-ag2-project-root ()
  (interactive)
  (let ((rootdir (helm-ag2--project-root)))
    (unless rootdir
      (error "Could not find the project root. Create a git, hg, or svn repository there first. "))
    (helm-ag2 rootdir)))

;;;###autoload
(defun helm-do-ag-project-root ()
  (interactive)
  (let ((rootdir (helm-ag2--project-root)))
    (unless rootdir
      (error "Could not find the project root. Create a git, hg, or svn repository there first. "))
    (helm-do-ag rootdir)))

;;;###autoload
(defun helm-ag2-buffers ()
  (interactive)
  (let ((helm-ag2--buffer-search t))
    (helm-ag2)))

;;;###autoload
(defun helm-do-ag-buffers ()
  (interactive)
  (let ((helm-ag2--buffer-search t))
    (helm-do-ag)))

(provide 'helm-ag2)

;;; helm-ag2.el ends here