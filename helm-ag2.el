;;; helm-ag2.el --- the silver searcher with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-ag2
;; Version: 0.58
;; Package-Requires: ((emacs "26.3") (helm "3.6.0"))

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
  (defvar helm-help-message))

(require 'cl-lib)
(require 'helm)
(require 'helm-grep)
(require 'helm-occur)
(require 'helm-utils)
(require 'compile)
(require 'subr-x)

(declare-function helm-help "helm-help")

(defgroup helm-ag2 nil
  "the silver searcher with helm interface"
  :group 'helm)

(defsubst helm-ag2--windows-p ()
  (memq system-type '(ms-dos windows-nt)))

(defcustom helm-ag2-base-command
  (if (helm-ag2--windows-p)
      '("ag" "--vimgrep")
    '("ag" "--nocolor" "--nogroup"))
  "Base command of `ag'"
  :type '(repeat (string)))

(defcustom helm-ag2-insert-at-point 'symbol
  "Insert thing at point as search pattern.
   You can set value same as `thing-at-point'"
  :type 'symbol)

(defcustom helm-ag2-edit-save t
  "Save buffers you edit at completed."
  :type 'boolean)

(defface helm-ag2-edit-deleted-line
  '((t (:inherit font-lock-comment-face :strike-through t)))
  "Face of deleted line in edit mode.")

(defface helm-ag2-edit-edited-line
  '((((class color) (background light))
     :background "aquamarine" :foreground "black")
    (((class color) (background dark))
     :background "limegreen" :foreground "black")
    (t :inverse-video t))
  "Face of edited line in edit mode")

(defvar helm-ag2--command-history '())
(defvar helm-ag2--helm-history '())
(defvar helm-ag2--context-stack nil)
(defvar helm-ag2--default-directory nil)
(defvar helm-ag2--last-default-directory nil)
(defvar helm-ag2--last-query nil)
(defvar helm-ag2--last-command nil)
(defvar helm-ag2--elisp-regexp-query nil)
(defvar helm-ag2--valid-regexp-for-emacs nil)
(defvar helm-ag2--original-window nil)
(defvar helm-ag2--default-target nil)
(defvar helm-ag2--ignore-case nil)

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

(defun helm-ag2--parse-options-and-query (input)
  (with-temp-buffer
    (insert input)
    (let (end options)
      (goto-char (point-min))
      (when (re-search-forward "\\s-*--\\s-+" nil t)
        (setq end (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward "\\(?:\\=\\|\\s-+\\)\\(-\\S-+\\)\\(?:\\s-+\\|$\\)" end t)
        (push (match-string-no-properties 1) options)
        (when end
          (cl-decf end (- (match-end 0) (match-beginning 0))))
        (replace-match ""))
      (cons options (buffer-string)))))

(defun helm-ag2--parse-query (input)
  (let* ((parsed (helm-ag2--parse-options-and-query input))
         (options (car parsed))
         (query (cdr parsed)))
    (setq helm-ag2--last-query query
          helm-ag2--elisp-regexp-query (helm-ag2--pcre-to-elisp-regexp query))
    (setq helm-ag2--valid-regexp-for-emacs
          (helm-ag2--validate-regexp helm-ag2--elisp-regexp-query))
    (if (not options)
        (list (helm-ag2--join-patterns query))
      (nconc (nreverse options) (list (helm-ag2--join-patterns query))))))

(defun helm-ag2--construct-targets (targets)
  (let ((default-directory helm-ag2--default-directory))
    (cl-loop for target in targets
             collect (file-relative-name target))))

(defun helm-ag2--construct-command ()
  (let ((command (car helm-ag2-base-command))
        (args (cdr helm-ag2-base-command)))
    (setq args (append args (helm-ag2--parse-query helm-ag2--last-query)))
    (when helm-ag2--default-target
      (setq args (append args (helm-ag2--construct-targets helm-ag2--default-target))))
    (cons command args)))

(defun helm-ag2--remove-carrige-returns ()
  (when (helm-ag2--windows-p)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\xd" nil t)
        (replace-match "")))))

(defun helm-ag2--init ()
  (let ((buf-coding buffer-file-coding-system))
    (helm-attrset 'recenter t)
    (with-current-buffer (helm-candidate-buffer 'global)
      (let* ((default-directory (or helm-ag2--default-directory
                                    default-directory))
             (cmds (helm-ag2--construct-command))
             (search-command (car cmds))
             (coding-system-for-read buf-coding)
             (coding-system-for-write buf-coding))
        (setq helm-ag2--ignore-case (helm-ag2--ignore-case-p cmds helm-ag2--last-query)
              helm-ag2--last-command cmds)
        (let ((ret (apply #'process-file search-command nil t nil (cdr cmds))))
          (if (zerop (length (buffer-string)))
              (error "No output: '%s'" helm-ag2--last-query)
            (unless (zerop ret)
              (unless (executable-find search-command)
                (error "'%s' is not installed" search-command))
              (error "Failed: '%s'" helm-ag2--last-query))))
        (helm-ag2--remove-carrige-returns)
        (helm-ag2--save-current-context)))))

(add-to-list 'debug-ignored-errors "^No ag output: ")

(defsubst helm-ag2--vimgrep-option ()
  (member "--vimgrep" helm-ag2--last-command))

(defun helm-ag2--search-only-one-file-p ()
  (when (and (not (helm-ag2--vimgrep-option)) (assoc 'single-file (helm-get-current-source)))
    (when (= (length helm-ag2--default-target) 1)
      (let ((target (car helm-ag2--default-target)))
        (unless (file-directory-p target)
          target)))))

(defun helm-ag2--find-file-action (candidate find-func &optional persistent)
  (let* ((one-file (helm-ag2--search-only-one-file-p))
	 (file-line (helm-grep-split-line candidate))
         (filename (or one-file (cl-first file-line) candidate))
         (line (if one-file
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

(defun helm-ag2--persistent-action (candidate)
  (let* ((helm-ag2-p (assoc-default 'real-to-display (helm-get-current-source))))
    (helm-ag2--find-file-action candidate #'find-file t)
    (let ((helm-input (if helm-ag2-p
                          (concat helm-ag2--last-query " " helm-input)
                        helm-input)))
      (helm-highlight-current-line))))

(defun helm-ag2--validate-regexp (regexp)
  (condition-case nil
      (progn
        (string-match-p regexp "")
        t)
    (invalid-regexp nil)))

(defun helm-ag2--pcre-to-elisp-regexp (pcre)
  (if (string-match-p "\\s-+" pcre)
      (string-join (split-string pcre " " t) "\\|")
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
      (buffer-string))))

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
            (setq last-pos (1+ end))))))
    candidate))

(defun helm-ag2--candidate-transform-for-files (candidate)
  (helm-aif (helm-grep-split-line candidate)
      (format "%s:%s:%s"
              (propertize (cl-first it) 'face 'helm-moccur-buffer)
              (propertize (cl-second it) 'face 'helm-grep-lineno)
              (helm-ag2--highlight-candidate (cl-third it)))))

(defun helm-ag2--candidate-transformer (candidate)
  (or (helm-ag2--candidate-transform-for-files candidate)
      candidate))

(defun helm-ag2--action-find-file (candidate)
  (helm-ag2--find-file-action candidate #'find-file))

(defun helm-ag2--action-find-file-other-window (candidate)
  (helm-ag2--find-file-action candidate #'find-file-other-window))

(defvar helm-ag2--actions
  (helm-make-actions
   "Open file"              #'helm-ag2--action-find-file
   "Open file other window" #'helm-ag2--action-find-file-other-window
   "Save results in buffer" #'helm-ag2--action-save-buffer
   "Edit search results"    #'helm-ag2--edit))

(defvar helm-ag2-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") #'helm-ag2--run-other-window-action)
    (define-key map (kbd "C-l") #'helm-ag2--up-one-level)
    (define-key map (kbd "C-c C-e") #'helm-ag2-edit)
    (define-key map (kbd "C-x C-s") #'helm-ag2--run-save-buffer)
    (define-key map (kbd "C-c >") #'helm-ag2--next-file)
    (define-key map (kbd "<right>") #'helm-ag2--next-file)
    (define-key map (kbd "C-c <") #'helm-ag2--previous-file)
    (define-key map (kbd "<left>") #'helm-ag2--previous-file)
    map)
  "Keymap for `helm-ag2'.")

(defvar helm-ag2-source
  (helm-build-in-buffer-source "The Silver Searcher"
    :init #'helm-ag2--init
    :real-to-display #'helm-ag2--candidate-transformer
    :persistent-action #'helm-ag2--persistent-action
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

(defun helm-ag2--marked-input ()
  (when (use-region-p)
    (let ((input (buffer-substring-no-properties (region-beginning) (region-end))))
      (deactivate-mark)
      input)))

(defun helm-ag2--query ()
  (let* ((searched-word (helm-ag2--searched-word))
         (marked-word (helm-ag2--marked-input))
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

(defun helm-ag2--helm-header (dir)
  (concat "Search at " (abbreviate-file-name dir)))

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
      (let* ((file (match-string-no-properties 1))
             (line (string-to-number (match-string-no-properties 2)))
             (body (match-string-no-properties 3))
             (ovs (overlays-at (line-beginning-position))))
        (with-current-buffer (or (get-file-buffer file)
                                 (find-file-noselect file nil t))
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
    (define-key map (kbd "C-c C-c") #'helm-ag2--edit-commit)
    (define-key map (kbd "C-c C-k") #'helm-ag2--edit-abort)
    (define-key map (kbd "C-c C-d") #'helm-ag2--mark-line-deleted)
    (define-key map (kbd "C-c C-u") #'helm-ag2--unmark)
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
                  (body-end (match-end 3))
                  (ov (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put ov 'helm-ag2-edit-line t)
              (overlay-put ov 'orig-content (buffer-substring-no-properties body-begin body-end))
              (overlay-put ov 'start-offset (- body-begin (line-beginning-position)))
              (add-text-properties file-line-begin file-line-end
                                   '(face font-lock-function-name-face
                                          intangible t))
              (remove-text-properties body-begin body-end '(read-only t))
              (set-text-properties body-end (1+ body-end)
                                   '(read-only t rear-nonsticky t))))))))
  (other-window 1)
  (switch-to-buffer (get-buffer "*helm-ag2-edit*"))
  (goto-char (point-min))
  (setq next-error-function #'compilation-next-error-function)
  (setq-local compilation-locs (make-hash-table :test 'equal :weakness 'value))
  (add-hook 'after-change-functions #'helm-ag2--after-change-function nil t)
  (use-local-map helm-ag2-edit-map))

(defun helm-ag2--edit-line-info (start end)
  (cl-loop for o in (overlays-in start end)
           when (overlay-get o 'helm-ag2-edit-line)
           return o))

(defun helm-ag2--after-change-function (_beg _end _len)
  (save-excursion
    (save-match-data
      (let* ((line-start (line-beginning-position))
             (line-end (line-end-position))
             (line-info-ov (helm-ag2--edit-line-info line-start line-end))
             (ov (cl-loop for o in (overlays-in line-start line-end)
                          when (overlay-get o 'helm-ag2-edited)
                          return o)))
        (when line-info-ov
          (let* ((orig (overlay-get line-info-ov 'orig-content))
                 (offset (overlay-get line-info-ov 'start-offset))
                 (current (buffer-substring-no-properties (+ line-start offset) line-end)))
            (if (not (string= orig current))
                (if ov
                    (move-overlay ov line-start line-end)
                  (let ((o (make-overlay line-start line-end)))
                    (overlay-put o 'helm-ag2-edited t)
                    (overlay-put o 'face 'helm-ag2-edit-edited-line)))
              (when ov
                (delete-overlay ov)))))))))

(defun helm-ag2-edit ()
  (interactive)
  (helm-exit-and-execute-action 'helm-ag2--edit))

(defun helm-ag2-mode-jump ()
  (interactive)
  (let ((line (helm-current-line-contents)))
    (helm-ag2--find-file-action line #'find-file)))

(defun helm-ag2-mode-jump-other-window ()
  (interactive)
  (let ((line (helm-current-line-contents)))
    (helm-ag2--find-file-action line #'find-file-other-window)))

(defvar helm-ag2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'helm-ag2-mode-jump)
    (define-key map (kbd "C-o") #'helm-ag2-mode-jump-other-window)
    (define-key map (kbd "g") #'helm-ag2--update-save-results)
    map))

(define-derived-mode helm-ag2-mode special-mode "helm-ag2"
  "Major mode to provide actions in helm grep saved buffer.

Special commands:
\\{helm-ag2-mode-map}")

(defun helm-ag2--put-result-in-save-buffer (result)
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "-*- mode: helm-ag2 -*-\n\n"
            (format "Ag Results for `%s':\n\n" helm-ag2--last-query))
    (save-excursion
      (insert result)))
  (helm-ag2-mode)
  (setq-local helm-ag2--default-directory default-directory))

(defun helm-ag2--save-results (use-other-buf)
  (let* ((result (with-current-buffer helm-buffer
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
      (helm-ag2--put-result-in-save-buffer result)
      (pop-to-buffer buf)
      (message "Helm Ag Results saved in `%s' buffer" buf))))

(defun helm-ag2--update-save-results ()
  (interactive)
  (let* ((default-directory helm-ag2--default-directory)
         (result (with-temp-buffer
                   (apply #'process-file (car helm-ag2--last-command) nil t nil
                          (cdr helm-ag2--last-command))
                   (helm-ag2--remove-carrige-returns)
                   (helm-ag2--propertize-candidates helm-ag2--last-query)
                   (buffer-string))))
    (helm-ag2--put-result-in-save-buffer result)
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
  (file-directory-p ".git/"))

(defun helm-ag2--up-one-level ()
  (interactive)
  (if (or (not (helm-ag2--root-directory-p))
          (y-or-n-p "Here is project root. Continue searching the parent dir ? "))
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
(defun helm-ag2 (&optional basedir)
  (interactive)
  (helm-ag2--init-state)
  (let ((helm-ag2--default-directory (or basedir default-directory)))
    (helm-ag2--query)
    (helm-attrset 'name (helm-ag2--helm-header helm-ag2--default-directory) helm-ag2-source)
    (helm :sources '(helm-ag2-source) :buffer "*helm-ag2*" :keymap helm-ag2-map
          :history 'helm-ag2--helm-history)))

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
  (when (and (string-prefix-p "!" pattern) (> (length pattern) 1))
    (concat "^(?!.*" (substring pattern 1) ").+$")))

(defun helm-ag2--join-patterns (input)
  (let ((patterns (helm-ag2--split-string input)))
    (if (= (length patterns) 1)
        (or (helm-ag2--convert-invert-pattern (car patterns))
            (car patterns))
      (cl-loop for s in patterns
               if (helm-ag2--convert-invert-pattern s)
               concat (concat "(?=" it ")")
               else
               concat (concat "(?=.*" s ".*)")))))

(defun helm-ag2--highlight-patterns (input)
  (cl-loop with regexp = (helm-ag2--pcre-to-elisp-regexp input)
           for pattern in (helm-ag2--split-string regexp)
           when (helm-ag2--validate-regexp pattern)
           collect pattern))

(defun helm-ag2--propertize-candidates (input)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((patterns (helm-ag2--highlight-patterns input)))
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

(defun helm-ag2--project-root ()
  (cl-loop for dir in '(".git/" ".git") ;; consider symlink case
           when (locate-dominating-file default-directory dir)
           return it))

;;;###autoload
(defun helm-ag2-project-root ()
  (interactive)
  (let ((rootdir (helm-ag2--project-root)))
    (unless rootdir
      (error "Could not find the project root. You need to 'git init'"))
    (helm-ag2 rootdir)))

(defvar helm-do-ag2--commands)

(defun helm-ag2--do-ag-set-command ()
  (setq helm-do-ag2--commands
        (cons helm-ag2-base-command (helm-ag2--construct-targets helm-ag2--default-target))))

(defun helm-ag2--do-ag-propertize (input)
  (with-helm-window
    (helm-ag2--remove-carrige-returns)
    (helm-ag2--propertize-candidates input)))

(defun helm-ag2--construct-do-ag-command (pattern)
  (let* ((opt-query (helm-ag2--parse-options-and-query pattern))
         (options (car opt-query))
         (query (cdr opt-query)))
    (unless (string-empty-p query)
      (append (car helm-do-ag2--commands)
              options
              (list (helm-ag2--join-patterns query))
              (cdr helm-do-ag2--commands)))))

(defun helm-ag2--do-ag-candidate-process (dir)
  (let* ((non-essential nil)
         (default-directory dir)
         (cmd-args (helm-ag2--construct-do-ag-command helm-pattern)))
    (when cmd-args
      (let ((proc (apply #'start-file-process "helm-do-ag2" nil cmd-args)))
        (setq helm-ag2--last-query helm-pattern
              helm-ag2--last-command cmd-args
              helm-ag2--ignore-case (helm-ag2--ignore-case-p cmd-args helm-pattern)
              helm-ag2--last-default-directory default-directory)
        (prog1 proc
          (set-process-sentinel
           proc
           (lambda (process event)
             (let ((status-code (process-exit-status proc)))
               (unless (and status-code (zerop status-code))
                 (helm-process-deferred-sentinel-hook
                  process event (helm-default-directory)))))))))))

(defvar helm-do-ag2-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-ag2-map)
    map)
  "Keymap for `helm-do-ag2'.")

(defun helm-ag2--highlight-string-matched (str patterns)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (dolist (pattern patterns)
      (let ((last-point (point)))
        (while (re-search-forward pattern nil t)
          (set-text-properties (match-beginning 0) (match-end 0)
                               '(face helm-match))
          (when (= last-point (point))
            (forward-char 1))
          (setq last-point (point)))))
    (buffer-string)))

(defun helm-ag2--filter-one (candidate input)
  (let ((patterns (helm-ag2--highlight-patterns input))
        (one-file-p (and (not (helm-ag2--vimgrep-option))
                         (helm-ag2--search-only-one-file-p))))
    (if one-file-p
        (if (string-match "^\\([^:]+\\):\\(.*\\)$" candidate)
            (cons (concat (propertize (match-string-no-properties 1 candidate)
                                      'face 'helm-grep-lineno)
                          ":"
                          (helm-ag2--highlight-string-matched
                           (match-string-no-properties 2 candidate) patterns))
                  candidate)
          candidate)
      (let* ((split (helm-grep-split-line candidate))
             (file (nth 0 split))
             (lineno (nth 1 split))
             (str (nth 2 split)))
        (if (and lineno str)
            (cons (concat (propertize file 'face 'helm-moccur-buffer)
                          ":"
                          (propertize lineno 'face 'helm-grep-lineno)
                          ":"
                          (helm-ag2--highlight-string-matched str patterns))
                  candidate)
          candidate)))))

(defun helm-do-ag2--filter-one-by-one (candidate)
  (save-excursion
    (if (consp candidate)
        candidate
      (when (stringp candidate)
        (helm-ag2--filter-one candidate helm-input)))))

(defclass helm-do-ag2-class (helm-source-async)
  ((nohighlight :initform t)
   (keymap :initform helm-do-ag2-map)
   (history :initform 'helm-ag2--helm-history)
   (filter-one-by-one :initform 'helm-do-ag2--filter-one-by-one)
   (candidate-number-limit :initform 99999)
   (requires-pattern :initform 3)
   (nomark :initform t)
   (action :initform 'helm-ag2--actions)
   (persistent-action :initform 'helm-ag2--persistent-action)
   (group :initform 'helm-grep)))

(defvar helm-source-do-ag2 nil)

(defun helm-do-ag2--helm (single-file)
  (let ((dir (or helm-ag2--default-directory
                 helm-ag2--last-default-directory
                 default-directory)))
    (setq helm-source-do-ag2
          (helm-make-source "AG" 'helm-do-ag2-class
            :candidates-process (lambda ()
                                  (helm-ag2--do-ag-set-command)
                                  (helm-ag2--do-ag-candidate-process dir))
            :header-name (lambda (_name)
                           (helm-ag2--helm-header dir))
            :follow (and helm-follow-mode-persistent 1)))
    (helm-attrset 'single-file single-file helm-source-do-ag2)
    (helm-set-local-variable 'helm-input-idle-delay helm-grep-input-idle-delay)
    (helm :sources 'helm-source-do-ag2 :buffer "*helm-ag2*"
          :input (or (helm-ag2--marked-input)
                     (helm-ag2--insert-thing-at-point helm-ag2-insert-at-point)))))

;;;###autoload
(defun helm-do-ag2-this-file ()
  (interactive)
  (unless (buffer-file-name)
    (error "Error: This buffer is not visited file."))
  (let ((helm-follow-mode-persistent t)
        (helm-ag2-insert-at-point nil))
    (helm-do-ag2 (list (buffer-file-name)))))

;;;###autoload
(defun helm-do-ag2 (&optional targets)
  (interactive)
  (require 'helm-mode)
  (helm-ag2--init-state)
  (let* ((helm-ag2--default-directory default-directory)
         (helm-ag2--default-target (cond (targets targets)
                                         (t (list default-directory))))
         (single-file (and (= (length helm-ag2--default-target) 1)
                           (not (file-directory-p (car helm-ag2--default-target))))))
    (helm-ag2--save-current-context)
    (if (or (helm-ag2--windows-p) targets) ;; Path argument must be specified on Windows
        (helm-do-ag2--helm single-file)
      (let* ((helm-ag2--default-directory
              (file-name-as-directory (car helm-ag2--default-target)))
             (helm-ag2--default-target nil))
        (helm-do-ag2--helm single-file)))))

(provide 'helm-ag2)

;;; helm-ag2.el ends here
