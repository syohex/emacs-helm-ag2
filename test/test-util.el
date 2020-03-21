;;; test-util.el --- test helm-ag2 utility functions

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>

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

;;; Code:

(require 'ert)
(require 'helm-ag2)
(require 'cl-lib)

(ert-deftest parse-query ()
  "Parsing input which may contains option"
  (let ((got (helm-ag2--parse-query "foo bar")))
    (should (equal got '("foo bar"))))

  (let ((got (helm-ag2--parse-query "--nogroup --column foo bar")))
    (should (equal got '("--nogroup" "--column" "foo bar"))))

  (let ((got (helm-ag2--parse-query "--column helm-ag2 ()")))
    (should (equal got '("--column" "helm-ag2 ()")))))

(ert-deftest construct-command ()
  "helm-ag2--construct--command"
  (let ((helm-ag2-base-command "ag --nocolor --nogroup")
        (helm-ag2--last-query "pattern"))
    (let ((got (helm-ag2--construct-command nil))
          (expected '("ag" "--nocolor" "--nogroup" "pattern")))
      (should (equal got expected)))))

(ert-deftest construct-command-this-file ()
  "helm-ag2--construct--command for this file"
  (let ((helm-ag2-base-command "ag --nocolor --nogroup")
        (helm-ag2--last-query "pattern"))
    (let ((got (helm-ag2--construct-command "foo.el"))
          (expected '("ag" "--nocolor" "--nogroup" "pattern" "foo.el")))
      (should (equal got expected)))))

(ert-deftest construct-command-with-options ()
  "helm-ag2--construct--command with options"
  (let ((helm-ag2-base-command "ag --nocolor --nogroup")
        (helm-ag2-command-option "--all-text --hidden -D")
        (helm-ag2--last-query "pattern"))
    (let ((got (helm-ag2--construct-command nil))
          (expected '("ag" "--nocolor" "--nogroup" "--all-text" "--hidden" "-D"
                      "pattern")))
      (should (equal got expected)))))

(ert-deftest construct-command-with-options-in-input ()
  "helm-ag2--construct--command with options in input"
  (let ((helm-ag2-base-command "ag --nocolor --nogroup")
        (helm-ag2-command-option "--all-text --hidden -D")
        (helm-ag2--last-query "-G\\.md$ foo bar"))
    (let ((got (helm-ag2--construct-command nil))
          (expected '("ag" "--nocolor" "--nogroup" "--all-text" "--hidden" "-D"
                      "-G\\.md$" "foo bar")))
      (should (equal got expected))))

  (let ((helm-ag2-base-command "ag")
        (helm-ag2-command-option "")
        (helm-ag2--last-query "-- --count"))
    (let ((got (helm-ag2--construct-command nil))
          (expected '("ag" "--" "--count")))
      (should (equal got expected))))

  (let ((helm-ag2-base-command "ag")
        (helm-ag2-command-option "")
        (helm-ag2--last-query "helm-ag2"))
    (let ((got (helm-ag2--construct-command nil))
          (expected '("ag" "helm-ag2")))
      (should (equal got expected))))

  (let ((helm-ag2-base-command "ag")
        (helm-ag2-command-option "")
        (helm-ag2--last-query "--count -G.md$ -- --count foo bar"))
    (let ((got (helm-ag2--construct-command nil))
          (expected '("ag" "--count" "-G.md$" "--" "--count foo bar")))
      (should (equal got expected)))))

(ert-deftest construct-command-with-ignore-patterns ()
  "helm-ag2--construct--command with ignore options"
  (let ((helm-ag2-base-command "ag --nocolor --nogroup")
        (helm-ag2-ignore-patterns '("*.md" "*.el"))
        (helm-ag2--last-query "foo"))
    (let ((got (helm-ag2--construct-command nil))
          (expected '("ag" "--nocolor" "--nogroup" "--ignore=*.md" "--ignore=*.el" "foo")))
      (should (equal got expected)))))

(ert-deftest validate-regexp-with-valid-regexp ()
  (should (helm-ag2--validate-regexp "[a-z]\\([[:word:]]\\)")))

(ert-deftest validate-regexp-with-invalid-regexp ()
  (should-not (helm-ag2--validate-regexp "\\(")))

(ert-deftest transform-for-this-file ()
  "helm-ag2--candidate-transform-for-this-file"
  (let ((helm-ag2--last-query "hoge"))
    (should (helm-ag2--candidate-transform-for-this-file "10:hoge"))
    (should-not (helm-ag2--candidate-transform-for-this-file ":hoge"))))

(ert-deftest transform-for-files ()
  "helm-ag2--candidate-transform-for-files"
  (let ((helm-ag2--last-query "hoge"))
    (should (helm-ag2--candidate-transform-for-files "10:5:hoge"))
    (should-not (helm-ag2--candidate-transform-for-files "10:hoge"))))

(ert-deftest pcre-to-emacs-lisp-regexp ()
  "Simple convertion from PCRE to Emacs lisp regexp"
  (let ((got (helm-ag2--pcre-to-elisp-regexp "(foo|bar)")))
    (should (string= got "\\(foo\\|bar\\)")))
  (let ((got (helm-ag2--pcre-to-elisp-regexp "foo{1,2}")))
    (should (string= got "foo\\{1,2\\}")))

  (let ((got (helm-ag2--pcre-to-elisp-regexp "\\(foo\\|bar\\)")))
    (should (string= got "(foo|bar)")))

  (let ((got (helm-ag2--pcre-to-elisp-regexp "foo\\{1,2\\}")))
    (should (string= got "foo{1,2}")))

  (let ((got (helm-ag2--pcre-to-elisp-regexp "\\\\(foo\\\\|bar\\\\)")))
    (should (string= got "\\\\(foo\\\\|bar\\\\)")))

  (let ((got (helm-ag2--pcre-to-elisp-regexp "\\S\\s\\S")))
    (should (string= got "\\S-\\s-\\S-")))

  (let ((got (helm-ag2--pcre-to-elisp-regexp "\\\\S\\\\s")))
    (should (string= got "\\\\S\\\\s"))))

(ert-deftest emacs-lisp-regexp-to-pcre ()
  "Simple convertion from Emacs lisp regexp to PCRE"
  (let ((got (helm-ag2--elisp-regexp-to-pcre "\\(foo\\|bar\\)")))
    (should (string= got "(foo|bar)")))
  (let ((got (helm-ag2--elisp-regexp-to-pcre "foo\\{1,2\\}")))
    (should (string= got "foo{1,2}")))

  (let ((got (helm-ag2--elisp-regexp-to-pcre "(foo|bar)")))
    (should (string= got "\\(foo\\|bar\\)")))

  (let ((got (helm-ag2--elisp-regexp-to-pcre "foo{1,2}")))
    (should (string= got "foo\\{1,2\\}")))

  (let ((got (helm-ag2--elisp-regexp-to-pcre "\\\\(foo\\\\|bar\\\\)")))
    (should (string= got "\\\\(foo\\\\|bar\\\\)"))))

(ert-deftest judge-ignore-case ()
  "Judge ignore case searching or not "
  (should (helm-ag2--ignore-case-p nil "aa"))
  (should (not (helm-ag2--ignore-case-p nil "AA")))
  (should (helm-ag2--ignore-case-p '("-i") "AA"))
  (should (helm-ag2--ignore-case-p '("--ignore-case") "Apple"))
  (should (not (helm-ag2--ignore-case-p '("-s") "apple")))
  (should (not (helm-ag2--ignore-case-p '("--case-sensitive") "apple"))))

(ert-deftest split-string ()
  "Own split-string function for handling escaped space"
  (should (equal (helm-ag2--split-string "aa") '("aa")))
  (should (equal (helm-ag2--split-string "  aa") '("aa")))
  (should (equal (helm-ag2--split-string "aa bb cc") '("aa" "bb" "cc")))
  (should (equal (helm-ag2--split-string "aa       bb         cc") '("aa" "bb" "cc")))
  (should (equal (helm-ag2--split-string "aa\\ bb cc") '("aa bb" "cc")))
  (should (equal (helm-ag2--split-string "aa\\\\ bb cc") '("aa\\\\" "bb" "cc"))))

(ert-deftest search-this-file-p ()
  "Ag does not show file name at searching only one file except '--vimgrep'
option specified"
  (let ((helm-ag2--last-command '("--vimgrep")))
    (should-not (helm-ag2--search-this-file-p)))

  (cl-letf (((symbol-function 'helm-get-current-source)
             (lambda () 'helm-source-ag2))
            ((symbol-function 'helm-attr)
             (lambda (attr &optional source compute)
               t)))
    (should (helm-ag2--search-this-file-p))))

(ert-deftest visited-buffers ()
  "Remove buffers which are matched against helm-ag2-ignore-buffer-patterns"
  (cl-letf (((symbol-function 'buffer-list)
             (lambda () '("aa.txt" "bb.md" "cc.el")))
            ((symbol-function 'buffer-file-name)
             (lambda (b) b))
            ((symbol-function 'buffer-name)
             (lambda (b) b)))
    (let* ((helm-ag2-ignore-buffer-patterns nil)
           (got (helm-ag2--file-visited-buffers)))
      (should (equal got '("aa.txt" "bb.md" "cc.el"))))

    (let* ((helm-ag2-ignore-buffer-patterns '("\\.md\\'" "\\`cc"))
           (got (helm-ag2--file-visited-buffers)))
      (should (equal got '("aa.txt"))))))

;;; test-util.el ends here
