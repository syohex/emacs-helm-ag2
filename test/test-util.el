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
  (let ((got (helm-ag2--parse-query "foo")))
    (should (equal got '("foo"))))

  (let ((got (helm-ag2--parse-query "--nogroup --column foo")))
    (should (equal got '("--nogroup" "--column" "foo"))))

  (let ((got (helm-ag2--parse-query "--column helm-ag2")))
    (should (equal got '("--column" "helm-ag2"))))

  (let ((got (helm-ag2--parse-query "query -a -b")))
    (should (equal got '("-a" "-b" "query")))))

(ert-deftest construct-command ()
  "helm-ag2--construct--command"
  (let ((helm-ag2-base-command '("ag" "--nocolor" "--nogroup"))
        (helm-ag2--last-query "pattern"))
    (let ((got (helm-ag2--construct-command))
          (expected '("ag" "--nocolor" "--nogroup" "pattern")))
      (should (equal got expected)))))

(ert-deftest construct-command-with-options-in-input ()
  "helm-ag2--construct--command with options in input"
  (let ((helm-ag2-base-command '("ag" "--nocolor" "--nogroup"))
        (helm-ag2--last-query "-G\\.md$ foo"))
    (let ((got (helm-ag2--construct-command))
          (expected '("ag" "--nocolor" "--nogroup" "-G\\.md$" "foo")))
      (should (equal got expected))))

  (let ((helm-ag2-base-command '("ag"))
        (helm-ag2--last-query "-- --count"))
    (let ((got (helm-ag2--construct-command))
          (expected '("ag" "--" "--count")))
      (should (equal got expected))))

  (let ((helm-ag2-base-command '("ag"))
        (helm-ag2--last-query "helm-ag2"))
    (let ((got (helm-ag2--construct-command))
          (expected '("ag" "helm-ag2")))
      (should (equal got expected))))

  (let ((helm-ag2-base-command '("ag"))
        (helm-ag2--last-query "--count -G.md$ -- --count"))
    (let ((got (helm-ag2--construct-command))
          (expected '("ag" "--count" "-G.md$" "--" "--count")))
      (should (equal got expected)))))

(ert-deftest validate-regexp-with-valid-regexp ()
  (should (helm-ag2--validate-regexp "[a-z]\\([[:word:]]\\)")))

(ert-deftest validate-regexp-with-invalid-regexp ()
  (should-not (helm-ag2--validate-regexp "\\(")))

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
    (should (string= got "\\\\S\\\\s")))

  (let ((got (helm-ag2--pcre-to-elisp-regexp "foo bar")))
    (should (string= got "foo\\|bar"))))

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

(ert-deftest join-patterns ()
  "join pattern"
  (should (string= (helm-ag2--join-patterns "abc") "abc"))
  (should (string= (helm-ag2--join-patterns "!abc") "^(?!.*abc).+$")))

;;; test-util.el ends here
