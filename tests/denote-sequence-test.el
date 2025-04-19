;;; denote-test.el --- Unit tests for denote-sequence.el -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for denote-sequence.el.  Note that we are using Shorthands in
;; this file, so the "dst-" prefix really is "denote-sequence-test-".
;; Evaluate the following to learn more:
;;
;;    (info "(elisp) Shorthands")

;;; Code:

(require 'ert)
(require 'denote-sequence)

(defun dst-relative-p (sequence type &rest files)
  "Return non-nil if FILES are relatives of SEQUENCE given TYPE."
  (when-let* ((relatives (denote-sequence-get-relative sequence type files))
              (found (seq-filter
                      (lambda (file)
                        (member file relatives))
                      (mapcar (lambda (file) (expand-file-name file denote-directory)) files))))
    (>= (length files) (length found))))

(ert-deftest dst-denote-sequence--get-new-exhaustive ()
  "Test if we get the correct parent, child, sibling, or relatives of a sequence.
Use the function `denote-sequence-get-new' for child and sibling with
the numeric and alphanumeric `denote-sequence-scheme', as well as the
function `denote-sequence-get-relative'."
  (let* ((denote-sequence-scheme 'numeric)
         (denote-directory (expand-file-name "denote-test" temporary-file-directory))
         (files
          (mapcar
           (lambda (file)
             (let ((path (expand-file-name file (denote-directory))))
               (if (file-exists-p path)
                   path
                 (with-current-buffer (find-file-noselect path)
                   (save-buffer)
                   (kill-buffer (current-buffer)))
                 path)))
           '("20241230T075023==1--test__testing.txt"
             "20241230T075023==1=1--test__testing.txt"
             "20241230T075023==1=1=1--test__testing.txt"
             "20241230T075023==1=1=2--test__testing.txt"
             "20241230T075023==1=2--test__testing.txt"
             "20241230T075023==1=2=1--test__testing.txt"
             "20241230T075023==1=2=1=1--test__testing.txt"
             "20241230T075023==2--test__testing.txt"
             "20241230T075023==10--test__testing.txt"
             "20241230T075023==10=1--test__testing.txt"
             "20241230T075023==10=1=1--test__testing.txt"
             "20241230T075023==10=2--test__testing.txt"
             "20241230T075023==10=10--test__testing.txt"
             "20241230T075023==10=10=1--test__testing.txt")))
         (sequences (denote-sequence-get-all-sequences files)))
    (should (string= (denote-sequence-get-new 'parent) "11"))

    (should (string= (denote-sequence-get-new 'child "1" sequences) "1=3"))
    (should (string= (denote-sequence-get-new 'child "1=1" sequences) "1=1=3"))
    (should (string= (denote-sequence-get-new 'child "1=1=2" sequences) "1=1=2=1"))
    (should (string= (denote-sequence-get-new 'child "1=2" sequences) "1=2=2"))
    (should (string= (denote-sequence-get-new 'child "1=2=1" sequences) "1=2=1=2"))
    (should (string= (denote-sequence-get-new 'child "2" sequences) "2=1"))
    (should-error (denote-sequence-get-new 'child "11" sequences))

    (should (string= (denote-sequence-get-new 'sibling "1" sequences) "11"))
    (should (string= (denote-sequence-get-new 'sibling "1=1" sequences) "1=3"))
    (should (string= (denote-sequence-get-new 'sibling "1=1=1" sequences) "1=1=3"))
    (should (string= (denote-sequence-get-new 'sibling "1=1=2" sequences) "1=1=3"))
    (should (string= (denote-sequence-get-new 'sibling "1=2" sequences) "1=3"))
    (should (string= (denote-sequence-get-new 'sibling "1=2=1" sequences) "1=2=2"))
    (should (string= (denote-sequence-get-new 'sibling "2" sequences) "11"))
    (should-error (denote-sequence-get-new 'sibling "12" sequences))

    (should (string= (denote-sequence-get-relative "1=2=1=1" 'parent files)
                     (expand-file-name "20241230T075023==1=2=1--test__testing.txt" denote-directory)))
    (should (string= (denote-sequence-get-relative "10=1=1" 'parent files)
                     (expand-file-name "20241230T075023==10=1--test__testing.txt" denote-directory)))
    (should (string= (denote-sequence-get-relative "10=10=1" 'parent files)
                     (expand-file-name "20241230T075023==10=10--test__testing.txt" denote-directory)))
    (should (dst-relative-p "1=2=1=1" 'all-parents
                            "20241230T075023==1--test__testing.txt"
                            "20241230T075023==1=2--test__testing.txt"
                            "20241230T075023==1=2=1--test__testing.txt"))
    (should (dst-relative-p "10=1=1" 'all-parents
                            "20241230T075023==10--test__testing.txt"
                            "20241230T075023==10=1--test__testing.txt"))
    (should (dst-relative-p "10=10=1" 'all-parents
                            "20241230T075023==10--test__testing.txt"
                            "20241230T075023==10=10--test__testing.txt"))
    (should (dst-relative-p "1=1" 'siblings
                            "20241230T075023==1=1--test__testing.txt"
                            "20241230T075023==1=2--test__testing.txt"))
    (should (dst-relative-p "10=1" 'siblings
                            "20241230T075023==10=1--test__testing.txt"
                            "20241230T075023==10=10--test__testing.txt"
                            "20241230T075023==10=2--test__testing.txt"))
    (should (dst-relative-p "1" 'children
                            "20241230T075023==1=1--test__testing.txt"
                            "20241230T075023==1=2--test__testing.txt"))
    (should (dst-relative-p "10" 'children
                            "20241230T075023==10=1--test__testing.txt"
                            "20241230T075023==10=2--test__testing.txt"
                            "20241230T075023==10=10--test__testing.txt"))
    (should (dst-relative-p "1=1" 'all-children
                            "20241230T075023==1=1=1--test__testing.txt"
                            "20241230T075023==1=1=2--test__testing.txt")))

  (let* ((denote-sequence-scheme 'alphanumeric)
         (denote-directory (expand-file-name "denote-test" temporary-file-directory))
         (files
          (mapcar
           (lambda (file)
             (let ((path (expand-file-name file (denote-directory))))
               (if (file-exists-p path)
                   path
                 (with-current-buffer (find-file-noselect path)
                   (save-buffer)
                   (kill-buffer (current-buffer)))
                 path)))
           '("20241230T075023==1--test__testing.txt"
             "20241230T075023==1a--test__testing.txt"
             "20241230T075023==1a1--test__testing.txt"
             "20241230T075023==1a2--test__testing.txt"
             "20241230T075023==1b--test__testing.txt"
             "20241230T075023==1b1--test__testing.txt"
             "20241230T075023==1b1a--test__testing.txt"
             "20241230T075023==2--test__testing.txt"
             "20241230T075023==10--test__testing.txt"
             "20241230T075023==10a--test__testing.txt"
             "20241230T075023==10b--test__testing.txt")))
         (sequences (denote-sequence-get-all-sequences files)))
    (should (string= (denote-sequence-get-new 'parent) "11"))

    (should (string= (denote-sequence-get-new 'child "1" sequences) "1c"))
    (should (string= (denote-sequence-get-new 'child "1a" sequences) "1a3"))
    (should (string= (denote-sequence-get-new 'child "1a2" sequences) "1a2a"))
    (should (string= (denote-sequence-get-new 'child "1b" sequences) "1b2"))
    (should (string= (denote-sequence-get-new 'child "1b1" sequences) "1b1b"))
    (should (string= (denote-sequence-get-new 'child "2" sequences) "2a"))
    (should-error (denote-sequence-get-new 'child "11" sequences))

    (should (string= (denote-sequence-get-new 'sibling "1" sequences) "11"))
    (should (string= (denote-sequence-get-new 'sibling "1a" sequences) "1c"))
    (should (string= (denote-sequence-get-new 'sibling "1a1" sequences) "1a3"))
    (should (string= (denote-sequence-get-new 'sibling "1a2" sequences) "1a3"))
    (should (string= (denote-sequence-get-new 'sibling "1b" sequences) "1c"))
    (should (string= (denote-sequence-get-new 'sibling "1b1" sequences) "1b2"))
    (should (string= (denote-sequence-get-new 'sibling "2" sequences) "11"))
    (should-error (denote-sequence-get-new 'sibling "12" sequences))

    (should (string= (denote-sequence-get-relative "1b1a" 'parent files)
                     (expand-file-name "20241230T075023==1b1--test__testing.txt" denote-directory)))
    (should (string= (denote-sequence-get-relative "10a" 'parent files)
                     (expand-file-name "20241230T075023==10--test__testing.txt" denote-directory)))
    (should (dst-relative-p "1b1a" 'all-parents
                            "20241230T075023==1--test__testing.txt"
                            "20241230T075023==1b--test__testing.txt"
                            "20241230T075023==1b1--test__testing.txt"))
    (should (dst-relative-p "1a" 'siblings
                            "20241230T075023==1a--test__testing.txt"
                            "20241230T075023==1b--test__testing.txt"))
    (should (dst-relative-p "10a" 'siblings
                            "20241230T075023==10a--test__testing.txt"
                            "20241230T075023==10b--test__testing.txt"))
    (should (dst-relative-p "1" 'children
                            "20241230T075023==1a--test__testing.txt"
                            "20241230T075023==1b--test__testing.txt"))
    (should (dst-relative-p "10" 'children
                            "20241230T075023==10a--test__testing.txt"
                            "20241230T075023==10b--test__testing.txt"))
    (should (dst-relative-p "1a" 'all-children
                            "20241230T075023==1a1--test__testing.txt"
                            "20241230T075023==1a2--test__testing.txt"))))

(ert-deftest dst-denote-sequence-split ()
  "Test that `denote-sequence-split' splits a sequence correctly."
  (should (equal (denote-sequence-split "1") '("1")))
  (should (equal (denote-sequence-split "1=1=2") '("1" "1" "2")))
  (should (equal (denote-sequence-split "1za5zx") '("1" "za" "5" "zx"))))

(ert-deftest dst-denote-sequence-make-conversion ()
  "Test that `denote-sequence-make-conversion' converts from alpha to numeric and vice versa."
  (should (string= (denote-sequence-make-conversion "3") "c"))
  (should (string= (denote-sequence-make-conversion "18") "r"))
  (should (string= (denote-sequence-make-conversion "26") "z"))
  (should (string= (denote-sequence-make-conversion "27") "za"))
  (should (string= (denote-sequence-make-conversion "130") "zzzzz"))
  (should (string= (denote-sequence-make-conversion "131") "zzzzza"))
  (should (string= (denote-sequence-make-conversion "c") "3"))
  (should (string= (denote-sequence-make-conversion "r") "18"))
  (should (string= (denote-sequence-make-conversion "z") "26"))
  (should (string= (denote-sequence-make-conversion "za") "27"))
  (should (string= (denote-sequence-make-conversion "zzzzz") "130"))
  (should (string= (denote-sequence-make-conversion "zzzzza") "131"))
  (should (string= (denote-sequence-make-conversion "1=1=2" :string-is-sequence) "1a2"))
  (should (string= (denote-sequence-make-conversion "1a2" :string-is-sequence) "1=1=2"))
  (should (string= (denote-sequence-make-conversion "1=27=2=55" :string-is-sequence) "1za2zzc"))
  (should (string= (denote-sequence-make-conversion "1za2zzc" :string-is-sequence) "1=27=2=55"))
  (should (string= (denote-sequence-make-conversion "1=1=2=2=4=1" :string-is-sequence) "1a2b4a"))
  (should (string= (denote-sequence-make-conversion "1a2b4a" :string-is-sequence) "1=1=2=2=4=1"))
  (should-error (denote-sequence-make-conversion "111=a" :string-is-sequence))
  (should-error (denote-sequence-make-conversion "a1" :string-is-sequence)))

(ert-deftest dst-denote-sequence-increment ()
  "Test that `denote-sequence-increment' works with numbers and letters."
  (should (string= (denote-sequence-increment "z") "za"))
  (should (string= (denote-sequence-increment "ab") "ac"))
  (should (string= (denote-sequence-increment "az") "aza"))
  (should (string= (denote-sequence-increment "bbcz") "bbcza"))
  (should (string= (denote-sequence-increment "1") "2"))
  (should (string= (denote-sequence-increment "10") "11"))
  (should-error (denote-sequence-increment "1=a")))

(provide 'denote-test)
;;; denote-test.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("dst" . "denote-sequence-test-"))
;; End:
