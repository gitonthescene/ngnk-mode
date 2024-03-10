;;; ngnk-mode.el --- major-mode and a few utilities for working with ngn/k. -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Douglas Mennella <douglas.mennella@gmail.com>
;; 
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;
;; Author: Douglas Mennella <douglas.mennella@gmail.com>
;; Created: January 13th, 2024
;; Keywords: ngn/k, k, emacs-mode
;; URL: https://github.com/gitonthescene/ngnk-mode
;;
;; Version: 0.1
;;
;;
(require 'cl-lib)

(defgroup ngnk nil "ngn/k language editing mode."
  :group 'languages
  :prefix "ngnk-")

(defvar ngnk-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\/ "." table)
    (modify-syntax-entry ?\_ "." table)
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?\$ "." table)
    (modify-syntax-entry ?\% "." table)
    (modify-syntax-entry ?\& "." table)
    (modify-syntax-entry ?\+ "." table)
    (modify-syntax-entry ?\, "." table)
    (modify-syntax-entry ?\- "." table)
    (modify-syntax-entry ?\* "." table)
    (modify-syntax-entry ?\= "." table)
    (modify-syntax-entry ?\< "." table)
    (modify-syntax-entry ?\> "." table)
    (modify-syntax-entry ?\| "." table)
    (modify-syntax-entry ?\. "_" table)
    (modify-syntax-entry ?\` "_" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table)
    table)
  "Syntax table for `ngnk-mode'.")

;;;###autoload
(define-derived-mode ngnk-mode text-mode "Ngnk"
  "Major mode for editing ngn/k code.

\\<ngnk-mode-map>"
  nil "Ngnk"
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'tabs-width) 2))


(provide 'ngnk-mode)
;;; ngnk-mode.el ends here
