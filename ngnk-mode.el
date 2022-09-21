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
;; Created: September 7th, 2022
;; Keywords: ngn/k, k, emacs-mode
;; URL: https://github.com/gitonthescene/ngnk-mode
;;
;; Version: 0.1
;;
;;; Commentary:
;;
;; Modeled on [[https://www.masteringemacs.org/article/comint-writing-command-interpreter]]
;;
(require 'comint)
(require 'cl-lib)

(defgroup ngnk nil "ngn/k language editing mode."
  :group 'languages
  :prefix "ngnk-")

(defcustom ngnk-cli-file-path
  (or
   (executable-find "k")
   "k")
  "Path to the program used by `run-ngnk'"
  :group 'ngnk
  :type 'string)

(defcustom ngnk-cli-arguments '()
  "Commandline arguments to pass to `ngnk-cli'"
  :group 'ngnk
  :type '(repeat string))

(defun ngnk-cli-args ()
  (or ngnk-cli-arguments
      (let ((fpth (executable-find ngnk-cli-file-path)))
        (if fpth
            (let ((fpth (expand-file-name "repl.k" (file-name-directory fpth))))
              (or (and (file-readable-p fpth) (list fpth)) nil))
          nil))))

(defcustom ngnk-start-file ""
  "Commandline arguments to pass to `ngnk-cli'"
  :group 'ngnk
  :type 'string)

(defun ngnk-sfile ()
  (or (if (eq (length ngnk-start-file) 0)
          nil
        ngnk-start-file)))

(defcustom ngnk-max-output-length 0
  "Maximum length of output printed"
  :group 'ngnk
  :type 'number)

(defun ngnk-remove-marker (s)
  (cl-remove ?\a s))

(defun ngnk-preout-filter (s)
  "When ngnk-max-output-length is > 0, limit output to ngnk-max-output-length lines"
  (progn
    (if (not ngnk-buffer-limit)
        (setq ngnk-buffer-limit ngnk-max-output-length))
    (if (= 0 ngnk-max-output-length)
        (progn
          (setq ngnk-buffer-limit nil)
          (ngnk-remove-marker s))
      (let ((nlix (cl-search "\n" s))
            (origlen (length s))
            (start 0)
            (body "")
            (pix nil))
        (while (and (> ngnk-buffer-limit 0) nlix (not pix))
          (setq pix (cl-search "\a" s :start2 start :end2 nlix))
          (setq ngnk-buffer-limit (- ngnk-buffer-limit 1))
          (setq body (concat body (substring s start (min (+ 1 nlix) (or pix (+ 1 nlix))))))
          (if (= ngnk-buffer-limit 0) (setq body (concat body "...")))
          (setq start (+ nlix 1))
          (setq nlix (cl-search "\n" s :start2 start)))
        (if (or pix (setq pix (cl-search "\a" s :start2 start))) ;; Done with output
            (progn
              (setq ngnk-buffer-limit nil) ;; reset
              (setq body (concat body (substring s (+ 1 pix) origlen)))))
        (ngnk-remove-marker body)))))

(defcustom ngnk-prompt-regexp "^ "
  "Prompt for `run-ngnk'."
  :group 'ngnk
  :type 'string)

(defcustom ngnk-mark-line-continuations nil
  "Mark line continuations when sending a buffer"
  :group 'ngnk
  :type 'boolean)

(defvar ngnk-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-ngnk'")

(setq ngnk-buffer-name "*Ngnk*")
(defun ngnk-buffer ()
    (get-buffer ngnk-buffer-name))
(defun ngnk-buffer-proc ()
  (get-buffer-process (ngnk-buffer)))
(defun ngnk-send-string (s)
  (comint-send-string (ngnk-buffer-proc) s))
(defun ngnk-send-region ()
  (let* ((s (concat (buffer-substring (point) (mark)) "\n"))
       (idx (cl-search "\n" s))
       (ret ""))
    (if ngnk-mark-line-continuations
        (progn
          (while idx
            (setq ret (concat ret (substring s 0 idx) "\a\n"))
            (setq s (substring s (+ 1 idx) (length s)))
            (setq idx (cl-search "\n" s)))
          (setq s (concat ret s))
          (let ((end (- (length s) 1))
                (chr nil))
            (while (or (eq (setq chr (aref s end)) ?\n)
                       (eq (setq chr (aref s end)) ?\a))
              (setq end (- end 1)))
            (setq s (concat (substring s 0 (+ end 1)) "\n")))))
    (message s)
    (comint-send-string (ngnk-buffer-proc) s)))

(defun ngnk-attach-proc (buffer)
  "Attach ngn/k process to buffer"
  (let* ((ngnk-program ngnk-cli-file-path))
    (apply 'make-comint-in-buffer "Ngnk" buffer
           ngnk-program (ngnk-sfile) (ngnk-cli-args))
    (ngnk-mode)))

(defun run-ngnk ()
  "Run an inferior instance of `ngnk-cli' inside Emacs."
  (interactive)
  (progn
    ;; pop to the "*Ngnk*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (if (not (comint-check-proc (current-buffer)))
        (pop-to-buffer-same-window
         (get-buffer-create ngnk-buffer-name)))

    ;; create the comint process if there is no buffer.
    (ngnk-attach-proc (current-buffer))))

(defun ngnk--initialize ()
  "Helper function to initialize Ngnk"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp nil)
        (add-hook 'comint-preoutput-filter-functions
                  'ngnk-preout-filter nil t))

(define-derived-mode ngnk-mode comint-mode "Ngnk"
  "Major mode for `run-ngnk'.

\\<ngnk-mode-map>"
  nil "Ngnk"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp ngnk-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'ngnk-buffer-limit) nil)
;;  (set (make-local-variable 'font-lock-defaults) '(ngnk-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) ngnk-prompt-regexp))
(add-hook 'ngnk-mode-hook 'ngnk--initialize)

;; (set (make-local-variable 'font-lock-defaults) '(ngnk-font-lock-keywords t))
;; 
;; (defconst ngnk-keywords
;;   '())
;; 
;; (defvar ngnk-font-lock-keywords
;;   (list
;;    ;; highlight all the reserved commands.
;;    `(,(concat "\\_<" (regexp-opt ngnk-keywords) "\\_>") . font-lock-keyword-face))
;;   "Additional expressions to highlight in `ngnk-mode'.")

(provide 'ngnk-mode)
;;; ngnk-mode.el ends here
