;;; ngnk-deserialize.el --- deserialze ngn/k data. -*- lexical-binding: t; -*-

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
;; Created: February 22nd, 2025
;; Keywords: ngn/k, k, org-babel
;; URL: https://github.com/gitonthescene/ngnk-mode
;;
;; Version: 0.1
;;

(require 'bitpack)
(require 'dash)
(defun ngnk-serlst (fn)
  (let* ((n (bitpack-load-u32 :<))
        (res nil))
    (-dotimes n (lambda (k) (push (apply fn ()) res)))
    (reverse res)))

(defun ngnk-getstr ()
  (let* ((n (bitpack-load-u32 :<))
        (ret (buffer-substring-no-properties (point) (+ n (point)))))
    (forward-char n)
    ret))

(defun ngnk-getatom ()
  (let* ((value (bitpack-load-u32 :<)))
    (forward-char 4)
    value))

(defun ngnk-getpair (maker)
  (let* ((n (bitpack-load-u32 :<)))
    (if (not (= n 2)) (error "not a dict?"))
    (apply maker (list :keys (ngnk-deserialize) :values (ngnk-deserialize)))))

(defun ngnk-getenum ()
  (make-ngnk-enum :start (bitpack-load-u64 :<) :end (bitpack-load-u64 :<)))

(defun ngnk-getbits ()
  (let* ((n (ash (+ (bitpack-load-u32 :<) 7) -3))
            (ret (buffer-substring-no-properties (point) (+ n (point)))))
    (forward-char n)
    (make-ngnk-bits :data (string-to-list ret))))

(defun ngnk-getsym (r)
  (let* ((i (point))
         (key (bitpack-load-u64 :<)))
    (if (> r 0) (cdr (assoc key (apply ngnk-getsymhash ())))
      (if (> (logand key (ash 1 31)) 0)
          (let* ((n (bitpack-load-u8))
                 (sym (make-symbol
                       (buffer-substring-no-properties
                        (point) (+ (point) n)))))
            (forward-char n)
            (apply updsymhash (list key sym))
            sym)
        (make-symbol (buffer-substring-no-properties i (+ i 4)))))))

(cl-defstruct ngnk-dict keys  values)
(cl-defstruct ngnk-map  keys  values)
(cl-defstruct ngnk-enum start end)
(cl-defstruct ngnk-bits data)

(defun ngnk-deserialize ()
  (let* ((type (bitpack-load-u8))
         (r (logand type (ash 1 7))))
    (pcase (logand type (lognot (ash 1 7)))
      (1  (ngnk-serlst 'ngnk-deserialize))                     ;; mixed list
      (2  (ngnk-getenum))                                      ;; enumerations
      (3  (ngnk-getbits))                                      ;; bits
      (4  (ngnk-serlst 'bitpack-load-s8))                      ;; chars
      (5  (ngnk-serlst (-partial 'bitpack-load-s16 :<)))       ;; shorts
      (6  (ngnk-serlst (-partial 'bitpack-load-s32 :<)))       ;; ints
      (7  (ngnk-serlst (-partial 'bitpack-load-s64 :<)))       ;; longs
      (8  (ngnk-serlst (-partial 'bitpack-load-f64 :<)))       ;; floats
      (9  (ngnk-getstr))                                       ;; characters
      (10 (ngnk-serlst 'ngnk-deserialize))                     ;; symbols
      (11 (ngnk-getpair 'make-ngnk-map))                       ;; maps
      (12 (ngnk-getpair 'make-ngnk-dict))                      ;; dicts
      (13 (ngnk-getatom))                                      ;; int
      (14 (car (ngnk-serlst (-partial 'bitpack-load-s64 :<)))) ;; long
      (15 (car (ngnk-serlst (-partial 'bitpack-load-f64 :<)))) ;; float
      (16 (prin1-char (ngnk-getatom)))                         ;; char
      (17 (ngnk-getsym r))                                     ;; symbols
      (_  (error "Not handled.")))))

;;; ngnk-deserialize.el ends here

;; EXAMPLE:
;;
;; "/tmp/array.kbin"1:`@(1.0 2.3;9 7 589;"oh happy day";9;12.3;123878979871981;"z";"abc"!1 7 3;9+!6;01101011B;2;`happy`dog`day;`happy)

;; (with-temp-buffer
;;   (set-buffer-multibyte nil)
;;   (insert-file-contents-literally "/tmp/array.kbin")
;;   (let* ((version (bitpack-load-u8))
;;     (symhash ())
;;     (ngnk-getsymhash (lambda () symhash))
;;     (updsymhash (lambda (k v) (setq symhash (push (cons k v) symhash)))))
;;   (message "Version: %d" version)
;;   (ngnk-deserialize)))
