(require 'ngnk-mode)

(ert-deftest ngnk-test-prefilter ()
  "Test prefilter"
  (unwind-protect
      (progn
        (let ((chk (lambda (s r)
                     (should (equal (ngnk-preout-filter s) r))
                     (should (equal ngnk-buffer-limit nil)))))
          (setq ngnk-buffer-limit nil)
          (setq orig ngnk-max-output-length)
          (setq ngnk-max-output-length 2)
          (funcall chk
                   "1\n2\n3\n\aprompt"
                   "1\n2\n...prompt")
          (funcall chk
                   "1\n2\n3\n\a\nprompt"
                   "1\n2\n...\nprompt")
          (funcall chk
                   "1\n2\n\aprompt"
                   "1\n2\n...prompt")
          (funcall chk
                   "1\n2\aprompt"
                   "1\n2prompt")
          (funcall chk
                   "1\n2\n\a\n\nprompt\nextra"
                   "1\n2\n...\n\nprompt\nextra")
          (funcall chk
                   "1\n\aprompt"
                   "1\nprompt")
          (funcall chk
                   "1\n\a prompt"
                   "1\n prompt")
          (setq ngnk-max-output-length 0)
          (funcall chk
                   "1\n2\n3\n\aprompt"
                   "1\n2\n3\nprompt")
          (funcall chk
                   "1\n2\n3\n\aprompt"
                   "1\n2\n3\nprompt")
          (funcall chk
                   "1\n2\n\aprompt"
                   "1\n2\nprompt")
          (funcall chk
                   "1\n2\aprompt"
                   "1\n2prompt")
          (funcall chk
                   "1\n2\n\a\n\nprompt\nextra"
                   "1\n2\n\n\nprompt\nextra")
          (funcall chk
                   "1\n\aprompt"
                   "1\nprompt")
          (funcall chk
                   "1\n\a prompt"
                   "1\n prompt")))
    (progn
      ;; remove buffer local variable
      (makunbound 'ngnk-buffer-limit)
      ;; restore custom setting
      (setq ngnk-max-output-length orig)
      )))