(require 'ngnk-cli)

(ert-deftest ngnk-test-preout-filter ()
  "Test preout-filter"
  (unwind-protect
      (let ((chk (lambda (s r)
                   (should (equal (ngnk-preout-filter s) r))
                   (should (equal ngnk-buffer-limit nil))))
            (chk-incomplete (lambda (s r)
                   (should (equal (ngnk-preout-filter s) r)))))
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
        (funcall chk-incomplete
                 "1\n prompt"
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
                 "1\n prompt")
        (setq ngnk-max-output-length 1)
        (funcall chk
                 "1\n2\n3\n\aprompt"
                 "1\n...prompt")
        (setq ngnk-max-output-length 2)
        (funcall chk
                 "1\n2\n3\n\aprompt"
                 "1\n2\n...prompt")
        (setq ngnk-max-output-length 1)
        (funcall chk
                 "1\n2\n3\n\aprompt"
                 "1\n...prompt")
        (setq ngnk-max-output-length 0)
        (funcall chk
                 "1\n2\n3\n\aprompt"
                 "1\n2\n3\nprompt")
        (setq ngnk-max-output-length 2)
        (funcall chk
                 "1\n2\n3\n\aprompt"
                 "1\n2\n...prompt"))
    (progn
      ;; remove buffer local variable
      (makunbound 'ngnk-buffer-limit)
      ;; restore custom setting
      (setq ngnk-max-output-length orig)
      )))
