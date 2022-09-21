(require 'ngnk-mode)

(ert-deftest ngnk-test-prefilter ()
  "Test prefilter"
  (unwind-protect
      (progn
        (setq ngnk-buffer-limit nil)
        (setq orig ngnk-max-output-length)
        (setq ngnk-max-output-length 2)
        (should (equal (ngnk-preout-filter "1\n2\n3\n\aprompt") "1\n2\n...prompt"))
        (should (equal (ngnk-preout-filter "1\n2\n3\n\a\nprompt") "1\n2\n...\nprompt"))
        (should (equal (ngnk-preout-filter "1\n2\n\aprompt") "1\n2\n...prompt"))
        (should (equal (ngnk-preout-filter "1\n2\aprompt") "1\n2prompt"))
        (should (equal (ngnk-preout-filter "1\n2\n\a\n\nprompt\nextra") "1\n2\n...\n\nprompt\nextra"))
        (should (equal (ngnk-preout-filter "1\n\aprompt") "1\nprompt"))
        (should (equal (ngnk-preout-filter "1\n\a prompt") "1\n prompt")))
    (progn
      (makunbound 'ngnk-buffer-limit)
      (setq ngnk-max-output-length orig)
      )))
