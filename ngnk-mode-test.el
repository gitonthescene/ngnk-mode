(ert-deftest ngnk-test-require ()
  "Test that module loads"
  (should (equal
           'ngnk-mode
           (condition-case nil
               (require 'ngnk-mode)
             (error 'fail-to-load)))))
