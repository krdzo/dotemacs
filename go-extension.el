;;; helper to test code
(when (get-buffer "kwgnc_test.go" )
  (with-current-buffer "kwgnc_test.go"
    (setq test-func-node
          (alist-get 'func
                     (treesit-query-capture
                      'go
                      '((function_declaration
                         (identifier) @func-name
                         (:equal "TestTry" @func-name)) @func))))))

;;; code starts here

(defvar ex/query-test-run
  '((block
     ;; test struct
     (short_var_declaration
      (expression_list
       (composite_literal
        body: (literal_value
               (literal_element
                (literal_value
                 (keyed_element
                  key: (literal_element
                        (identifier) @t-case-name)
                  value: (literal_element
                          (interpreted_string_literal) @t-case-value))) @t-case

                )))))
     ;; test for loop
     (for_statement
        body:
        (block
         (expression_statement
          (call_expression
           function: (selector_expression
                      field: (field_identifier)) @t-run
                      (:equal @t-run "t.Run")
           arguments: (argument_list (selector_expression
                                      field: (field_identifier) @t-run-symbol))))))
     (:equal @t-run-symbol @t-case-name)
     (:pred ex-case-with-point-p @t-case)))
  "Query to find the node that has the symbol to use as test run name.")

(defun ex-case-with-point-p (node)
  (and (< (treesit-node-start node) (point))
       (< (point) (treesit-node-end node))))

(defun ex/get-name-of-test-case ()
  (let ((quoted-name
         (treesit-node-text
          (alist-get 't-case-value
                     (treesit-query-capture test-func-node
                                            ex/query-test-run))
          t)))
    (substring quoted-name 1 (1- (length quoted-name)))))

(defvar ex/query-for-test-name-symbol
  '(
    (argument_list
     (selector_expression operand: (identifier) "." field: (field_identifier) @ind)
     (func_literal))
    )
  "Query to get \"t.Run\" argument list node.")

(defun ex/get-thing-around (pos thing)
  (or (treesit-thing-at pos thing)
      (treesit-thing-next pos thing)
      (treesit-thing-prev pos thing)))

(defun ex/regex-from-capture (capture)
  "Get regex from CAPTURE.
CAPTURE is a treesit query capture of the form (name . node)"
  (format "^%s$" (treesit-node-text (cdr capture) t)))

(defun ex/get-function-names-regex (&optional node start end)
  (seq-filter (lambda (r) (string-prefix-p "^Test" r))
      (mapcar #'ex/regex-from-capture
              (treesit-query-capture
               (or node 'go) ex/query-get-functions start end))))

(defun ex/find-definition-info ()
  "Find the definition under point and return its location as a marker.
Does not move the point or change the buffer."
  (let* ((backend (xref-find-backend))
         (identifier (xref-backend-identifier-at-point backend))
         (defs (xref-backend-definitions backend identifier)))
    (xref-location-marker (xref-item-location (car defs)))))

(defun ex/get-case-names-regex (fnode)
  "Get regex for case name under FNODE.
If region active then run all cases under region else just
the one under point."
  (when-let* ((name (ex/get-name-of-test-case)))
    (format "^%s$" name)))

(defun ex/get-test-regex (&optional node start end)
  (let ((functions (ex/get-function-names-regex node start end)))
    (if (length> functions 1)
        (string-join functions "|")
      (concat (car functions) "/" (ex/get-case-names-regex node) )
      )))

(defun ex/get-regex-of-test-to-run ()
  (if (region-active-p)
      (ex/get-test-regex nil (region-beginning) (region-end))
    (ex/get-test-regex
     (ex/get-thing-around (point) "function_declaration"))))

(defun ex/test-compile (regex)
  "Compile the tests matching REGEXP."
  (if (not (string-empty-p regex))
      (compile (format "go test -v -run '%s'" regex))
    (error "No test function found")))

;;; COMMAND
(defun ex/run-test ()
  "Command to run go tests.
If region ac"
  (interactive)
  (ex/test-compile (ex/get-regex-of-test-to-run)))
