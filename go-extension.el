;;; helper to test code

(defun my-point ()
  (interactive)
  (message "%s"(point)))

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

(defvar ex/query-get-test-functions
  '((function_declaration name: (identifier) @func-name
                          (:match "^Test.*" @func-name)))
  "Treesit query to get function name node.")


(defun ex-case-with-point-p (node)
  (if (region-active-p)
      (let ((nstart (treesit-node-start node))
            (nend (treesit-node-end node))
            (rstart (region-beginning))
            (rend (region-end)))
        (or (< nstart rstart rend nend)
            (< nstart rstart nend)
            (< nstart rend nend)
            (< rstart nstart nend rend)))
    (< (treesit-node-start node) (point) (treesit-node-end node))))

(defun ex/get-name-of-test-case (fnode)
  "Return names of test cases in function that is under FNODE."
  (when-let*
      ((quoted-names
        (seq-filter #'identity
            (seq-map
             (lambda (capture)
               (when (eql (car capture) 't-case-value)
                 (treesit-node-text (cdr capture) t)))
             (treesit-query-capture fnode
                                    ex/query-test-run)))))
    ;; the string values in caputre have qotes around them, we need to remove them
    (mapcar (lambda (name)
              (substring name 1 (1- (length name))))
            quoted-names)))

(defun ex/get-thing-around (pos thing)
  (or (treesit-thing-at pos thing)
      (treesit-thing-next pos thing)
      (treesit-thing-prev pos thing)))

(defun ex/regex-from-capture (capture)
  "Get regex from CAPTURE.
CAPTURE is a treesit query capture of the form (name . node)"
  (format "^%s$" (treesit-node-text (cdr capture) t)))

(defun ex/get-function-names-regex (&optional node start end)
  (mapcar #'ex/regex-from-capture
          (treesit-query-capture
           (or node 'go) ex/query-get-test-functions start end)))

(defun ex/get-case-names-regex (fnode)
  "Get regex for case name under FNODE.
If region active then run all cases under region else just
the one under point."
  (when-let* ((names (ex/get-name-of-test-case fnode)))
    (format "^%s$" (string-join names "|"))))

(defun ex/get-test-regex (&optional node start end)
  (let ((functions (ex/get-function-names-regex nil start end)))
    (if (length> functions 1)
        (string-join functions "|")
      (concat (car functions) "/" (ex/get-case-names-regex node)))))


(defun ex/build-test-regex (nodes)
  (string-join
   (mapcar (lambda (node)
             (format "^%s$" (treesit-node-text (cdr node) t)))
           nodes)
   "|"))

(defun ex/get-test-functions-in-region (start end)
  (treesit-query-capture
   'go ex/query-get-test-functions
   start end))

(defun ex/func-and-case ()
  (let* ((func (ex/get-thing-around (point) "function_declaration"))
         (cases (ex/get-name-of-test-case func)))
    (if  cases
        (format "^%s$/%s" (treesit-defun-name func) (format "^%s$" (string-join cases "|")))
      (format "^%s$" (treesit-defun-name func)))))

(defun ex/get-regex-of-test-to-run ()
  (if (region-active-p)
      (let ((functions (ex/get-test-functions-in-region (region-beginning) (region-end))))
        (if (length> functions 1)
            (ex/build-test-regex functions)
          (ex/func-and-case)))
    (ex/func-and-case)))

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
