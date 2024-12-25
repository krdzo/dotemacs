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

(defun ex/get-test-cases (fnode)
  "Return nodes of test cases in function FNODE.
If region is active return all nodes in region."
  (mapcar #'cdr
          (seq-filter
           (lambda (n) (eql (car n) 't-case-value))
           (treesit-query-capture fnode ex/query-test-run))))

(defun ex/get-test-functions ()
  "Return nodes of test function uder point
If region is active return all funciton nodes in region."
  (when (region-active-p)
    (mapcar #'cdr (treesit-query-capture
                   'go ex/query-get-test-functions (region-beginning) (region-end)))))

(defun ex/get-thing-around (pos thing)
  "Get THING around point.

First check thing at POS, if no thing is found this funciton returns
the first THING after POS.

Finally, if there is no THING after POS, try finding THING before
POS.

THING sould be a thing defined in `treesit-thing-settings'."
  (or (treesit-thing-at pos thing)
      (treesit-thing-next pos thing)
      (treesit-thing-prev pos thing)))

(defun ex/build-test-regex (nodes)
  "Create a regex from tree-sitter NODES."
  (format "^(:?%s)$"
          (string-join
           (mapcar (lambda (n)
                     (string-trim (treesit-node-text n t) "\"" "\""))
                   nodes)
           "|")))


;; TODO can be merged with `ex/build-test-regex'
(defun ex/func-and-case (func)
  (let* ((cases (ex/get-test-cases func)))
    (if cases
        (format "^%s$/%s" (treesit-defun-name func) (ex/build-test-regex cases))
      (format "^%s$" (treesit-defun-name func)))))

(defun ex/get-regex-of-test-to-run ()
  (let ((funcs (ex/get-test-functions)))
    (if (length> funcs 1)
        (ex/build-test-regex funcs)
      (ex/func-and-case (ex/get-thing-around (point) "function_declaration")))))

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
