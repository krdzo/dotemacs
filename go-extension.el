(defvar ex/query-for-test-name-symbol
  '(
    (argument_list
     (selector_expression operand: (identifier) "." field: (field_identifier) @ind)
     (func_literal))
    )
  "Query to get \"t.Run\" argument list node.")


(defvar ex/query-for-test-table
  '(
    (short_var_declaration
     (_)
     right: (expression_list
             (composite_literal
              (_)
              (literal_value (literal_element) @zanima))))
    )
  "Query to get table of test cases from test function.")

(defvar ex/query-get-functions
  '((function_declaration name: (identifier) @func-name))
  "Treesit query to get function name node.")

(defun ex/test-case-name-symbol ()
  (let* ((run-test-node
          (save-excursion
            (treesit-beginning-of-defun)
            (search-forward "t.Run")
            (treesit-node-parent (treesit-node-at (point)))))
         (args (treesit-query-capture
                run-test-node
                ex/query-for-test-name-symbol)))
    (treesit-node-text (cdar args) t)))

;; (defun ex/get-regex-of-test-to-run ()
;;   "Return regex for test we want to run."
;;   (if (region-active-p)
;;       (string-join
;;        (ex/get-regex-functions-in-region (region-beginning) (region-end)) "|")
;;     (let* ((func-node
;;             (or (treesit-thing-at (point) "function_declaration")
;;                 (treesit-thing-next (point) "function_declaration")
;;                 (treesit-thing-prev (point) "function_declaration")))
;;            (func-name (treesit-defun-name func-node)))
;;       (if (not (string-prefix-p "Test" func-name))
;;           (error "Func \"%s\" is not a test function" func-name)
;;         (list func-name func-node)))))

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

;; TODO: finish implement
(defun ex/get-case-names-regex (fnode)
  "Get regex for case name under FNODE.
If region active then run all cases under region else just
the one under point."
  (when-let* ((t-run-pos
               (save-excursion
                 (goto-char (treesit-node-start fnode))
                 (search-forward "t.Run" (treesit-node-end fnode) t)))
              (t-run-node (treesit-node-parent (treesit-node-at t-run-pos)))
              (t-symbol-node (cdar (treesit-query-capture
                                    t-run-node
                                    ex/query-for-test-name-symbol)))
              (t-name-symbol (treesit-node-text t-symbol-node t))
              (t-struct-marker
               (save-excursion
                 (goto-char (treesit-node-start t-symbol-node))
                 (ex/find-definition-info))))
    t-name-symbol

     ))

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

;; (with-current-buffer "kwgnc_test.go"
;;   (treesit-query-capture
;;    'go ex/query-get-functions (point) (point)))

;; (with-current-buffer "kwgnc_test.go"
;;   (treesit-query-capture
;;    (treesit-thing-at (point) "function_declaration")
;;    '(
;;      (short_var_declaration) @struct
;;      )))




;; (with-current-buffer "kwgnc_test.go"
;;   (ex/find-definition-info))

;; (with-current-buffer "kwgnc_test.go"
;;   (let* ((test-symbol (ex/test-case-name-symbol))
;;          (func-node (treesit-thing-at (point) "function_declaration"))
;;          (func-name (treesit-defun-name func-node))
;;          (test-cases (treesit-query-capture
;;                       func-node
;;                       ex/query-for-test-table)))

;;     (dolist (case test-cases)
;;       (let ((node (cdr case)))
;;         (when (and (<= (treesit-node-start node) (point) )
;;                    (<= (point) (treesit-node-end node)))
;;           (dolist (child (treesit-node-children node))
;;             (print child)))))))
