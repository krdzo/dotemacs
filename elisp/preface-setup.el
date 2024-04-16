;;;; All the function and utils that are needed somewhere in the configuration
;;;; Like a utils file

(use-package general)

(defun kr-mac-p ()
  (if (string= system-type "darwin") t nil))


(provide 'preface-setup)
