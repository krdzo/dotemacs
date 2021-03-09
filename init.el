;; Mesuring startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (emacs-init-time)
                     gcs-done)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap straight.el and use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; imply :straight t in every use-package statment
(setq straight-use-package-by-default t) 

;; installing use-package
;;; to see what use-package does you can use pp-macroexpand-last-sexp
(straight-use-package 'use-package)
;;;; use-package options for debuging init ;;;;
;; (setq use-package-verbose t)
;; (setq use-package-compute-statistics t)
;; (define-key global-map (kbd "C-c x") 'use-package-report)

;; tweaking the GC
(straight-use-package 'gcmh)
(gcmh-mode 1)




;; this part of config is responsable for loading the or creating
;; the right init files at the right time
(defvar kr-org-conf-file (expand-file-name "pravila.org" user-emacs-directory)
  "Path to org file of my config")
(defvar kr-el-conf-file (expand-file-name "pravila.el" user-emacs-directory)
  "Path to tangled org file of my config")
(if (file-exists-p kr-el-conf-file)
    (load-file kr-el-conf-file)
    (org-babel-load-file kr-org-conf-file))

