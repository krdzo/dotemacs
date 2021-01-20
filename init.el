;; Change default for faster startup 
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold (* 64 1024 1024)) ;; 100MB

;; Mesuring startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init package and use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("org"       . "https://orgmode.org/elpa/")
			 ("gnu"       . "https://elpa.gnu.org/packages/")
			 ("melpa"     . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")))
(require 'package)

;; to see what use-package does you can use pp-macroexpand-last-sexp
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq-default use-package-always-ensure t)
;; (setq use-package-compute-statistics t)

(define-key global-map (kbd "C-c x") 'use-package-report)

(use-package org
  :config
  (add-to-list 'org-modules 'org-tempo t)
  (org-babel-load-file (expand-file-name "pravila.org" user-emacs-directory)))
