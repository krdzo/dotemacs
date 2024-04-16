(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
;; messure emacs startup
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (emacs-init-time)
                       gcs-done)))

;;; Small builtin options that don't have any section
(setq ring-bell-function 'ignore)
(setq use-file-dialog nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq delete-by-moving-to-trash t)
(setq-default save-interprogram-paste-before-kill t)
(defalias 'yes-or-no-p 'y-or-n-p)

(column-number-mode 1)
(global-auto-revert-mode 1)
(add-hook 'focus-out-hook #'garbage-collect)
(add-hook 'before-save-hook #'whitespace-cleanup)


(require 'elpaca-bootstrap)
(require 'preface-setup)
(require 'completion)
(require 'meow-setup)
(require 'ux-setup)
(require 'ui-setup)
(require 'dev-setup)
(require 'apps-setup)
