(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
;; messure emacs startup
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (emacs-init-time)
                       gcs-done)))

;;; Builtin options,
;; will move them to other section at a later date
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
(delete-selection-mode 1)
(subword-mode 1)
(define-key minibuffer-mode-map (kbd "<escape>") 'minibuffer-keyboard-quit)


(require 'elpaca-bootstrap)
(require 'preface-setup)
(require 'completion)
(require 'ux-setup)
(require 'ui-setup)
(require 'dev-setup)
(require 'apps-setup)
(require 'editing)
