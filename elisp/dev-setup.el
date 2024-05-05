(electric-pair-mode 1)
(add-hook 'prog-mode-hook #'toggle-truncate-lines)
(use-package markdown-mode)

(use-package xref
  :config
  (setq xref-prompt-for-identifier nil))

;;; git
(setq auth-sources '("~/.authinfo"))
(use-package transient
  :bind (:map transient-base-map
              ("<escape>" . transient-quit-one)))
(use-package magit
  :demand t
  :bind (:map magit-status-mode-map
              ("S-<tab>" . 'magit-section-cycle)
              ("C-<tab>" . 'tab-next))
  :hook (git-commit-mode . flyspell-mode)
  :config
  (use-package forge :disabled)
  (setq git-commit-fill-column 72)
  (setq magit-process-finish-apply-ansi-colors t)

  (with-eval-after-load 'magit
    (dolist (face '(magit-diff-added
                    magit-diff-added-highlight
                    magit-diff-removed
                    magit-diff-removed-highlight))
      (set-face-background face (face-attribute 'magit-diff-context-highlight :background)))
    (set-face-background 'magit-diff-context-highlight
                         (face-attribute 'default :background)))

  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

  (with-eval-after-load 'project
    (define-key project-prefix-map
                (kbd "v") 'magit-project-status)
    (remove-hook 'project-switch-commands '(project-vc-dir "VC-Dir"))
    (add-hook 'project-switch-commands '(magit-project-status "Magit") 100)))


;;; tree-sitter
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs
        '(c cpp cmake go gomod dockerfile markdown tsx typescript html css javascript json yaml))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; lsp
(use-package jsonrpc)
(use-package eglot
  :hook ((go-ts-mode c-ts-mode) . eglot-ensure)
  :config
  (add-hook 'special-mode-hook 'visual-line-mode)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format nil t))))

;;; languages

;; go
(use-package go-ts-mode
  :ensure nil
  :custom (go-ts-mode-indent-offset 4)
  :config
  (setq treesit-font-lock-level 4))


;; common-lisp
(if (executable-find "ros")
    (setq inferior-lisp-program "ros -Q run")
  (setq inferior-lisp-program "sbcl"))

(use-package sly
  :config
  (setq sly-mrepl-prevent-duplicate-history t)

  (defun kr-sly-mrepl ()
    (interactive)
    (call-interactively #'sly-mrepl)
    (end-of-buffer))
  (modaled-define-substate "sly")
  (modaled-define-keys
    :substates '("sly")
    :bind
    '(("m e" . sly-eval-last-expression)
      ("m c" . sly-compile-defun)
      ("M-h" . sly-describe-symbol)
      ("SPC '" . kr-sly-mrepl)
      ("SPC ~" . sly-mrepl-sync)))
  (modaled-enable-substate-on-state-change
    "sly"
    :states '("normal")
    :major '(lisp-mode))

  (modaled-define-substate "sly-mrepl")
  (modaled-define-keys
    :substates '("sly-mrepl")
    :bind
    '(("M-h" . sly-describe-symbol)
      ("SPC '" . sly-switch-to-most-recent)))
  (modaled-enable-substate-on-state-change
    "sly-mrepl"
    :states '("normal")
    :major '(sly-mrepl-mode))

  ;; always open sly REPl in other window
  (add-to-list 'display-buffer-alist
               `("*sly-mrepl for sbcl*"
                 kr-display-buffer-reuse-window
                 (inhibit-same-window . t))))

(use-package sly-repl-ansi-color
  :config
  (push 'sly-repl-ansi-color sly-contribs))


(provide 'dev-setup)
