(electric-pair-mode 1)

(use-package xref
    :config
    (setq xref-prompt-for-identifier nil))

;;; git
(setq auth-sources '("~/.authinfo"))
(use-package transient)
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
  (setq treesit-auto-langs '(go gomod))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; lsp
(use-package jsonrpc)
(use-package eglot
  :hook (go-ts-mode . eglot-ensure)

  :config
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format nil t))))

;;; languages
(use-package go-ts-mode
  :ensure nil
  :custom (go-ts-mode-indent-offset 4))

(provide 'dev-setup)
