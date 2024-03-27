(electric-pair-mode 1)
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
  :bind
  :config
  (setq sly-mrepl-prevent-duplicate-history t)

  (general-def 'sly-mode-map
    "C-z h" 'sly-describe-symbol)

  (with-eval-after-load 'meow
    (add-to-list 'meow-mode-state-list '(sly-mrepl-mode . normal))
    (add-to-list 'meow-mode-state-list '(sly-db-mode . motion))
    (add-to-list 'meow-mode-state-list '(sly-xref-mode . motion))
    (add-to-list 'meow-mode-state-list '(sly-stickers--replay-mode . motion))
    (add-to-list 'meow-mode-state-list '(sly-inspector-mode . motion)))
  ;; switch bufers REPL - DB - Source
  (general-def '(lisp-mode-map sly-mrepl-mode-map)
    "C-c d" #'(lambda () (interactive) (switch-to-buffer "*sly-db for sbcl (thread 1)*")))
  (general-def '(lisp-mode-map sly-db-mode-map sly-db-frame-map)
    "C-c '" #'(lambda ()
                (interactive)
                (call-interactively #'sly-mrepl)
                (end-of-buffer)))
  (general-def '(sly-db-mode-map sly-db-frame-map)
    "C-c d" #'sly-switch-to-most-recent)
  (general-def 'sly-mrepl-mode-map
    "C-j" 'sly-mrepl-next-prompt
    "C-k" 'sly-mrepl-previous-prompt
    "C-p" 'sly-mrepl-previous-input-or-button
    "C-n" 'sly-mrepl-next-input-or-button
    "C-c '" #'sly-switch-to-most-recent)

  (general-def 'sly-stickers--replay-mode-map
    "/" 'sly-stickers-replay-jump)
  ;; always open sly REPl in other window
  (add-to-list 'display-buffer-alist
               `("*sly-mrepl for sbcl*"
                 kr-display-buffer-reuse-window
                 (inhibit-same-window . t)))
  (defun kr-sly-db-new-window-direction (buffer alist)
    "Control where sly-db buffer is shown.
BUFFER and ALIST are the same type that are needed
for `display-buffer' funcitons."
    (display-buffer "*sly-mrepl for sbcl*")
    (add-to-list 'alist (cons 'window (get-buffer-window "*sly-mrepl for sbcl*")))
    (display-buffer-in-direction buffer alist))

  (add-to-list 'display-buffer-alist
               `("*sly-db for sbcl (thread [0-9]+)*"
                 kr-sly-db-new-window-direction
                 (direction . below)
                 (window-height . 0.5))))

(use-package sly-repl-ansi-color
  :config
  (push 'sly-repl-ansi-color sly-contribs))


(provide 'dev-setup)
