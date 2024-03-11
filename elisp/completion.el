;;; general
(setq tab-always-indent 'complete)
(setq completion-styles '(basic partial-completion))

(use-package prescient
  :config
  (add-to-list 'completion-styles 'prescient)
  (with-eval-after-load 'prescient
    (prescient-persist-mode 1))

  (setq prescient-filter-method '(literal prefix fuzzy))
  (setq prescient-filter-method '(literal prefix literal-prefix regexp fuzzy))

  (add-hook 'minibuffer-mode-hook
            #'(lambda ()
                (setq-local prescient-filter-method
                      '(literal prefix literal-prefix regexp))))

  (add-to-list 'completion-category-overrides '(file (styles basic partial-completion)))

  ;; setups are copyed from wikis
  ;; setup for vertico
  (with-eval-after-load 'vertico
    (setq vertico-sort-function #'prescient-completion-sort)

    (defun vertico-prescient-remember ()
      "Remember the chosen candidate with Prescient."
      (when (>= vertico--index 0)
        (prescient-remember
         (substring-no-properties
          (nth vertico--index vertico--candidates)))))
    (advice-add #'vertico-insert :after #'vertico-prescient-remember))

  ;; setup for corfu
  (with-eval-after-load 'corfu
    (defun dima-corfu-prescient-remember (&rest _)
      "Advice for `corfu--insert.'"
      (when (>= corfu--index 0)
        (prescient-remember (nth corfu--index corfu--candidates))))

    (advice-add #'corfu--insert :before #'dima-corfu-prescient-remember)

    (setq corfu-sort-function #'prescient-completion-sort)
    (setq corfu-sort-override-function #'prescient-completion-sort)))

;;; minibuffer completion
(use-package vertico
  :bind (("M-c" . 'vertico-repeat)
         :map vertico-map
         ("C-j" . 'vertico-next)
         ("C-k" . 'vertico-previous)
         ("C-<backspace>" . 'vertico-directory-delete-word)
         ("<backspace>" . 'vertico-directory-delete-char)
         ("<enter>" . 'vertico-directory-enter))
  :config
  (setq enable-recursive-minibuffers t)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (setq vertico-cycle t)

  (vertico-multiform-mode 1)

  (setq vertico-multiform-commands
        '((xref-find-references buffer)
          (xref-find-references-at-mouse buffer)
          (consult-yank-pop indexed)
          (project-find-regexp buffer)
          (consult-grep buffer)
          (consult-ripgrep buffer)
          (consult-git-grep buffer)
          (consult-imenu buffer)
          (eglot-find-implementation buffer)
          (imenu buffer)))

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy))


;;; buffer completion
(use-package corfu
  :bind (:map corfu-map
         ("S-SPC" . 'corfu-insert-separator)
         ("M-h" . 'corfu-info-documentation)
         ("C-j" . 'corfu-next)
         ("C-k" . 'corfu-previous))
  :config
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 1)
  (setq corfu-auto-delay 0.1)
  (setq corfu-max-width 50)
  (setq corfu-min-width corfu-max-width)
  (setq corfu-preselect-first t)

  (global-corfu-mode 1)
  (with-eval-after-load 'corfu
      (add-hook 'meow-insert-exit-hook #'corfu-quit)))

(provide 'completion)