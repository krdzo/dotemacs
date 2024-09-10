;;;; Setup text editing how I like it
;;; This is a merge between emacs, vim and helix

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :config
  (define-key undo-tree-visualizer-mode-map (kbd "<escape>") 'undo-tree-visualizer-quit)
  :bind (:map undo-tree-visualizer-mode-map
         ("l" . undo-tree-visualize-switch-branch-right)
         ("h" . undo-tree-visualize-switch-branch-left)
         ("j" . undo-tree-visualize-redo)
         ("k" . undo-tree-visualize-undo)))

(use-package puni
  :config
  (defun kr-puni-kill-dwim ()
    (interactive)
    (if (use-region-p)
        (puni-kill-region)
      (puni-kill-line)))
  (modaled-define-keys
    :states '("normal")
    :bind
    `(("E" . puni-forward-sexp)
      ("B" . puni-backward-sexp)
      ("Z" . puni-force-delete)
      ("x" . puni-backward-delete-char)
      ("X" . puni-forward-delete-char)
      ("d" . kr-puni-kill-dwim)
      (">" . puni-slurp-forward)
      ("<" . puni-barf-forward))))

(use-package find-char
  :ensure '(find-char :repo "https://github.com/casouri/find-char")
  :config
  (modaled-define-keys
    :states '("normal")
    :bind
    `(("f" . find-char))))


(use-package expand-region
  :config
  (setq expand-region-subword-enabled t)
  (modaled-define-keys
    :states '("normal")
    :bind
    `(("s" . er/expand-region)
      ("S" . er/contract-region))))

(use-package expreg
  :config
  (modaled-define-keys
    :states '("normal")
    :bind
    `(("w" . expreg-expand)
      ("W" . expreg-contract))))

(use-package embrace
  :config
  (defun kr-embrace-append ()
    (interactive)
    (when (use-region-p)
        (exchange-point-and-mark))
    (embrace-add))

  (modaled-define-keys
    :states '("normal")
    :bind
    '(("C" . embrace-commander)
      ("I" . embrace-add)
      ("A" . kr-embrace-append))))

(provide 'editing)
