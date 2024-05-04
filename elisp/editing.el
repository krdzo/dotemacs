;;;; Setup text editing how I like it
;;; This is a merge between emacs, vim and helix

(use-package vundo
  :bind (:map vundo-mode-map
         ("<escape>" . vundo-quit)
         ("l" . vundo-forward)
         ("h" . vundo-backward)
         ("j" . vundo-next)
         ("k" . vundo-previous)))

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

(provide 'editing)
