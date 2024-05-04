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

  (defun mod-puni-setup ()
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
        ("<" . puni-barf-forward)))))

(use-package find-char
  :ensure '(find-char :repo "https://github.com/casouri/find-char")
  :config
  (defun mod-find-char-setup ()
    (modaled-define-keys
      :states '("normal")
      :bind
      `(("f" . find-char)))))


(use-package expand-region
  :config
  (setq expand-region-subword-enabled t)
  (defun mod-er-setup ()
    (modaled-define-keys
      :states '("normal")
      :bind
      `(("s" . er/expand-region)
        ("S" . er/contract-region)))))

(use-package expreg
  :config
  (defun mod-expr-setup ()
    (modaled-define-keys
      :states '("normal")
      :bind
      `(("w" . expreg-expand)
        ("W" . expreg-contract)))))

(use-package modaled
  :config
  (modaled-define-state "normal"
    :lighter " [NOR]"
    :cursor-type 'box)
  (modaled-define-keys
    :states '("normal")
    :bind
    `(("<escape>" . keyboard-quit)
      ("`" . kr-last-buffer)
      ("@" . goto-line)
      ("Q" . kill-current-buffer)
      (";" . exchange-point-and-mark)
      ("{" . backward-paragraph)
      ("}" . forward-paragraph)
      ("@" . goto-line)
      ("h" . backward-char)
      ("l" . forward-char)
      ("k" . previous-line)
      ("j" . next-line)
      ("b" . backward-word)
      ("e" . forward-word)
      ("i" . kr-insert)
      ("a" . kr-append)
      ("c" . kr-change)
      ("u" . undo)
      ("U" . vundo)
      ("p" . yank)
      ("y" . copy-region-as-kill)
      ("v" . set-mark-command)
      ("o" . kr-open-next-line)
      ("O" . kr-open-previous-line)
      ("L" . move-end-of-line)
      ("H" . beginning-of-line-text)

      ("SPC p" . ,project-prefix-map)
      ("SPC f f" . find-file)
      ("SPC f j" . dired-jump)
      ("SPC f s" . save-buffer)
      ("SPC b b" . list-buffers)
      ("SPC v" . magit-status)
      ))
  (modaled-define-state "insert"
    :sparse t
    :no-suppress t
    :cursor-type 'bar
    :lighter " [INS]")
  (modaled-define-keys
    :states '("insert")
    :bind
    '(("<escape>" . modaled-set-main-state)))


  (with-eval-after-load 'corfu
    (defun mod-corfu-shim ()
      (if corfu--input
          (corfu-quit)))
    (advice-add 'modaled-set-main-state :before #'mod-corfu-shim))

  (modaled-setup
    '("insert" vundo-mode magit-status-mode dired-mode)
    '("normal"))

  (mod-puni-setup)
  (mod-find-char-setup)
  (mod-er-setup)
  (mod-expr-setup))

;;; Custom commands that enhance modaled package

(defun kr-last-buffer ()
  "Switch to last viewed buffer in the same window."
    (interactive)
    (let ((switch-to-buffer-obey-display-actions nil))
      (call-interactively #'mode-line-other-buffer)))

(defun kr-insert ()
    (interactive)
    (if mark-active
        (progn
          (goto-char (region-beginning))
          (deactivate-mark)
          (modaled-set-state "insert"))
      (modaled-set-state "insert")))

(defun kr-append ()
    (interactive)
    (if mark-active
        (progn
          (goto-char (region-end))
          (deactivate-mark)
          (modaled-set-state "insert"))

      (modaled-set-state "insert")))

(defun kr-change ()
  "If region active delete and go to insert state.
If no region is active delete  char and go to insert."
  (interactive)
  (if mark-active
      (delete-region (region-beginning) (region-end))
    (delete-char 1))
  (call-interactively #'kr-insert))

(defun kr-open-previous-line ()
  (interactive)
  (deactivate-mark)
  (previous-line)
  (end-of-line)
  (kr-insert)
  (newline-and-indent))

(defun kr-open-next-line ()
  (interactive)
  (deactivate-mark)
  (end-of-line)
  (kr-insert)
  (newline-and-indent))

(provide 'editing)
