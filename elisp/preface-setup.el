;;;; Setup states for vim line editing
;;; Keybinding are a merge of vim, emacs, helix

(use-package modaled
  :config
  (modaled-define-state "normal"
    :lighter " [NOR]"
    :cursor-type 'box)
  (modaled-define-state "insert"
    :sparse t
    :no-suppress t
    :lighter " [INS]"
    :cursor-type 'bar)

  (modaled-define-keys
    :states '("normal")
    :bind
    `(("<escape>" . keyboard-quit)
      ("`" . kr-last-buffer)
      ("C-`" . next-error)
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
      ("M-h" . kr-describe-at-point)
      ("m e" . eval-last-sexp)

      ("J" . join-line)

      ("/" . consult-line)

      ("SPC p" . ,project-prefix-map)
      ("SPC f f" . find-file)
      ("SPC f j" . dired-jump)
      ("SPC f s" . save-buffer)
      ("SPC b b" . list-buffers)
      ("SPC v" . magit-status)
      ("SPC V w" . browse-at-remote)
      ("SPC w w" . other-window)
      ))
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
    '("insert" vundo-mode magit-popup-mode)
    '("normal")))

(elpaca-wait)


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
  (beginning-of-line)
  (newline-and-indent)
  (previous-line)
  (indent-for-tab-command)
  (kr-insert))

(defun kr-open-next-line ()
  (interactive)
  (deactivate-mark)
  (end-of-line)
  (kr-insert)
  (newline-and-indent))

(defun kr-describe-at-point (symbol)
  "Call `describe-symbol' for the SYMBOL at point."
  (interactive (list (symbol-at-point)))
  (cond ((eql last-command 'kr-describe-at-point)
         (dolist (window (window-list))
           (when (equal (buffer-name (window-buffer window))
                        "*Help*")
             (quit-window nil window))))
        ((and symbol (or (fboundp symbol)
                         (boundp symbol)
                         (facep symbol)))
         (describe-symbol symbol))
        (t (call-interactively #'describe-symbol))))

;;; Utils function and packcages used somewhere else in configuration

(defun kr-mac-p ()
  (if (string= system-type "darwin") t nil))

(provide 'preface-setup)
