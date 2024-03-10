(use-package puni)
(use-package expand-region)

(defun meow-configure ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-define-keys 'motion
    '("j" . next-line)
    '("k" . previous-line)
    '("`" . kr-last-buffer)
    '("<escape>" . ignore))
  (meow-define-keys 'leader
    ;; SPC j/k will run the original command in MOTION state.
    (cons "p"  project-prefix-map)
    '("v" . magit-status)
    '("`" . "H-`")
    '("j" . "H-j")
    '("k" . "H-k")
    '("/" . meow-keypad-describe-key)
    '("?" . meow-cheatsheet))
  (meow-define-keys 'normal
    '("`" . kr-last-buffer)
    '("@" . goto-line)
    '("/" . imenu)
    '(";" . exchange-point-and-mark)
    '("{" . backward-paragraph)
    '("}" . forward-paragraph)
    '("0" . move-beginning-of-line)
    '("r" . meow-replace)
    '("q" . meow-quit)
    '("x" . backward-delete-char-untabify)
    '("X" . delete-char)
    '("d" . kill-region)
    '("D" . puni-kill-line)
    '("y" . copy-region-as-kill)
    '("p" . yank)
    '("u" . undo)
    '("o" . meow-open-below)
    '("O" . meow-open-above)
    '("c" . kr-change)
    '("j" . next-line)
    '("e" . forward-word)
    '("b" . backward-word)
    ;; '("C-j" . scroll-up-command)
    ;; '("C-k" . scroll-down-command)
    '("s" . er/expand-region)
    '("S" . er/contract-region)
    '("k" . previous-line)
    '("h" . backward-char)
    '("l" . forward-char)
    '("v" . set-mark-command)
    '("L" . move-end-of-line)
    '("H" . beginning-of-line-text)
    '("<escape>" . keyboard-quit)))

(defun kr-last-buffer ()
  "Switch to last viewed buffer in the same window."
    (interactive)
    (let ((switch-to-buffer-obey-display-actions nil))
      (call-interactively #'meow-last-buffer)))

(defun kr-change ()
  "If region active delete and go to insert state.
If no region is active delete  char and go to insert."
 (interactive
  (if mark-active
      (delete-region (region-beginning) (region-end))
    (delete-char 1))
  (meow-insert)))

(defun slick-cut (beg end)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-region :before #'slick-cut)

(defun slick-copy (beg end)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'copy-region-as-kill :before #'slick-copy)

(use-package meow
  :config
  (meow-configure)
  (meow-global-mode 1)
  (setq meow-keypad-leader-dispatch "C-c")
  (setq meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-ansi))

(provide 'meow-setup)
