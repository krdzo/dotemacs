(save-place-mode 1)
(setq calendar-date-style 'european)
(setq calendar-week-start-day 1)
(setq isearch-lazy-count t)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 75)
  (setq recentf-exclude `(,(expand-file-name "eln-cache/" user-emacs-directory))))

(use-package consult
  :bind (([remap list-buffers] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap project-list-buffers] . consult-project-buffer)
         (:map meow-normal-state-keymap
          ("/" . consult-line)))
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (define-key global-map [remap list-buffers] 'consult-buffer)
  (define-key global-map [remap yank-pop] 'consult-yank-pop)
  (define-key global-map [remap project-list-buffers] 'consult-project-buffer)

  ;; preview only works with consult commands
  (setq consult-preview-key 'any)
  (with-eval-after-load 'consult
    (consult-customize
     consult-project-buffer
     :preview-key "C-o"
     consult-buffer
     :preview-key "C-o")))


;;; Mac Os
(defvar kr-mac-p (if (string= system-type "darwin") t nil))

(setq ns-command-modifier 'super)
(setq ns-option-modifier 'meta)

(when kr-mac-p
  (setq trash-directory  (expand-file-name "~/.Trash/"))

  (when (string= system-type "darwin")
    (setq mac-option-modifier 'meta))

  (use-package exec-path-from-shell
   :config
   (require 'exec-path-from-shell)
   (when (memq window-system '(mac ns))
     (dolist (var '("NPM_TOKEN" "NVM_DIR" "INFOPATH"))
       (add-to-list 'exec-path-from-shell-variables var))
     (exec-path-from-shell-initialize))))

(use-package elisp-demos
    :config
    (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))

;;; Util functions
(defun kr-er-auto-create-missing-dirs ()
      (let ((target-dir (file-name-directory buffer-file-name)))
        (unless (file-exists-p target-dir)
          (make-directory target-dir t))))
(add-to-list 'find-file-not-found-functions #'kr-er-auto-create-missing-dirs)

;; ANSI colors
(defun kr-display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun ansi-color-after-scroll (window start)
  "Used by ansi-color-mode minor mode"
  (ansi-color-apply-on-region start (window-end window t) t))

(define-minor-mode ansi-color-mode
  "A very primitive minor mode to view log files containing ANSI color codes.

Pros: this minor mode runs `ansi-color-apply-on-region' lazily,
i.e. only the visible part of the buffer. Hence, it does NOT
freeze Emacs even if the log file is huge.

Cons: a) when the minor code is toggled off, it does not undo
what has already been ansi colorized. b) assumes the buffer
content etc. does not change. c) jumping to random places within
the buffer may incur incorrect/incomplete colorization.

How to install: put this code into your init.el, then evaluate it or
restart Emacs for the code to take effect.

How to use: in the log buffer of need run `M-x ansi-color-mode'.
Alternatively, feel free to enable this minor mode via mode hooks
so that you needn't enable it manually.

-- lgfang
"
  :global nil
  :lighter ""
  (if ansi-color-mode
      (progn
        (ansi-color-apply-on-region (window-start) (window-end) t)
        (add-hook 'window-scroll-functions 'ansi-color-after-scroll 80 t))
    (remove-hook 'window-scroll-functions 'ansi-color-after-scroll t)))

;; UNIX timestamp
(defun kr-unix-ts-to-str (&optional time zone)
  "Convert unix timestamp integer to human-readable string in RFC3339 format."
  (interactive "nTimestamp: ")
  (setq zone (or zone "UTC"))
  (setq ts-str (format "%s" (or time (current-word))))
  (if (numberp (read ts-str))
      (progn
        (setq ts-int (string-to-number ts-str))
        ;; send message to Message buffer
        ;; copy to kill-ring (clipboard)
        (setq rfc_str (format-time-string "%Y-%m-%dT%H:%M:%S%z" ts-int zone))
        (message (format "%d %s ==> %s" ts-int zone rfc_str))
        (kill-new rfc_str))

    (message "not a number")))

(defun kr-unix-ts-to-str-dwim ()
  (interactive)
  (if mark-active
      (let ((ts (buffer-substring-no-properties (region-beginning) (region-end))))
        (kr-unix-ts-to-str ts))
    (call-interactively #'kr-unix-ts-to-str)))

(provide 'ux-setup)
