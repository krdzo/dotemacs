(save-place-mode 1)
(setq calendar-date-style 'european)
(setq calendar-week-start-day 1)
(setq isearch-lazy-count t)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)


(setq mouse-wheel-tilt-scroll t)
(when (kr-mac-p)
  (setq mouse-wheel-flip-direction t))

(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  ;; here because it needs to be after no-littering
  ;; should be moved somewhere else
  (when (file-exists-p custom-file)
    (load-file custom-file))
  (no-littering-theme-backups)

  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(use-package recentf
  :ensure nil
  :config
  (with-eval-after-load 'no-littering
    (recentf-mode 1))
  (setq recentf-max-saved-items 75)
  (setq recentf-exclude `(,(expand-file-name "eln-cache/" user-emacs-directory))))

(use-package consult
  :bind (([remap list-buffers] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap project-list-buffers] . consult-project-buffer)
         ([remap project-find-regexp] . consult-ripgrep)
         (:map meow-normal-state-keymap
          ("/" . consult-line)))
  :config
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip --hidden --glob=!.git")
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

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :config
  (marginalia-mode 1)
  (setq marginalia-annotators '(marginalia-annotators-heavy
                                marginalia-annotators-light nil)))

;;; Mac Os
(defvar (kr-mac-p) (if (string= system-type "darwin") t nil))

(setq ns-command-modifier 'super)
(setq ns-option-modifier 'meta)

(when (kr-mac-p)
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


;;; window menagment
(setq switch-to-buffer-obey-display-actions t)
(defun kr-swith-to-buffer-obey ()
  (interactive)
  (let ((switch-to-buffer-obey-display-actions nil))
    (call-interactively 'switch-to-buffer)))
(define-key global-map (kbd "C-x C-S-b") 'kr-swith-to-buffer-obey)

;; Custom display function copied from built in funciton
(defun kr-display-buffer-reuse-window (buffer alist)
  "Same ad `display-buffer-reuse-window' just doesn't respect
'inhibit-same-window' alist variable"
  (let* ((alist-entry (assq 'reusable-frames alist))
         (frames (cond (alist-entry (cdr alist-entry))
                       ((if (eq pop-up-frames 'graphic-only)
                            (display-graphic-p)
                          pop-up-frames)
                        0)
                       (display-buffer-reuse-frames 0)
                       (t (last-nonminibuffer-frame))))
         (window (if (eq buffer (window-buffer))
                     (selected-window)
                   ;; Preferably use a window on the selected frame,
                   ;; if such a window exists (Bug#36680).
                   (let* ((windows (delq (selected-window)
                                         (get-buffer-window-list
                                          buffer 'nomini frames)))
                          (first (car windows))
                          (this-frame (selected-frame)))
                     (cond
                      ((eq (window-frame first) this-frame)
                       first)
                      ((catch 'found
                         (dolist (next (cdr windows))
                           (when (eq (window-frame next) this-frame)
                             (throw 'found next)))))
                      (t first))))))
    (when (window-live-p window)
      (prog1 (window--display-buffer buffer window 'reuse alist)
        (unless (cdr (assq 'inhibit-switch-frame alist))
          (window--maybe-raise-frame (window-frame window)))))))
(provide 'ux-setup)
