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
  (setq recentf-exclude `(,(expand-file-name "straight/build/" user-emacs-directory)
                          ,(expand-file-name "eln-cache/" user-emacs-directory))))

(use-package consult
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


;; Mac Os
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




(provide 'ux-setup)
