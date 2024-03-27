(use-package kubernetes
  :bind (:map kubernetes-overview-mode-map
         ("S-<tab>" . magit-section-cycle)
         ("C-<tab>" . tab-next))
  :hook ((kubernetes-logs-mode . visual-line-mode)
         (kubernetes-logs-mode . display-line-numbers-mode)
         (kubernetes-logs-mode . ansi-color-mode))
  :init
  (setq kubernetes-overview-custom-views-alist
        '((custom-overview . (context statefulsets deployments))))
  (setq kubernetes-default-overview-view 'custom-overview))


;;; Dired
(defun kr-revert-dired-remote ()
  (unless (file-remote-p default-directory)
    (auto-revert-mode)))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("K" . dired-kill-subdir)
              ("<mouse-2>" . dired-mouse-find-file)
              ("C-c '" . dired-toggle-read-only)
              ("/" . dired-goto-file))
  :hook ((dired-mode . kr-revert-dired-remote)
         (dired-mode . toggle-truncate-lines))
  :config
  (put 'dired-jump 'repeat-map nil)
  (setq dired-dwim-target t)
  (setq dired-isearch-filenames 'dwim)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-create-destination-dirs 'always)
  (setq dired-listing-switches "-valh --group-directories-first")

  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program (executable-find "gls")
          dired-listing-switches "-aBhl --group-directories-first")))

(provide 'apps-setup)
