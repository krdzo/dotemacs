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


(provide 'apps-setup)
