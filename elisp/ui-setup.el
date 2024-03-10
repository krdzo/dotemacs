;; start maximized
(when window-system
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(setq display-line-numbers-width 3)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(load-theme 'misterioso)
(set-face-background 'cursor "#338f86")


(provide 'ui-setup)
