;; start maximized
(when window-system
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(setq display-line-numbers-width 3)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(load-theme 'misterioso)
(set-face-background 'cursor "#338f86")

(elpaca nil
  (if kr-mac-p
      (set-face-attribute 'default nil :height 145)
    (set-face-attribute 'default nil :height 115)))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":")
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF4500")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF")
          ("NOTE"   . "#AAD700"))))

(provide 'ui-setup)
