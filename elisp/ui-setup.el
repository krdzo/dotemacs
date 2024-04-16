;; start maximized
(when window-system
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(setq display-line-numbers-width 3)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(load-theme 'misterioso)
(set-face-background 'cursor "#338f86")

(if kr-mac-p
    (set-face-attribute 'default nil :height 125)
  (set-face-attribute 'default nil :height 115))

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


(global-hl-line-mode 1)
(set-face-background 'hl-line "#323D4D")
(set-face-background 'region "#5B7089")

(provide 'ui-setup)
