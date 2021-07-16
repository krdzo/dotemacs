(when (version<  "28" emacs-version)
    (setq straight-base-dir "~/.emacsen/"))

(defvar kr/config-org "init-kr.org"
  "Name of my config file")
(org-babel-load-file (expand-file-name kr/config-org user-emacs-directory))

