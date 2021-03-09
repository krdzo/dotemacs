;; disable package.el in favor of straight.el
(setq package-enable-at-startup nil)


;; Do not resize the frame at this early stage.
;; Doesn't change load times, it's just more visualy appealing like this
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-echo-area-message "ivan")



;; ovo je za brze loadovanje emacs-a
;; ali cu ga tek ukljuciti kad budem imao vremena da se
;; zazam sa ovim sad sam ga samo postavio radi podsecanja
;; (setq generated-autoload-file t)
;; (setq package-quickstart t)
