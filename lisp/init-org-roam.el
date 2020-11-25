;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'org-roam)

(add-hook 'after-init-hook 'org-roam-mode)

(setq org-roam-directory "~/gitLocal/pensieve/")
(setq org-roam-buffer-width 0.17)

(define-key org-roam-mode-map (kbd "C-c n l") 'org-roam)
(define-key org-roam-mode-map (kbd "C-c n f") 'org-roam-find-file)
(define-key org-roam-mode-map (kbd "C-c n g") 'org-roam-graph-show)
(define-key org-roam-mode-map (kbd "C-c n i") 'org-roam-insert)
;(define-key org-roam-mode-map (kbd "C-c n I") 'org-roam-insert-immediate)

(provide 'init-org-roam)