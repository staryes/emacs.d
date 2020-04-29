;; -*- coding: utf-8; lexical-binding: t; -*-

(add-to-list 'load-path "~/gitLocal/matlab-emacs-src/")
(load-library "matlab-load")

;; Enable CEDET feature support for MATLAB code. (Optional)
;; (matlab-cedet-setup)

                                        ;(require 'matlab-mode)

(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function nil)
                                        ;(setq matlab-shell-command "matlab")

;; CLI matlab from the shell:
;; /Applications/MATLAB_R2016a.app/bin/matlab -nodesktop

;; elisp setup for matlab-mode:
(setq matlab-shell-command "/Applications/MATLAB_R2020a.app/bin/matlab")
(setq matlab-shell-command-switches (list "-nodesktop"))

;; (with-eval-after-load 'matlab-mode
;;   (define-key matlab-mode-map (kbd "C-.") 'company-ctags))

;;                                         ;(define-key octave-mode-map (kbd "C-.") 'company-ctags)

;; (add-hook 'matlab-mode-hook '(lambda () (auto-fill-mode nil)))


(provide 'init-matlab-mode)
