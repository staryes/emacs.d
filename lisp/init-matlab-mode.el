;;(require-package 'matlab-mode)

(autoload 'octave-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
 'auto-mode-alist
 '("\\.m$" . octave-mode))
(setq octave-indent-function t)
(setq octave-shell-command "matlab")

(provide 'init-matlab-mode)
