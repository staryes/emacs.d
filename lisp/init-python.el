;; -*- coding: utf-8; lexical-binding: t; -*-

(with-eval-after-load 'python
  ;; run command `pip install jedi flake8 importmagic` in shell,
  ;; or just check https://github.com/jorgenschaefer/elpy
  (unless (or (is-buffer-file-temp)
              (not buffer-file-name)
              ;; embed python code in org file
              (string= (file-name-extension buffer-file-name) "org"))
    )
  (elpy-enable)

  ;; Use IPython for REPL
  (setq elpy-shell-echo-output nil
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--pylab --simple-prompt -c exec('__import__(\\'readline\\')') -i" ;
        python-shell-prompt-detect-failure-warning nil)

  ;; Enable Flycheck
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  (setq elpy-remove-modeline-lighter t)
  (advice-add 'elpy-modules-remove-modeline-lighter
              :around (lambda (fun &rest args)
                        (unless (eq (car args) 'flymake-made)
                          (apply fun args))))

  ;; Enable autopep8
  (require 'py-autopep8)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

  )

;; hook lpy
(add-hook 'python-mode-hook #'lpy-mode )

;; http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem/3338#3338
;; emacs 24.4+
(setq electric-indent-chars (delq ?: electric-indent-chars))

(defalias 'workon 'pyvenv-workon)

(provide 'init-python)

