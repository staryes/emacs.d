;; -*- coding: utf-8; lexical-binding: t; -*-

(with-eval-after-load 'ox
  (require 'ox-hugo))

(setq hugo_base_dir "~/gitLocal/cooking-around-the-world/"
      hugo-buffer "*hugo*")

(provide 'init-ox-hugo)