;; -*- coding: utf-8; lexical-binding: t; -*-

;; (setq zot_bib "~/gitLocal/pensieve/papernotes.bib"
;;       ;org_notes "~/gitLocal/pensieve/papernotes.org"
;;       )

(setq ;bibtex-completion-notes-path org_notes
 bibtex-completion-bibliography  '(;"~/gitLocal/pensieve/SRL_paper_club.bib"
                                   "~/gitLocal/pensieve/myLibrary.bib"
                                  ;"~/gitlocal/pensieve/slinki_library.bib"
                                   )
 bibtex-completion-pdf-field "file"
 )

;(setq reftex-default-bibliography '(zot_bib))

(setq org-ref-completion-library 'org-ref-ivy-cite
      ;org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
;      org-ref-default-bibliography '(zot_bib)
      ;org-ref-bibliography-notes org_notes
     
      ;org-ref-pdf-directory "~/Documents/Literature/"
      )

;(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

;; Sometimes it is necessary to tell bibtex what dialect you are using to support the different bibtex entries that are possible in biblatex. You can do it like this globally.
(setq bibtex-dialect 'biblatex)

(require 'org-ref)

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (car (bibtex-completion-find-pdf key)))
         )
    (if (file-exists-p pdf-file)
        (org-open-file pdf-file)
      (message "No PDF found for %s" key))
    )
    )

(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

(global-set-key (kbd "H-o") 'org-ref-cite-hydra/body)

(provide 'init-org-ref)
