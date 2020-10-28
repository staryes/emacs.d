;; -*- coding: utf-8; lexical-binding: t; -*-

;; some cool org tricks
;; @see http://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'org-clock
  ;; Change task state to STARTED when clocking in
  (setq org-clock-in-switch-to-state "STARTED")
  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; Show the clocked-in task - if any - in the header line
  (defun sanityinc/show-org-clock-in-header-line ()
    ;(setq-default header-line-format '((" " org-mode-line-string " ")))
    ;; 将header-line-format设置为mode-line-misc-info的值，可以将计时器加入
    (setq-default header-line-format 'mode-line-misc-info)
    )

  (defun sanityinc/hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))

  (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))

;; {{ org2nikola set up
(setq org2nikola-output-root-directory "~/.config/nikola")
;; }}

;; using dvisvgm to show formula in preview latex 
(setq org-latex-create-formula-image-program 'dvisvgm)

(defun org-demote-or-promote (&optional is-promote)
  "Demote or promote current org tree."
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (unless (or (region-active-p)
                (let ((line (thing-at-point 'line t)))
                  (and (string-match-p "^\\*+ $" line) ;; is node only one spaced
                       (= (point) (- (point-max) (length line))) ;; is line at EOF
                       )))
      (org-mark-subtree)))
  (if is-promote (org-do-promote) (org-do-demote)))

;; {{ @see http://orgmode.org/worg/org-contrib/org-mime.html
(with-eval-after-load 'org-mime
  (setq org-mime-export-options '(:section-numbers nil :with-author nil :with-toc nil))
  (defun org-mime-html-hook-setup ()
    (org-mime-change-element-style "pre"
                                   "color:#E6E1DC; background-color:#232323; padding:0.5em;")
    (org-mime-change-element-style "blockquote"
                                   "border-left: 2px solid gray; padding-left: 4px;"))
  (add-hook 'org-mime-html-hook 'org-mime-html-hook-setup))
;; }}

(defun org-mode-hook-setup ()
  (unless (is-buffer-file-temp)
    (setq evil-auto-indent nil)

    ;; org-mime setup, run this command in org-file, than yank in `message-mode'
    (local-set-key (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)

    ;; don't spell check double words
    (setq-local wucuo-flyspell-check-doublon nil)

    ;; create updated table of contents of org file
    ;; @see https://github.com/snosov1/toc-org
    (toc-org-enable)

    ;; default `org-indent-line' inserts extra spaces at the beginning of lines
    (setq-local indent-line-function 'indent-relative)

    ;; display wrapped lines instead of truncated lines
    (setq truncate-lines nil)
    (setq word-wrap t)))
(add-hook 'org-mode-hook 'org-mode-hook-setup)

(with-eval-after-load 'org
  ;; {{
  (defvar my-org-src--saved-temp-window-config nil
    "Window layout before edit special element.")
  (defun my-org-edit-special (&optional arg)
    "Save current window layout before `org-edit' buffer is open.
ARG is ignored."
    (setq my-org-src--saved-temp-window-config (current-window-configuration)))

  (defun my-org-edit-src-exit ()
    "Restore the window layout that was saved before `org-edit-special' is called."
    (when my-org-src--saved-temp-window-config
      (set-window-configuration my-org-src--saved-temp-window-config)
      (setq my-org-src--saved-temp-window-config nil)))


  ;; org 9.3 do not restore windows layout when editing special element
  (advice-add 'org-edit-special :before 'my-org-edit-special)
  (advice-add 'org-edit-src-exit :after 'my-org-edit-src-exit)
  ;; }}

  (my-ensure 'org-clock)

  ;; org-re-reveal requires org 8.3 while Emacs 25 uses org 8.2
  (when *emacs26*
    (my-ensure 'org-re-reveal))

  ;; odt export
  (add-to-list 'org-export-backends 'odt)

  ;; markdown export
  (my-ensure 'ox-md)
  (add-to-list 'org-export-backends 'md)

  (defun org-agenda-show-agenda-and-todo (&optional arg)
    "Better org-mode agenda view."
    (interactive "P")
    (org-agenda arg "n"))


  (defun my-org-open-at-point-hack (orig-func &rest args)
    "\"C-u M-x org-open-at-point\" to open link with `browse-url-generic-program'.
It's value could be customized liked \"/usr/bin/firefox\".
\"M-x org-open-at-point\" to open the url with embedded emacs-w3m."
    (let* ((arg (nth 0 args))
           (reference-buffer (nth 1 args))
           (browse-url-browser-function
            (cond
             ;; open with `browse-url-generic-program'
             ((equal arg '(4)) 'browse-url-generic)
             ;; open with w3m
             (t 'w3m-browse-url))))
      (apply orig-func args)))
  (advice-add 'org-open-at-point :around #'my-org-open-at-point-hack)

  (defun my-org-publish-hack (orig-func &rest args)
    "Stop running `major-mode' hook when `org-publish'."
    (let* ((load-user-customized-major-mode-hook nil))
      (apply orig-func args)))
  (advice-add 'org-publish :around #'my-org-publish-hack)

  ;; {{ convert to odt
  (defun my-setup-odt-org-convert-process ()
    (interactive)
    (let* ((cmd "/Applications/LibreOffice.app/Contents/MacOS/soffice"))
      (when (and *is-a-mac* (file-exists-p cmd))
        ;; org v8
        (setq org-odt-convert-processes
              '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i"))))))
  (my-setup-odt-org-convert-process)
  ;; }}

  (defun my-org-refile-hack (orig-func &rest args)
    "When `org-refile' scans org files,
skip user's own code in `org-mode-hook'."
    (let* ((force-buffer-file-temp-p t))
      (apply orig-func args)))
  (advice-add 'org-refile :around #'my-org-refile-hack)

  ;; {{ export org-mode in Chinese into PDF
  ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
  ;; and you need install texlive-xetex on different platforms
  ;; To install texlive-xetex:
  ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f")) ;; org v8
  ;; }}

  ;; misc
  (setq org-log-done t
        org-completion-use-ido t
        org-edit-src-content-indentation 0
        org-edit-timestamp-down-means-later t
        org-agenda-start-on-weekday nil
        org-agenda-span 14
        org-agenda-include-diary t
        org-agenda-window-setup 'current-window
        org-fast-tag-selection-single-key 'expert
        org-export-kill-product-buffer-when-displayed t
        ;; org-startup-indented t
        ;; {{ org 8.2.6 has some performance issue. Here is the workaround.
        ;; @see http://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
        org-agenda-inhibit-startup t ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        ;; }}
        ;; org v8
        org-odt-preferred-output-format "doc"
        org-tags-column 80

        ;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
        org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                                  (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)")))
        org-imenu-depth 9
        ;; @see http://irreal.org/blog/1
        org-src-fontify-natively t))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   ;(julia . t)
   (python . t)
   (jupyter . t)
   (matlab . t)
   (octave . t)))

;;work with image setting
;;from  https://llazarek.com/2018/10/images-in-org-mode.html
(defvar ll/org/insert-screenshot/redisplay-images t
  "Redisplay images after inserting a screenshot with
`ll/org/insert-screenshot'?")

(defun ll/org/insert-screenshot (&optional arg)
  "Capture a screenshot and insert a link to it in the current
buffer. If `ll/org/insert-screenshot/redisplay-images' is non-nil,
redisplay images in the current buffer.

By default saves images to ./resources/screen_%Y%m%d_%H%M%S.png,
creating the resources directory if necessary.

With a prefix arg (C-u) prompt for a filename instead of using the default.

Depends upon `import` from ImageMagick."
  (interactive)
  (unless (or arg
              (file-directory-p "./resources"))
    (make-directory "resources"))
  (let* ((default-dest
           (format-time-string "./resources/screen_%Y%m%d_%H%M%S.png"))
         (dest (if arg
                   (helm-read-string "Save to: " default-dest)
                 default-dest)))
    (start-process "import" nil "/usr/bin/import" dest)
    (read-char "Taking screenshot... Press any key when done.")
    (org-insert-link t (concat "file:" dest) "")
    (when ll/org/insert-screenshot/redisplay-images
      (org-remove-inline-images)
      (org-display-inline-images))))


(defvar ll/org/edit-image/redisplay-images t
  "Redisplay images after editing an image with `ll/org/edit-image'?")

(defun ll/org/edit-image (&optional arg)
  "Edit the image linked at point. If
`ll/org/insert-screenshot/redisplay-images' is non-nil, redisplay
images in the current buffer."
  (interactive)
  (let ((img (ll/org/link-file-path-at-point)))
    (start-process "gimp" nil "/usr/bin/gimp" img)
    (read-char "Editing image... Press any key when done.")
    (when ll/org/edit-image/redisplay-images
      (org-remove-inline-images)
      (org-display-inline-images))))

(defun ll/org/resize-image-at-point (&optional arg)
  "Resize the image linked at point. If
`ll/org/insert-screenshot/redisplay-images' is non-nil, redisplay
images in the current buffer."
  (interactive)
  (let ((img (ll/org/link-file-path-at-point))
        (percent (read-number "Resize to what percentage of current size? ")))
    (start-process "mogrify" nil "/usr/bin/mogrify"
                   "-resize"
                   (format "%s%%" percent)
                   img)
    (when ll/org/edit-image/redisplay-images
      (org-remove-inline-images)
      (org-display-inline-images))))

(defun ll/org/link-file-path-at-point ()
  "Get the path of the file referred to by the link at point."
  (let* ((org-element (org-element-context))
         (is-subscript-p (equal (org-element-type org-element) 'subscript))
         (is-link-p (equal (org-element-type org-element) 'link))
         (is-file-p (equal (org-element-property :type org-element) "file")))
    (when is-subscript-p
      (user-error "Org thinks you're in a subscript. Move the point and try again."))
    (unless (and is-link-p is-file-p)
      (user-error "Not on file link"))
    (expand-file-name (org-element-property :path org-element))))

(provide 'init-org)
