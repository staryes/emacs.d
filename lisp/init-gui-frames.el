;; -*- coding: utf-8; lexical-binding: t; -*-

;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Show a marker in the left fringe for lines not in the buffer
(setq indicate-empty-lines t)

;; =======
;; already set in init-essential.el
;; =======
;; NO tool bar
;; (if (fboundp 'tool-bar-mode)
;;   (tool-bar-mode -1))
;; ;; no scroll bar
;; (if (fboundp 'set-scroll-bar-mode)
;;   (set-scroll-bar-mode nil))
;; ;; no menu bar
;; (if (fboundp 'menu-bar-mode)
;;   (menu-bar-mode -1))

(defun set-frame-size-according-to-resolution ()
    (interactive)
    (if window-system
    (progn
      (if (> (x-display-pixel-width) 1500) ;; 1500 is the delimiter marging in px to consider the screen big
             (set-frame-width (selected-frame) 173) ;; on the big screen make the fram 237 columns big
             (set-frame-width (selected-frame) 158)) ;; on the small screen we use 177 columns
      (setq my-height (/ (- (x-display-pixel-height) 150) ;; cut 150 px of the screen height and use the rest as height for the frame
                               (frame-char-height)))
      (set-frame-height (selected-frame) my-height)
      (set-frame-position (selected-frame) 83 50) ;; position the frame 3 pixels left and 90 px down
      )))
 ;; (set-frame-size-according-to-resolution)
(global-set-key (kbd "C-x 9") 'set-frame-size-according-to-resolution)

(set-frame-size-according-to-resolution)

(provide 'init-gui-frames)
