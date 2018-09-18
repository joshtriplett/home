;; Try to find and activate CUA mode early.  If something goes wrong
;; later, this ensures that CUA mode has already loaded before emacs
;; stops reading .emacs.
(cond ((fboundp 'cua-mode) (cua-mode t))
      ((fboundp 'CUA-mode) (CUA-mode t))
      ((require 'cua nil t) (CUA-mode t))
      (t (message "Can't find CUA mode")))

(setq inhibit-startup-message t
      inhibit-startup-buffer-menu t
      package-enable-at-startup nil
      ispell-program-name "aspell"
      make-backup-files nil
      diff-switches "-u"
      scroll-margin 8
      scroll-preserve-screen-position t
      scroll-step 1
      frame-title-format "%b - emacs")

(setq-default indent-tabs-mode nil)

(when (require 'bar-cursor nil t)
  (bar-cursor-mode 1))
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode 0))
(column-number-mode t)
(mouse-wheel-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Don't wait for window manager when changing font - avoids long delays.
(modify-frame-parameters nil '((wait-for-wm . nil)))

(defun my-kill ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'my-kill)

(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-w") 'backward-kill-word)

;; Disable VC mode
(remove-hook `find-file-hooks `vc-find-file-hook)
(remove-hook `find-file-not-found-hooks `vc-file-not-found-hook)
(setq vc-handled-backends ())

;; Use tabbar mode if available
(when (require 'tabbar nil t)
  (setq tabbar-buffer-groups-function
	(lambda (&optional b) (list "All Buffers")) )
  (setq tabbar-buffer-home-button '(("") "")
        tabbar-home-button '(("") "")
        tabbar-scroll-left-button '(("") "")
        tabbar-scroll-right-button '(("") ""))
  (tabbar-mode)
  (define-key esc-map (kbd "<left>") 'tabbar-backward)
  (define-key esc-map (kbd "<right>") 'tabbar-forward)
  (define-key esc-map (kbd "S-<left>") 'tabbar-backward)
  (define-key esc-map (kbd "S-<right>") 'tabbar-forward)
  (global-set-key (kbd "M-<left>") 'tabbar-backward)
  (global-set-key (kbd "M-<right>") 'tabbar-forward)
  (global-set-key (kbd "S-M-<left>") 'tabbar-backward)
  (global-set-key (kbd "S-M-<right>") 'tabbar-forward))

;; TeX
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (TeX-PDF-mode)))
(setq bibtex-align-at-equal-sign t)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tabbar-default ((t (:inherit variable-pitch :background "gray" :foreground "black"))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "white" :foreground "blue"))))
 '(tabbar-unselected ((t (:inherit tabbar-default)))))
