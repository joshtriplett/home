;;;; Early customizations, which need to happen before anything else.

;; Add ~/.elisp to load-path early.  Some of the modes and features
;; loaded later may come from this directory.
(add-to-list 'load-path "~/.elisp")

;; Try to find and activate CUA mode early.  If something goes wrong
;; later, this ensures that CUA mode has already loaded before emacs
;; stops reading .emacs.
(cond ((fboundp 'cua-mode) (cua-mode t))
      ((fboundp 'CUA-mode) (CUA-mode t))
      ((require 'cua nil t) (CUA-mode t))
      (t (message "Can't find CUA mode")))

;;;; Global customizations

(setq inhibit-startup-message t
      inhibit-startup-buffer-menu t
      make-backup-files nil
      diff-switches "-u"
      scroll-margin 8
      scroll-preserve-screen-position t
      scroll-step 1
      frame-title-format "%b - emacs")

(setq-default indent-tabs-mode nil)

;; Set email address based on $EMAIL
(let (email (getenv "EMAIL"))
  (when email (setq user-mail-address email)))

(when (require 'bar-cursor nil t)
  (bar-cursor-mode 1))
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode 0))
(column-number-mode t)
(global-font-lock-mode t)
(mouse-wheel-mode t)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'right))

;; Don't show the menu in a terminal
(unless window-system
  (menu-bar-mode -1))

;; Don't wait for window manager when changing font - avoids long delays.
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; Make cut/copy/paste set/use the X CLIPBOARD (for use with C-v) in
;; preference to the X PRIMARY (for use with middle-mouse-button).
(setq x-select-enable-clipboard t)

;; Make mouse-based cut/copy/paste use the X PRIMARY
(defadvice mouse-yank-at-click (around yank-from-primary last act)
  (let ((x-select-enable-clipboard nil))
        ad-do-it))

(defadvice mouse-set-region (around kill-to-primary last act)
  (let ((x-select-enable-clipboard nil))
        ad-do-it))

(defadvice mouse-drag-region (around kill-to-primary last act)
  (let ((x-select-enable-clipboard nil))
        ad-do-it))

(defun my-kill ()
  "Kill current buffer, and tell any emacsclient pending on the
buffer to finish."
  (interactive)
  (if server-buffer-clients
      (server-edit)
    (kill-buffer (current-buffer))))
(global-set-key (kbd "C-x k") 'my-kill)

(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; Enable some disabled commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)

;; Disable VC mode
(remove-hook `find-file-hooks `vc-find-file-hook)
(remove-hook `find-file-not-found-hooks `vc-file-not-found-hook)
(setq vc-handled-backends ())

;; Use tabbar mode if available
(when (require 'tabbar nil t)
  (setq tabbar-buffer-groups-function
	(lambda (&optional b) (list "All Buffers")) )
  (tabbar-mode)
  (define-key esc-map (kbd "<left>") 'tabbar-backward)
  (define-key esc-map (kbd "<right>") 'tabbar-forward)
  (define-key esc-map (kbd "S-<left>") 'tabbar-backward)
  (define-key esc-map (kbd "S-<right>") 'tabbar-forward)
  (global-set-key (kbd "M-<left>") 'tabbar-backward)
  (global-set-key (kbd "M-<right>") 'tabbar-forward)
  (global-set-key (kbd "S-M-<left>") 'tabbar-backward)
  (global-set-key (kbd "S-M-<right>") 'tabbar-forward))

;; Make shifted direction keys work on the Linux console or in an xterm
(when (member (getenv "TERM") '("linux" "xterm"))
  (dolist (prefix '("\eO" "\eO1;" "\e[1;"))
    (dolist (m '(("2" . "S-") ("3" . "M-") ("4" . "S-M-") ("5" . "C-")
                 ("6" . "S-C-") ("7" . "C-M-") ("8" . "S-C-M-")))
      (dolist (k '(("A" . "<up>") ("B" . "<down>") ("C" . "<right>")
                   ("D" . "<left>") ("H" . "<home>") ("F" . "<end>")))
	(define-key function-key-map
		    (concat prefix (car m) (car k))
		    (read-kbd-macro (concat (cdr m) (cdr k))))))))

;;;; Major mode customizations

;; cc-mode
(defun my-c-mode-common-hook ()
  ;; Make enter automatically indent the next line
  (define-key c-mode-base-map (kbd "RET") 'c-context-line-break)
  ;; Use "linux" indentation style for files in a linux-2.6 directory
  (when (and buffer-file-name
             (string-match "/linux-2.6/" buffer-file-name))
    (c-set-style "linux"))
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(c-add-style "bsd4" '("bsd"
                      (c-basic-offset . 4)
                      (c-hanging-braces-alist . nil)))
(setq c-default-style "bsd4")

;; TeX
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (TeX-PDF-mode)))
(setq tex-dvi-view-command "xdvi")

;; nxml-mode
(when (require 'nxml-mode nil t)
  (push '("\\.xsd\\'" . nxml-mode) auto-mode-alist)
  (push '("\\.html\\'" . nxml-mode) auto-mode-alist)
  (push '("\\.kid\\'" . nxml-mode) auto-mode-alist)
  (fset 'html-mode 'nxml-mode)
  (fset 'xml-mode 'nxml-mode))

;; sieve-mode
(when (fboundp 'sieve-mode)
  (push '("\\.sieve\\'" . sieve-mode) auto-mode-alist))

;; css-mode
(defun my-css-mode-hook ()
  ;; Turn off mirror mode
  (cssm-leave-mirror-mode))
(add-hook 'css-mode-hook 'my-css-mode-hook)

;; makefile-mode
;; Disable whitespace cleanup in Makefiles
(setq makefile-cleanup-continuations-p nil
      makefile-cleanup-continuations nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(ecb-directories-menu-user-extension-function nil)
 '(ecb-history-menu-user-extension-function nil)
 '(ecb-methods-menu-user-extension-function nil)
 '(ecb-options-version "2.27")
 '(ecb-sources-menu-user-extension-function nil)
 '(global-font-lock-mode t)
 '(ispell-choices-win-default-height 4)
 '(ispell-program-name "aspell")
 '(nxml-slash-auto-complete-flag t)
 '(rng-schema-locating-files (quote ("schemas.xml" "~/.nxml-mode/schemas.xml" "/usr/share/emacs/site-lisp/nxml-mode/schema/schemas.xml")))
 '(tabbar-buffer-home-button (quote (("") "")))
 '(tabbar-home-button (quote (("") "")))
 '(tabbar-scroll-left-button (quote (("") "")))
 '(tabbar-scroll-right-button (quote (("") "")))
 '(transient-mark-mode t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tabbar-default ((t (:inherit variable-pitch :background "gray" :foreground "black" :height 0.8))))
 '(tabbar-default-face ((t (:inherit variable-pitch :background "gray" :foreground "black" :height 0.8))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "white" :foreground "blue"))))
 '(tabbar-selected-face ((t (:inherit tabbar-default-face :background "white" :foreground "blue"))))
 '(tabbar-unselected ((t (:inherit tabbar-default))))
 '(tabbar-unselected-face ((t (:inherit tabbar-default-face)))))
