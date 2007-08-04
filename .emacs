;; Disable the startup message
(setq inhibit-startup-message t)

;; Disable the buffer list when starting with multiple files
(setq inhibit-startup-buffer-menu t)

;; Turn off the blinking cursor
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

;; Add ~/.elisp to load-path
(add-to-list 'load-path "~/.elisp")

;; Run the emacs server
(server-start)

;; Activate CUA mode
(cond ((fboundp 'cua-mode) (cua-mode t))
      ((fboundp 'CUA-mode) (CUA-mode t))
      ((require 'cua nil t) (CUA-mode t))
      (t (message "Can't find CUA mode")))

;; Set email address based on $EMAIL
(let (email (getenv "EMAIL"))
  (if email (setq user-mail-address email)))

;; Activate mouse-wheel mode
(mouse-wheel-mode t)

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

;; Assign Ctrl-a to mark-whole-buffer
(global-set-key [(control a)] 'mark-whole-buffer)

;; Activate font-lock-mode (syntax coloring)
(global-font-lock-mode t)

;; Activate column-number-mode
(column-number-mode t)

;; Don't make backup files
(setq make-backup-files nil)

;; Enable some disabled commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)

;; Make the cursor a vertical bar instead of a block
(when (require 'bar-cursor nil t)
  (bar-cursor-mode 1))

;; Use tabbar mode if available
(when (require 'tabbar nil t)
  (setq tabbar-buffer-groups-function (lambda (b) (list "All Buffers")) )
  (tabbar-mode)
  (define-key esc-map [left] 'tabbar-backward)
  (define-key esc-map [right] 'tabbar-forward)
  (define-key esc-map [S-left] 'tabbar-backward)
  (define-key esc-map [S-right] 'tabbar-forward)
  (global-set-key [M-left] 'tabbar-backward)
  (global-set-key [M-right] 'tabbar-forward)
  (global-set-key [S-M-left] 'tabbar-backward)
  (global-set-key [S-M-right] 'tabbar-forward))

;; Indent using spaces, never tabs
(setq-default indent-tabs-mode nil)

;; default to unified diffs
(setq diff-switches "-u")

;; Hook for cc-mode customizations
(defun my-c-mode-common-hook ()
  ;; Make enter automatically indent the next line
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;; Set the default style to bsd
(setq c-default-style "bsd")
;; Set the indent size to 4 spaces
(setq c-basic-offset 4)
;; Never put a brace on the same line as another statement
(setq c-hanging-braces-alist nil)

;; TeX
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (TeX-PDF-mode)))

;; nxml-mode setup
(when (require 'nxml-mode nil t)
  (push '("\\.xsd\\'" . nxml-mode) auto-mode-alist)
  (push '("\\.html\\'" . nxml-mode) auto-mode-alist)
  (push '("\\.kid\\'" . nxml-mode) auto-mode-alist)
  (fset 'html-mode 'nxml-mode)
  (fset 'xml-mode 'nxml-mode))

;; Hook for css-mode customizations
(defun my-css-mode-hook ()
  ;; Turn off mirror mode
  (cssm-leave-mirror-mode))
(add-hook 'css-mode-hook 'my-css-mode-hook)

;; Disable VC mode
(remove-hook `find-file-hooks `vc-find-file-hook)
(remove-hook `find-file-not-found-hooks `vc-file-not-found-hook)
(setq vc-handled-backends ())

;; Disable whitespace cleanup in Makefiles
(setq makefile-cleanup-continuations-p nil)

;; Don't show the menu in a terminal
(unless window-system (menu-bar-mode -1))

;; Put scroll bars on the right
(when (fboundp 'set-scroll-bar-mode)
      (set-scroll-bar-mode 'right))

;; Wrap lines at 78 columns
(setq-default fill-column 78)

;; Use xdvi to view TeX dvi files
(setq tex-dvi-view-command "xdvi")

;; Include filename in title bar
(setq frame-title-format "%b - emacs")

;; Don't wait for window manager when changing font - avoids long delays.
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; Make shifted direction keys work on the Linux console or in an xterm
(when (or (string= (getenv "TERM") "linux")
          (string= (getenv "TERM") "xterm"))
  (define-key function-key-map "\eO2A"   [S-up])
  (define-key function-key-map "\eO1;2A" [S-up])
  (define-key function-key-map "\eO2B"   [S-down])
  (define-key function-key-map "\eO1;2B" [S-down])
  (define-key function-key-map "\eO2D"   [S-left])
  (define-key function-key-map "\eO1;2D" [S-left])
  (define-key function-key-map "\eO2C"   [S-right])
  (define-key function-key-map "\eO1;2C" [S-right])
  (define-key function-key-map "\eO2H"   [S-home])
  (define-key function-key-map "\eO1;2H" [S-home])
  (define-key function-key-map "\eO2F"   [S-end])
  (define-key function-key-map "\eO1;2F" [S-end])
  (define-key function-key-map "\eO3A"   [M-up])
  (define-key function-key-map "\eO1;3A" [M-up])
  (define-key function-key-map "\eO3B"   [M-down])
  (define-key function-key-map "\eO1;3B" [M-down])
  (define-key function-key-map "\eO3D"   [M-left])
  (define-key function-key-map "\eO1;3D" [M-left])
  (define-key function-key-map "\eO3C"   [M-right])
  (define-key function-key-map "\eO1;3C" [M-right])
  (define-key function-key-map "\eO3H"   [M-home])
  (define-key function-key-map "\eO1;3H" [M-home])
  (define-key function-key-map "\eO3F"   [M-end])
  (define-key function-key-map "\eO1;3F" [M-end])
  (define-key function-key-map "\eO4A"   [S-M-up])
  (define-key function-key-map "\eO1;4A" [S-M-up])
  (define-key function-key-map "\eO4B"   [S-M-down])
  (define-key function-key-map "\eO1;4B" [S-M-down])
  (define-key function-key-map "\eO4D"   [S-M-left])
  (define-key function-key-map "\eO1;4D" [S-M-left])
  (define-key function-key-map "\eO4C"   [S-M-right])
  (define-key function-key-map "\eO1;4C" [S-M-right])
  (define-key function-key-map "\eO4H"   [S-M-home])
  (define-key function-key-map "\eO1;4H" [S-M-home])
  (define-key function-key-map "\eO4F"   [S-M-end])
  (define-key function-key-map "\eO1;4F" [S-M-end])
  (define-key function-key-map "\eO5A"   [C-up])
  (define-key function-key-map "\eO1;5A" [C-up])
  (define-key function-key-map "\eO5B"   [C-down])
  (define-key function-key-map "\eO1;5B" [C-down])
  (define-key function-key-map "\eO5D"   [C-left])
  (define-key function-key-map "\eO1;5D" [C-left])
  (define-key function-key-map "\eO5C"   [C-right])
  (define-key function-key-map "\eO1;5C" [C-right])
  (define-key function-key-map "\eO5H"   [C-home])
  (define-key function-key-map "\eO1;5H" [C-home])
  (define-key function-key-map "\eO5F"   [C-end])
  (define-key function-key-map "\eO1;5F" [C-end])
  (define-key function-key-map "\eO6A"   [S-C-up])
  (define-key function-key-map "\eO1;6A" [S-C-up])
  (define-key function-key-map "\eO6B"   [S-C-down])
  (define-key function-key-map "\eO1;6B" [S-C-down])
  (define-key function-key-map "\eO6D"   [S-C-left])
  (define-key function-key-map "\eO1;6D" [S-C-left])
  (define-key function-key-map "\eO6C"   [S-C-right])
  (define-key function-key-map "\eO1;6C" [S-C-right])
  (define-key function-key-map "\eO6H"   [S-C-home])
  (define-key function-key-map "\eO1;6H" [S-C-home])
  (define-key function-key-map "\eO6F"   [S-C-end])
  (define-key function-key-map "\eO1;6F" [S-C-end])
  (define-key function-key-map "\eO7A"   [C-M-up])
  (define-key function-key-map "\eO1;7A" [C-M-up])
  (define-key function-key-map "\eO7B"   [C-M-down])
  (define-key function-key-map "\eO1;7B" [C-M-down])
  (define-key function-key-map "\eO7D"   [C-M-left])
  (define-key function-key-map "\eO1;7D" [C-M-left])
  (define-key function-key-map "\eO7C"   [C-M-right])
  (define-key function-key-map "\eO1;7C" [C-M-right])
  (define-key function-key-map "\eO7H"   [C-M-home])
  (define-key function-key-map "\eO1;7H" [C-M-home])
  (define-key function-key-map "\eO7F"   [C-M-end])
  (define-key function-key-map "\eO1;7F" [C-M-end])
  (define-key function-key-map "\eO8A"   [S-C-M-up])
  (define-key function-key-map "\eO1;8A" [S-C-M-up])
  (define-key function-key-map "\eO8B"   [S-C-M-down])
  (define-key function-key-map "\eO1;8B" [S-C-M-down])
  (define-key function-key-map "\eO8D"   [S-C-M-left])
  (define-key function-key-map "\eO1;8D" [S-C-M-left])
  (define-key function-key-map "\eO8C"   [S-C-M-right])
  (define-key function-key-map "\eO1;8C" [S-C-M-right])
  (define-key function-key-map "\eO8H"   [S-C-M-home])
  (define-key function-key-map "\eO1;8H" [S-C-M-home])
  (define-key function-key-map "\eO8F"   [S-C-M-end])
  (define-key function-key-map "\eO1;8F" [S-C-M-end])
)

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
 '(tabbar-home-button (quote (("") "")))
 '(tabbar-scroll-left-button (quote (("") "")))
 '(tabbar-scroll-right-button (quote (("") "")))
 '(transient-mark-mode t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tabbar-default-face ((t (:inherit variable-pitch :background "gray" :foreground "black" :height 0.8))))
 '(tabbar-selected-face ((t (:inherit tabbar-default-face :background "white" :foreground "blue"))))
 '(tabbar-unselected-face ((t (:inherit tabbar-default-face)))))
