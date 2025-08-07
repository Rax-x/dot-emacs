;;; Melpa

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Packages

(defun ensure-package-installed (packages)
  (dolist (package packages)
    (unless (package-installed-p package)
      (when (y-or-n-p (format "Package %s is missing. Install it? " package))
        (package-install package)))))

(ensure-package-installed 
 '(rust-mode
   spacemacs-theme
   zenburn-theme 
   company 
   doom-themes 
   markdown-mode))

;;; Utility functions

(defun set-font-size (font-name size)
  (set-face-attribute 'default nil
      :font (concat font-name " " (number-to-string size))))

(defun set-custom-tab-size (tab-size)
  (electric-indent-mode 0)
  (setq-default indent-tabs-mode nil)
  (setq tab-width tab-size)
  (setq c-basic-offset tab-size))

(defun move-line-up () 
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down () 
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;; TODO(Rax): Check if selected text is a valid URL
(defun open-url-in-browser (start end)
  "Open selected link with the default browser"
  (interactive "r")
  (let ((url (buffer-substring start end)))
    (browse-url url)))

;; Reference: https://www.emacswiki.org/emacs/KillingBuffers#h5o-2
(defun close-buffers ()
  "Kills all buffers except the buffers in the exclusion list"
  (interactive)
  (let ((exclusion-list '("*scratch*")))
    (dolist (buffer (buffer-list))
      (unless (member (buffer-name buffer) exclusion-list)
        (when (buffer-modified-p buffer)
          (save-buffer buffer))
        (kill-buffer buffer)))))

;;; Editor configuration

;; key-bindings

;; TODO(Rax): update these bindings
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<down>") 'windmove-down)
(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)

(global-set-key (kbd "<f5>") 'open-url-in-browser)

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(global-set-key (kbd "C-l") ; Mark the whole line
    (lambda ()
        (interactive)
        (beginning-of-line)
        (push-mark nil nil 1)
        (end-of-line)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(company doom-themes magit markdown-mode rust-mode spacemacs-theme
             zenburn-theme)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'doom-badger t)

(setq make-backup-files nil)
(setq column-number-mode t)

(display-battery-mode t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(global-display-line-numbers-mode)
(ido-mode t)

(set-font-size 
 (if (string-equal system-type "windows-nt") 
     "Courier New" "Ubuntu Mono") 
 13)

(set-custom-tab-size 4)

;;; Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;; Eglot
(require 'eglot)

(use-package eglot
  :custom (eglot-ignored-server-capabilities 
           '(:documentOnTypeFormattingProvider)))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'javascript-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)

;; Enable extension
(put 'upcase-region 'disabled nil)
