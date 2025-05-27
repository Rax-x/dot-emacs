;;; Melpa & packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun ensure-packages-installed (packages)
  (mapcar (lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
          packages))

(ensure-packages-installed '(company zenburn-theme spacemacs-theme rust-mode nord-theme doom-themes))

;;; Utility functions

(defun set-font-size (font-name size)
  (set-face-attribute 'default nil
      :font (concat font-name " " (number-to-string size))))

(defun setup-tabsize-and-behavior (tab-size)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width tab-size)
  (setq c-basic-offset tab-size)
  (electric-indent-mode 0))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))


;; FIXME(Rax)
(defun move-region (start end position)
  (interactive "r")
  (let ((text (delete-and-extract-region start end)))
    (forward-line position)
    (insert text)
    (setq deactivate-mark nil)
    (set-mark start)))

(defun move-region-up (start end)
  (interactive "r")
  (move-region start end -1))

(defun move-region-down (start end)
  (interactive "r")
  (move-region start end 1))

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

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(global-set-key (kbd "C-<down>") 'move-region-down)
(global-set-key (kbd "C-<up>") 'move-region-up)

(global-set-key (kbd "<f5>") 'open-url-in-browser)
(global-set-key (kbd "C-l") ; Mark the whole line
    (lambda ()
        (interactive)
        (beginning-of-line)
        (push-mark nil nil 1)
        (end-of-line)))

(global-set-key (kbd "\C-ca") 'org-agenda)

;; Appearence

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(magit doom-themes nord-theme web-mode rust-mode spacemacs-theme zenburn-theme company)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)

(load-theme 'doom-badger t)

(setq make-backup-files nil)
(setq column-number-mode t)

(display-battery-mode t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(global-display-line-numbers-mode)

(ido-mode t)
(set-font-size (face-attribute 'default :family) 13)

(setup-tabsize-and-behavior 4)


;;; Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;; Eglot
(use-package eglot 
  :custom (eglot-ignored-server-capabilities 
           '(:documentOnTypeFormattingProvider)))

(add-hook 'html-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'javascript-mode 'eglot-ensure)

(add-hook 'web-mode-hook 'eglot-ensure)
