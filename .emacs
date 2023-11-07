;;; Melpa

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Utility functions

(defun set-font-size (font-name size)
  (set-face-attribute 'default nil
      :font (concat font-name " " (number-to-string size))))

(defun set-custom-tab-size (tab-size)
  (setq-default indent-tabs-mode nil)
  (setq tab-width tab-size)
  (setq c-basic-offset tab-size)
  (electric-indent-mode 0))

;;; Editor configuration

;; key-bindings

(global-set-key (kbd "C-l") ; Mark the whole line
    (lambda ()
        (interactive)
        (beginning-of-line)
        (push-mark nil nil 1)
        (end-of-line)))


(custom-set-variables
 '(inhibit-startup-screen t)
 '(package-selected-packages '(zenburn-theme company)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'zenburn t)

(setq make-backup-files nil)
(setq column-number-mode t)

(display-battery-mode t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(global-display-line-numbers-mode)

(ido-mode t)

(set-font-size 
 (if (string-equal system-type "windows-nt") "Courier New" "Ubuntu Mono") 13)

(set-custom-tab-size 4)

;;; Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;; Eglot
(require 'eglot)

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
