;;; Melpa

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Utility functions

(defun set-font-size (font-name size)
  (set-face-attribute 'default nil
      :font (concat font-name " " (number-to-string size))))

(defun setup-tabsize-and-behavior (tab-size)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width tab-size)
  (setq c-basic-offset tab-size)
  (electric-indent-mode -1))

(defun search-selection-on-internet (&optional keywords)
  (interactive (list 
                (if (not (region-active-p))
                    (user-error "Mark isn't set!")
                    (read-string "Enter more search keywords: "))))
  (let ((selected-text (buffer-substring (region-beginning) (region-end))))
    (eww (format "%s %s" selected-text keywords))))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))
  

;;; Editor configuration

;; key-bindings

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

(global-set-key (kbd "<f5>") 'search-selection-on-internet)
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
 '(package-selected-packages '(spacemacs-theme zenburn-theme company)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'spacemacs-dark t)

(setq make-backup-files nil)
(setq column-number-mode t)

(display-battery-mode t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(global-display-line-numbers-mode)

(ido-mode t)

(set-font-size 
 (if (string-equal system-type "windows-nt") "Courier New" "Ubuntu Mono") 13)

(setup-tabsize-and-behavior 4)


;;; Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;; Eglot
(require 'eglot)

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
