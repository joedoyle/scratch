(package-initialize)


;; (setq exwm-input-global-keys
;;       `(([?\s-r] . exwm-reset)
;;         ([?\s-w] . exwm-workspace-switch)
;;         ([?\M-o] . ace-window)
;; 	(,(kbd "S-<left>") . previous-multiframe-window)
;; 	(,(kbd "S-<right>") . next-multiframe-window)
;;         ([?\s-&] . (lambda (command)
;;                      (interactive (list (read-shell-command "$ ")))
;;                      (start-process-shell-command command nil command)))
;;         ,@(mapcar (lambda (i)
;;                     `(,(kbd (format "s-%d" i)) .
;;                       (lambda ()
;;                         (interactive)
;;                         (exwm-workspace-switch-create ,i))))
;;                   (number-sequence 0 9))))

;;(require 'exwm-config)
;;(exwm-config-example)

;; (setq exwm-workspace-number 2)

;;(windmove-default-keybindings)
;;(add-to-list 'default-frame-alist '(alpha 85 85))

;; start non-exwm config
(setq inhibit-startup-message t)
(setq visible-bell t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(ido-mode -1)
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)


(global-set-key (vector (append (list 'shift) '(right)))  'next-multiframe-window)
(global-set-key (vector (append (list 'shift) '(left)))  'previous-multiframe-window)
(global-set-key (vector (append (list 'shift) '(up)))  'windmove-up)
(global-set-key (vector (append (list 'shift) '(down)))  'windmove-down)

;;(set-face-attribute 'default nil :font "Fira Code Retina" :height 150)
(set-face-attribute 'default nil :font "Inconsolata" :height 130)

(load-theme 'wombat)


;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(ivy-mode)
(counsel-mode)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-x C-r") (lambda () (interactive) (revert-buffer t t)))

(global-set-key
 (kbd "C-x \\")
 (lambda () (interactive)
   (let ((down-win (window-in-direction 'below))
         (up-win (window-in-direction 'above)))
     (cond ((and down-win (not (window-minibuffer-p down-win)))
            (delete-window down-win))
           ((and up-win (not (window-minibuffer-p up-win)))
            (delete-window up-win))))))

(defconst avesta-c++-style
  '((c-basic-offset . 4)
    (c-offsets-alist .
		     ((innamespace . [0])
		      (inline-open . 0)
		      (substatement-open . 0))))
  "Avesta c++ style")

;;(add-hook 'c-mode-hook '(lambda() (c-set-offset innamespace 0)))
(add-hook 'c-mode-common-hook
	  '(lambda()
	     (c-add-style "avesta" avesta-c++-style t)))

;;(use-package doom-modeline
;;  :ensure t
;;  :init (doom-modeline-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (exwm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
