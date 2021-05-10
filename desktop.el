(package-initialize)
(require 'exwm-randr)
 (setq exwm-randr-workspace-output-plist '(0 "DP-1" 1 "DP-2-8"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output DP-1 --right-of DP-2 --auto")))
(exwm-randr-enable)

(setq exwm-input-global-keys
      `(([?\s-r] . exwm-reset)
        ([?\s-w] . exwm-workspace-switch)
        ([?\M-o] . ace-window)
	(,(kbd "S-<left>") . previous-multiframe-window)
	(,(kbd "S-<right>") . next-multiframe-window)
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))

(require 'exwm-config)
(exwm-config-example)

(setq exwm-workspace-number 2)

;;(windmove-default-keybindings)
(add-to-list 'default-frame-alist '(alpha 85 85))

;; start non-exwm config
(setq inhibit-startup-message t)
(setq visible-bell t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(ido-mode -1)
(setq-default make-backup-files nil)

(global-set-key (vector (append (list 'shift) '(right)))  'next-multiframe-window)
(global-set-key (vector (append (list 'shift) '(left)))  'previous-multiframe-window)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 150)
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

(global-set-key (kbd "M-o") 'ace-window)


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
