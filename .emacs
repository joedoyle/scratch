(add-to-list 'load-path "/apps/avestafs1/jdoyle/.emacs.d/repos/xelb/")
(add-to-list 'load-path "/apps/avestafs1/jdoyle/.emacs.d/repos/exwm/")

(require 'exwm-randr)
 (setq exwm-randr-workspace-output-plist '(0 "DisplayPort-5" 1 "DisplayPort-3" 2 "DisplayPort-4"))

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

(require 'exwm)
(require 'exwm-config)
(exwm-config-example)

(load "/apps/avestafs1/jdoyle/repos/scratch/desktop.el")

;;(require 'exwm-config)
;;(exwm-config-example)
(setq exwm-workspace-number 3)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (csv-mode ace-window ace-jump-mode exwm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
