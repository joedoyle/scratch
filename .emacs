(add-to-list 'load-path "/home/jdoyle/.emacs.d/repos/xelb/")
(add-to-list 'load-path "/home/jdoyle/.emacs.d/repos/exwm/")

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "DP-0" 1 "DP-2"))

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

(load "/home/jdoyle/scratch/desktop.el")

;;(require 'exwm-config)
;;(exwm-config-example)
;;(setq exwm-workspace-number 3)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ccls lsp-mode lsp-ui clang-format magit csv-mode ace-window ace-jump-mode exwm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
