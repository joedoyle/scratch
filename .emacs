(add-to-list 'load-path "/home/jdoyle/.emacs.d/repos/xelb/")
(add-to-list 'load-path "/home/jdoyle/.emacs.d/repos/exwm/")

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "DP-0" 1 "DP-2" 2 "DP-4" 3 "DP-6"))

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
 '(auth-source-save-behavior nil)
 '(ede-project-directories '("/home/jdoyle/repos/quartz/granite"))
 '(elfeed-feeds
   '("https://lobste.rs/rss" "https://news.ycombinator.com/rss" "https://feeds.arstechnica.com/arstechnica/index" "https://sachachua.com/blog/category/emacs-news/feed/" "https://cpp.libhunt.com/newsletter/feed" "https://gcc.gnu.org/mailman/subscribe/gcc-announce" "https://lwn.net/headlines/rss"
     ("https://techpowerup.com/rss/news" security second)))
 '(package-selected-packages
   '(company-coq proof-general orderless consult vertico auth-source-xoauth2 slack elfeed ivy-gitlab ivy-xref ccls lsp-mode lsp-ui magit csv-mode exwm)))
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "UKWN" :family "Iosevka Term")))))
