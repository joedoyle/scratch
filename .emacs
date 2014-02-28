(setq-default indent-tabs-mode 'nil)
(setq-default tab-width 4)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(add-to-list 'load-path "/home/users/jdoyle/.emacs.d/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/home/users/jdoyle/.emacs.d/auto-complete/ac-dict")
(ac-config-default)

(column-number-mode 1)

;;(iswitchb-mode)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;(fringe-mode "none")

(setq make-backup-files nil)
(setq visible-bell t)
(setq c-style-variables-are-local-p t)
(setq max-specpdl-size 10000)
(setq max-lisp-eval-depth 2000)

(add-to-list 'load-path "~/lisp")
;; (add-to-list 'load-path "~/lisp/ecb_new")

;; (load-file "~/src/cedet-bzr/trunk/cedet-devel-load.el")
;; (setq semantic-default-submodes (list 'global-semanticdb-minor-mode 'global-semantic-idle-scheduler-mode 'global-semantic-idle-summary-mode) )

;; (semantic-mode 1)
;; (global-ede-mode t)

;; (ede-cpp-root-project "Blackbird"
;;                       :name "Blackbird Project"
;; ;;                      :file "/sandbox/repos/ise_nest/c++/blackbird/CMakeLists.txt"
;;                       :file "/sandbox/repos/integration3.3/CMakeLists.txt"
;;                       :system-include-path '("/opt/boost/include"))

;;(require 'ecb)

(require 'xcscope)

;(setq load-path (append load-path '("/net/leo/workspace1/jdoyle/xref/emacs")))
;(load "xrefin.el")

(setq make-backup-files nil)

;;(set-face-attribute 'default nil :font "Terminus-10")
;;(set-face-attribute 'default nil :height 90)

(setq Info-default-directory-list
      (cons "~/emacs-20.4/info" Info-default-directory-list))

;(load "vc-custom.elc")
;(load "vc-hooks.elc")

(defconst sun-c-style 
  '( (c-tab-always-indent . nil)
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist .
      (
       (access-label . -)
       (arglist-close . +)
       (arglist-cont c-lineup-gcc-asm-reg 0)
       (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist)
       (arglist-intro . +)
       (block-close . 0)
       (block-open . 0)
       (brace-entry-open . 0)
       (brace-list-close . 0)
       (brace-list-entry . 0)
       (brace-list-intro . +)
       (brace-list-open . 0)
       (case-label . +)
       (catch-clause . 0)
       (c . c-lineup-C-comments)
       (class-close . 0)
       (class-open . 0)
       (comment-intro c-lineup-knr-region-comment c-lineup-comment)
       (composition-close . 0)
       (composition-open . 0)
       (cpp-define-intro c-lineup-cpp-define +)
       (cpp-macro . [0])
       (cpp-macro-cont . +)
       (defun-block-intro . +)
       (defun-close . 0)
       (defun-open . 0)
       (do-while-closure . 0)
       (else-clause . 0)
       (extern-lang-close . 0)
       (extern-lang-open . 0)
       (friend . 0)
       (func-decl-cont . +)
       (inclass . +)
       (incomposition . +)
       (inexpr-class . 0)
       (inexpr-statement . +)
       (inextern-lang . +)
       (inher-cont . c-lineup-multi-inher)
       (inher-intro . +)
       (inlambda . c-lineup-inexpr-block)
       (inline-close . 0)
       (inline-open . 0)
       (inmodule . +)
       (innamespace . 0)
       (knr-argdecl . 0)
       (knr-argdecl-intro . +)
       (label . 0)
       (lambda-intro-cont . +)
       (member-init-cont . c-lineup-multi-inher)
       (member-init-intro . 1)
       (module-close . 0)
       (module-open . 0)
       (namespace-close . 0)
       (namespace-open . 0)
       (objc-method-args-cont . c-lineup-ObjC-method-args)
       (objc-method-call-cont . c-lineup-ObjC-method-call)
       (objc-method-intro . [0])
       (statement . 0)
       (statement-block-intro . +)
       (statement-case-intro . +)
       (statement-case-open . 0)
       (statement-cont c-lineup-math)
;       (statement-cont . +)
       (stream-op . c-lineup-streamop)
       (string . c-lineup-dont-change)
       (substatement . +)
       (substatement-label . 0)
       (substatement-open . 0)
       (template-args-cont c-lineup-template-args +)
       (topmost-intro . 0)
       (topmost-intro-cont . c-lineup-topmost-intro-cont)
       )
      )
     (c-echo-syntactic-information-p . t)
     )
  "Sun Trading C++ Style")

(defun my-c-mode-common-hook ()
  (c-add-style "sun" sun-c-style t)
  (c-toggle-hungry-state 1)
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(setq-default indent-tabs-mode nil)

;;(autoload 'turn-on-bc-mode "bc-mode" "Turn on bc-mode unconditionally.")
;;(add-hook 'c++-mode-hook 'turn-on-bc-mode)
(add-hook 'c++-mode-hook 'auto-fill-mode)

(setq scroll-conservatively 3)

(setq-default show-trailing-whitespace t)

(global-font-lock-mode 1)

(setq-default next-line-add-newlines nil)

(setq auto-mode-alist
  (append '(("\\.C$"  . c++-mode)
            ("\\.cc$" . c++-mode)
            ("\\.hxx$" . c++-mode)
            ("\\.[cy]$"  . c-mode)
            ("\\.m4c$"  . c-mode)
            ("\\.h$"  . c++-mode)
            ("\\.pl$"  . perl-mode)
            ("\\.perl$"  . perl-mode)
            ("\\.txt$" . indented-text-mode)
            (".*[Mm]akefile.*" . makefile-mode)
            ("\\.idl$" . idl-mode)
           ) auto-mode-alist))

(put 'narrow-to-region 'disabled nil)

;; (cond (window-system
;;        (setq default-frame-alist
;;              '(
;;                (foreground-color . "#c0c0c0")
;;                (background-color . "#000000")
;;                (cursor-color . "grey80")
;;                ))
;;        ))

(setq default-frame-alist
      '(
        (foreground-color . "#c0c0c0")
        (background-color . "#000000")
        (cursor-color . "grey80")
        ))

(windmove-default-keybindings)

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)
(global-set-key (kbd "C-x C-r") (lambda () (interactive) (revert-buffer t t)))

(global-set-key
 (kbd "C-x \\")
 (lambda () (interactive)
   (let ((down-win (windmove-find-other-window 'down))
         (up-win (windmove-find-other-window 'up)))
     (cond ((and down-win (not (window-minibuffer-p down-win)))
            (delete-window down-win))
           ((and up-win (not (window-minibuffer-p up-win)))
            (delete-window up-win))))))

(global-set-key (kbd "C-c s d") 'semantic-ia-fast-jump)
