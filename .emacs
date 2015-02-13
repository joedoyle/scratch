(setq-default indent-tabs-mode 'nil)
(setq-default tab-width 4)

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

(if (require 'auto-complete-config "auto-complete-config.el" t)
      (progn
        ((add-to-list 'ac-dictionary-directories "/home/users/jdoyle/.emacs.d/auto-complete/ac-dict")
         (ac-config-default))))


;; (add-to-list 'load-path "/home/users/jdoyle/.emacs.d/auto-complete/")
;; (require 'auto-complete-config "auto-complete-config.el" t)
;; (add-to-list 'ac-dictionary-directories "/home/users/jdoyle/.emacs.d/auto-complete/ac-dict")
;; (ac-config-default)

(column-number-mode 1)

;;(iswitchb-mode)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)

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

(require 'xcscope "xcscope.el" t)

;(setq load-path (append load-path '("/net/leo/workspace1/jdoyle/xref/emacs")))
;(load "xrefin.el")

(setq make-backup-files nil)

;;(set-face-attribute 'default nil :font "Terminus-10")
;;(set-face-attribute 'default nil :height 90)

(setq font-use-system-font t)

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

(defconst nebula-c++-style
  '(; The simplest and most used kind of "offset" setting in 
                                        ; c-offsets-alist is in terms of multiples of c-basic-offset
    (c-basic-offset             . 4)
    (c-max-one-liner-length     . 160)
    (c-tab-always-indent        . 4)
    (c-comment-only-line-offset . 4)
                                        ; This variable is an association list which maps syntactic symbols to 
                                        ; lists of places to insert a newline.
                                        ; http://cc-mode.sourceforge.net/html-manual/Hanging-Braces.html
    (c-hanging-braces-alist     . ((namespace-open)
                                   (namespace-close)
                                   (innamespace)
                                   (class-open after)
                                   (inline-open after)
                                   (defun-open after)
                                   (block-open) ; Statement block open brace.
                                   (brace-list-open after) ; Open brace of an enum or static array list.
                                   (brace-entry-open) ; Subsequent lines in an enum or static array list where the line begins with an open brace.
                                   (statement-case-open after) ; The first line in a case block that starts with a brace.
                                   (substatement-open after) ; The brace that opens a substatement block.
                                   (statement-block-intro)
                                   (statement)
                                   (access-label) ; C++ access control label.
                                   (else-clause after) ; The else line of an if-else construct.
                                   (defun-block-intro after)
                                   (catch-clause after)
                                   (block-close . c-snug-do-while)
                                   (member-init-intro)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
                                        ; Clean-ups are mechanisms which remove (or exceptionally, add) 
                                        ; whitespace in specific circumstances and are complementary to 
                                        ; colon and brace hanging. 
    (c-cleanup-list             . (brace-else-brace
                                   brace-elseif-brace
                                   brace-catch-brace
                                        ; empty-defun-braces
                                   defun-close-semi
                                   list-close-comma
                                   scope-operator))
                                        ; c-offsets-alist the principal variable for configuring indentation.
                                        ; These are passed to c-set-offset.
    (c-offsets-alist            .
                                ((innamespace . [0])
                                 (namespace-open                . -)
                                 (namespace-close               . [0])
                                 (inline-open . 0)
                                 (substatement-open . 0)
                                 (block-open        . 0)
                                 (comment-intro      . 0)
                                 (label . 0)
                                 (case-label . +)
                                 ))
    (c-echo-syntactic-information-p . t))
  "Nebula C++ Style")

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

;;(set-default-font "Terminus-10")

(setq default-frame-alist
      '(
        (foreground-color . "#c0c0c0")
        (background-color . "#000000")
        (cursor-color . "grey80")
        (font . "Terminus-10")
        (left-fringe . 0)
        (right-fringe . 0)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
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

(defun select-window-index (ix)
  "Select a window based on index"
  (select-window
   (get-window-with-predicate
    (lambda (window)
      (let ((window-coords (window-edges window)))
        (let ((left-coord (car window-coords))
              (top-coord (car (cdr window-coords))))
          (and (eq left-coord 0) (eq top-coord 0)))))))
  (other-window ix))

(global-set-key
 (kbd "M-g w")
 (lambda ()
   "prompt for a window index, and then select that window"
   (interactive)
   (let ((ix (read-from-minibuffer "index: ")))
   (select-window-index (string-to-number ix)))))

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


(defun bfc-build-trade-list ()
  (interactive)
  (kill-buffer "*alltrades*")
  (call-process "alltrades" nil "*alltrades*")
  (set-buffer (get-buffer-create "*alltrades*"))
  (goto-char (point-min))
  (setq bfc-trade-list '())
  (let ((trade-info nil))
    (while (not (eobp))
      (setq trade-info (split-string (thing-at-point 'line) "[,\n]"))
      (add-to-list 'bfc-trade-list `(,(car trade-info) . (,trade-info)))
      (forward-line))))


(defun bfc-goto-trade ()
  (interactive)
  (let ((trade-info (car (cdr (assoc (completing-read "Trade: " bfc-trade-list) bfc-trade-list)))))
    (find-file (format "/ssh:orion@%s:%s" (car (cdr trade-info)) (car (cdr (cdr trade-info)))))))
