;; setup:
;; go get github.com/golang/lint/golint
;; go get github.com/gordonklaus/ineffassign
;; go get github.com/fzipp/gocyclo
;; go get github.com/client9/misspell/...
;; go get github.com/remyoudompheng/go-misc/deadcode/...
;; go get -u github.com/nsf/gocode
;; go get golang.org/x/tools/cmd/goimports
;; go get github.com/rogpeppe/godef
;;
;; ;; Oracle is now called guru
;; go get golang.org/x/tools/cmd/guru
;; go build golang.org/x/tools/cmd/guru
;; mv guru $GOPATH/bin
;; ln -s ~/go/bin/guru ~/go/bin/oracle
;;
;; manually copy dockerfile-mode.el to ~/.emacs.d/extensions
;; from: https://github.com/spotify/dockerfile-mode
;;
;; manually copy git-gutter.el to ~/.emacs.d/extensions
;; from https://github.com/syohex/emacs-git-gutter
;;
;; manually copy go-autocomplete.el to ~/.emacs.d/extensions
;; from: https://github.com/nsf/gocode/tree/master/emacs
;;
;; manually clone async from https://github.com/jwiegley/emacs-async
;; because melpa isn't auto-installing it properly
;; git clone https://github.com/jwiegley/emacs-async.git ~/.emacs.d/packages/async
;;
;; manually clone helm from https://github.com/emacs-helm/helm
;; because melpa can't auto-install it because async can't auto-install lol
;; git clone https://github.com/emacs-helm/helm.git ~/.emacs.d/packages/helm
;;
;; run `make` in the helm dir
;;

;;Load package-install sources
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t)
  (package-initialize))

(defvar my-packages
  '(
    go-mode
    go-eldoc
    go-autocomplete

    exec-path-from-shell

    ido-ubiquitous
    ido-vertical-mode
    ido-yes-or-no

    git-gutter

    find-file-in-repository

    markdown-mode

    json-mode

    project-explorer
    smooth-scroll
    buffer-move
    window-number)
  "My packages!")

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;;Custom Compile Command
(defun go-mode-setup ()
  (setq compile-command "go build -v && go test -v && go vet && golint && errcheck")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)


(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;;Project Explorer
(require 'project-explorer)
(global-set-key (kbd "M-e") 'project-explorer-toggle)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (git-gutter window-number smooth-scroll project-explorer markdown-mode json-mode ido-yes-or-no ido-vertical-mode ido-ubiquitous go-eldoc go-autocomplete find-file-in-repository exec-path-from-shell buffer-move)))
 '(shift-select-mode t)
 '(show-trailing-whitespace t)
 '(track-eol t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(require 'find-file-in-repository)
(require 'ido)
(require 'ido-ubiquitous)
(require 'ido-vertical-mode)
(global-set-key (kbd "C-x f") 'find-file-in-repository)
(ido-ubiquitous-mode 1)
(ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(require 'ido-yes-or-no)
(ido-yes-or-no-mode 1)

(add-to-list 'load-path "~/.emacs.d/extensions/")

(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)


(setq ac-auto-show-menu 0.6)
(setq ac-menu-height 40)
(setq ac-ignore-case 'smart)
(setq make-backup-files nil)

(with-eval-after-load 'go-mode
   (require 'go-autocomplete))


;; from: https://github.com/spotify/dockerfile-mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)


;; github.com/syohex/emacs-git-gutter
(require 'git-gutter)
(global-git-gutter-mode +1)

;;(custom-set-variables
;; '(git-gutter:update-interval 2))

(setq auto-save-default nil)

(setq column-number-mode t)

(setq create-lockfiles nil)

(add-to-list 'load-path "~/.emacs.d/packages/async")
(add-to-list 'load-path "~/.emacs.d/packages/helm")
(require 'helm-config)

(helm-mode 1)

(global-set-key (kbd "C-x b") 'helm-buffers-list)


(custom-set-faces
 '(my-carriage-return-face ((((class color)) (:foreground "#464646"))) t)
 '(my-tab-face ((((class color)) (:foreground "#363636"))) t)
 '(my-space-face ((((class color)) (:foreground "#565656"))) t)
)

;; add custom font locks to all buffers and all files
(add-hook
 'font-lock-mode-hook
 (function
  (lambda ()
    (setq
     font-lock-keywords
     (append
      font-lock-keywords
      '(
        ("\n" (0 'my-carriage-return-face t))
        ("\r" (0 'my-carriage-return-face t))
        ("\t" (0 'my-tab-face t))
        ("\s" (0 'my-space-face t))
        ))))))

;; make characters after column 80 purple
;;(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
;;(setq whitespace-style (quote (face trailing tab-mark lines-tail)))
(setq whitespace-style (quote (space-mark face newline-mark tab-mark)))
(add-hook 'find-file-hook 'whitespace-mode)

;; transform literal tabs into a right-pointing triangle
(setq
 whitespace-display-mappings ;http://ergoemacs.org/emacs/whitespace-mode.html
 '(
   (tab-mark 9 [9654 9] [92 9])
   (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
   (newline-mark 10 [8617 10]) ; 8617 LEFTWARDS ARROW WITH HOOK
   ))
