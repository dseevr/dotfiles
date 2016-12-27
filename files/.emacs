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
;; manually copy git-gutter.el to ~/.emacs.d/extensions
;; from https://github.com/syohex/emacs-git-gutter
;;
;; manually copy go-autocomplete.el to ~/.emacs.d/extensions
;; from: https://github.com/nsf/gocode/tree/master/emacs
;;

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

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

    ;;go-autocomplete
    golint

    helm

    dockerfile-mode

    anaconda-mode

    clang-format

    company
    company-go

    exec-path-from-shell

    ido-ubiquitous
    ido-vertical-mode
    ido-yes-or-no

    git-gutter

    find-file-in-repository
    
    markdown-mode
    nginx-mode

    rust-mode

    json-mode
    yaml-mode

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

(add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

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
 '(git-gutter:window-width 2)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "vendor" "log" "Godeps")))
 '(helm-grep-ignored-directories
   (quote
    ("SCCS/" "RCS/" "CVS/" "MCVS/" ".svn/" ".git/" ".hg/" ".bzr/" "_MTN/" "_darcs/" "{arch}/" ".gvfs/" "vendor" "Godeps")))
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
 '(default ((t (:background nil))))
 '(my-carriage-return-face ((((class color)) (:foreground "#464646"))) t)
 '(my-space-face ((((class color)) (:foreground "#464646"))) t)
 '(my-tab-face ((((class color)) (:foreground "#313131"))) t))


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

;; from: https://www.emacswiki.org/emacs/buffer-move.el
;; TODO: need to re-run git-gutter after buffers are moved since the gutter vanishes
(require 'buffer-move)
(defun win-swap () "Swap windows using buffer-move.el" (interactive) (if (null (windmove-find-other-window 'right)) (buf-move-left) (buf-move-right)))
(global-set-key (kbd "C-x <") 'win-swap)
(global-set-key (kbd "C-x >") 'win-swap)

(global-unset-key (kbd "C-x C-o"))
(global-set-key (kbd "C-x C-o") 'win-swap)

;;(require 'go-autocomplete)
;;(require 'auto-complete-config)
;;(ac-config-default)
;;(setq ac-delay 0.5)
;;(setq ac-auto-show-menu 5)
;;(setq ac-menu-height 40)
;;(setq ac-ignore-case 'smart)

(require 'company)                                   ; load company mode
(require 'company-go)                                ; load company mode go backend

(setq company-tooltip-limit 40)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
;;(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

(add-hook 'go-mode-hook (lambda ()
			  (set (make-local-variable 'company-backends) '(company-go))
			  (company-mode)))


(add-hook 'python-mode-hook 'anaconda-mode)

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

(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)

(helm-mode 1)

;;(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)

(setq helm-M-x-fuzzy-match t)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x b") 'helm-mini)

(setq helm-buffers-fuzzy-matching t
            helm-recentf-fuzzy-match t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(global-set-key (kbd "C-c h o") 'helm-occur)

(helm-autoresize-mode t)


(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
	helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

;; TODO: write functions which will enable/disable/toggle drawing of whitespace
;;       sometimes it's more convenient to copy lines using the terminal rather than in emacs itself


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


(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; from https://github.com/zachallaun/emacs-config/blob/master/init.el
(setq confirm-nonexistent-file-or-buffer nil)
(show-paren-mode 1)
(setq system-uses-terminfo nil)

(setq ido-enable-flex-matching t)
(setq ido-flex-match t)
(setq ido-everywhere t)
(setq ido-case-fold t)


(put 'upcase-region 'disabled nil)

;; use only one desktop
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")
(setq desktop-lock-file "~/.emacs.d/.emacs.desktop.lock")

(defun saved-session-exists-p ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if
       (saved-session-exists-p)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session-exists-p)
      (desktop-save-in-desktop-dir)
    ))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
	  '(lambda ()
		 (if (y-or-n-p "Restore desktop? ")
		     (progn
		       (remove-desktop-lock-file)
		       (session-restore)))))

(defun save-desktop-on-exit ()
	"ask whether to save on exit"
	(interactive)
	(if
	     (y-or-n-p "Save desktop?")
		(session-save)))

(defun remove-desktop-lock-file ()
  "removes the desktop lock file... if it exists"
  (if (file-exists-p desktop-lock-file)
      (delete-file desktop-lock-file)))

(add-hook 'kill-emacs-hook 'save-desktop-on-exit)

(setq desktop-restore-frames t)
(setq desktop-restore-in-current-display t)
(setq desktop-restore-forces-onscreen nil)

(setq inhibit-startup-message t)

(setq ring-bell-function 'ignore)

(when (fboundp 'winner-mode)
  (winner-mode 1))

(defun duplicate-line ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank)
  )
)
(global-set-key (kbd "C-c C-d") 'duplicate-line)

;; can't set (setq kill-whole-line t) because that breaks (duplicate-line)
;; new logic: if the current line has 0 characters or we're not at the beginning
;;            of the line, just call (kill-line) once.  otherwise, call it twice.
(defun kill-line-including-newline ()
  (interactive)
  (if
    (and
      (not
       (zerop
	(-
	 (line-end-position)
	 (line-beginning-position))))
      (=
       (current-column)
       0))
    (kill-line))
  (kill-line))
(global-unset-key (kbd "C-k"))
(global-set-key (kbd "C-k") 'kill-line-including-newline)

(defun kill-line-backwards ()
  (interactive)
  (let ((start_column (current-column)))
    (move-beginning-of-line 1)
    (delete-forward-char start_column)
  )
)
(global-unset-key (kbd "C-l"))
(global-set-key (kbd "C-l") 'kill-line-backwards)

(global-unset-key (kbd "C-\\"))

;; reload buffers when files change on disk
(global-auto-revert-mode t)
(auto-revert-mode t)

;(add-hook 'after-revert-hook 'git-gutter:update-all-windows)
;; TODO: this is super inefficient, but it works and doesn't seem to be noticeable...
;(add-hook 'buffer-list-update-hook 'git-gutter:update-all-windows)

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
	 (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
		 (not (buffer-modified-p buffer)))
	(set-buffer buffer)
	(revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  )

(global-set-key (kbd "C-x <end>") 'revert-all-buffers)

;; always follow symlinks without prompting
;; minibuffer still shows a warning if the symlink points to a git-tracked file
(setq vc-follow-symlinks nil)

(add-hook 'after-init-hook 'global-company-mode)


(defun clang-format-before-save-hook ()
  (when c-buffer-is-cc-mode
        (clang-format-buffer)))

(add-hook 'before-save-hook #'clang-format-before-save-hook)
