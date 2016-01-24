;;; my.emacs --- making emacs the way i want it
;;; Commentary:
;;; This is my .emacs file

;;; Code:
(require 'package)

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(setq my-packages '(
		    magit
                    company
                    company-tern
                    editorconfig
                    exec-path-from-shell
                    flycheck
		    flycheck-status-emoji
                    flycheck-color-mode-line
                    flycheck-pos-tip
                    flycheck-clojure
                    js2-mode
                    web-mode
                    websocket
                    markdown-mode
                    paredit
                    clojure-mode
		    cider
                    scad-mode
                    scad-preview
                    elfeed
		    elfeed-org
                    projectile
                    helm
                    helm-ls-git
                    helm-descbinds
                    helm-projectile
                    atom-one-dark-theme
                    dired+
                    org-plus-contrib))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'magit)
(require 'company)
(require 'editorconfig)
(require 'exec-path-from-shell)
(require 'flycheck)
(require 'flycheck-color-mode-line)
(require 'js2-mode)
(require 'web-mode)
(require 'websocket)
(require 'markdown-mode)
(require 'paredit)
(require 'clojure-mode)
(require 'cider)
(require 'elfeed)
(require 'elfeed-org)
(require 'projectile)
(require 'helm)
(require 'helm-projectile)
(require 'helm-ls-git)
(require 'helm-descbinds)
(require 'scad-preview)
(require 'dired+)

(set-face-attribute 'default nil :font "Menlo 14")
(set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)
(electric-pair-mode t)
(setq-default indent-tabs-mode nil)

;; elfeed
(elfeed-org)

;; enable windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; projectile settings
(projectile-global-mode)

;; helm settings
(helm-projectile-on)
(setq helm-M-x-fuzzy-match t)
(helm-descbinds-mode)

;; make auto save use system temp folder
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;; editor-config
(editorconfig-mode 1)

;; flycheck
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))
(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; cider-mode
(add-hook 'cider-mode-hook #'eldoc-mode)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
    (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; enable company mode in all buffers and add to it
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-tern)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; set up key bindings
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-<left>") 'move-beginning-of-line)
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<up>") 'scroll-down-commmand)
(global-set-key (kbd "s-<down>") 'scroll-up-command)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "s-b") 'helm-mini)
(global-set-key (kbd "s-o") 'helm-projectile)
(global-set-key (kbd "s-k") 'other-window)
(global-set-key (kbd "s-[") 'magit-status)
(global-unset-key [escape])
(global-set-key [escape] 'keyboard-escape-quit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (atom-one-dark)))
 '(custom-safe-themes
   (quote
    ("4904daa168519536b08ca4655d798ca0fb50d3545e6244cefcf7d0c7b338af7e" default)))
 '(flycheck-status-emoji-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide '.emacs)
;;; .emacs ends here
