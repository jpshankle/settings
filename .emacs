(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(setq my-packages '(
		    magit
                    company
                    company-tern
                    flycheck
                    flycheck
                    flycheck-clojure
                    js2-mode
                    web-mode
                    websocket
                    markdown-mode
                    paredit
                    clojure-mode
		    cider
                    elfeed
                    helm
                    helm-ls-git))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'magit)
(require 'company)
(require 'flycheck)
(require 'js2-mode)
(require 'web-mode)
(require 'markdown-mode)
(require 'paredit)
(require 'clojure-mode)
(require 'cider)
(require 'elfeed)
(require 'helm)

(set-frame-font "Menlo 14")
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)
(electric-pair-mode t)
(setq-default indent-tabs-mode nil)

;; enable windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; helm settings
(setq helm-M-x-fuzzy-match t)

;; make auto save use system temp folder
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
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
(add-to-list 'company-backends 'company-tern)
(add-hook 'after-init-hook 'global-company-mode)

;; set up key bindings
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "s-b") 'helm-mini)
(global-set-key (kbd "s-o") 'helm-find-files)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (atom-one-dark)))
 '(custom-safe-themes
   (quote
    ("4904daa168519536b08ca4655d798ca0fb50d3545e6244cefcf7d0c7b338af7e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
