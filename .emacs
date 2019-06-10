;;; package --- Summary
;;; Commentary:
;; Alec's Emacs config
;;

;; a little transparency never hurt no body
(set-frame-parameter (selected-frame) 'alpha '(100 100))

;; dont show scrollbar
(scroll-bar-mode -1)

;; display time
(display-time-mode 1)

;; update buffers when files change on disk
(global-auto-revert-mode t)

;; line numbers please
(global-linum-mode t)
(setq column-number-mode t)

;; add some padding to the window
(set-frame-parameter nil 'internal-border-width 0)
(setq scroll-margin 0)


;; Set default font
(defun monitor-font-size ()
    (interactive)
    (set-face-attribute 'default nil
                        :family "FuraCode Nerd Font"
                        :height 70 ;; monitor
                        :weight 'normal
                        :width 'normal))

(defun laptop-font-size ()
  (interactive)
  (set-face-attribute 'default nil
                    :family "FuraCode Nerd Font"
                    :height 110 ;; laptop screen
                    :weight 'normal
                    :width 'normal))

(monitor-font-size)

;; no startup screen
(setq inhibit-startup-screen t)

;; set indentations to spaces
(setq-default indent-tabs-mode nil)

;; buffer viewer config
(global-set-key "\C-x\C-b" 'ibuffer)
(tool-bar-mode -1)

;; remove trailing white space on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; TODO: be able to duplicate mutliple lines
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;; custom scripts
(add-to-list 'load-path "~/.emacs.d/custom/sort-lines-by-length.el")
(add-to-list 'load-path "~/.emacs.d/snippets/lunch.el")

;; custom key bindings
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c o")  'sort-lines-by-length)
(global-set-key (kbd "C-c q") 'query-replace-regexp)
(global-set-key (kbd "C-x <up>")     'windmove-up)
(global-set-key (kbd "C-x <down>")   'windmove-down)
(global-set-key (kbd "C-x <right>")  'windmove-right)
(global-set-key (kbd "C-x <left>")   'windmove-left)
(global-set-key (kbd "<C-B-up>")     'buf-move-up)
(global-set-key (kbd "<C-B-down>")   'buf-move-down)
(global-set-key (kbd "<C-B-left>")   'buf-move-left)
(global-set-key (kbd "<C-B-right>")  'buf-move-right)
;; (global-set-key (kbd "C-c s") 'sort-lines)
(global-set-key (kbd "C-c s") 'rgrep)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key "\C-z" 'advertised-undo)

;; custom utility methds from /.emacs.d/snippets
(load "~/.emacs.d/snippets/lunch.el")
(global-set-key (kbd "C-c C-u l") 'copy-lunch-poll)
(global-set-key (kbd "C-c C-u b") 'laptop-font-size)
(global-set-key (kbd "C-c C-u s") 'monitor-font-size)

;; Dont use CTRL-C style copy pase
(cua-mode -1)


;; flex file matching for finding files - see http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
;; and http://www.emacswiki.org/emacs/InteractivelyDoThings
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

;; My Packages
;;
;; I use use-package, so I need to make sure thats installed before doing anything else
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa-milkbox" . "http://melpa.milkbox.net/packages/")))

(setq package-archive-priorities
  '(("melpa-stable" . 30)
    ("marmalade" . 20)
    ("gnu" . 10)
    ("melpa" . 1)
    ("melpa-milkbox". 0)))

;; (setq-default mode-line-format
;;               '("%e" (:eval (if vc-mode
;;                                 (let* ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode))
;;                                        (face (cond ((string-match "^ -" noback) 'mode-line-vc) ((string-match "^ [:@]" noback) 'mode-line-vc-edit)
;;                                                    ((string-match "^ [!\\?]" noback) 'mode-line-vc-modified)))) (format " %s" (substring noback 2)))))))

;; activate installed packages
(package-initialize)

;; make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(menu-bar-mode -1)

(eval-when-compile
  (require 'use-package))

(use-package go
  :ensure t)

(use-package slime
  :ensure t
  :commands slime
  :init
  (setq inferior-lisp-program "/usr/bin/clisp")
  :config
  (progn
    (add-hook
     'slime-load-hook
     #'(lambda ()
	 (slime-setup
	  '(slime-fancy
	    slime-repl
	    slime-fuzzy))))
    (setq slime-net-coding-system 'utf-8-unix)))

  ;; :init
  ;; ((setq inferior-lisp-program "/usr/bin/sbcl")
  ;;  (setq slime-contribs '(slime-fancy)))
  ;; :config
  ;; (slime-setup))

;; (use-package lua-mode
;;   :ensure t
;;   :config (load "~/.emacs.d/custom/pico8.el"))

(use-package org-journal
  :ensure t
  :pin melpa
  :config
  (customize-set-variable 'org-journal-dir "~/Documents/personal/journal/")
  (customize-set-variable 'org-journal-file-type 'weekly)
  (customize-set-variable 'org-journal-file-format "%Y-%m-%d"))

(use-package fireplace
  :ensure t)

(use-package zone-select
  :ensure t)

(use-package zone-tunnels
  :ensure t)

(use-package zone-matrix
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package yaml-mode
  :ensure t
  :after (highlight-indentation)
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package highlight-indentation
  :ensure t
  :hook
  ((yaml-mode . highlight-indentation-current-column-mode)
   (python-mode . highlight-indentation-current-column-mode))
  :custom
  (highlight-indentation-current-column-face "#343d46"))

(use-package blacken
  :ensure t
  :hook ((python-mode . blacken-mode))
  :custom (blacken-line-length 100))

(defcustom python-shell-interpreter "python3"
  "Default Python interpreter for shell."
  :type 'string
  :group 'python)

(use-package django-commands
  :ensure t)


(use-package less-css-mode
  :ensure t
  :mode ("\\.less\\'" . less-css-mode))

(use-package magit
  :ensure t
  :commands magit-status
  :bind ("C-x g" . magit-status))

;; (use-package forge
;;   :ensure t)

(use-package with-editor
  :ensure t)

(use-package kubernetes
  :ensure t
  :commands kubernetes-overview
  :bind ("C-x k" . kubernetes-overview))

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . mc/edit-lines)
         ("C-c m n" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this)))

(use-package company
  :ensure t
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode))

(use-package company-web
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-web))

(use-package restclient
  :ensure t
  :interpreter ("restclient" . restclient-mode)
  :mode ("\\.http\\'" . restclient-mode)
  :after (company)
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package ac-html-csswatcher
  :ensure t
  :config
  (company-web-csswatcher-setup))

(use-package ac-html-csswatcher
  :ensure t
  :config (ac-html-csswatcher-setup))

(use-package web-mode
  :ensure t
  :mode (("\\.vue\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :custom
  (web-mode-style-padding 0)
  (web-mode-script-padding 0)
  (web-mode-block-padding 0)
  (web-mode-comment-style 0))


(use-package haxe-mode
  :ensure t
  :mode ("\\.hx\\'" . haxe-mode)
  :init
  (add-hook 'haxe-mode-hook
            (function
             (lambda ()
               (setq c-basic-offset 4)))))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :after (yasnippet)
  :commands js2-imenu-extras-mode
  :hook ((js2-mode . js2-imenu-extras-mode))
  :custom-face
  (js2-function-param ((t (:foreground "#bf616a"))))
  (js2-external-variable ((t (:foreground "#d08770"))))
  (js2-function-param ((t (:foreground "#bf616a"))))
  :custom
  (js2-use-font-lock-faces t)
  (js2-highlight-level 3)
  (js2-global-externs (quote
                       ("this"
                        "require"
                        "module"
                        "exports"
                        "process"
                        "__dirname"
                        "setTimeout"
                        "setInterval")))
  (js2-ignored-warnings (quote ("msg.extra.trailing.comma")))
  (js2-include-jslint-declaration-externs nil)
  (js2-missing-semi-one-line-override nil)
  (js2-strict-missing-semi-warning nil)
  (setq local-abbrev-table js2-mode-abbrev-table))

(use-package nodejs-repl
  :ensure t
  :after (js2-mode)
  :bind (:map js-mode-map
              ("C-c C-r" . nodejs-repl-send-region)
              ("C-x C-e" . nodejs-repl-send-line)
              ("C-c C-l" . nodejs-repl-load-file)
              ("C-c C-z" . nodejs-repl-switch-to-repl)))

;; (use-package js-comint
;;   :ensure t
;;   :after (js2-mode)
;;   :custom
;;   (inferior-js-program-command "node --interactive")
;;   (add-hook 'js2-mode-hook
;;             (lambda ()
;;               (local-set-key (kbd "C-c C-r") 'js-send-last-sexp)
;;               (local-set-key (kbd "C-c b") 'js-send-buffer))))

(use-package js2-refactor
  :ensure t
  ;; :interpreter ("js2-refactor" . js2-refactor-mode)
  :after (js2-mode)
  :hook ((js2-mode . js2-refactor-mode))
  :config
  (js2r-add-keybindings-with-prefix "C-c"))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-.") 'er/expand-region)
  (global-set-key (kbd "C-,") 'er/contract-region))

(use-package add-node-modules-path
  :ensure t)

(use-package prettier-js
  :ensure t
  :interpreter ("prettier-js" . prettier-js-mode)
  :after (js2-mode add-node-modules-path)
  :hook ((js2-mode . prettier-js-mode)
         (web-mode . prettier-js-mode))
  :config
  (eval-after-load
      'web-mode
    '(add-hook 'web-mode-hook #'add-node-modules-path))
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook #'add-node-modules-path)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package smartparens
  :ensure t
  :interpreter (("smartparens-config" . smartparens-mode)
                ("smartparens-config" . smartparens-strict-mode))
  :hook ((emacs-lisp-mode . smartparens-strict-mode)
         (lisp-mode . smartparens-strict-mode)
         (eval-expression-minibuffer-setup . smartparens-mode)
         (js2-mode . smartparens-mode)
         (rust-mode . smartparens-mode))
  :config
  (sp-pair "'" nil :actions :rem))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-ocean t))

(use-package go-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package nginx-mode
  :ensure t
  :mode ("\\nginx.conf\\'" . nginx-mode))

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq rust-format-on-save t))

;; (use-package racer
;;   :ensure t
;;   :hook ((rust-mode . racer-mode)
;;          (racer-mode . eldoc-mode)
;;          (racer-mode . company-mode))
;;   :config
;;   (setq racer-cmd "~/.cargo/bin/racer")
;;   (setq racer-rust-src-path "~/Documents/personal_projects/rust/projects/rust/src"))

(use-package cargo
  :ensure t
  :hook ((rust-mode . cargo-minor-mode)))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-x-prompt yas-ido-prompt)))


(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet-mode)
  :pin melpa-milkbox)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package which-key
  :ensure t
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package cheat-sh
  :ensure)

(use-package markdown-mode
  :ensure)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blacken-line-length 100)
 '(django-commands-python-executable "python3")
 '(django-commands-shell-args (quote ("-i" "ipython")))
 '(expand-region-preferred-python-mode (quote fgallina-python))
 '(highlight-indentation-current-column-face "#343d46" t)
 '(js2-global-externs
   (quote
    ("this" "require" "module" "exports" "process" "__dirname" "setTimeout" "setInterval")))
 '(js2-highlight-level 3)
 '(js2-ignored-warnings (quote ("msg.extra.trailing.comma")))
 '(js2-include-jslint-declaration-externs nil)
 '(js2-missing-semi-one-line-override nil)
 '(js2-strict-missing-semi-warning nil)
 '(js2-use-font-lock-faces t t)
 '(org-agenda-files (quote ("~/Documents/Mirus/TODO.org")))
 '(package-selected-packages
   (quote
    (org-journal add-node-modules-path markdown-mode cheat-sh forge ghub memoize web-mode zone-matrix zone-tunnels zone-select fireplace nodejs-repl projectile-mode js-comint django-commands emacs-django-commands pony-mode python-django django-mode blacken which-key which-key-mode nginx-mode kubernetes magit-todo-mode magit-todo expand-region auto-package-update racer tide whitespace-mode yasnippet-snippets cargo-minor-mode cargo terraform-mode go-mode smartparens-config restclient-mode yaml-mode window-numbering window-number vue-mode use-package toml-mode telephone-line spotify smartparens slime slack restart-emacs project-explorer prettier-js pkg-info parinfer paredit npm-mode neotree markdown-preview-mode magit less-css-mode haxe-mode haxe-imports focus flymd flymake-jshint flymake exec-path-from-shell elpy drag-stuff dockerfile-mode dirtree diminish company-web company-tern company-restclient coffee-mode buffer-move base16-theme auto-dim-other-buffers auto-complete ac-html-csswatcher)))
 '(prettier-js-args nil)
 '(python-shell-exec-path nil)
 '(python-shell-interpreter "python3")
 '(python-shell-interpreter-interactive-arg "manage.py shell")
 '(safe-local-variable-values
   (quote
    ((python-shell-interpreter-args . "/Users/alectroemel/Documents/Mirus/desjardins/caboto/manage.py shell")
     (python-shell-interpreter . "python3"))))
 '(setq
   [## 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] t)
 '(web-mode-block-padding 0)
 '(web-mode-comment-style 0)
 '(web-mode-script-padding 0)
 '(web-mode-style-padding 0))

;;; .emacs ends here
(provide '.emacs)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indentation-current-column-face ((t (:background "#343d46"))))
 '(js2-external-variable ((t (:foreground "#d08770"))))
 '(js2-function-param ((t (:foreground "#bf616a")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
