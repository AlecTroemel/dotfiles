;;; package --- Summary
;;; Commentary:
;; Alec's Spacemacs config
;;

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     go
     shell
     ruby
     sql
     typescript
     html
     restclient
     yaml
     (python :variables python-test-runner 'pytest)
     django
     lua
     rust
     theming
     javascript
     helm
     auto-completion
     better-defaults
     emacs-lisp
     common-lisp
     git
     markdown
     org
     html
     docker
     nginx
     spotify
     asm
     terraform
     (vue :variables vue-backend 'lsp)
     (node :variables node-add-modules-path t)
     lsp
     )
   dotspacemacs-additional-packages
   '(
     prettier-js
     web-mode
     multiple-cursors
     blacken
     know-your-http-well
     company-restclient
     fennel-mode
     keychain-environment
     )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the vadriable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'emacs
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   ;; dotspacemacs-themes '(base16-mocha :location local)
   ;; dotspacemacs-themes 'adwaita
   dotspacemacs-themes '(base16-ocean :location local)
   dotspacemacs-colorize-cursor-according-to-state nil
   dotspacemacs-default-font '("FuraCode Nerd Font"
                               :size 18 ;;13
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)

   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "C-<return>"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "M-s-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 100
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers t
   dotspacemacs-folding-method 'emacs
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup t))

(defun dotspacemacs/user-init ()
  (setq rust-format-on-save t))

(defun dotspacemacs/user-config ()
  "99% of the time stuff should go here instead of in user-init"
  (load "/home/alec/.emacs.d/private/local/lunch.el")
  (spacemacs/toggle-highlight-current-line-globally-off)
  (spacemacs/toggle-indent-guide-globally-on)
  (global-company-mode nil)
  ;; delete the trailing whitespace like a sane person
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-to-list 'auto-mode-alist '("\\.hla\\'" . asm-mode))
  (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

  ;; expand region
  (global-set-key (kbd "C-.") 'er/expand-region)
  (global-set-key (kbd "C-,") 'er/contract-region)

  ;; multi curser
  (global-set-key (kbd "C-c m c") 'mc/edit-lines)
  (global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)

  ;; lisp mode
  ;; (setq inferior-lisp-program "/usr/bin/clisp")
  (setq inferior-lisp-program "love .")

  ;; web settings
  ;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (setq web-mode-style-padding 0)
  (setq web-mode-script-padding 0)
  (setq web-mode-block-padding 0)
  (setq web-mode-comment-style 0)

  ;; python style settings
  (add-hook 'python-mode-hook 'blacken-mode)
  (setq blacken-line-length 100)

  ;; projectile settings
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recentf)

  ;; rust mode
  (setq rust-rustfmt-bin "/home/alec/.cargo/bin/rustfmt")

  ;; ssh keychain (so I dont have to keep on typing password)
  (keychain-refresh-environment)
  )

;;; .emacs ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blacken-executable "/home/alec/.pyenv/shims/black")
 '(blacken-line-length 100)
 '(python-shell-interpreter "python3")
 '(company-backends (quote (company-restclient)))
 '(package-selected-packages
   (quote
    (keychain-environment fennel-mode go-guru go-eldoc company-go go-mode base16-tomorrow-night-eighties-theme xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help terraform-mode hcl-mode x86-lookup nasm-mode blacken rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby sql-indent tide typescript-mode flycheck slime-company slime common-lisp-snippets unfill spotify pony-mode nginx-mode mwim lua-mode helm-spotify-plus multi dockerfile-mode docker tablist docker-tramp prettier-js web-mode tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data vue-mode edit-indirect ssass-mode vue-html-mode restclient-helm ob-restclient ob-http company-restclient restclient know-your-http-well yaml-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic toml-mode racer pos-tip cargo rust-mode web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern dash-functional tern coffee-mode zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme orgit organic-green-theme org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow magit-popup madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme htmlize heroku-theme hemisu-theme helm-gitignore helm-company helm-c-yasnippet hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md gandalf-theme fuzzy flatui-theme flatland-theme farmhouse-theme exotica-theme evil-magit magit transient git-commit with-editor espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-statistics company color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet yasnippet apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
   This is an auto-generated function, do not modify its content directly, use
   Emacs customize menu instead.
   This function is called at the very end of Spacemacs initialization.")
