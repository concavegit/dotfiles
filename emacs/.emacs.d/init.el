(add-to-list 'default-frame-alist '(font . "Terminus (TTF) 13"))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(fset 'yes-or-no-p 'y-or-n-p)
(global-prettify-symbols-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(electric-pair-mode 1)
(recentf-mode 1)

(setq-default indent-tabs-mode nil
              tab-width 4
              kill-buffer-query-functions
              (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq straight-use-package-by-default t
      backup-directory-alist '(("." . "~/.emacs.d/saves"))
      inhibit-startup-screen t
      initial-scratch-message nil)

(straight-use-package 'use-package)

(use-package diminish)

(use-package general
  :config
  (setq leader "SPC")
  (general-create-definer general-project-definer
    :states 'normal
    :keymaps 'override
    :prefix  (concat leader " p"))
  (general-create-definer general-application-definer
    :states 'normal
    :keymaps 'override
    :prefix  (concat leader " a"))
  (general-create-definer general-buffer-definer
    :states 'normal
    :keymaps 'override
    :prefix  (concat leader " b"))
  (general-create-definer general-code-definer
    :states 'normal
    :keymaps 'override
    :prefix (concat leader " c"))
  (general-create-definer general-debug-definer
    :states 'normal
    :keymaps 'override
    :prefix (concat leader " d"))
  (general-create-definer general-toggle-definer
    :states 'normal
    :keymaps 'override
    :prefix (concat leader " t"))
  (general-create-definer general-window-definer
    :states 'normal
    :keymaps 'override
    :prefix (concat leader " w"))
  (general-buffer-definer
    "w" 'save-buffer
    "R" 'revert-buffer
    "d" 'kill-buffer
    "b" 'switch-to-buffer))

(use-package lsp-mode
  :hook ((python-mode . lsp)
         (django-mode . lsp)
         (typescript-mode . lsp)
         (js-mode . lsp))
  :config
  (setq lsp-prefer-flymake nil)
  (general-code-definer
    :keymaps 'lsp-mode-map
    "r" 'lsp-rename
    "R" 'lsp-restart-workspace
    "f" 'lsp-format-buffer
    "a" 'lsp-execute-code-action)
  )

(use-package whitespace
  :general
  (general-toggle-definer
    "w" 'whitespace-mode)
  (general-buffer-definer
    "c" 'whitespace-cleanup))

(use-package find-file
  :commands find-file
  :general
  (general-buffer-definer
    "f" 'find-file))

(use-package straight
  :config
  (setq straight-check-for-modifications '(watch-files find-when-checking)))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package color-theme-sanityinc-tomorrow
  :init (load-theme 'sanityinc-tomorrow-night t))

(use-package powerline
  :config
  (setq powerline-default-separator nil))

(use-package spaceline
  :init
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

(use-package company
  :diminish ""
  :init
  (global-company-mode 1))

(use-package lispy
  :diminish ""
  :general
  (general-toggle-definer
    "d" 'lispy-mode))

(use-package magit
  :general
  (general-project-definer
    "g" 'magit-status))

(use-package evil-magit :after magit)

(use-package lispyville
  :after evil
  :diminish ""
  :init
  (define-globalized-minor-mode
    my-lispyville-global-mode
    lispyville-mode
    (lambda () (lispyville-mode 1)))
  (my-lispyville-global-mode 1)
  :config
  (general-add-hook 'prog-mode-hook #'lispyville-mode)
  (lispyville-set-key-theme '(operators c-w additional commentary)))

(use-package flyspell
  :diminish ""
  :hook (((LaTeX-mode conf-mode prog-mode) . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(use-package ispell
  :config
  (setq ispell-program-name "aspell"))

(use-package hideshow
  :diminish hs-minor-mode
  :hook ((LaTeX-mode conf-mode prog-mode) . hs-minor-mode))

(use-package ivy :diminish " ":init (ivy-mode 1))
(use-package projectile
  :diminish counsel-mode
  :general
  (general-buffer-definer
    :keymaps 'counsel-mode-map
    "r" 'counsel-buffer-or-recentf)
  (general-project-definer
    :keymaps 'projectile-mode-map
    "s" 'projectile-switch-project
    "f" 'projectile-find-file)

  :init
  (projectile-mode 1)
  (counsel-mode 1)

  :config
  (general-project-definer
    :keymaps 'projectile-mode-map
    "t" 'projectile-run-term))

(use-package counsel-projectile :after projectile
  :config
  (counsel-projectile-mode 1))


(use-package lsp-ui :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-use-childframe nil)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (general-code-definer
    :keymaps 'lsp-ui-mode-map
    "d" 'lsp-ui-doc-focus-frame)
  (general-define-key :states 'normal :keymaps 'lsp-ui-doc-frame-mode-map "q" 'lsp-ui-doc-unfocus-frame))

(use-package company-lsp :commands company-lsp
  :config (push 'company-lsp company-backends))
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :after lsp-mode
  :hook lsp-mode
  :config

  (require 'dap-python)
  (dap-mode 1)
  (dap-ui-mode 1)

  (general-debug-definer
    :keymaps 'dap-mode-map
    "d" 'dap-debug
    "b" 'dap-ui-breakpoints
    "D" 'dap-debug-edit-template
    "t" 'dap-breakpoint-toggle
    "c" 'dap-continue
    "i" 'dap-step-in
    "o" 'dap-step-out
    "n" 'dap-next
    "s" 'dap-stop-thread
    "l" 'dap-ui-locals
    "r" 'dap-ui-repl
    "R" 'dap-debug-restart))

(use-package flycheck
  :diminish ""
  :init (global-flycheck-mode 1)
  :config
  (general-code-definer
    "n" 'flycheck-next-error
    "p" 'flycheck-previous-error))

(use-package yasnippet :diminish yas-minor-mode :init (yas-global-mode 1))
(use-package yasnippet-snippets :after yasnippet)

(use-package evil-surround :init (global-evil-surround-mode 1))

(use-package evil-exchange
  :init
  (setq evil-exchange-key "gs"
        evil-exchange-cancel-key "gS")

  (evil-exchange-install))

(use-package xref
  :config (general-define-key :keymaps 'lsp-mode-map :states 'normal "M-." 'xref-find-definitions))

(use-package lsp-ivy
  :after lsp
  :config
  (general-code-definer
    "s" 'lsp-ivy-workspace-symbol
    "S" 'lsp-ivy-global-workspace-symbol))

(use-package lsp-treemacs
  :after lsp
  :config
  (lsp-treemacs-sync-mode 1)
  (general-code-definer :keymaps 'lsp-mode-map
    "t s" 'lsp-treemacs-symbols
    "t e" 'lsp-treemacs-errors-list
    "t r" 'lsp-treemacs-references
    "t i" 'lsp-treemacs-implementations
    "t c" 'lsp-treemacs-call-hierarchy))

(use-package emmet-mode
  :hook (html-mode scss-mode css-mode django-html-mode))

(use-package django-mode
  :mode ("\\.djhtml$" . django-html-mode)
  :config
  (add-to-list 'lsp-language-id-configuration '(django-mode . "django"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     (lambda () lsp-clients-python-command))
                    :major-modes '(django-mode)
                    :priority -1
                    :server-id 'django-pyls
                    :library-folders-fn (lambda (_workspace) lsp-clients-python-library-directories)
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                        (lsp--set-configuration (lsp-configuration-section "django-pyls"))))))


  )

(use-package sendmail :config (setq send-mail-function 'smtpmail-send-it))
(use-package smtpmail :config (setq smtpmail-smtp-service 587))

(use-package org
  :straight org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :config
  (setq org-confirm-babel-evaluate nil
        org-pretty-entities t
        org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
        org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"

        org-latex-pdf-process (list "latexmk %f -shell-escape -bibtex -f -pdf"))

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  (when (executable-find "pygmentize")
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-latex-listings 'minted
          org-latex-minted-options '(("frame" "single"))))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)
                                 (latex . t)
                                 (plantuml . t)
                                 (dot . t)
                                 (ditaa . t)
                                 (shell . t))))

(use-package treemacs)
(use-package treemacs-evil :after treemacs evil)
(use-package treemacs-magit :after treemacs magit)

(use-package forge :after magit)
(use-package eldoc
  :diminish "")

(use-package pyvenv
  :init (pyvenv-mode 1)
  :config
  (general-code-definer :keymaps 'pyvenv-mode-map
    "v" 'pyvenv-workon))

(use-package haskell-mode
  :mode "\\.hs$"
  :config
  (setq haskell-font-lock-symbols t)

  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template))

(use-package lsp-haskell :init (add-hook 'haskell-mode-hook 'lsp))

(use-package undo-tree
  :diminish ""
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree"))
        undo-tree-auto-save-history t)
  (general-buffer-definer :keymaps 'undo-tree-map
    "u" 'undo-tree-visualize))

(use-package diff-hl
  :init
  (global-diff-hl-mode 1)
  :config
  (diff-hl-flydiff-mode 1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq diff-hl-side 'right))

(use-package image+
  :init
  (imagex-global-sticky-mode 1))

(use-package term
  :general
  (general-application-definer
    "t" 'term))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))

(use-package erc
  :general
  (general-application-definer
    "i" 'erc)
  :config
  (setq erc-prompt-for-password nil
        erc-nick "concaveirc"

        erc-autojoin-channels-alist
        '(("freenode.net" "#haskell")
          ("freenode.net" "##c++")
          ("freenode.net" "##C")
          ("freenode.net" "##java")
          ("freenode.net" "#python"))))

(use-package eyebrowse
  :init (eyebrowse-mode 1)
  :config
  (setq eyebrowse-wrap-around t)
  (general-define-key
   :states 'normal
   :keymaps 'eyebrowse-mode-map
   "gt" 'eyebrowse-next-window-config
   "gT" 'eyebrowse-prev-window-config
   "gC" 'eyebrowse-close-window-config
   "M-0" 'eyebrowse-switch-to-window-config-0
   "M-1" 'eyebrowse-switch-to-window-config-1
   "M-2" 'eyebrowse-switch-to-window-config-2
   "M-3" 'eyebrowse-switch-to-window-config-3
   "M-4" 'eyebrowse-switch-to-window-config-4
   "M-5" 'eyebrowse-switch-to-window-config-5
   "M-6" 'eyebrowse-switch-to-window-config-6
   "M-7" 'eyebrowse-switch-to-window-config-7
   "M-8" 'eyebrowse-switch-to-window-config-8
   "M-9" 'eyebrowse-switch-to-window-config-9))

(use-package winner
  :init (winner-mode 1)
  :config
  (general-window-definer
    :keymaps 'winner-mode-map
    "u" 'winner-undo
    "U" 'winner-redo))

(use-package protobuf-mode
  :mode "\\.proto$")

(use-package evil-commentary :diminish "" :init (evil-commentary-mode 1))

(use-package exec-path-from-shell :init (exec-path-from-shell-initialize))

(use-package vc
  :config
  (setq vc-follow-symlinks t))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook (conf-mode prog-mode text-mode))

(use-package ripgrep
  :general
  (general-project-definer
    :keymaps 'projectile-mode-map
    "r" 'projectile-ripgrep))

(use-package docker
  :general
  (general-application-definer
    "d" 'docker))

(use-package dockerfile-mode :mode "Dockerfile\\'")

(use-package yaml-mode
  :mode( "\\.yml\\'" "\\.yaml\\'"))

(use-package helm-aws
  :general
  (general-application-definer
    "a" 'helm-aws))

(use-package swift-mode
  :mode "\\.swift\\'")

(use-package typescript-mode
  :mode "\\.tsx\\'\\|\\.jsx\\'\\|\\.ts\\'"
  :config
  (require 'dap-chrome))

(use-package graphviz-dot-mode
  :mode "\\.dot\\'\\|\\.gv\\'"
  :config
  (require 'company-graphviz-dot))

(use-package display-line-numbers
  :general
  (general-toggle-definer
    "l" 'display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative))
