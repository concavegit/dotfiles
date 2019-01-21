;;; init --- Evil configuration for myself as a haskell and python
;;; programmer.

;;; Commentary:
;; This configuration is for myself as a haskell and python programmer
;; using vi key-bindings.  However, as a generic Emacs user, I also
;; have goodies such as email, org-mode, and other filetype tools
;; configured.

;;; Code:

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

;; (use-package quelpa-use-package)

;; (setq use-package-ensure-function 'quelpa)

(use-package general
  :config
  (setq general-override-states '(motion
                                  normal
                                  operator)

        leader "SPC"

        leader-app (concat leader " a")
        leader-buffer (concat leader " b")
        leader-console (concat leader " t")
        leader-dir (concat leader " d")
        leader-file (concat leader " f")
        leader-lint (concat leader " e")
        leader-major (concat leader " x")
        leader-media (concat leader " m")
        leader-project (concat leader " p")
        leader-toggle (concat leader " \\"))

  (general-override-mode)
  (general-create-definer my-key-def
    :states '(motion normal operator)
    :keymaps 'override))

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (my-key-def :prefix leader-file
    "SPC" 'find-file
    "w" 'save-buffer)

  (my-key-def :prefix leader-buffer
    "SPC" 'switch-to-buffer
    "k" 'kill-buffer
    "r" 'revert-buffer))

;;; Orphans

(setenv "PATH"
        (concat
         "~/.local/bin:"
         (getenv "PATH")))

(setq backup-directory-alist '(("." . "~/.emacs.d/saves"))
      custom-file "~/.emacs.d/custom.el"
      exec-path (append exec-path '("~/.local/bin"))
      inhibit-startup-screen t
      initial-scratch-message nil
      user-full-name "Kawin Nikomborirak"

      kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(add-to-list 'evil-motion-state-modes 'image-mode)

(fset 'yes-or-no-p 'y-or-n-p)
(global-prettify-symbols-mode 1)
(menu-bar-mode -1)
(recentf-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(my-key-def :keymaps 'image-mode-map
  "h" 'image-previous-frame
  "j" 'image-previous-file
  "k" 'image-next-file
  "l" 'image-next-frame)

;;; Appearance

(setq-default indent-tabs-mode nil)

(use-package diminish)
(use-package org-bullets :hook (org-mode . org-bullets-mode))
(use-package powerline :init (setq powerline-default-separator nil))

(use-package color-theme-sanityinc-tomorrow
  :init (load-theme 'sanityinc-tomorrow-night t))

(use-package diff-hl
  :init
  (global-diff-hl-mode 1)
  :config
  (diff-hl-flydiff-mode 1)
  (setq diff-hl-side 'right))

(use-package eshell-git-prompt
  :after eshell
  :config (eshell-git-prompt-use-theme 'git-radar))

(use-package eyebrowse
  :init (eyebrowse-mode 1)
  :config
  (setq eyebrowse-wrap-around t)
  (my-key-def :keymaps 'eyebrowse-mode-map
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

(use-package heaven-and-hell
  :general
  (my-key-def
    :prefix leader-toggle
    "t" 'heaven-and-hell-toggle-theme)
  :config
  (setq heaven-and-hell-theme-type 'dark
        heaven-and-hell-themes
        '((light . leuven)
          (dark . sanityinc-tomorrow-night))))

(use-package hideshow
  :diminish hs-minor-mode
  :hook ((LaTeX-mode conf-mode prog-mode) . hs-minor-mode))

(use-package rainbow-delimiters
  :hook ((conf-mode prog-mode text-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook (conf-mode prog-mode text-mode))

(use-package spaceline
  :after powerline
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

(use-package whitespace
  :general
  (my-key-def
    :prefix leader-toggle
    "w" 'whitespace-mode))

(use-package winner
  :init (winner-mode 1)
  :config
  (my-key-def :keymaps 'winner-mode-map
    :prefix leader
    "u" 'winner-undo
    "U" 'winner-redo))

;;; Applications
(use-package evil-collection :after evil :config (evil-collection-init))
(use-package evil-magit :after magit)
(use-package htmlize)
(use-package nov :mode ("\\.epub$" . nov-mode))
(use-package org-ref :after org)
(use-package sendmail :config (setq send-mail-function 'smtpmail-send-it))
(use-package smtpmail :config (setq smtpmail-smtp-service 587))

(use-package erc
  :general
  (my-key-def
    :prefix leader-app
    "i" 'erc)
  :config
  (setq erc-prompt-for-password nil
        erc-nick "concaveirc"

        erc-autojoin-channels-alist
        '(("freenode.net" "#haskell")
          ("freenode.net" "#c++"))

        erc-track-position-in-mode-line t))

(use-package mingus
  :general
  (my-key-def
    :prefix leader-app
    "a" 'mingus)
  :config
  (add-hook 'mingus-browse-hook 'evil-motion-state)
  (add-hook 'mingus-make-playlist-hook 'evil-motion-state)
  (my-key-def :keymaps '(mingus-playlist-map mingus-browse-map)
    "l" 'mingus-load-playlist
    "z" 'mingus-random)

  (my-key-def :keymaps 'mingus-browse-map
    "2" 'mingus
    "RET" 'mingus-down-dir-or-play-song)

  (my-key-def :keymaps 'mingus-playlist-map
    "3" 'mingus-browse
    "RET" 'mingus-play))

(use-package mu4e
  :ensure nil
  :general
  (my-key-def
    :prefix leader-app
    "e" 'mu4e
    "s" 'mu4e-compose-new)
  :config
  (setq mu4e-confirm-quit nil
        mu4e-contexts
        `(,(make-mu4e-context
            :name "Primary"
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                                                                :to "concavemail@gmail.com")))
            :vars '((user-mail-address . "concavemail@gmail.com")
                    (smtpmail-smtp-server . "smtp.gmail.com")))
          ,(make-mu4e-context
            :name "Olin"
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                                                                :to "knikomborirak@olin.edu")))
            :vars '((user-mail-address . "knikomborirak@olin.edu")
                    (smtpmail-smtp-server . "smtp.office365.com")))
          ,(make-mu4e-context
            :name "Wellesley"
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                                                                :to "knikombo@wellesley.edu")))
            :vars '((user-mail-address . "knikombo@wellesley.edu")
                    (smtpmail-smtp-server . "smtp.gmail.com"))))

        mu4e-context-policy 'pick-first
        mu4e-headers-date-format "%F"
        mu4e-get-mail-command "offlineimap -o"
        mu4e-headers-time-format "%T"
        mu4e-view-prefer-html t
        mu4e-view-show-addresses t
        mu4e-view-show-images t)
  (add-to-list 'mu4e-view-actions
               '("open in browser" . mu4e-action-view-in-browser) t))

(use-package ob-ipython
  :after org
  :config
  (add-to-list 'org-latex-minted-langs '(ipython "python")))

(use-package org-mu4e
  :ensure nil
  :general
  (my-key-def
    :prefix leader-major
    "o" 'org-mu4e-compose-org-mode)
  :config
  (setq org-mu4e-convert-to-html t))

(use-package proced
  :general
  (my-key-def
    :prefix leader-app
    "p" 'proced))

(use-package pyvenv
  :init (pyvenv-mode 1)
  :config
  (my-key-def :keymaps 'pyvenv-mode-map
    :prefix leader-app
    "v" 'pyvenv-workon))

;;; Completion

(use-package company :diminish "" :init (global-company-mode 1))
(use-package eshell-z :after eshell)
(use-package yasnippet :init (yas-global-mode 1))
(use-package yasnippet-snippets :after yasnippet)

(use-package company-lsp
  :after company
  config (push 'company-lsp company-backends))

(use-package evil-smartparens
  :diminish ""
  :hook (smartparens-enabled . evil-smartparens-mode))

(use-package fcitx
  :if (and (executable-find "fcitx-remote")
           (= (with-temp-buffer (call-process "fcitx-remote" nil t)) 0))
  :config
  ;; (setq fcitx-use-dbus t)
  (fcitx-aggressive-setup))

(use-package ivy
  :diminish ""
  :init (ivy-mode 1)
  :config
  (my-key-def :prefix leader-dir
    "s" 'counsel-rg)
  (setq ivy-use-virtual-buffers t))

(use-package smartparens
  :diminish ""
  :init (smartparens-global-mode 1)
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode 1)
  (my-key-def :keymaps 'smartparens-mode-map
    "C-M-$" 'sp-end-of-sexp
    "C-M-S-h" 'sp-backward-sexp
    "C-M-S-l" 'sp-forward-sexp
    "C-M-^" 'sp-beginning-of-sexp

    "C-S-h" 'sp-backward-barf-sexp
    "C-S-l" 'sp-backward-slurp-sexp
    "C-h" 'sp-forward-barf-sexp
    "C-l" 'sp-forward-slurp-sexp

    "C-M-S-k" 'sp-split-sexp
    "C-M-t" 'sp-transpose-sexp))

;;; Consoles

(use-package eshell
  :general
  (my-key-def
    :prefix leader-console
    "SPC" 'eshell)
  :config (setq eshell-banner-message ""))

(use-package ielm
  :general
  (my-key-def
    :prefix leader-console
    "e" 'ielm)
  :config (setq ielm-header ""))

(use-package term
  :general
  (my-key-def
    :prefix leader-console
    "t" 'my/ansi-term-zsh)
  :config
  (defun my/ansi-term-zsh ()
    (interactive)
    (ansi-term "/bin/zsh"))
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))

;;; Extension Specific

(use-package doc-view :mode ("\\.odt$" . doc-view-mode))
(use-package dockerfile-mode :mode "Dockerfile$")
(use-package evil-matchit :init (global-evil-matchit-mode 1))
(use-package gitattributes-mode :mode "\\.gitattributes$")
(use-package gitconfig-mode :mode "\\.gitconfig$")
(use-package gitignore-mode :mode "\\.gitignore$")
(use-package graphviz-dot-mode :mode "\\.dot$\\|\\.gv$")
(use-package nxml :mode ("\\.xml$\\|\\.launch$" . nxml-mode))
(use-package qml-mode :mode "\\.qml$")
(use-package rust-mode :mode "\\.rs$")
(use-package scad-mode :mode "\\.scad$")
(use-package spice-mode :mode "\\.sp$")
(use-package toml-mode :mode "\\.toml$")
(use-package yaml-mode :mode "\\.ya?ml$\\|\\.rosinstall$")

(use-package cquery
  :init
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  :config
  (setq cquery-sem-highlight-method 'font-lock)
  (cquery-use-default-rainbow-sem-highlight))

(use-package dap-mode
  :after lsp-mode
  :config
  (require 'dap-java)
  (require 'dap-python)
  (dap-mode t)
  (dap-ui-mode t))

(use-package emmet-mode
  :hook (html-mode css-mode)
  :config
  (add-hook 'emmet-mode-hook (lambda () (smartparens-strict-mode -1))))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package groovy-mode
  :mode "\\.gradle$")

(use-package haskell-mode
  :mode "\\.hs$"
  :general
  (my-key-def
    :prefix leader-console
    "h" 'haskell-interactive-bring)
  :config
  (setq haskell-font-lock-symbols t
        haskell-stylish-on-save t
        haskell-tags-on-save t)

  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  (add-to-list 'evil-motion-state-modes 'haskell-error-mode)

  (my-key-def :keymaps 'haskell-mode-map
    :prefix leader-major
    "r" 'haskell-process-load-file)

  (my-key-def :keymaps 'haskell-interactive-mode-map
    "RET" 'haskell-interactive-mode-return))

(use-package lsp-haskell
  :init (add-hook 'haskell-mode-hook 'lsp))

(use-package lsp-mode
  :commands lsp
  :hook
  ((python-mode mhtml-mode css-mode js-mode sh-mode rust-mode rustic-mode)
   . lsp)
  :config
  (setq lsp-prefer-flymake nil)
        ;; lsp-auto-guess-root t)

  (my-key-def :keymaps 'prog-mode-map
    :prefix leader-lint
    "a" 'lsp-execute-code-action
    "f" 'lsp-format-buffer
    "r" 'lsp-rename))

(use-package lsp-java
  :hook (java-mode . lsp)
  :config
  (my-key-def :keymaps 'java-mode-map
    :prefix leader-lint
    "i o" 'lsp-java-import-order
    "i a" 'lsp-java-add-import))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (my-key-def
    :keymaps 'lsp-ui-mode-map
    :prefix leader-lint
    "m" 'lsp-ui-imenu)

  (my-key-def :keymaps 'lsp-ui-mode-map [remap xref-find-definitions]
    'lsp-ui-peek-find-definitions)

  (my-key-def :keymaps 'lsp-ui-mode-map [remap xref-find-references]
    'lsp-ui-peek-find-references)
  )

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ("\\.md$" . gfm-mode)
  :config
  (setq markdown-command "pandoc -f markdown_github")

  (my-key-def :keymaps 'markdown-mode-map
    "M-l" 'markdown-demote
    "M-h" 'markdown-promote)

  (my-key-def :keymaps 'markdown-mode-map
    :prefix leader-major
    "s" 'markdown-insert-gfm-code-block
    "n" 'markdown-cleanup-list-numbers))

(use-package meghanada
  :disabled t
  :hook (java-mode . meghanada-mode)
  :config
  (my-key-def :keymaps 'meghanada-mode-map
    :prefix leader-major
    "C" 'meghanada-compile-project
    "SPC" 'meghanada-jump-declaration
    "c" 'meghanada-compile-file
    "i" 'meghanada-import-all))

(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :config
  (require 'ox-beamer)
  (require 'org-tempo)

  (add-to-list 'org-latex-minted-langs '(conf "ini"))

  (setq org-confirm-babel-evaluate nil
        org-pretty-entities t
        org-startup-indented t
        org-plantuml-jar-path "/usr/share/plantuml/lib/plantuml.jar"

        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -shell-escape -interaction nonstopmode --synctex=1 -output-directory %o %f"))

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  (when (executable-find "pygmentize")
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-latex-listings 'minted
          org-latex-minted-options '(("frame" "single"))))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)
                                 (haskell . t)
                                 (ipython . t)
                                 (latex . t)
                                 (plantuml . t)
                                 (dot . t)
                                 (spice . t))))

(use-package pdf-tools
  :mode ("\\.pdf$" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq pdf-misc-print-programm "gtklp"))

(use-package plantuml-mode
  :mode "\\.plantuml$"
  :config (setq plantuml-jar-path "/usr/share/plantuml/lib/plantuml.jar"))

(use-package platformio-mode
  :hook (c++-mode . platformio-conditionally-enable))

(use-package scad-preview
  :general
  (my-key-def
    :prefix leader-major
    "SPC" 'scad-preview-mode)
  :config
  (add-to-list 'evil-motion-state-modes 'scad-preview--image-mode)
  (my-key-def :keymaps 'scad-preview--image-mode-map
    "C-h" 'scad-preview-roty+
    "C-j" 'scad-preview-rotx-
    "C-k" 'scad-preview-rotx+
    "C-l" 'scad-preview-roty-
    "M-h" 'scad-preview-trnsx+
    "M-j" 'scad-preview-trnsz+
    "M-k" 'scad-preview-trnsz-
    "M-l" 'scad-preview-trnsx-
    "h" 'scad-preview-rotz-
    "j" 'scad-preview-dist+
    "k" 'scad-preview-dist-
    "l" 'scad-preview-rotz+
    "r" 'scad-preview-reset-camera-parameters))

(use-package sh-script
  :mode ("\\.sh$\\|\\.ebuild" . sh-mode)
  :interpreter ("sh" . sh-mode))

(use-package shakespeare-mode
  :mode (("\\.hamlet$" . shakespeare-hamlet-mode)
         ("\\.julius" . shakespeare-julius-mode)
         ("\\.lucius$" . shakespeare-lucius-mode)))

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex$" . LaTeX-mode)
  :config (setq TeX-newline-function 'newline-and-indent))

(use-package verilog-mode
  :hook
  (verilog-mode . (lambda () (clear-abbrev-table verilog-mode-abbrev-table)))
  :mode "\\.\\(v\\|vs\\)$"
  :config
  (setq flycheck-verilog-verilator-executable "verilator_bin"
        verilog-linter "verilator -lint-only"
        verilog-auto-newline nil))

;;; General Text

(setq require-final-newline t)

(use-package evil-commentary :diminish "" :init (evil-commentary-mode 1))
(use-package evil-surround :init (global-evil-surround-mode 1))

(use-package evil-exchange
  :general
  (my-key-def

    "gs" 'evil-exchange
    "gS" 'evil-exchange-cancel))

(use-package evil-numbers
  :general
  (my-key-def
    :prefix leader
    "+" 'evil-numbers/inc-at-pt
    "-" 'evil-numbers/dec-at-pt))

(use-package sudo-edit
  :general
  (my-key-def
    :prefix leader-file
    "s" 'sudo-edit))

(use-package undo-tree
  :diminish ""
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree"))
        undo-tree-auto-save-history t)
  (my-key-def :keymaps 'undo-tree-map
    "U" 'undo-tree-visualize))

(use-package xref
  :config (my-key-def "M-." 'xref-find-definitions))

;;; Linters

(use-package flycheck
  :init (global-flycheck-mode 1)
  :diminish ""
  :config
  (my-key-def :keymaps 'flycheck-mode-map
    :prefix leader-lint
    "j" 'flycheck-next-error
    "k" 'flycheck-previous-error
    "SPC" 'flycheck-list-errors))

(use-package flyspell
  :diminish ""
  :hook (((conf-mode prog-mode) . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

;;; Projects

(use-package vc :config (setq vc-follow-symlinks t))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode 1)
  (my-key-def :keymaps 'projectile-mode-map
    :prefix leader-project
    "s" 'counsel-projectile-rg))

(use-package magit
  :general
  (my-key-def
    :prefix leader-project
    "g" 'magit-status))

(use-package projectile
  :init
  (projectile-mode 1)
  :config
  (my-key-def :keymaps 'projectile-mode-map
    :prefix leader-project
    "SPC" 'projectile-switch-project
    "c" 'projectile-compile-project
    "f" 'projectile-find-file))

;;; init.el ends here
