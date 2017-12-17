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

(use-package general
  :config
  (setq general-default-states 'motion
	leader "SPC"

	leader-app (concat leader " a")
	leader-buffer (concat leader " b")
	leader-console (concat leader " t")
	leader-file (concat leader " f")
	leader-lint (concat leader " e")
	leader-major (concat leader " x")
	leader-media (concat leader " m")
	leader-project (concat leader " p")
	leader-toggle (concat leader " \\")))

(use-package evil
  :init (evil-mode 1)
  :config
  (general-define-key :prefix leader-file
		      "SPC" 'find-file
		      "w" 'save-buffer)

  (general-define-key :prefix leader-buffer
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

(general-define-key :keymaps 'image-mode-map
		    "h" 'image-previous-frame
		    "j" 'image-previous-file
		    "k" 'image-next-file
		    "l" 'image-next-frame)

;;; Appearance

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
  (general-define-key :keymaps 'eyebrowse-mode-map
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

(use-package hideshow
  :diminish hs-minor-mode
  :hook ((LaTeX-mode . hs-minor-mode)
	 (conf-mode . hs-minor-mode)
	 (prog-mode . hs-minor-mode)))

(use-package rainbow-delimiters
  :hook ((conf-mode . rainbow-delimiters-mode)
	 (prog-mode . rainbow-delimiters-mode)
	 (text-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook (conf-mode
	 prog-mode
	 text-mode))

(use-package spaceline
  :after powerline
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

(use-package whitespace
  :general
  (:prefix leader-toggle
	   "w" 'whitespace-mode))

(use-package winner
  :init (winner-mode 1)
  :config
  (general-define-key :keymaps 'winner-mode-map
		      :prefix leader
		      "u" 'winner-undo
		      "U" 'winner-redo))

;;; Applications

(use-package htmlize)
(use-package sendmail :config (setq send-mail-function 'smtpmail-send-it))

(use-package evil-magit :after magit)

(use-package browse-url
  :config
  (setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "qutebrowser"))

(use-package erc
  :general
  (:prefix leader-app
	   "i" 'erc)
  :config
  (setq erc-prompt-for-password nil
	erc-nick "concaveirc"))

(use-package mingus
  :general
  (:states 'motion :prefix leader-app
	   "a" 'mingus)
  :config
  (add-hook 'mingus-browse-hook 'evil-motion-state)
  (add-hook 'mingus-make-playlist-hook 'evil-motion-state)
  (general-define-key :keymaps '(mingus-playlist-map mingus-browse-map)
		      :states 'motion
		      "l" 'mingus-load-playlist
		      "z" 'mingus-random)

  (general-define-key :keymaps 'mingus-browse-map
		      :states 'motion
		      "2" 'mingus
		      "RET" 'mingus-down-dir-or-play-song)

  (general-define-key :keymaps 'mingus-playlist-map
		      :states 'motion
		      "3" 'mingus-browse
		      "RET" 'mingus-play))

(use-package mu4e
  :ensure nil
  :general
  (:prefix leader-app
	   "e" 'mu4e
	   "s" 'mu4e-compose-new)
  :config
  (add-to-list 'evil-motion-state-modes 'mu4e-headers-mode)
  (add-to-list 'evil-motion-state-modes 'mu4e-main-mode)
  (add-to-list 'evil-motion-state-modes 'mu4e-view-mode)

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
	    :name "Work"
	    :match-func (lambda (msg)
			  (when msg
			    (mu4e-message-contact-field-matches msg
								:to "kawin.nikomborirak@students.olin.edu")))
	    :vars '((user-mail-address . "kawin.nikomborirak@students.olin.edu")
		    (smtpmail-smtp-server . "smtps.olin.edu"))))

	mu4e-context-policy 'pick-first
	mu4e-get-mail-command "offlineimap -o"
	mu4e-view-prefer-html t
	mu4e-view-show-addresses t
	mu4e-view-show-images t)

  (general-define-key :keymaps '(mu4e-headers-mode-map
				 mu4e-main-mode-map
				 mu4e-view-mode-map)
		      "s" 'mu4e-context-switch
		      "J" 'mu4e~headers-jump-to-maildir
		      "b" 'mu4e-headers-search-bookmark)

  (general-define-key :keymaps 'mu4e-view-mode-map
		      "C-n" 'mu4e-view-headers-next
		      "C-p" 'mu4e-view-headers-prev
		      "TAB" 'shr-next-link)

  (general-define-key :keymaps 'mu4e-headers-mode-map
		      "RET" 'mu4e-headers-view-message)

  (general-define-key :keymaps 'mu4e-main-mode-map
		      "u" 'mu4e-update-mail-and-index)

  (general-define-key :keymaps '(mu4e-headers-mode-map
				 mu4e-view-mode-map)
		      "F" 'mu4e-compose-forward))

(use-package nov
  :mode ("\\.epub$" . nov-mode)
  :config
  (add-to-list 'evil-motion-state-modes 'nov-mode)
  (general-define-key :keymaps 'nov-mode-map
		      "o" 'nov-goto-toc
		      "C-n" 'nov-next-document
		      "C-p" 'nov-previous-document))

(use-package org-mu4e
  :ensure nil
  :general
  (:keymaps 'mu4e-compose-mode-map
	    :prefix leader-major
	    "o" 'org-mu4e-compose-org-mode)
  :config
  (setq org-mu4e-convert-to-html t))

(use-package ox-reveal
  :after org
  :config
  (setq org-reveal-root "https://cdn.rawgit.com/hakimel/reveal.js/master"
	org-reveal-theme "black"))

(use-package proced
  :general
  (:prefix leader-app
	   "p" 'proced)
  :config
  (add-to-list 'evil-motion-state-modes 'proced-mode)
  (general-define-key :keymaps 'proced-mode-map
		      "TAB" 'proced-mark
		      "d" 'proced-send-signal))

(use-package pyvenv
  :init (pyvenv-mode 1)
  :config
  (general-define-key :keymaps 'pyvenv-mode-map
		      :prefix leader-app
		      "v" 'pyvenv-workon))

(use-package smtpmail
  :config
  (setq 
   smtpmail-smtp-service 587))

;;; Completion

(use-package company :diminish "" :init (global-company-mode 1))
(use-package eshell-z :after eshell)
(use-package yasnippet :init (yas-global-mode 1))

(use-package evil-smartparens
  :diminish ""
  :hook (smartparens-enabled . evil-smartparens-mode))

(use-package fcitx
  :when (and (executable-find "fcitx-remote")
	     (= (with-temp-buffer (call-process "fcitx-remote" nil t)) 0))
  :config
  ;; (setq fcitx-use-dbus t)
  (fcitx-aggressive-setup))

(use-package ivy
  :diminish ""
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (general-define-key :keymaps 'ivy-mode-map
		      :states nil
		      [escape] 'minibuffer-keyboard-quit))

(use-package smartparens
  :diminish ""
  :init (smartparens-global-mode 1)
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode 1)
  (general-define-key :keymaps 'smartparens-mode-map
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
  (:prefix leader-console
	   "SPC" 'eshell)
  :config
  (setq eshell-banner-message "")
  (general-define-key :keymaps 'eshell-mode-map
		      "RET" 'eshell-send-input))

(use-package ielm
  :general
  (:prefix leader-console
	   "e" 'ielm)
  :config (setq ielm-header ""))

(use-package term
  :general
  (:prefix leader-console 
	   "t" 'my/ansi-term-zsh)
  :config
  (defun my/ansi-term-zsh ()
    (interactive)
    (ansi-term "/bin/zsh"))
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))
  (evil-set-initial-state 'term-mode 'normal))

;;; Extension Specific

(use-package evil-matchit :init (global-evil-matchit-mode 1))
(use-package gitattributes-mode :mode "\\.gitattributes$")
(use-package gitconfig-mode :mode "\\.gitconfig$")
(use-package gitignore-mode :mode "\\.gitignore$")
(use-package qml-mode :mode "\\.qml$")
(use-package yaml-mode :mode "\\.ya?ml$")

(use-package arduino-mode
  :mode "\\.\\(pde\\|ino\\)$")

(use-package doc-view
  :mode ("\\.odt$" . doc-view-mode)
  :config
  (evil-define-motion my/doc-view-goto-page (count)
    (if count
	(doc-view-goto-page count)
      (doc-view-last-page)))

  (evil-define-motion my/doc-view-next-page (count)
    (if count
	(dotimes (number count nil)
	  (doc-view-next-page))
      (doc-view-next-page)))

  (evil-define-motion my/doc-view-previous-page (count)
    (if count
	(dotimes (number count nil)
	  (doc-view-previous-page))
      (doc-view-previous-page)))

  (add-to-list 'evil-motion-state-modes 'doc-view-mode)

  (general-define-key :keymaps 'doc-view-mode-map
		      :states 'motion
		      "G" 'my/doc-view-goto-page
		      "gg" 'doc-view-first-page
		      "j" 'my/doc-view-next-page
		      "k" 'my/doc-view-previous-page

		      "s" 'doc-view-fit-height-to-window
		      "a" 'doc-view-fit-page-to-window
		      "S" 'doc-view-fit-width-to-window

		      "/" 'doc-view-search
		      "?" 'doc-view-search-backward))

(use-package emmet-mode
  :hook (html-mode
	 css-mode)
  :config
  (add-hook 'emmet-mode-hook (lambda () (smartparens-strict-mode -1))))

(use-package elpy
  :mode ("\\.py$" . python-mode)
  :init (elpy-enable)
  :general
  (:prefix leader-console
	   "p" 'elpy-shell-switch-to-shell)
  :config
  (when (executable-find "ipython")
    (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
    (elpy-use-ipython))

  (delete 'elpy-module-highlight-indentation elpy-modules)

  (general-define-key :keymaps 'elpy-mode-map
		      :prefix leader-major
		      "SPC" '(:keymap elpy-refactor-map :package elpy)
		      "t SPC" 'elpy-test
		      "tr" 'elpy-set-test-runner))

(use-package haskell-mode
  :mode "\\.hs$"
  :general
  (:prefix leader-console
	   "h" 'haskell-interactive-bring)
  :config
  (setq haskell-font-lock-symbols t
	haskell-stylish-on-save t)

  (add-to-list 'evil-motion-state-modes 'haskell-error-mode)

  (general-define-key :keymaps 'haskell-interactive-mode-map
		      "RET" 'haskell-interactive-mode-return))

(use-package intero
  :hook (haskell-mode . intero-mode)
  :config
  (add-to-list 'evil-motion-state-modes 'intero-help-mode)

  (setq intero-stack-executable "~/.local/bin/stack")

  (general-define-key :keymaps 'intero-mode-map
		      :prefix leader-major
		      "SPC" 'intero-goto-definition
		      "i" 'intero-info
		      "r" 'intero-repl-load
		      "s" 'intero-apply-suggestions
		      "t" 'intero-type-at
		      "x" 'intero-restart)
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ("\\.md$" . gfm-mode)
  :config
  (setq markdown-command "pandoc -f markdown_github")

  (general-define-key :keymaps 'markdown-mode-map
		      "M-l" 'markdown-demote
		      "M-h" 'markdown-promote)

  (general-define-key :keymaps 'markdown-mode-map
		      :prefix leader-major
		      "s" 'markdown-insert-gfm-code-block
		      "n" 'markdown-cleanup-list-numbers))

(use-package meghanada
  :hook (java-mode . meghanada-mode)
  :config
  (general-define-key :keymaps 'meghanada-mode-map
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

  (setq org-confirm-babel-evaluate nil
	org-pretty-entities t
	org-startup-indented t
	org-plantuml-jar-path "/usr/share/plantuml/lib/plantuml.jar"

	org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode --synctex=1 -output-directory %o %f"))

  (defun my/org-meta-return ()
    (interactive)
    (end-of-line)
    (org-meta-return))

  (when (executable-find "pygmentize")
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-latex-listings 'minted
	  org-latex-minted-options '(("frame" "single"))))

  (org-babel-do-load-languages 'org-babel-load-languages
			       '((python . t)
				 (haskell . t)
				 (latex . t)
				 (plantuml . t)
				 (sh . t)))

  (general-define-key :keymaps 'org-mode-map
		      :prefix leader-major
		      "SPC" 'org-export-dispatch
		      "b SPC" 'org-babel-execute-buffer
		      "bj" 'org-babel-next-src-block
		      "bk" 'org-babel-previous-src-block
		      "bs" 'org-babel-execute-subtree
		      "t SPC" 'org-table-recalculate-buffer-tables)

  (general-define-key :keymaps 'org-mode-map
		      "<M-return>" 'my/org-meta-return
		      "M-h" 'org-metaleft
		      "M-l" 'org-metaright)

  (general-define-key :keymaps 'org-mode-map
		      :prefix "g"
		      "h" 'org-previous-visible-heading
		      "j" 'org-forward-heading-same-level
		      "k" 'org-backward-heading-same-level
		      "l" 'org-next-visible-heading
		      "o" 'outline-up-heading))

(use-package pdf-tools
  :mode ("\\.pdf$" . pdf-view-mode)
  :config
  (pdf-tools-install)

  (evil-define-motion my/pdf-view-goto-page (count)
    (if count
	(pdf-view-goto-page count)
      (pdf-view-last-page)))

  (evil-define-motion my/pdf-view-scroll-down (count)
    (if count
	(dotimes (number count nil)
	  (pdf-view-scroll-down-or-previous-page))
      (pdf-view-scroll-down-or-previous-page)))

  (evil-define-motion my/pdf-view-scroll-up (count)
    (if count
	(dotimes (number count nil)
	  (pdf-view-scroll-up-or-next-page))
      (pdf-view-scroll-up-or-next-page)))

  (setq pdf-misc-print-programm "gtklp")

  (add-to-list 'evil-motion-state-modes 'pdf-view-mode)
  (add-to-list 'evil-motion-state-modes 'pdf-outline-buffer-mode)

  (general-define-key :keymaps 'pdf-view-mode-map
		      "G" 'my/pdf-view-goto-page
		      "gg" 'pdf-view-first-page
		      "j" 'my/pdf-view-scroll-up
		      "k" 'my/pdf-view-scroll-down
		      "o" 'pdf-outline

		      "+" 'pdf-view-enlarge
		      "-" 'pdf-view-shrink
		      "0" 'pdf-view-scale-reset
		      "H" 'pdf-view-fit-height-to-window
		      "P" 'pdf-view-fit-page-to-window
		      "W" 'pdf-view-fit-width-to-window

		      "zr" 'pdf-view-reset-slice
		      "zs" 'pdf-view-set-slice-from-bounding-box

		      "/" 'isearch-forward-regexp
		      "?" 'isearch-backward-regexp)

  (general-define-key :keymaps 'pdf-outline-buffer-mode-map
		      "RET" 'pdf-outline-follow-link-and-quit
		      "TAB" 'outline-toggle-children)

  (add-hook 'pdf-view-mode-hook 'pdf-isearch-minor-mode))

(use-package plantuml-mode
  :mode "\\.plantuml$"
  :config (setq plantuml-jar-path "/usr/share/plantuml/lib/plantuml.jar"))

(use-package scad-mode
  :mode "\\.scad$")

(use-package scad-preview
  :general (:keymaps 'scad-mode-map
		     :prefix leader-major
		     "SPC" 'scad-preview-mode)
  :config
  (add-to-list 'evil-motion-state-modes 'scad-preview--image-mode)
  (general-define-key :keymaps 'scad-preview--image-mode-map
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

(use-package shakespeare-mode
  :mode (("\\.hamlet$" . shakespeare-hamlet-mode)
	 ("\\.julius" . shakespeare-julius-mode)
	 ("\\.lucius$" . shakespeare-lucius-mode)))

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex$" . LaTeX-mode)
  :config (setq TeX-newline-function 'newline-and-indent))

(use-package toml-mode
  :mode "\\.toml$")

;;; General Text

(setq require-final-newline t)

(use-package evil-commentary :diminish "" :init (evil-commentary-mode 1))
(use-package evil-surround :init (global-evil-surround-mode 1))
(use-package sudo-edit :general (:prefix leader-file "s" 'sudo-edit))

(use-package evil-exchange
  :general
  ("gx" 'evil-exchange
   "gX" 'evil-exchange-cancel))

(use-package evil-numbers
  :general
  (:prefix leader
	   "+" 'evil-numbers/inc-at-pt
	   "-" 'evil-numbers/dec-at-pt))

(use-package undo-tree
  :diminish ""
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree"))
	undo-tree-auto-save-history t)
  (general-define-key :keymaps 'undo-tree-map
		      "U" 'undo-tree-visualize))

;;; Linters

(use-package flycheck
  :init (global-flycheck-mode 1)
  :diminish ""
  :config
  (general-define-key :keymaps 'flycheck-mode-map
		      :prefix leader-lint
		      "j" 'next-error
		      "k" 'previous-error))

(use-package flyspell
  :diminish ""
  :hook ((conf-mode . flyspell-prog-mode)
	 (prog-mode . flyspell-prog-mode)
	 (text-mode . flyspell-mode)))

;;; Projects

(use-package vc :config (setq vc-follow-symlinks t))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-on)
  (general-define-key :keymaps 'projectile-mode-map
		      :prefix leader-project
		      "s" 'counsel-rg))

(use-package magit
  :general
  (:prefix leader-project
	   "g" 'magit-status))

(use-package projectile
  :init (projectile-mode 1)
  :config
  (general-define-key :keymaps 'projectile-mode-map
		      :prefix leader-project
		      "f" 'projectile-find-file
		      "SPC" 'projectile-switch-project))

;;; init.el ends here
