;;; -*- lexical-binding: t; -*-

;; Straight package pananger.
;; Also load during byte-compilation, else the following
;; `use-package` macros would expand without calls to `straight-use-package`.
(eval-and-compile
  (setq
   straight-vc-git-default-clone-depth 1
   straight-repository-branch "develop"
   straight-check-for-modifications nil
   straight-use-package-by-default t)
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; Byte-compilation requirements.
(eval-when-compile
  (setq use-package-always-defer t)
  (require 'use-package))

;; I don't care about this even when debugging, and it messes up with byte-compilation, so turn it off.
(setq use-package-compute-statistics nil)

;; Setup no-littering as early as possible.
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :demand t
  :config
  (setq custom-file (expand-file-name "custom.el" no-littering-etc-directory)))

;; Basic UI.

(when (display-graphic-p)
  ;; https://emacs.stackexchange.com/questions/20240/how-to-distinguish-c-m-from-return
  ;; to define C-m key
  (define-key input-decode-map [?\C-m] [C-m])
  ;; remove some keybindings that can be accidentally triggered..
  ;; suspend-frame
  (define-key global-map (kbd "C-x C-z") nil)
  ;; save-buffers-kill-terminal
  (define-key global-map (kbd "C-x C-c") nil))

;; Blackout: hide minor modes from modeline
(use-package blackout
  :demand t)

;; Better help.
(when (version< emacs-version "30.0") (straight-use-package 'which-key))
(use-package which-key
  :straight nil ; Built-in after emacs 30.
  :ensure nil ; Built-in after emacs 30.
  :blackout t
  :hook (emacs-startup . which-key-mode)

  :custom
  ;; We configure it so that `which-key' is triggered by typing C-h
  ;; during a key sequence (the usual way to show bindings). See
  ;; <https://github.com/justbur/emacs-which-key#manual-activation>.
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay most-positive-fixnum)
  (which-key-idle-secondary-delay 0.05))

;; Better repeat.
(use-package repeat-fu
  :blackout t
  ;; repeat-fu defines ~incf~ & ~decf~ instead of using ~cl-lib~,
  ;; inside a ~eval-when-compile~. This causes trouble for byte-compile
  ;; and native-compile, where we must load the uncompiled ~repeat-fu.el~
  ;; when compiling ~repeat-fu-preset-meow.el~. I can control this order
  ;; for byte-compile by explicitly setting the loading order for ~straight.el~,
  ;; but I cannot do the same thing for native-compile (as native-compile must
  ;; use ~.elc~ files instead, so I just disable native-compile for
  ;; ~repeat-fu-preset-meow.el~.
  ;; Something I don't understand: I think the macros should be gone in the ~.elc~
  ;; and native-compile should have nothing to do with them. So how did things
  ;; happen?
  ;; :straight (:build (:not compile))
  :straight (:files ("repeat-fu-preset-meow.el" "repeat-fu.el"))
  :commands (repeat-fu-mode repeat-fu-execute)

  :custom
  (repeat-fu-preset 'meow)

  :hook
  (meow-mode . (lambda ()
                 (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
                   (repeat-fu-mode)))))

;; Modal editing.
(use-package meow
  ;; Somehow ~meow-global-mode~ does not trigger evaluation
  ;; of :config block.
  :demand t
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (add-to-list
   'meow-char-thing-table
   '(?\( . round))
  (add-to-list
   'meow-char-thing-table
   '(?\[ . square))
  (add-to-list
   'meow-char-thing-table
   '(?\{ . curly))

  ;; Fix digit-arguments.
  (eval-when-compile
    (defmacro meow-expand-n (name n)
      `(defun ,name ()
         (interactive)
         (let ((this-command 'meow-expand))
           (meow-expand ,n)))))
  (meow-expand-n meow-expand-1 1)
  (meow-expand-n meow-expand-2 2)
  (meow-expand-n meow-expand-3 3)
  (meow-expand-n meow-expand-4 4)
  (meow-expand-n meow-expand-5 5)
  (meow-expand-n meow-expand-6 6)
  (meow-expand-n meow-expand-7 7)
  (meow-expand-n meow-expand-8 8)
  (meow-expand-n meow-expand-9 9)
  (meow-expand-n meow-expand-0 0)

  ;; Copied from https://github.com/meow-edit/meow/issues/506
  ;; -------------------- ;;
  ;;         UTILS        ;;
  ;; -------------------- ;;
  (defun meow-kill-line ()
    "Kill till the end of line."
    (interactive)
    (let ((select-enable-clipboard meow-use-clipboard))
      (kill-line)))

  (defun meow-change-line ()
    "Kill till end of line and switch to INSERT state."
    (interactive)
    (meow--cancel-selection)
    (meow-end-of-thing
     (car (rassoc 'line meow-char-thing-table)))
    (meow-change))

  (defun meow-save-clipboard ()
    "Copy in clipboard."
    (interactive)
    (let ((meow-use-clipboard t))
      (meow-save)))

  (defun meow-smart-reverse ()
    "Reverse selection or begin negative argument."
    (interactive)
    (if (use-region-p)
        (meow-reverse)
      (negative-argument nil)))

  (add-to-list
   'meow-selection-command-fallback
   '(meow-kill . meow-delete))
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("SPC" . nil)
   '("S-SPC" . meow-keypad)
   '("<escape>" . meow-cancel-selection))
  (meow-define-keys 'normal
    ;; expansion
    '("0" . meow-expand-0)
    '("1" . meow-expand-1)
    '("2" . meow-expand-2)
    '("3" . meow-expand-3)
    '("4" . meow-expand-4)
    '("5" . meow-expand-5)
    '("6" . meow-expand-6)
    '("7" . meow-expand-7)
    '("8" . meow-expand-8)
    '("9" . meow-expand-9)
    '("-" . meow-smart-reverse)

    ;; movement
    '("k" . meow-prev)
    '("j" . meow-next)
    '("h" . meow-left)
    '("l" . meow-right)

    '("n" . meow-search)
    '("/" . meow-visit)

    '("<backspace>" . meow-page-up)
    '("RET" . meow-page-down)

    ;; expansion
    '("K" . meow-prev-expand)
    '("J" . meow-next-expand)
    '("H" . meow-left-expand)
    '("L" . meow-right-expand)

    '("i" . meow-back-word)
    '("I" . meow-back-symbol)
    '("o" . meow-next-word)
    '("O" . meow-next-symbol)

    '("a" . meow-mark-word)
    '("A" . meow-mark-symbol)
    '("w" . meow-line)
    '("W" . meow-block)
    '("q" . meow-join)
    '("y" . meow-grab)
    '("Y" . meow-pop-grab)
    '("tw" . meow-swap-grab)
    '("ts" . meow-sync-grab)
    '("p" . meow-cancel-selection)
    '("P" . meow-pop-selection)
    '("." . repeat-fu-execute)

    '("f" . meow-till)
    '("F" . meow-find)

    '("m" . meow-beginning-of-thing)
    '("," . meow-end-of-thing)
    '("M" . meow-inner-of-thing)
    '("<" . meow-bounds-of-thing)

    ;; editing
    '("d" . meow-kill)
    '("D" . meow-kill-line)
    '("s" . meow-change)
    '("S" . meow-change-line)
    '("c" . meow-save)
    '("C" . meow-save-clipboard)
    '("v" . meow-yank)
    '("V" . meow-yank-pop)

    '("e" . meow-insert)
    '("E" . meow-open-above)
    '("r" . meow-append)
    '("R" . meow-open-below)

    '("u" . undo-only)
    '("U" . undo-redo)

    '("b" . open-line)
    '("B" . split-line)

    '("[" . indent-rigidly-left-to-tab-stop)
    '("]" . indent-rigidly-right-to-tab-stop)

    ;; prefix ;
    '(";f" . save-buffer)
    '(";F" . save-some-buffers)
    '(";d" . meow-query-replace-regexp)
    '(";c" . meow-comment)
    '(";t" . meow-start-kmacro-or-insert-counter)
    '(";r" . meow-start-kmacro)
    '(";e" . meow-end-or-call-kmacro)
    '(";g" . magit)
    '(";q" . meow-quit)
    '(";Q" . kill-emacs)

    ;; prefix g
    '("gl" . meow-goto-line)

    ;; ignore escape
    '("<escape>" . meow-cancel-selection))
  (meow-global-mode))

;; Clipboard.
(setq save-interprogram-paste-before-kill t)

;; Mouse.
(setq mouse-drag-and-drop-region-cross-program t)

;; Scrolling.
(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
  ;; versions, except for emacs-mac.
  ;;
  ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
  ;; this version of Emacs natively supports smooth scrolling.
  ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
  (setq pixel-scroll-precision-use-momentum nil) ; Precise/smoother scrolling
  (pixel-scroll-precision-mode 1))

;; Completion.

;; Corfu enhances in-buffer completion by displaying a compact popup with
;; current candidates, positioned either below or above the point. Candidates
;; can be selected by navigating up or down.
(use-package corfu
  :commands (corfu-mode global-corfu-mode)

  :hook (after-init . global-corfu-mode)

  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary 'separator)   ;; Never quit at completion boundary
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  ;; (corfu-auto-trigger ".")         ;; Custom trigger characters)
  (corfu-quit-no-match 'separator) ;; or t)
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match 'insert) ;; Configure handling of exact matches
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  :config
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; Cape, or Completion At Point Extensions, extends the capabilities of
;; in-buffer completion. It integrates with Corfu or the default completion UI,
;; by providing additional backends through completion-at-point-functions.
(use-package cape
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Command prompts.

;; Vertico provides a vertical completion interface, making it easier to
;; navigate and select from completion candidates (e.g., when `M-x` is pressed).
(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :hook (after-init . vertico-mode))

;; Vertico leverages Orderless' flexible matching capabilities, allowing users
;; to input multiple patterns separated by spaces, which Orderless then
;; matches in any order against the candidates.
(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
;; In addition to that, Marginalia also enhances Vertico by adding rich
;; annotations to the completion candidates displayed in Vertico's interface.
(use-package marginalia
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

;; Embark integrates with Consult and Vertico to provide context-sensitive
;; actions and quick access to commands based on the current selection, further
;; improving user efficiency and workflow within Emacs. Together, they create a
;; cohesive and powerful environment for managing completions and interactions.
(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Consult offers a suite of commands for efficient searching, previewing, and
;; interacting with buffers, file contents, and more, improving various tasks.
(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Aggressive asynchronous that yield instantaneous results. (suitable for
  ;; high-performance systems.) Note: Minad, the author of Consult, does not
  ;; recommend aggressive values.
  ;; Read: https://github.com/minad/consult/discussions/951
  ;;
  ;; However, the author of minimal-emacs.d uses these parameters to achieve
  ;; immediate feedback from Consult.
  ;; (setq consult-async-input-debounce 0.02
  ;;       consult-async-input-throttle 0.05
  ;;       consult-async-refresh-delay 0.02)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

;; History & stuffs.

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(use-package autorevert
  :straight (:type built-in)
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling t)
  (auto-revert-verbose t))

;; Recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(use-package recentf
  :straight (:type built-in)
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)

  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))

  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
  :straight (:type built-in)
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring)))

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(use-package saveplace
  :straight (:type built-in)
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom  (save-place-limit 400))

;; Enable `auto-save-mode' to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)
(setq auto-save-interval 300)
(setq auto-save-timeout 30)

;; Dired.
(use-package dired
  :straight (:type built-in)
  :custom
  (dired-mouse-drag-files t)
  (dired-free-space nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group"))

(use-package image-dired
  :straight (:type built-in)
  :custom (image-dired-thumbnail-storage 'standard))

(use-package dirvish
  :commands (dirvish dirvish-dwim dirvish-dispatch dirvish-override-dired-mode)
  :bind
  (("C-c f" . dirvish-dwim)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu))
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("a" "~/work/ponyai/.sub-repos"    "ponyai")
     ("b" "~/work/ponyai1/.sub-repos"   "ponyai1")
     ("c" "~/work/ponyai2/.sub-repos"   "ponyai2")))
  :init
  (with-eval-after-load 'dired
    (dirvish-override-dired-mode)))

(use-package ibuffer  ;; builtin
  :straight (:type built-in)
  :bind
  (("C-x C-b" . ibuffer))
  :custom
  (ibuffer-default-sorting-mode 'filename/process))

;; Visual cues.
(add-hook
 'prog-mode-hook
 (lambda ()
   (display-line-numbers-mode)
   (setq show-trailing-whitespace t)))

;; (use-package whitespace
;;   :straight (:type built-in)
;;   :hook (prog-mode . whitespace-mode)
;;   :blackout whitespace-mode)

(use-package hl-line
  :straight (:type built-in)
  :blackout hi-line-mode
  :hook
  (prog-mode . hl-line-mode)
  (tabulated-list-mode . hl-line-mode))

;; Version control.
(use-package magit
  :bind
  (:map magit-status-mode-map
        ("p" . magit-push)
        ("n" . magit-status-jump)
        ("x" . magit-discard))
  :commands (magit))

(use-package diff-hl
  :hook
  (after-init . global-diff-hl-mode)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :config
  (diff-hl-flydiff-mode))

;; More speed optimizations.

;; Copied from
;; https://github.com/jamescherti/minimal-emacs.d?tab=readme-ov-file#optimization-native-compilation.
;; Native compilation enhances Emacs performance by converting Elisp code into
;; native machine code, resulting in faster execution and improved
;; responsiveness.
;;
;; Ensure adding the following compile-angel code at the very beginning
;; of your `~/.emacs.d/post-init.el` file, before all other packages.
(use-package compile-angel
  :blackout t
  :demand t
  :custom
  ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
  ;; Drawback: The minibuffer will not display compile-angel's actions.
  (compile-angel-verbose t)

  :config
  ;; See above ~use-package repeat-fu~ for explanation.
  (push "/repeat-fu-preset-meow.el" compile-angel-excluded-files)
  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; the `use-package' macro, you'll need to explicitly add:
  ;; (eval-when-compile (require 'use-package))
  ;; at the top of your init file.
  ;; (push "/init.el" compile-angel-excluded-files)
  ;; (push "/early-init.el" compile-angel-excluded-files)
  ;; (push "/pre-init.el" compile-angel-excluded-files)
  ;; (push "/post-init.el" compile-angel-excluded-files)
  ;; (push "/pre-early-init.el" compile-angel-excluded-files)
  ;; (push "/post-early-init.el" compile-angel-excluded-files)

  ;; A local mode that compiles .el files whenever the user saves them.
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files prior to loading them via `load' or
  ;; `require'. Additionally, it compiles all packages that were loaded before
  ;; the mode `compile-angel-on-load-mode' was activated.
  ;; Disable this in debug mode because it may trigger loading of code not defined
  ;; during normal compilation.
  (when (not minimal-emacs-debug)
    (compile-angel-on-load-mode 1)))

;; Theme.
(load-theme 'modus-operandi t)
(setq modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui t
      modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-completions '((t . (bold)))
      modus-themes-prompts '(bold)
      modus-themes-headings
      '((agenda-structure . (variable-pitch light 2.2))
        (agenda-date . (variable-pitch regular 1.3))
        (t . (regular 1.15))))
(setq modus-themes-common-palette-overrides nil)

(load custom-file 'noerror 'no-message)
