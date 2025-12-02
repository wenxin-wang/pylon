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
    '("gl" . meow-goto-line)
    '("gs" . magit)
    '("q" . meow-join)
    '("t" . meow-grab)
    '("Tp" . meow-pop-grab)
    '("Tw" . meow-swap-grab)
    '("Ts" . meow-sync-grab)
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

    ;; prefix y
    '("yf" . meow-comment)
    '("yt" . meow-start-kmacro-or-insert-counter)
    '("yr" . meow-start-kmacro)
    '("ye" . meow-end-or-call-kmacro)
    ;; ...etc

    ;; prefix ;
    '(";f" . save-buffer)
    '(";F" . save-some-buffers)
    '(";d" . meow-query-replace-regexp)
    ;; ... etc
    ;; prefix z
    '("zs" . save-buffer)
    '("zS" . save-some-buffers)

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

;; Visual cues.
(setq show-trailing-whitespace t)
(add-hook 'prog-mode 'display-line-numbers-mode)
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
