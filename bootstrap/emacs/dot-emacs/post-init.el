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
  :hook ((emacs-startup . which-key-mode))

  :config
  ;; We configure it so that `which-key' is triggered by typing C-h
  ;; during a key sequence (the usual way to show bindings). See
  ;; <https://github.com/justbur/emacs-which-key#manual-activation>.
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay most-positive-fixnum)
  (setq which-key-idle-secondary-delay 0.05))

;; Clipboard.
(setq save-interprogram-paste-before-kill t)

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
  :demand t
  :custom
  ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
  ;; Drawback: The minibuffer will not display compile-angel's actions.
  (compile-angel-verbose t)

  :config
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
