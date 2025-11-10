;;; -*- lexical-binding: t; -*-

;; This file loads before any startup optimizations are applied, so do as little
;; as possible.

(setq minimal-emacs-ui-features '(tooltips))
(setq minimal-emacs-package-initialize-and-refresh nil)
(setq minimal-emacs-load-compiled-init-files t)
