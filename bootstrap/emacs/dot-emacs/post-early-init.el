;;; -*- lexical-binding: t; -*-

;; Straight package pananger.
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
  (load bootstrap-file nil 'nomessage))

;; Byte-compilation requirements.
(eval-when-compile
  (setq
   use-package-always-defer t)
  (require 'use-package))
(when (bound-and-true-p use-package-compute-statistics)
  (require 'use-package))

;; Setup no-littering as early as possible.
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :demand t
  :config
  (setq custom-file (expand-file-name "custom.el" no-littering-etc-directory)))
