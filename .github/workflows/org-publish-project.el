;; Shameless copy from
;; https://gitlab.com/fstamour/public-notes/-/blob/main/scripts/org-publish-project.el?ref_type=heads#L10

(require 'cl-lib)

(message "%s" (emacs-version))
(message "Features: %s"
         (mapconcat (lambda (x) (format "\n - %s" x))
                    (sort (cl-copy-list features) 'string<)))

(message "Installing packages")

(progn
  (require 'package)
  (push '("melpa" . "https://melpa.org/packages/") package-archives)
  (package-initialize)
  (unless (seq-find (lambda (e) (string= "melpa" (package-desc-archive (cadr e)))) package-archive-contents)
    (package-refresh-contents)))

(package-install 'htmlize)
(package-install 'org-roam)

(message "Exporting")

(require 'org)
(require 'org-id)
(require 'org-element)
(require 'org-roam)
(require 'htmlize)

(add-hook 'org-export-before-processing-functions
          (lambda (backend)
            (when-let ((note (org-roam-node-at-point))
                       (backlinks (org-roam-backlinks-get note :unique t)))
              (save-excursion
                (goto-char (point-max))
                (insert "\n* Backlinks\n\n")
                (dolist (backlink backlinks)
                  (let* ((source-node (org-roam-backlink-source-node backlink))
                         (point (org-roam-backlink-point backlink)))
                    (insert
                     (format "- [[id:%s][%s]] (%s)\n"
                             (org-roam-node-id source-node)
                             (org-roam-node-title source-node)
                             (file-relative-name (org-roam-node-file source-node))))))))
            (message "%s" (buffer-substring-no-properties (point-min) (point-max)))))

(setf org-roam-directory "."
      org-roam-db-location "./org-roam.publish.db")

(org-roam-db-sync)
(org-id-update-id-locations (directory-files-recursively "." "\\.org$"))

;; Trying to speed things up
(defun org-publish-ignore-mode-hooks (orig-func &rest args)
  (let ((lexical-binding nil))
    (cl-letf (((symbol-function #'run-mode-hooks) #'ignore))
      (apply orig-func args))))
(advice-add 'org-publish :around #'org-publish-ignore-mode-hooks)

;; See (describe-variable 'org-publish-project-alist)
;; See https://orgmode.org/manual/Publishing-options.html for more options
(setq org-publish-project-alist
      `(("orgfiles"
         :base-directory "./"
         :base-extension "org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :publishing-directory "./public"
         :author "Wenxin Wang"
         :creator "Wenxin Wang"
         :auto-sitemap t
         :time-stamp-file nil
         :with-toc nil
         :with-author nil
         :with-date nil
         :html-validation-link nil)
        ("other"
         :base-directory "./"
         :exclude ,(rx (or (seq line-start "public/")
                           (seq line-start "library/Dictionaries/")
                           (seq line-start "library/Zotero/")))
         ;; :base-extension "css\\|js\\|png\\|jpeg\\|jpg\\|gif\\|yaml\\|yml"
         :base-extension "css\\|html\\|js\\|yaml\\|yml"
         :recursive t
         :publishing-directory "./public"
         :publishing-function org-publish-attachment)
        ("website"
         :components ("orgfiles" "other"))))
(org-publish-project "website" t)
(copy-file "./public/README.html" "./public/index.html")
