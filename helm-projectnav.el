;;; helm-projectnav.el

;; Author: Bartłomiej Nankiewicz <bartlomiej.nankiewicz@gmail.com>
;; Url: http://github.com/bnankiewicz/helm-projectnav
;; Version: 0.2-pre
;; Package-Requires: ((emacs "24.4") (dash "2.12") (f "0.18.1") (helm "1.9.4") (s "1.10.0"))
;; Keywords: project, navigation

;;; Commentary:
;;; Code:

;;;; Require

(require 'helm)
(require 'org)

;;;; Vars

(defgroup helm-projectnav nil
  "Settings for `helm-projectnav'."
  :group 'helm
  :link '(url-link "http://github.com/bnankiewicz/helm-projectnav"))

(defcustom helm-projectnav-src-dir "src"
  "Source code directory relative to local-dirs.el"
  :group 'helm-projectnav
  :type 'string)

(defcustom helm-projectnav-src-suffix ""
  "Source file extension eg. .js"
  :group 'helm-projectnav
  :type 'string)

(defcustom helm-projectnav-test-dir ""
  "Doc"
  :group 'helm-projectnav
  :type 'string)

(defcustom helm-projectnav-test-suffix ""
  "Doc"
  :group 'helm-projectnav
  :type 'string)


(defcustom helm-projectnav-components-dirs ""
  "Doc"
  :group 'helm-projectnav
  :type 'string)

(defcustom helm-projectnav-alternatives-table ""
  "Doc"
  :group 'helm-projectnav
  :type 'string)


;;;; Functions
;;;;; Commands

;; ###autoload
(defun helm-projectnav-components ()
  "docstring"
  (interactive)
  (setq component-sources '())
  (loop for row in helm-projectnav-components-dirs do
        (let ((component-type (nth 0 row)))
          (add-to-list 'component-sources
                       `((name . ,(nth 0 row))
                         (candidates . ,(helm-projectnav-create-components-list row))
                         (action . ,(apply-partially 'helm-projectnav-goto-component component-type)))
                       'append)))
  (helm :prompt "Project components: "
        :sources component-sources))

;; ###autoload
(defun helm-projectnav-create-test-for-function (beg end)
  "docstring"
  (interactive "r")
  (setq path (helm-projectnav-relative-buffer-file-name))
  (if (helm-projectnav-is-source-file path)
      (progn
        (setq test-file-path (helm-projectnav-create-test-file-path))
        (message "Go to test file: %s" test-file-path)
        (set-register ?1 (thing-at-point 'symbol))
        (find-file-other-window test-file-path)
        ;; (goto-char (point-max))               ; go to the end of the .emacs
        ;; TODO go to next blank space
        (newline)                             ; insert a newline
        (insert "auto-test")
        (yas/expand)
        (evil-insert-state 1))))

;; ###autoload
(defun helm-projectnav-alternate-between-test-and-implementation ()
  "docstring"
  (interactive)
  (setq path (helm-projectnav-relative-buffer-file-name))
  (if (helm-projectnav-is-source-file path)
      (progn
        (setq test-file-path (helm-projectnav-create-test-file-path))
        (message "Go to test file: %s" test-file-path)
        (find-file test-file-path)))

  (if (helm-projectnav-is-test-file path)
      (progn
        (setq src-file-path (helm-projectnav-create-src-file-path))
        (message "Go to src file: %s" src-file-path)
        (find-file src-file-path))))

;; ###autoload
(defun helm-projectnav-goto-alternative-file ()
  "doc"
  (interactive)
  (setq file-name (buffer-file-name))
  (setq alternatives (helm-projectnav-find-alternatives file-name))
  (setq alternatives-paths (helm-projectnav-create-alternatives-paths alternatives))
  (setq helm-existing-alternative-files-source
        `((name . "Go to alternative file")
          (candidates . ,(remove-if-not 'file-exists-p alternatives-paths))
          (action . find-file)))

  (setq helm-non-existing-alternative-files-source
        `((name . "Create alternative file")
          (candidates . ,(remove-if 'file-exists-p alternatives-paths))
          (action . find-file)))
  (helm
   :prompt "Project alternative files: "
   :sources '(helm-existing-alternative-files-source helm-non-existing-alternative-files-source)))

;; ###autoload
(defun helm-projectnav-find-alternatives (file-path)
  "docstring"
  (interactive "P")
  (message "alternatives table: %s" helm-projectnav-alternatives-table)
  (message "file path: %s" file-path)
  (let ((helm-projectnav-is-alternative-with (apply-partially 'helm-projectnav-is-alternative-with
                                                              (file-name-directory file-path)
                                                              (file-name-nondirectory file-path))))
    (remove-if-not helm-projectnav-is-alternative-with helm-projectnav-alternatives-table)))

;;;;; Path

(defun helm-projectnav-is-path-relative (path)
  (s-starts-with-p "." path))

(defun helm-projectnav-get-project-path ()
  (locate-dominating-file default-directory ".dir-locals.el"))

;;;;; Alternatives

(defun helm-projectnav-is-alternative-with (file-directory file-name alternate-table-row)
  (and (string-suffix-p (nth 1 alternate-table-row) file-name)
       (s-contains-p (nth 4 alternate-table-row) file-directory)))

(defun helm-projectnav-get-alternative-directory (path)
  "docstring"
  (file-name-as-directory
   (if (helm-projectnav-is-path-relative path)
       (eval path)
     (concat (helm-projectnav-get-project-path) path))))

(defun helm-projectnav-create-alternatives-paths (helm-projectnav-alternatives-table)
  "docstring"
  (let ((project-path (helm-projectnav-get-project-path))
        (filename (file-name-nondirectory file-name) ))
    (mapcar
     (lambda (row) (concat
               (helm-projectnav-get-alternative-directory (nth 2 row))
               (replace-regexp-in-string (nth 1 row) (nth 3 row) filename)))
     helm-projectnav-alternatives-table)))

(defun helm-projectnav-relative-buffer-file-name ()
  "docstring"
  (concat "~/" (file-relative-name (buffer-file-name) (expand-file-name "~"))))

;;;;; Tests/Source

(defun helm-projectnav-test-path ()
  "docstring"
  (concat (helm-projectnav-get-project-path)
          (file-name-as-directory helm-projectnav-test-dir)))

(defun helm-projectnav-src-path ()
  "docstring"
  (concat (helm-projectnav-get-project-path)
          (file-name-as-directory helm-projectnav-src-dir)))

(defun helm-projectnav-is-source-file (path)
  "docstring"
  (and (s-ends-with-p helm-projectnav-src-suffix path)
       (s-starts-with-p (helm-projectnav-src-path) path)))

(defun helm-projectnav-is-test-file (path)
  "docstring"
  (and (s-ends-with-p helm-projectnav-test-suffix path)
       (s-starts-with-p (helm-projectnav-test-path) path)))

(defun helm-projectnav-create-test-file-path ()
  "docstring"
  (concat (helm-projectnav-test-path)
          (s-chop-suffix helm-projectnav-src-suffix (s-chop-prefix (helm-projectnav-src-path) (helm-projectnav-relative-buffer-file-name)))
          helm-projectnav-test-suffix))

(defun helm-projectnav-create-src-file-path ()
  "docstring"
  (concat (helm-projectnav-src-path)
          (s-chop-suffix helm-projectnav-test-suffix (s-chop-prefix (helm-projectnav-test-path) (helm-projectnav-relative-buffer-file-name)))
          helm-projectnav-src-suffix))

;;;;; Components

(defun helm-projectnav-create-components-list (args)
  "docstring"
  (setq pp (helm-projectnav-get-project-path))
  (setq component-files (cdr(cdr (directory-files (concat pp (nth 1 args))))))
  (setq excluded-suffixes (nth 2 args))
  (setq strip-suffix (nth 3 args))
  (loop for suffix in excluded-suffixes do
        (setq component-files (remove-if (lambda (file)(s-ends-with-p suffix file)) component-files)))
  (mapcar (lambda (file)(s-chop-suffix strip-suffix file)) component-files))

(defun helm-projectnav-goto-component (component-type component-name)
  "docstring"
  (setq component-args
        (remove-if-not
         (lambda (row) (equal (nth 0 row) component-type))
         helm-projectnav-components-dirs))
  (setq component-args (nth 0 component-args))
  (find-file (concat (helm-projectnav-get-project-path)
                     (file-name-as-directory (nth 1 component-args) )
                     component-name
                     (nth 3 component-args))))

(provide 'helm-projectnav)

;;; helm-projectnav.el ends here
