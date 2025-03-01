;;; universal-search.el --- Universal search across local files, Google Drive and GitHub

;; Copyright (C) 2025 Seijiro Ikehata
;; Author: Your Name <modeverv@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.4") (helm "3.0") (projectile "2.0"))
;; Keywords: search, convenience
;; URL: https://github.com/modeverv/universal-search

;;; Commentary:
;; This package provides unified search across local files (using ripgrep),
;; Google Drive documents, and GitHub repositories.

;;; Code:

(require 'helm)
(require 'projectile)
(require 'json)

(defgroup universal-search nil
  "Search across local files, Google Drive and GitHub."
  :group 'tools
  :prefix "universal-search-")

(defcustom universal-search-python-path (expand-file-name "python" (file-name-directory load-file-name))
  "Path to the directory containing Python scripts."
  :type 'string
  :group 'universal-search)

;;; Google Drive Search
(defun universal-search-gdrive (keyword)
  "Search Google Drive for KEYWORD."
  (let ((script-path (expand-file-name "gdrive_search.py" universal-search-python-path)))
    (if (file-exists-p script-path)
        (let ((temp-buffer (generate-new-buffer "*gdrive-output*")))
          (call-process "python" nil temp-buffer nil
                        script-path keyword)
          (with-current-buffer temp-buffer
            (let ((output (buffer-string)))
              (kill-buffer)
              (condition-case nil
                  (json-read-from-string output)
                (error
                 (message "Failed to parse Google Drive search results")
                 nil)))))
      (message "Google Drive search script not found at %s" script-path)
      nil)))

;;; GitHub Search
(defun universal-search-github (keyword)
  "Search GitHub for KEYWORD."
  (let ((script-path (expand-file-name "github_search.py" universal-search-python-path)))
    (if (file-exists-p script-path)
        (let ((temp-buffer (generate-new-buffer "*github-output*")))
          (call-process "python" nil temp-buffer nil
                        script-path keyword)
          (with-current-buffer temp-buffer
            (let ((output (buffer-string)))
              (kill-buffer)
              (condition-case nil
                  (json-read-from-string output)
                (error
                 (message "Failed to parse GitHub search results")
                 nil)))))
      (message "GitHub search script not found at %s" script-path)
      nil)))

;;; Local Search
(defun universal-search-local (keyword)
  "Search local files for KEYWORD using ripgrep."
  (when (projectile-project-p)
    (let* ((default-directory (projectile-project-root))
           (cmd (format "rg --line-number --no-heading --color=never --smart-case %s %s"
                        (shell-quote-argument keyword)
                        (mapconcat (lambda (pattern)
                                     (format "-g '!%s'" pattern))
                                   '("private" "archive" "999_old" "flycheck_" "flymake_" ".git" "node_modules")
                                   " ")))
           (result (shell-command-to-string cmd))
           (results '()))
      (dolist (line (split-string result "\n" t))
        (when (string-match "\\`\\([^:]+\\):\\([0-9]+\\):\\(.*\\)" line)
          (let ((file (match-string 1 line))
                (linum (match-string 2 line))
                (content (match-string 3 line)))
            (push (list :type 'local
                        :display (format "%s:%s: %s" file linum content)
                        :file file
                        :linum (string-to-number linum))
                  results))))
      results)))

;;; Format candidates for Helm
(defun universal-search-format-candidates (results)
  "Format RESULTS for Helm display."
  (let ((candidates '()))
    ;; Format Google Drive results
    (dolist (item (cdr (assq 'gdrive results)))
      (let* ((name (cdr (assq 'name item)))
             (link (cdr (assq 'webViewLink item)))
             (id (cdr (assq 'id item)))
             (display (format "%s" name)))
        (push (cons display (list :type 'gdrive
                                  :link link
                                  :id id
                                  :name name))
              candidates)))

    ;; Format GitHub results
    ;;    (dolist (item (cdr (assq 'github results)))
    ;;      (let* ((type (cdr (assq 'type item)))
    ;;             (title (cdr (assq 'title item)))
    ;;             (url (cdr (assq 'html_url item)))
    ;;             (display (format "%s: %s" type title)))
    ;;        (push (cons display (list :type (intern (format "github-%s" type))
    ;;                                  :html_url url
    ;;                                  :title title))
    ;;              candidates)))

    ;; Format local results
    (dolist (item (cdr (assq 'local results)))
      (push (cons (plist-get item :display)
                  (list :type 'local
                        :file (plist-get item :file)
                        :linum (plist-get item :linum)))
            candidates))

    candidates))

;;;###autoload
(defun universal-search ()
  "Search across local files, Google Drive and GitHub."
  (interactive)
  (let* ((keyword (read-string "Search query: " (thing-at-point 'symbol)))
         ;; Search in parallel would be ideal but for simplicity, we'll do sequential
         (local-results (universal-search-local keyword))
         (gdrive-results (universal-search-gdrive keyword))
         ;;         (github-results (universal-search-github keyword))
         (all-results (list (cons 'local local-results)
                            (cons 'gdrive gdrive-results)
                            ;;                            (cons 'github github-results)
                            ))
         (candidates (universal-search-format-candidates all-results)))

    (helm :sources (helm-build-sync-source "Universal Search Results"
                     :candidates candidates
                     :candidate-transformer
                     (lambda (candidates)
                       (mapcar (lambda (candidate)
                                 (let* ((type (plist-get (cdr candidate) :type))
                                        (icon (cond
                                               ((eq type 'gdrive) " ") ; Google Drive icon
                                               ((eq type 'local) " ")  ; File icon
                                               ((eq type 'github-code) " ") ; Code icon
                                               ((eq type 'github-issue) " ") ; Issue icon
                                               ((eq type 'github-pr) " ")))) ; PR icon
                                   (cons (concat icon " " (car candidate)) (cdr candidate))))
                               candidates))
                     :action '(("Open" . (lambda (candidate)
                                           (let ((type (plist-get (cdr candidate) :type)))
                                             (cond
                                              ((eq type 'gdrive)
                                               (browse-url (plist-get (cdr candidate) :link)))
                                              ((eq type 'local)
                                               (find-file (plist-get (cdr candidate) :file))
                                               (goto-char (point-min))
                                               (forward-line (1- (plist-get (cdr candidate) :linum))))
                                              ((memq type '(github-code github-issue github-pr))
                                               (browse-url (plist-get (cdr candidate) :html_url)))))))
                               ("Copy link/path" . (lambda (candidate)
                                                     (let* ((type (plist-get (cdr candidate) :type))
                                                            (text (cond
                                                                   ((eq type 'gdrive)
                                                                    (plist-get (cdr candidate) :link))
                                                                   ((eq type 'local)
                                                                    (expand-file-name (plist-get (cdr candidate) :file)))
                                                                   ((memq type '(github-code github-issue github-pr))
                                                                    (plist-get (cdr candidate) :html_url)))))
                                                       (kill-new text)
                                                       (message "Copied to clipboard: %s" text)))))
                     :persistent-action
                     (lambda (candidate)
                       (let* ((type (plist-get (cdr candidate) :type))
                              (text (cond
                                     ((eq type 'gdrive)
                                      (plist-get (cdr candidate) :link))
                                     ((eq type 'local)
                                      (expand-file-name (plist-get (cdr candidate) :file)))
                                     ((memq type '(github-code github-issue github-pr))
                                      (plist-get (cdr candidate) :html_url)))))
                         (kill-new text)
                         (message "Copied to clipboard: %s" text)))
                     :persistent-help "Copy link/path to clipboard")
          :buffer "*helm universal search*")))

(provide 'universal-search)
;;; universal-search.el ends here
