;;; universal-search --- Universal search across local files, Google Drive

;; Copyright (C) 2025 Seijiro Ikehata
;; Author: Your Name <modeverv@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.4") (helm "3.0") (projectile "2.0"))
;; Keywords: search, convenience
;; URL: https://github.com/modeverv/universal-search

;;; Commentary:
;; This package provides unified search across local files (using ripgrep)
;; and Google Drive.
;; now not and GitHub repositories.

;;; Code:

(require 'helm)
(require 'projectile)
(require 'json)

(defgroup universal-search nil
  "Search across local files, Google Drive and GitHub."
  :group 'tools
  :prefix "universal-search-")

(defcustom universal-search-python-path
  (expand-file-name "python" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the directory containing Python scripts."
  :type 'string
  :group 'universal-search)

(defcustom universal-search-exclude-patterns
  '("private" "archive" "999_old" "flycheck_" "flymake_" ".git" "node_modules")
  "Patterns to exclude from local search."
  :type '(repeat string)
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
                  (let ((json-array-type 'list)
                        (json-object-type 'hash-table)
                        (json-key-type 'string))
                    (json-read-from-string output))
                (error
                 (message "Failed to parse Google Drive search results")
                 nil)))))
      (message "Google Drive search script not found at %s" script-path)
      nil)))

(defun universal-search-debug-gdrive ()
  "Debug Google Drive search results."
  (interactive)
  (let* ((keyword (read-string "Search keyword: "))
         (script-path (expand-file-name "gdrive_search.py" universal-search-python-path))
         (temp-buffer (generate-new-buffer "*gdrive-debug*")))
    (call-process "python" nil temp-buffer nil
                  script-path keyword)
    (switch-to-buffer temp-buffer)
    (json-pretty-print-buffer)))

;; ;;; GitHub Search
(defun universal-search-github (keyword)
  "Search GitHub for KEYWORD."
  (let ((script-path (expand-file-name "github_search.py" universal-search-python-path))
        (process-environment (append process-environment '("PYTHONIOENCODING=utf-8"))))
    (if (file-exists-p script-path)
        (let ((temp-buffer (generate-new-buffer "*github-output*")))
          (call-process "python" nil (list temp-buffer nil) nil
                        script-path keyword)
          (with-current-buffer temp-buffer
            (let ((output (buffer-string)))
              (kill-buffer)
              (condition-case nil
                  (let ((json-array-type 'list)
                        (json-object-type 'hash-table)
                        (json-key-type 'string))
                    (json-read-from-string output))
                (error
                 (message nil)
                 nil)))))
      (message nil)
      nil)))

;;; Local Search
(defun universal-search-local (keyword)
  "Search local files for KEYWORD using ripgrep."
  (when (projectile-project-p)
    (let* ((default-directory (projectile-project-root))
           (exclude-args (mapconcat (lambda (pattern)
                                      (format "-g '!%s'" pattern))
                                    universal-search-exclude-patterns
                                    " "))
           (cmd (format "rg --line-number --no-heading --color=never --smart-case %s %s"
                        (shell-quote-argument keyword)
                        exclude-args))
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

;;; Google Drive Search Results Processing
(defun universal-search-gdrive-process-results (results)
  "Process Google Drive search RESULTS into a unified format."
  (let ((candidates '()))
    (if (listp results)
        (dolist (item results)
          (when (hash-table-p item)
            (let ((name (gethash "name" item))
                  (link (gethash "webViewLink" item))
                  (id (gethash "id" item)))
              (when (and name link id)
                (push (list :type 'gdrive
                            :display (format "%s" name)
                            :link link
                            :id id
                            :name name)
                      candidates)))))
      (message "Google Drive results not a list: %s" (type-of results)))
    candidates))

;;; GitHub Search Results Processing
(defun universal-search-github-process-results (results)
  "Process GitHub search RESULTS into a unified format."
  (let ((candidates '()))
    (dolist (item results)
      (let* ((type (gethash "type" item))
             (title (gethash "title" item))
             (html-url (gethash "html_url" item))
             (display (format "%s: %s" type title)))
        (push (list :type (intern (format "github-%s" type))
                    :display display
                    :html_url html-url
                    :title title)
              candidates)))
    candidates))

;;; Process Results Function
(defun universal-search-process-results ()
  "Process all search results into a unified format."
  (let ((keyword (read-string "Search query: " (thing-at-point 'symbol)))
        (all-results '()))

    ;; Local search
    (message "Searching local files...")
    (let ((local-results (universal-search-local keyword)))
      (dolist (item local-results)
        (push item all-results)))

    ;; Google Drive search
    (message "Searching Google Drive...")
    (let ((gdrive-results (universal-search-gdrive keyword)))
      (when gdrive-results
        (let ((processed-results (universal-search-gdrive-process-results gdrive-results)))
          (dolist (item processed-results)
            (push item all-results)))))

    ;;    ;; GitHub search
    ;;    (message "Searching GitHub...")
    ;;    (let ((github-results (universal-search-github keyword)))
    ;;      (when github-results
    ;;        (let ((processed-results (universal-search-github-process-results github-results)))
    ;;          (dolist (item processed-results)
    ;;            (push item all-results)))))

    (cons keyword all-results)))

;;;###autoload
(defun universal-search ()
  "Search across local files, Google Drive and GitHub."
  (interactive)
  (let* ((results (universal-search-process-results))
         (keyword (car results))
         (candidates (cdr results)))

    (helm :sources (helm-build-sync-source (format "Universal Search: %s" keyword)
                     :candidates candidates
                     :candidate-transformer
                     (lambda (candidates)
                       (mapcar (lambda (candidate)
                                 (let* ((type (plist-get candidate :type))
                                        (icon (cond
                                               ;; todo icon
                                               ((eq type 'gdrive) "") ; Google Drive icon
                                               ((eq type 'local) "")  ; File icon
                                               ((eq type 'github-code) "") ; Code icon
                                               ((eq type 'github-issue) "") ; Issue icon
                                               ((eq type 'github-pr) "")))) ; PR icon
                                   (cons (concat icon " " (plist-get candidate :display)) candidate)))
                               candidates))
                     :action '(("Open" . (lambda (candidate)
                                           (let ((type (plist-get candidate :type)))
                                             (cond
                                              ((eq type 'gdrive)
                                               (browse-url (plist-get candidate :link)))
                                              ((eq type 'local)
                                               (find-file (plist-get candidate :file))
                                               (goto-char (point-min))
                                               (forward-line (1- (plist-get candidate :linum))))
                                              ((memq type '(github-code github-issue github-pr))
                                               (browse-url (plist-get candidate :html_url)))))))
                               ("Copy link/path" . (lambda (candidate)
                                                     (let* ((type (plist-get candidate :type))
                                                            (text (cond
                                                                   ((eq type 'gdrive)
                                                                    (plist-get candidate :link))
                                                                   ((eq type 'local)
                                                                    (expand-file-name (plist-get candidate :file)))
                                                                   ((memq type '(github-code github-issue github-pr))
                                                                    (plist-get candidate :html_url)))))
                                                       (kill-new text)
                                                       (message "Copied to clipboard: %s" text)))))
                     :persistent-action
                     (lambda (candidate)
                       (let* ((type (plist-get candidate :type))
                              (text (cond
                                     ((eq type 'gdrive)
                                      (plist-get candidate :link))
                                     ((eq type 'local)
                                      (expand-file-name (plist-get candidate :file)))
                                     ((memq type '(github-code github-issue github-pr))
                                      (plist-get candidate :html_url)))))
                         (kill-new text)
                         (message "Copied to clipboard: %s" text)))
                     :persistent-help "Copy link/path to clipboard")
          :buffer "*helm universal search*")))

(provide 'universal-search)
;;; universal-search ends here
