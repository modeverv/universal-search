;;; universal-search.el --- Universal search across local files, Google Drive, dictionaries and Spotlight

;; Copyright (C) 2025 Seijiro Ikehata
;; Author: Seijiro Ikehata <modeverv@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.4") (helm "3.0") (projectile "2.0"))
;; Keywords: search, convenience
;; URL: https://github.com/modeverv/universal-search

;;; Commentary:
;; This package provides unified search across:
;; - Local files (using ripgrep)
;; - Google Drive documents
;; - GitHub repositories (optional)
;; - Dictionaries (via lookup.el)
;; - macOS Spotlight (via mdfind)

;;; Code:

(require 'helm)
(require 'projectile)
(require 'json)
(require 'lookup nil t) ;; Optional dependency

(defgroup universal-search nil
  "Search across local files, Google Drive, GitHub, dictionaries, and Spotlight."
  :group 'tools
  :prefix "universal-search-")

(defcustom universal-search-python-path
  (expand-file-name "python" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the directory containing Python scripts."
  :type 'string
  :group 'universal-search)

(defun universal-search-get-python-bin ()
  "Get the appropriate Python binary for the current environment."
  (let ((candidates '("python3"
                      "~/globalenv/bin/python3"
                      "~/.pyenv/shims/python3"
                      "~/anaconda3/bin/python3"
                      "python")))
    (cl-loop for candidate in candidates
             when (executable-find (expand-file-name candidate))
             return (expand-file-name candidate)
             finally return (expand-file-name "~/globalenv/bin/python3"))))

(defcustom universal-search-python-bin
  (universal-search-get-python-bin)
  "Path to Python interpreter to use for executing scripts."
  :type 'string
  :group 'universal-search)

(defcustom universal-search-exclude-patterns
  '("private" "archive" "999_old" "flycheck_" "flymake_" ".git" "node_modules" "996_misc")
  "Patterns to exclude from local search."
  :type '(repeat string)
  :group 'universal-search)

(defcustom universal-search-spotlight-search-path
  "~/"
  "Patterns to exclude from local search."
  :type '(repeat string)
  :group 'universal-search)

(defcustom universal-search-enable-gdrive t
  "Whether to enable Google search."
  :type 'boolean
  :group 'universal-search)

(defcustom universal-search-enable-github t
  "Whether to enable GitHub search."
  :type 'boolean
  :group 'universal-search)

(defcustom universal-search-enable-local t
  "Whether to enable GitHub search."
  :type 'boolean
  :group 'universal-search)

(defcustom universal-search-enable-lookup t
  "Whether to enable dictionary lookup via lookup.el."
  :type 'boolean
  :group 'universal-search)

(defcustom universal-search-enable-spotlight (eq system-type 'darwin)
  "Whether to enable macOS Spotlight search via mdfind."
  :type 'boolean
  :group 'universal-search)

(defcustom universal-search-helm-height 20
  "Height of the Helm window for universal search."
  :type 'integer
  :group 'universal-search)

(defcustom universal-search-max-results-local 30
  "Maximum number of results to display per search source."
  :type 'integer
  :group 'universal-search)

(defcustom universal-search-max-results-gdrive 30
  "Maximum number of results to display per search source."
  :type 'integer
  :group 'universal-search)

(defcustom universal-search-max-results-github 30
  "Maximum number of results to display per search source."
  :type 'integer
  :group 'universal-search)

(defcustom universal-search-max-results-lookup 5
  "Maximum number of results to display per search source."
  :type 'integer
  :group 'universal-search)

(defcustom universal-search-max-results-spotight 10
  "Maximum number of results to display per search source."
  :type 'integer
  :group 'universal-search)

;;; face
(defface universal-search-filename-face
  '((t :foreground "light blue" :weight bold))
  "Face for filenames in universal search results.")

(defface universal-search-linenum-face
  '((t :foreground "light green" :weight normal))
  "Face for line numbers in universal search results.")

(defface universal-search-content-face
  '((t :foreground "white" :weight normal))
  "Face for content in universal search results.")

(defface universal-search-local-face
  '((t :foreground "light blue" :weight bold))
  "Face for local file results in universal search.")

(defface universal-search-gdrive-face
  '((t :foreground "light green" :weight bold))
  "Face for Google Drive results in universal search.")

(defface universal-search-github-face
  '((t :foreground "purple" :weight bold))
  "Face for GitHub results in universal search.")

(defface universal-search-lookup-face
  '((t :foreground "orange" :weight bold))
  "Face for Dictionary lookup results in universal search.")

(defface universal-search-spotlight-face
  '((t :foreground "yellow" :weight bold))
  "Face for Spotlight results in universal search.")

(defface universal-search-icon-face
  '((t :foreground "hot pink" :weight bold))
  "Face for icons in universal search results.")

;;; Google Drive Search
(defun universal-search-gdrive (keyword)
  "Search Google Drive for KEYWORD."
  (let ((script-path (expand-file-name "gdrive_search.py" universal-search-python-path)))
    (if (file-exists-p script-path)
        (let ((temp-buffer (generate-new-buffer "*gdrive-output*")))
          (call-process universal-search-python-bin nil temp-buffer nil
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
    (call-process universal-search-python-bin nil temp-buffer nil
                  script-path keyword)
    (switch-to-buffer temp-buffer)
    (json-pretty-print-buffer)))

;;; GitHub Search
(defun universal-search-github (keyword)
  "Search GitHub for KEYWORD."
  (let ((script-path (expand-file-name "github_search.py" universal-search-python-path))
        (process-environment (append process-environment '("PYTHONIOENCODING=utf-8"))))
    (if (file-exists-p script-path)
        (let ((temp-buffer (generate-new-buffer "*github-output*")))
          (call-process universal-search-python-bin nil (list temp-buffer nil) nil
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



                        :display (format "%s:%s: %s"
                                         (propertize file 'face 'universal-search-filename-face)
                                         (propertize linum 'face 'universal-search-linenum-face)
                                         content
                                         )
                        :file file
                        :linum (string-to-number linum))
                  results))))
      results)))

(defun universal-search-lookup (keyword)
  "Search dictionaries for KEYWORD using lookup.el."
  (when universal-search-enable-lookup
    (let ((results '()))
      (condition-case err
          (progn
            ;; load lookup
            (require 'lookup nil t)
            ;; search
            (when (featurep 'lookup)
              (lookup)
              (let* ((pattern (lookup-parse-pattern keyword))
                     (query (lookup-make-query 'default keyword))
                     (entries nil))
                ;; without session
                (dolist (dictionary (lookup-module-dictionaries (lookup-default-module)))
                  (when (lookup-dictionary-selected-p dictionary)
                    (setq entries (append entries (lookup-vse-search-query dictionary query)))))
                ;; format result
                (dolist (entry entries)
                  (let* ((heading (lookup-entry-heading entry))
                         (dict (lookup-entry-dictionary entry))
                         (dict-title (if dict (lookup-dictionary-title dict) "Unknown")))
                    (push (list :type 'lookup
                                :display (format "%s [%s]"
                                                 (propertize heading 'face 'universal-search-lookup-face)
                                                 dict-title
                                                 )
                                :entry entry)
                          results))))))
        (error
         (message "Lookup error: %S" err)))
      results)))


;;; macOS Spotlight Search
(defun universal-search-spotlight (keyword)
  "Search macOS using Spotlight (mdfind) for KEYWORD."
  (when (and universal-search-enable-spotlight
             (eq system-type 'darwin)
             (executable-find "mdfind"))
    ;; todo onlyin use custom
    (let* ((cmd (format "mdfind -name '%s' -onlyin %s 2>/dev/null"
                        (shell-quote-argument keyword)
                        universal-search-spotlight-search-path
                        ))
           (result (shell-command-to-string cmd))
           (results '()))
      (dolist (line (split-string result "\n" t))
        (let ((filename (file-name-nondirectory line))
              (directory (file-name-directory line)))
          (push (list :type 'spotlight
                      :display (format "%s [%s]"
                                       (propertize filename 'face 'universal-search-spotlight-face)
                                       (if directory
                                           (abbreviate-file-name directory)
                                         ""))
                      :path line
                      :full-path line)
                results)))
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
                            :display (format "%s"
                                             (propertize name 'face 'universal-search-gdrive-face)
                                             )
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
             (display (format "%s: %s"
                              (propertize type 'face 'universal-search-github-face)
                              title
                              )))
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
    (when universal-search-enable-local
      (message "Searching local files...")
      (let ((local-results (universal-search-local keyword)))
        (dolist (item (seq-take local-results universal-search-max-results-local))
          (push item all-results)))
      )

    ;; Google Drive search
    (when universal-search-enable-gdrive
      (message "Searching Google Drive...")
      (let ((gdrive-results (universal-search-gdrive keyword)))
        (when gdrive-results
          (let ((processed-results (universal-search-gdrive-process-results gdrive-results)))
            (dolist (item (seq-take processed-results universal-search-max-results-gdrive))
              (push item all-results)))))
      )

    ;; GitHub search
    (when universal-search-enable-github
      (message "Searching GitHub...")
      (let ((github-results (universal-search-github keyword)))
        (when github-results
          (let ((processed-results (universal-search-github-process-results github-results)))
            (dolist (item (seq-take processed-results universal-search-max-results-github))
              (push item all-results)))))
      )

    ;; Lookup dictionary search
    (when universal-search-enable-lookup
      (message "Searching dictionaries...")
      (let ((lookup-results (universal-search-lookup keyword)))
        (dolist (item (seq-take lookup-results universal-search-max-results-lookup))
          (push item all-results))))

    ;; Spotlight search (macOS only)
    (when universal-search-enable-spotlight
      (message "Searching with Spotlight...")
      (let ((spotlight-results (universal-search-spotlight keyword)))
        (dolist (item (seq-take spotlight-results universal-search-max-results-spotight))
          (push item all-results))))

    (cons keyword all-results)))

(defun universal-search-action-handler (candidate)
  "Handle actions for CANDIDATE based on its type."
  ;;;;   (message candidate) ; debug
  (let ((type (plist-get candidate :type)))
    (cond

     ;; Google Drive
     ((eq type 'gdrive)
      (browse-url (plist-get candidate :link)))

     ;; Local files
     ((eq type 'local)
      (let* ((project-root (projectile-project-root))
             (file-path (expand-file-name (plist-get candidate :file) project-root)))
        (find-file file-path)
        (goto-char (point-min))
        (forward-line (1- (plist-get candidate :linum)))))

     ;; GitHub items
     ((memq type '(github-code github-issue github-pr))
      (browse-url (plist-get candidate :html_url)))

     ;; Lookup dictionary entries
     ((eq type 'lookup)
      ;; start lookup session
      (let* ((entry (plist-get candidate :entry))
             (module (lookup-default-module))
             (query (lookup-make-query 'default (lookup-entry-heading entry))))
        (if (fboundp 'lookup-display-entries)
            (lookup-display-entries module query (list entry))
          ;; simple lookup search
          (lookup-pattern (lookup-entry-heading entry)
                          )
          )
        )
      )
     ;; Spotlight results
     ((eq type 'spotlight)
      (let ((path (plist-get candidate :path)))
        (if (file-directory-p path)
            (dired path)
          (find-file path))))

     )))

(defun universal-search-copy-link (candidate)
  "Copy link or path from CANDIDATE to clipboard."
  (let* ((type (plist-get candidate :type))
         (text (cond
                ((eq type 'gdrive)
                 (plist-get candidate :link))
                ((eq type 'local)
                 (expand-file-name (plist-get candidate :file)))
                ((memq type '(github-code github-issue github-pr))
                 (plist-get candidate :html_url))
                ((eq type 'lookup)
                 (format "%s" (plist-get candidate :display)))
                ((eq type 'spotlight)
                 (plist-get candidate :path)))))
    (kill-new text)
    (message "Copied to clipboard: %s" text)))

(defun universal-search-open-with-external-app (candidate)
  "Open CANDIDATE with external application."
  (let ((type (plist-get candidate :type)))
    (cond
     ((eq type 'spotlight)
      (if (eq system-type 'darwin)
          (call-process "open" nil 0 nil (plist-get candidate :path))
        (message "External app opening is only supported on macOS")))
     ((eq type 'gdrive)
      (browse-url (plist-get candidate :link)))
     ((eq type 'local)
      (if (eq system-type 'darwin)
          (call-process "open" nil 0 nil (plist-get candidate :file))
        (let ((process-connection-type nil))
          (start-process "" nil "xdg-open" (plist-get candidate :file)))))
     (t
      (message "Cannot open this type with external application")))))

(defun universal-search-get-file-metadata (candidate)
  "Display metadata for file in CANDIDATE."
  (let ((type (plist-get candidate :type)))
    (cond
     ((eq type 'spotlight)
      (let* ((path (plist-get candidate :path))
             (buffer (get-buffer-create "*File Metadata*"))
             (cmd (format "mdls '%s'" (shell-quote-argument path))))
        (with-current-buffer buffer
          (erase-buffer)
          (insert (format "Metadata for: %s\n\n" path))
          (call-process-shell-command cmd nil buffer)
          (goto-char (point-min)))
        (switch-to-buffer buffer)))
     ((eq type 'local)
      (let* ((path (plist-get candidate :file))
             (buffer (get-buffer-create "*File Info*")))
        (with-current-buffer buffer
          (erase-buffer)
          (insert (format "File: %s\n\n" path))
          (insert (format "Size: %s bytes\n" (file-attribute-size (file-attributes path))))
          (insert (format "Modified: %s\n" (format-time-string
                                            "%Y-%m-%d %H:%M:%S"
                                            (file-attribute-modification-time (file-attributes path)))))
          (insert (format "Permissions: %s\n" (file-attribute-modes (file-attributes path))))
          (goto-char (point-min)))
        (switch-to-buffer buffer)))
     (t
      (message "Metadata not available for this type of result")))))

;;;###autoload
(defun universal-search ()
  "Search across local files, Google Drive, GitHub, dictionaries, and Spotlight."
  (interactive)
  (let* ((results (universal-search-process-results))
         (keyword (car results))
         (candidates (cdr results))
         ;; ソース別に結果を分類
         (local-results (seq-filter (lambda (item) (eq (plist-get item :type) 'local)) candidates))
         (gdrive-results (seq-filter (lambda (item) (eq (plist-get item :type) 'gdrive)) candidates))
         (github-results (seq-filter (lambda (item) (or (eq (plist-get item :type) 'github-code)
                                                        (eq (plist-get item :type) 'github-issue)
                                                        (eq (plist-get item :type) 'github-pr)))
                                     candidates))
         (lookup-results (seq-filter (lambda (item) (eq (plist-get item :type) 'lookup)) candidates))
         (spotlight-results (seq-filter (lambda (item) (eq (plist-get item :type) 'spotlight)) candidates))
         (sources '()))

    ;; local
    (when (and universal-search-enable-local local-results)
      (push (helm-build-sync-source "Local Files"
              :candidates (mapcar (lambda (candidate)
                                    (let ((icon ""))
                                      (cons (concat
                                             (propertize icon 'face 'universal-search-icon-face)
                                             " "
                                             (plist-get candidate :display)
                                             )
                                            candidate)
                                      )
                                    )
                                  local-results)
              :action '(("Open" . universal-search-action-handler)
                        ("Copy path" . universal-search-copy-link)
                        ("Open with external app" . universal-search-open-with-external-app)
                        ("View metadata" . universal-search-get-file-metadata))
              :persistent-action 'universal-search-copy-link
              :persistent-help "Copy path to clipboard")
            sources))

    ;; Google Drive
    (when (and universal-search-enable-gdrive gdrive-results)
      (push (helm-build-sync-source "Google Drive"
              :candidates (mapcar (lambda (candidate)
                                    (let ((icon ""))
                                      (cons (concat
                                             ;;icon
                                             (propertize icon 'face 'universal-search-icon-face)
                                             " "
                                             (plist-get candidate :display)) candidate)))
                                  gdrive-results)
              :action '(("Open in browser" . universal-search-action-handler)
                        ("Copy link" . universal-search-copy-link))
              :persistent-action 'universal-search-copy-link
              :persistent-help "Copy link to clipboard")
            sources))

    ;; GitHub
    (when (and universal-search-enable-github github-results)
      (push (helm-build-sync-source "GitHub"
              :candidates (mapcar (lambda (candidate)
                                    (let* ((type (plist-get candidate :type))
                                           (icon (cond
                                                  ((eq type 'github-code) "")
                                                  ((eq type 'github-issue) " ")
                                                  ((eq type 'github-pr) " "))))
                                      (cons (concat
                                             ;;icon
                                             (propertize icon 'face 'universal-search-icon-face)
                                             " "
                                             (plist-get candidate :display)) candidate)))
                                  github-results)
              :action '(("Open in browser" . universal-search-action-handler)
                        ("Copy link" . universal-search-copy-link))
              :persistent-action 'universal-search-copy-link
              :persistent-help "Copy link to clipboard")
            sources))

    ;; lookup
    (when (and universal-search-enable-lookup lookup-results)
      (push (helm-build-sync-source "Dictionary"
              :candidates (mapcar (lambda (candidate)
                                    (let ((icon ""))
                                      (cons (concat
                                             ;;icon
                                             (propertize icon 'face 'universal-search-icon-face)
                                             " "
                                             (plist-get candidate :display)) candidate)))
                                  lookup-results)
              :action '(("Look up" . universal-search-action-handler)
                        ("Copy text" . universal-search-copy-link))
              :persistent-action 'universal-search-copy-link
              :persistent-help "Copy text to clipboard")
            sources))

    ;; Spotlight
    (when (and universal-search-enable-spotlight spotlight-results)
      (push (helm-build-sync-source "Spotlight"
              :candidates (mapcar (lambda (candidate)
                                    (let ((icon ""))
                                      (cons (concat
                                             ;;icon
                                             (propertize icon 'face 'universal-search-icon-face)
                                             " "
                                             (plist-get candidate :display)) candidate)))
                                  spotlight-results)
              :action '(("Open" . universal-search-action-handler)
                        ("Copy path" . universal-search-copy-link)
                        ("Open with external app" . universal-search-open-with-external-app)
                        ("View metadata" . universal-search-get-file-metadata))
              :persistent-action 'universal-search-copy-link
              :persistent-help "Copy path to clipboard")
            sources))

    ;; message when no source
    (when (null sources)
      (push (helm-build-sync-source "No Results"
              :candidates '(("No matching results found" . nil))
              :action (lambda (_) (message "No action available")))
            sources))

    (helm :sources (nreverse sources)
          :buffer "*helm universal search*"
          :prompt (format "Query (%s): " keyword)
          :height universal-search-helm-height)
    )
  )

(provide 'universal-search)
;;; universal-search.el ends here
