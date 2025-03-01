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

(defcustom universal-search-python-bin
  "~/globalenv/bin/python3"
  "Path to the directory containing Python scripts."
  :type 'string
  :group 'universal-search)

(defcustom universal-search-exclude-patterns
  '("private" "archive" "999_old" "flycheck_" "flymake_" ".git" "node_modules" "996_misc")
  "Patterns to exclude from local search."
  :type '(repeat string)
  :group 'universal-search)

(defcustom universal-search-enable-gdrive nil
  "Whether to enable Google search."
  :type 'boolean
  :group 'universal-search)

(defcustom universal-search-enable-github nil
  "Whether to enable GitHub search."
  :type 'boolean
  :group 'universal-search)

(defcustom universal-search-enable-local nil
  "Whether to enable GitHub search."
  :type 'boolean
  :group 'universal-search)

(defcustom universal-search-enable-lookup nil
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

(defcustom universal-search-max-results-local 20
  "Maximum number of results to display per search source."
  :type 'integer
  :group 'universal-search)

(defcustom universal-search-max-results-gdrive 10
  "Maximum number of results to display per search source."
  :type 'integer
  :group 'universal-search)

(defcustom universal-search-max-results-github 20
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


(setq universal-search-python-bin "~/globalenv/bin/python3")
(setq universal-search-enable-local t)
(setq universal-search-enable-lookup t)
(setq universal-search-enable-gdrive t)
(setq universal-search-enable-github t)
(setq universal-search-enable-spotlight t)
(setq universal-search-helm-height 60)

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
                        :display (format "%s:%s: %s" file linum content)
                        :file file
                        :linum (string-to-number linum))
                  results))))
      results)))

;;; Lookup Dictionary Search
;;; (defun universal-search-lookup (keyword)
;;;   "Search dictionaries for KEYWORD using lookup.el."
;;;   (when (and universal-search-enable-lookup
;;;              (featurep 'lookup))
;;;     (let ((results '()))
;;;       (condition-case nil
;;;           (let ((entries (lookup-entries (lookup-parse-pattern keyword))))
;;;             (dolist (entry entries)
;;;               (let* ((heading (lookup-entry-heading entry))
;;;                      (dict (lookup-entry-dictionary entry))
;;;                      (dict-title (if dict (lookup-dictionary-title dict) "Unknown")))
;;;                 (push (list :type 'lookup
;;;                             :display (format "%s [%s]" heading dict-title)
;;;                             :entry entry)
;;;                       results))))
;;;         (error nil))
;;;       results)))

;;;  (defun universal-search-lookup (keyword)
;;;    "Search dictionaries for KEYWORD using lookup.el."
;;;    (when (and universal-search-enable-lookup
;;;               (featurep 'lookup))
;;;      (let ((results '()))
;;;        (condition-case nil
;;;            (let* ((pattern (lookup-parse-pattern keyword))
;;;                   (query (lookup-make-query 'default keyword))
;;;                   (entries nil))
;;;              ;; 実際のセッションを作らず検索のみ実行
;;;              (dolist (dictionary (lookup-module-dictionaries (lookup-default-module)))
;;;                (when (lookup-dictionary-selected-p dictionary)
;;;                  (setq entries (append entries (lookup-vse-search-query dictionary query)))))
;;;              ;; 検索結果をフォーマット
;;;              (dolist (entry entries)
;;;                (let* ((heading (lookup-entry-heading entry))
;;;                       (dict (lookup-entry-dictionary entry))
;;;                       (dict-title (if dict (lookup-dictionary-title dict) "Unknown")))
;;;                  (push (list :type 'lookup
;;;                              :display (format "%s [%s]" heading dict-title)
;;;                              :entry entry)
;;;                        results))))
;;;          (error nil))
;;;        results)))

(defun universal-search-lookup (keyword)
  "Search dictionaries for KEYWORD using lookup.el."
  (when universal-search-enable-lookup
    (let ((results '()))
      (condition-case err
          (progn
            ;; lookupが読み込まれていなければ読み込む
            (require 'lookup nil t)

            ;; lookupが初期化されていなければ初期化する
            (when (and (featurep 'lookup)
                       ;;                       (fboundp 'lookup-initialized)
                       ;;                       (not (lookup-initialized))
                       )
              ;;              (if (fboundp 'lookup-initialize)
              ;;              (message "initialize!!!!!")
              ;;                (lookup-initialize)
              (lookup)
              ;;                )
              )

            ;; 検索実行
            (when (featurep 'lookup)
              (let* ((pattern (lookup-parse-pattern keyword))
                     (query (lookup-make-query 'default keyword))
                     (entries nil))
                ;; 実際のセッションを作らず検索のみ実行
                (dolist (dictionary (lookup-module-dictionaries (lookup-default-module)))
                  (when (lookup-dictionary-selected-p dictionary)
                    (setq entries (append entries (lookup-vse-search-query dictionary query)))))
                ;; 検索結果をフォーマット
                (dolist (entry entries)
                  (let* ((heading (lookup-entry-heading entry))
                         (dict (lookup-entry-dictionary entry))
                         (dict-title (if dict (lookup-dictionary-title dict) "Unknown")))
                    (push (list :type 'lookup
                                :display (format "%s [%s]" heading dict-title)
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
    (let* ((cmd (format "mdfind -name '%s' -onlyin ~/ 2>/dev/null"
                        (shell-quote-argument keyword)
                        ;;                        universal-search-spotlight-limit
                        ))
           (result (shell-command-to-string cmd))
           (results '()))
      (dolist (line (split-string result "\n" t))
        (let ((filename (file-name-nondirectory line))
              (directory (file-name-directory line)))
          (push (list :type 'spotlight
                      :display (format "%s [%s]" filename
                                       (if directory
                                           (abbreviate-file-name directory)
                                         ""))
                      :path line
                      :full-path line)
                results)))
      results)))

(defun universal-search-spotlight-advanced (keyword)
  "Advanced macOS Spotlight search for KEYWORD with metadata."
  (when (and universal-search-enable-spotlight
             (eq system-type 'darwin)
             (executable-find "mdfind"))
    (let* ((cmd (format "mdfind '%s' -onlyin ~/ 2>/dev/null"
                        (shell-quote-argument keyword)
                        ;;        universal-search-spotlight-limit
                        )
                )
           (result (shell-command-to-string cmd))
           (results '()))
      ;; Get metadata for each result
      (dolist (path (split-string result "\n" t))
        (let* ((filename (file-name-nondirectory path))
               (directory (file-name-directory path))
               (kind-cmd (format "mdls -name kMDItemKind '%s' 2>/dev/null || echo 'kMDItemKind = \"Unknown\"'"
                                 (shell-quote-argument path)))
               (kind-result (shell-command-to-string kind-cmd))
               (kind (if (string-match "kMDItemKind = \"\\(.*\\)\"" kind-result)
                         (match-string 1 kind-result)
                       "Unknown")))
          (push (list :type 'spotlight
                      :display (format "%s [%s] - %s"
                                       filename
                                       kind
                                       (if directory
                                           (abbreviate-file-name directory)
                                         ""))
                      :path path
                      :full-path path
                      :kind kind)
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

    ;; Spotlight search
    ;;    (when universal-search-enable-spotlight
    ;;      (message "Searching with Spotlight...")
    ;;      (let ((spotlight-results (universal-search-spotlight keyword)))
    ;;        (dolist (item spotlight-results)
    ;;          (push item all-results))))

    ;; Spotlight search (macOS only)
    (when universal-search-enable-spotlight
      (message "Searching with Spotlight...")
      (let ((spotlight-results (universal-search-spotlight keyword)))
        (dolist (item (seq-take spotlight-results universal-search-max-results-spotight))
          (push item all-results))))

    (cons keyword all-results)))

(defun universal-search-action-handler (candidate)
  "Handle actions for CANDIDATE based on its type."
  ;;  (message candidate)
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
      ;; lookupセッションを開始して表示
      (let* ((entry (plist-get candidate :entry))
             (module (lookup-default-module))
             (query (lookup-make-query 'default (lookup-entry-heading entry))))
        (if (fboundp 'lookup-display-entries)
            (lookup-display-entries module query (list entry))
          ;; 代替策: 単に検索を実行
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
         (candidates (cdr results)))

    (helm :sources (helm-build-sync-source (format "Universal Search: %s" keyword)
                     :candidates candidates
                     :candidate-transformer
                     (lambda (candidates)
                       (mapcar (lambda (candidate)
                                 (let* ((type (plist-get candidate :type))
                                        (icon (cond
                                               ((eq type 'gdrive) "")    ; Google Drive icon
                                               ((eq type 'local) "")     ; File icon
                                               ((eq type 'github-code) "") ; Code icon
                                               ((eq type 'github-issue) " ") ; Issue icon
                                               ((eq type 'github-pr) " ")    ; PR icon
                                               ((eq type 'lookup) "")    ; Dictionary icon
                                               ((eq type 'spotlight) "")))) ; Spotlight icon
                                   (cons (concat icon " " (plist-get candidate :display)) candidate)))
                               candidates))
                     :action '(("Open" . universal-search-action-handler)
                               ("Copy link/path" . universal-search-copy-link)
                               ("Open with external app" . universal-search-open-with-external-app)
                               ("View metadata" . universal-search-get-file-metadata))
                     :persistent-action 'universal-search-copy-link
                     :persistent-help "Copy link/path to clipboard")
          :buffer "*helm universal search*"
          :height universal-search-helm-height)))

(provide 'universal-search)
;;; universal-search.el ends here
