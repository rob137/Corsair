;;; corsair.el --- Enhancements for GPTel with context accumulation and file expansion
;;; Commentary:
;;; Code:

(require 'project)
(require 'subr-x)  ;; For string-prefix-p

;; 1. Open or switch to the GPTel chat buffer
(defun corsair-open-chat-buffer ()
  "Open or switch to the *GPTel Chat* buffer and set it up for GPTel."
  (interactive)
  (let ((buffer (get-buffer-create "*GPTel Chat*")))
    (with-current-buffer buffer
      ;; Switch to org-mode if the buffer is in fundamental-mode
      (unless (derived-mode-p 'text-mode)
        (org-mode))
      ;; Enable GPTel mode
      (gptel-mode))
    (switch-to-buffer buffer)))

(setq gptel-default-session "Chat")

;; 2. Accumulate file path and contents into the GPTel chat buffer
(defun corsair-accumulate-file-path-and-contents-to-chat ()
  "Append file path and contents to the GPTel chat buffer."
  (interactive)
  (when buffer-file-name
    (let* ((path (buffer-file-name))
           (contents (buffer-string))
           (data (concat "\n" path "\n" contents "\n")))
      (with-current-buffer "*GPTel Chat*"
        (goto-char (point-max))
        (insert data)
        (message "File path and contents accumulated to chat buffer.")))))

;; 3. Accumulate selected text into the GPTel chat buffer
(defun corsair-accumulate-selected-text-to-chat ()
  "Append selected text to the GPTel chat buffer."
  (interactive)
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (with-current-buffer "*GPTel Chat*"
        (goto-char (point-max))
        (insert (concat "\n" text "\n"))
        (message "Selected text accumulated to chat buffer.")))
    (deactivate-mark)))

;; GPTel-specific versions of accumulation functions

;; 4. Accumulate file path and contents into the GPTel chat buffer
(defun corsair-accumulate-file-path-and-contents ()
  "Append file path and contents to the GPTel chat buffer."
  (interactive)
  (when buffer-file-name
    (let* ((path (buffer-file-name))
           (contents (buffer-string))
           (data (concat "\n" path "\n" contents "\n")))
      (with-current-buffer "*GPTel Chat*"
        (goto-char (point-max))
        (insert data)
        (message "File path and contents accumulated to GPTel chat buffer.")))))

;; 5. Accumulate file name into the GPTel chat buffer
(defun corsair-accumulate-file-name ()
  "Append file name to the GPTel chat buffer."
  (interactive)
  (when buffer-file-name
    (let* ((name (file-name-nondirectory buffer-file-name)))
      (with-current-buffer "*GPTel Chat*"
        (goto-char (point-max))
        (insert (concat "\n" name "\n"))
        (message "File name accumulated to GPTel chat buffer.")))))

;; 6. Accumulate file path into the GPTel chat buffer
(defun corsair-accumulate-file-path ()
  "Append file path to the GPTel chat buffer."
  (interactive)
  (when buffer-file-name
    (let* ((path (buffer-file-name)))
      (with-current-buffer "*GPTel Chat*"
        (goto-char (point-max))
        (insert (concat "\n" path "\n"))
        (message "File path accumulated to GPTel chat buffer.")))))

;; 7. Accumulate selected text into the GPTel chat buffer
(defun corsair-accumulate-selected-text ()
  "Append selected text to the GPTel chat buffer."
  (interactive)
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (with-current-buffer "*GPTel Chat*"
        (goto-char (point-max))
        (insert (concat "\n" text "\n"))
        (message "Selected text accumulated to GPTel chat buffer.")))
    (deactivate-mark)))

;; 8. Drop accumulated buffer content (clear GPTel chat buffer)
(defun corsair-drop-accumulated-buffer ()
  "Clear the contents of the GPTel chat buffer."
  (interactive)
  (let ((buf (get-buffer "*GPTel Chat*")))
    (when buf
      (with-current-buffer buf
        (erase-buffer)
        (message "GPTel chat buffer cleared.")))))

;; 9. Expand @path with fuzzy matching for files and directories
(defun corsair-project-paths ()
  "Return a list of files and directories in the current project."
  (let ((project (project-current t)))
    (if project
        (let* ((project-root (project-root project))
               (files (project-files project))
               (dirs ()))
          ;; Collect directories from file paths
          (dolist (file files)
            (let ((dir (file-name-directory file)))
              (unless (member dir dirs)
                (push dir dirs))))
          ;; Combine files and directories, remove duplicates
          (delete-dups (append dirs files)))
      (error "No project found"))))

(defun corsair-expand-at-path ()
  "Expand @path at point to the contents of the matched file or directory in the project."
  (interactive)
  (let* ((end (point))
         (start (save-excursion
                  (search-backward "@" nil t)
                  (point)))
         (symbol (buffer-substring-no-properties start end)))
    (if (and (string-prefix-p "@" symbol)
             (> (length symbol) 1))
        (let* ((path-fragment (substring symbol 1))  ;; Remove '@'
               (project-root (project-root (project-current t)))
               ;; Get list of files and directories in the project
               (project-paths (corsair-project-paths))
               ;; Set the completion styles to flex for fuzzy matching
               (completion-styles '(flex))
               ;; Perform completion
               (matched-path (completing-read
                              (format "Select file or directory (default %s): " path-fragment)
                              project-paths
                              nil nil path-fragment)))
          ;; Check if matched-path is file or directory
          (let ((full-path (expand-file-name matched-path project-root)))
            (if (file-directory-p full-path)
                ;; If directory, insert contents of all files in directory recursively
                (let ((files (directory-files-recursively full-path ".*" nil nil)))
                  (delete-region start end)
                  (dolist (file files)
                    (insert (format "\n%s\n%s\n" file (with-temp-buffer
                                                        (insert-file-contents file)
                                                        (buffer-string)))))
                  (message "Inserted contents of directory %s" matched-path))
              ;; Else, it's a file, insert its content
              (let ((file-content (with-temp-buffer
                                    (insert-file-contents full-path)
                                    (buffer-string))))
                (delete-region start end)
                (insert file-content)
                (message "Inserted contents of %s" matched-path)))))
      (error "No @path at point or path is empty"))))

(global-set-key (kbd "C-c g c") 'corsair-open-chat-buffer)                  ;; Open chat buffer
(global-set-key (kbd "C-c g a c") 'corsair-accumulate-file-path-and-contents) ;; Accumulate file path and contents
(global-set-key (kbd "C-c g a n") 'corsair-accumulate-file-name)             ;; Accumulate file name
(global-set-key (kbd "C-c g a v") 'corsair-accumulate-file-path)             ;; Accumulate file path
(global-set-key (kbd "C-c g a w") 'corsair-accumulate-selected-text)         ;; Accumulate selected text
(global-set-key (kbd "C-c g a D") 'corsair-drop-chat-buffer)          ;; Drop chat buffer

;; Define key binding for @path expansion
(global-set-key (kbd "C-c g @") 'corsair-expand-at-path)

(provide 'corsair)

;;; corsair.el ends here
