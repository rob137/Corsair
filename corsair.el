;;; corsair.el --- Enhancements for GPTel with context accumulation and file expansion
;;; Commentary:
;;; Code:

(require 'project)
(require 'cl-lib)  ;; For cl-labels
(require 'subr-x)  ;; For string-prefix-p

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

(defun corsair-accumulate-file-name ()
  "Append file name to the GPTel chat buffer."
  (interactive)
  (when buffer-file-name
    (let* ((name (file-name-nondirectory buffer-file-name)))
      (with-current-buffer "*GPTel Chat*"
        (goto-char (point-max))
        (insert (concat "\n" name "\n"))
        (message "File name accumulated to GPTel chat buffer.")))))

(defun corsair-accumulate-file-path ()
  "Append file path to the GPTel chat buffer."
  (interactive)
  (when buffer-file-name
    (let* ((path (buffer-file-name)))
      (with-current-buffer "*GPTel Chat*"
        (goto-char (point-max))
        (insert (concat "\n" path "\n"))
        (message "File path accumulated to GPTel chat buffer.")))))

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

(defun corsair-drop-accumulated-buffer ()
  "Clear the contents of the GPTel chat buffer."
  (interactive)
  (let ((buf (get-buffer "*GPTel Chat*")))
    (when buf
      (with-current-buffer buf
        (erase-buffer)
        (message "GPTel chat buffer cleared.")))))

(defun corsair-project-paths ()
  "Return a list of files and directories in the current project."
  (let ((project (project-current t)))
    (if project
        (let ((project-root (project-root project))
              (paths ()))
          (cl-labels ((collect-paths (dir)
                        (dolist (entry (directory-files dir t nil t))
                          (let ((name (file-name-nondirectory entry)))
                            (unless (member name '("." ".."))
                              (let ((relative-path (file-relative-name entry project-root)))
                                (push relative-path paths)
                                (when (file-directory-p entry)
                                  (collect-paths entry))))))))
            (collect-paths project-root))
          paths)
      (error "No project found"))))

(defun corsair-insert-file-or-folder-contents ()
  "Prompt user to select a file or folder from the project, and insert its path and contents into the GPTel chat buffer."
  (interactive)
  (let ((project (project-current t)))
    (if project
        (let* ((project-root (project-root project))
               (project-paths (corsair-project-paths))
               (completion-styles '(flex))  ;; Enable fuzzy matching
               (path (completing-read "Select file or directory: " project-paths))
               (full-path (expand-file-name path project-root)))
          (with-current-buffer "*GPTel Chat*"
            (goto-char (point-max))
            (if (file-directory-p full-path)
                (progn
                  (insert (format "\nDirectory: %s\n" full-path))
                  (dolist (file (directory-files-recursively full-path ".*" nil nil))
                    (insert (format "\n%s\n%s\n"
                                    (file-relative-name file project-root)
                                    (with-temp-buffer
                                      (insert-file-contents file)
                                      (buffer-string)))))
                  (message "Inserted contents of directory %s" path))
              (insert (format "\n%s\n%s\n" path
                              (with-temp-buffer
                                (insert-file-contents full-path)
                                (buffer-string)))))
            (message "Inserted contents of %s" path)))
      (error "No project found"))))

;; Key bindings
(global-set-key (kbd "C-c g c") 'corsair-open-chat-buffer)                   ;; Open chat buffer
(global-set-key (kbd "C-c g a c") 'corsair-accumulate-file-path-and-contents) ;; Accumulate file path and contents
(global-set-key (kbd "C-c g a n") 'corsair-accumulate-file-name)              ;; Accumulate file name
(global-set-key (kbd "C-c g a v") 'corsair-accumulate-file-path)              ;; Accumulate file path
(global-set-key (kbd "C-c g a w") 'corsair-accumulate-selected-text)          ;; Accumulate selected text
(global-set-key (kbd "C-c g a D") 'corsair-drop-accumulated-buffer)           ;; Drop chat buffer
(global-set-key (kbd "C-c g f") 'corsair-insert-file-or-folder-contents)      ;; Insert file or folder contents

(provide 'corsair)

;;; corsair.el ends here
