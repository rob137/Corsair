;;; corsair.el --- Text accumulation enhancements for GPTel -*- lexical-binding: t; -*-
;; Author: Robert Kirby <corsair.el.package@gmail.com>
;; Maintainer: Robert Kirby <corsair.el.package@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (gptel "0.9.0"))
;; Homepage: https://github.com/rob137/Corsair
;; Keywords: convenience, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Corsair provides enhancements for GPTel, allowing for context accumulation
;; and file expansion within Emacs projects.

;;; Code:

(require 'project)
(require 'cl-lib)
(require 'subr-x)
(require 'gptel)

(defgroup corsair nil
  "Enhancements for GPTel with context accumulation and file expansion."
  :group 'tools
  :prefix "corsair-")

(defcustom corsair-chat-buffer-name "*GPTel Chat*"
  "Name of the GPTel chat buffer."
  :type 'string
  :group 'corsair)

(defun corsair-open-chat-buffer ()
  "Open or switch to the GPTel chat buffer and set it up for GPTel."
  (interactive)
  (let ((buffer (get-buffer-create corsair-chat-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (gptel-mode))
    (switch-to-buffer buffer)))

(defun corsair-accumulate-file-path-and-contents ()
  "Append file path and contents to the GPTel chat buffer."
  (interactive)
  (if buffer-file-name
      (let* ((path (buffer-file-name))
             (contents (buffer-string))
             (data (concat "\n" path "\n" contents "\n")))
        (with-current-buffer (get-buffer-create corsair-chat-buffer-name)
          (goto-char (point-max))
          (insert data)
          (message "File path and contents accumulated to GPTel chat buffer.")))
    (message "No file associated with this buffer.")))

(defun corsair-accumulate-file-name ()
  "Append file name to the GPTel chat buffer."
  (interactive)
  (if buffer-file-name
      (let ((name (file-name-nondirectory buffer-file-name)))
        (with-current-buffer (get-buffer-create corsair-chat-buffer-name)
          (goto-char (point-max))
          (insert "\n" name "\n")
          (message "File name accumulated to GPTel chat buffer.")))
    (message "No file associated with this buffer.")))

(defun corsair-accumulate-file-path ()
  "Append file path to the GPTel chat buffer."
  (interactive)
  (if buffer-file-name
      (let ((path buffer-file-name))
        (with-current-buffer (get-buffer-create corsair-chat-buffer-name)
          (goto-char (point-max))
          (insert "\n" path "\n")
          (message "File path accumulated to GPTel chat buffer.")))
    (message "No file associated with this buffer.")))

(defun corsair-accumulate-selected-text ()
  "Append selected text to the GPTel chat buffer."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (with-current-buffer (get-buffer-create corsair-chat-buffer-name)
          (goto-char (point-max))
          (insert "\n" text "\n")
          (message "Selected text accumulated to GPTel chat buffer."))
        (deactivate-mark))
    (message "No region selected.")))

(defun corsair-drop-accumulated-buffer ()
  "Clear the contents of the GPTel chat buffer."
  (interactive)
  (let ((buf (get-buffer corsair-chat-buffer-name)))
    (if buf
        (with-current-buffer buf
          (erase-buffer)
          (message "GPTel chat buffer cleared."))
      (message "GPTel chat buffer does not exist."))))

(defun corsair-project-paths ()
  "Return a list of files in the current project."
  (let ((project (project-current t)))
    (if project
        (project-files project)
      (user-error "No project found"))))

(defun corsair-insert-file-or-folder-contents ()
  "Insert contents of a selected file / directory from project into GPTel Chat."
  (interactive)
  (let ((project (project-current t)))
    (if project
        (let* ((project-root (project-root project))
               (project-paths (corsair-project-paths))
               (path (completing-read "Select file or directory: " project-paths))
               (full-path (expand-file-name path project-root)))
          (with-current-buffer (get-buffer-create corsair-chat-buffer-name)
            (goto-char (point-max))
            (cond
             ((file-directory-p full-path)
              (insert (format "\nDirectory: %s\n" path))
              (dolist (file (directory-files-recursively full-path ".*" nil nil))
                (insert (format "\n%s\n%s\n"
                                (file-relative-name file project-root)
                                (with-temp-buffer
                                  (insert-file-contents file)
                                  (buffer-string))))))
             ((file-regular-p full-path)
              (insert (format "\n%s\n%s\n" path
                              (with-temp-buffer
                                (insert-file-contents full-path)
                                (buffer-string)))))
             (t
              (user-error "Selected path is neither a file nor a directory"))))
          (message "Inserted contents of %s" path))
      (user-error "No project found"))))

;; Suggested Key bindings
;; (global-set-key (kbd "C-c g c") #'corsair-open-chat-buffer)                    ;; Open chat buffer
;; (global-set-key (kbd "C-c g a c") #'corsair-accumulate-file-path-and-contents) ;; Accumulate file path and contents
;; (global-set-key (kbd "C-c g a n") #'corsair-accumulate-file-name)              ;; Accumulate file name
;; (global-set-key (kbd "C-c g a v") #'corsair-accumulate-file-path)              ;; Accumulate file path
;; (global-set-key (kbd "C-c g a w") #'corsair-accumulate-selected-text)          ;; Accumulate selected text
;; (global-set-key (kbd "C-c g a D") #'corsair-drop-accumulated-buffer)           ;; Drop chat buffer
;; (global-set-key (kbd "C-c g f") #'corsair-insert-file-or-folder-contents)      ;; Insert file or folder contents

(provide 'corsair)

;;; corsair.el ends here
