;;; corsair.el --- Enhancements for GPTel with context accumulation and file expansion
;;; Commentary:
;;; Code:
;; 1. Open or switch to the GPTel chat buffer
(defun open-gptel-chat-buffer ()
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
(defun accumulate-file-path-and-contents-to-chat ()
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
(defun accumulate-selected-text-to-chat ()
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
(defun gptel-accumulate-file-path-and-contents ()
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
(defun gptel-accumulate-file-name ()
  "Append file name to the GPTel chat buffer."
  (interactive)
  (when buffer-file-name
    (let* ((name (file-name-nondirectory buffer-file-name)))
      (with-current-buffer "*GPTel Chat*"
        (goto-char (point-max))
        (insert (concat "\n" name "\n"))
        (message "File name accumulated to GPTel chat buffer.")))))

;; 6. Accumulate file path into the GPTel chat buffer
(defun gptel-accumulate-file-path ()
  "Append file path to the GPTel chat buffer."
  (interactive)
  (when buffer-file-name
    (let* ((path (buffer-file-name)))
      (with-current-buffer "*GPTel Chat*"
        (goto-char (point-max))
        (insert (concat "\n" path "\n"))
        (message "File path accumulated to GPTel chat buffer.")))))

;; 7. Accumulate selected text into the GPTel chat buffer
(defun gptel-accumulate-selected-text ()
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
(defun gptel-drop-accumulated-buffer ()
  "Clear the contents of the GPTel chat buffer."
  (interactive)
  (let ((buf (get-buffer "*GPTel Chat*")))
    (when buf
      (with-current-buffer buf
        (erase-buffer)
        (message "GPTel chat buffer cleared.")))))

;; Define key bindings for the GPTel shadowed functions
(global-set-key (kbd "C-c g c") 'open-gptel-chat-buffer)                  ;; Open GPTel chat buffer
(global-set-key (kbd "C-c g a c") 'gptel-accumulate-file-path-and-contents) ;; Accumulate file path and contents
(global-set-key (kbd "C-c g a n") 'gptel-accumulate-file-name)             ;; Accumulate file name
(global-set-key (kbd "C-c g a v") 'gptel-accumulate-file-path)             ;; Accumulate file path
(global-set-key (kbd "C-c g a w") 'gptel-accumulate-selected-text)         ;; Accumulate selected text
(global-set-key (kbd "C-c g a D") 'gptel-drop-accumulated-buffer)          ;; Drop GPTel chat buffer
