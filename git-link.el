;;; gh-link.el --- Create URLs to a buffer's location in its GitHub repository

;; Author: Skye Shaw <skye.shaw@gmail.com>
;; Version: 0.0.1

;;; Commentary:

;; Create a URL representing the current buffer's location in its GitHub repository at
;; the current line number or active region. The URL will be added to the kill ring.
;; 
;; With a prefix argument prompt for the remote's name. Defaults to "origin".

(defvar gh-link-default-remote "origin" "Name of the remote branch to link to")

(defun gh-link-chomp (s)
  (if (string-match "\\(\r?\n\\)+$" s)
      (replace-match "" t t s)
    s))

(defun gh-link-exec (cmd)
  (shell-command-to-string (format "%s 2>%s" cmd null-device)))

(defun gh-link-repo-root ()
  (gh-link-chomp (gh-link-exec "git rev-parse --show-toplevel")))

(defun gh-link-branch ()
  (let ((branch (gh-link-exec "git symbolic-ref HEAD")))
      (if (string-match "/\\(\\w+\\)$" branch)
	  (match-string 1 branch))))

(defun gh-link-remote-dir (remote-name)
  (let ((remote-dir (gh-link-exec (format "git config --get remote.%s.url" remote-name))))
    (if (and remote-dir
	     (string-match "\\([-.[:word:]]+/[-.[:word:]]+\\)\\(?:\\.git\\)$" remote-dir))
	(match-string 1 remote-dir))))

(defun gh-build-link (dir branch filename)
  (format "http://github.com/%s/tree/%s/%s#%s"
	  dir
	  branch
	  filename
	  (if (region-active-p)		;; instead of mark-active?
	      (apply 'format "L%s-L%s"
		     (mapcar 'line-number-at-pos (list (region-beginning) (region-end))))
	    (format "L%s" (line-number-at-pos)))))

(defun gh-link (&optional prompt?)
  "Create a URL representing the current buffer's location in its GitHub repository at
 the current line number or active region. The URL will be added to the kill ring.
 
 With a prefix argument prompt for the remote's name. Defaults to \"origin\"."

  (interactive "P")
  (let* ((local-root  (gh-link-repo-root))
	 (filename    (buffer-file-name))
	 (remote-name (if prompt? (read-string "Remote: " nil nil gh-link-default-remote)
			gh-link-default-remote))
	 (remote-root (gh-link-remote-dir remote-name))
	 (branch      (gh-link-branch)))


    (cond ((null filename)
	   (message "Buffer has no file"))
	  ((null branch)
	   (message "Detached head"))
	  ((null remote-root)
	   (message "Cannot deternmine remote root directory"))
	  ((kill-new (gh-build-link remote-root
				    branch
				    (substring filename (1+ (length local-root)))))))))
