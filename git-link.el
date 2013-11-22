;;; git-link.el --- Create URLs to a buffer's location in its GitHub repository

;; Author: Skye Shaw <skye.shaw@gmail.com>
;; Version: 0.0.2

;;; Commentary:

;; Create a URL representing the current buffer's location in its GitHub repository at
;; the current line number or active region. The URL will be added to the kill ring.
;; 
;; With a prefix argument prompt for the remote's name. Defaults to "origin".

(defvar git-link-default-remote "origin" "Name of the remote branch to link to")

(defun git-link-chomp (s)
  (if (string-match "\\(\r?\n\\)+$" s)
      (replace-match "" t t s)
    s))

(defun git-link-exec (cmd)
  (shell-command-to-string (format "%s 2>%s" cmd null-device)))

(defun git-link-repo-root ()
  (git-link-chomp (git-link-exec "git rev-parse --show-toplevel")))

(defun git-link-branch ()
  (let ((branch (git-link-exec "git symbolic-ref HEAD")))
      (if (string-match "/\\([^/]+?\\)$" branch)
	  (match-string 1 branch))))

(defun git-link-remote-dir (remote-name)
  (let ((remote-dir (git-link-exec (format "git config --get remote.%s.url" remote-name))))
    (if (and remote-dir
	     ;; matches traditional URL and scp style
	     (string-match "\\([-.[:word:]]+\\)[:/]\\([^/]+/[^/]+?\\)\\(?:\\.git\\)?$" remote-dir))
	;; (match-string 1 remote) ;; host
	;; (match-string 2 remote) ;; user/repo
	(match-string 2 remote-dir))))

(git-link-remote-dir "origin")


(defun git-link-build-link (dir branch filename)
  (format "http://github.com/%s/tree/%s/%s#%s"
	  dir
	  branch
	  filename
	  (if (region-active-p)		;; instead of mark-active?
	      (apply 'format "L%s-L%s"
		     (mapcar 'line-number-at-pos (list (region-beginning) (region-end))))
	    (format "L%s" (line-number-at-pos)))))

(defun git-link (&optional prompt?)
  "Create a URL representing the current buffer's location in its GitHub repository at
 the current line number or active region. The URL will be added to the kill ring.
 
 With a prefix argument prompt for the remote's name. Defaults to \"origin\"."

  (interactive "P")
  (let* ((local-root  (git-link-repo-root))
	 (filename    (buffer-file-name))
	 (remote-name (if prompt? (read-string "Remote: " nil nil git-link-default-remote)
			git-link-default-remote))
	 (remote-root (git-link-remote-dir remote-name))
	 (branch      (git-link-branch)))


    (cond ((null filename)
	   (message "Buffer has no file"))
	  ((null branch)
	   (message "Detached head"))
	  ((null remote-root)
	   (message "Cannot deternmine remote root directory"))
	  ((kill-new (git-link-build-link remote-root
					  branch
					  (substring filename (1+ (length local-root)))))))))
