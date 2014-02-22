;;; git-link.el --- Create URLs to a buffer's location in its GitHub/Bitbucket/Gitorious/... repository

;; Author: Skye Shaw <skye.shaw@gmail.com>
;; Version: 0.0.2
;; URL: http://github.com/sshaw/git-link

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Create a URL representing the current buffer's location in its GitHub/Bitbucket/Gitorious/... repository
;; at the current line number or active region. The URL will be added to the kill ring.
;;
;; With a prefix argument prompt for the remote's name. Defaults to "origin".

(defvar git-link-default-remote "origin" "Name of the remote branch to link to")

(defvar git-link-remote-alist
  '(("github.com"    git-link-github)
    ("bitbucket.org" git-link-bitbucket)
    ("gitorious.org" git-link-gitorious))
  "Maps remote hostnames to a function capable of creating the appropriate URL")

;; Matches traditional URL and scp style
;; This probably wont work for git remotes that aren't services
(defconst git-link-remote-regex "\\([-.[:word:]]+\\)[:/]\\([^/]+/[^/]+?\\)\\(?:\\.git\\)?$")

(defun git-link-chomp (s)
  (if (string-match "\\(\r?\n\\)+$" s)
      (replace-match "" t t s)
    s))

(defun git-link-exec (cmd)
  (shell-command-to-string (format "%s 2>%s" cmd null-device)))

(defun git-link-last-commit ()
  (git-link-exec "git --no-pager log -n 1 --pretty=format:%H"))

(defun git-link-current-branch ()
  (let ((branch (git-link-exec "git symbolic-ref HEAD")))
      (if (string-match "/\\([^/]+?\\)$" branch)
	  (match-string 1 branch))))

(defun git-link-repo-root ()
  (git-link-chomp (git-link-exec "git rev-parse --show-toplevel")))

(defun git-link-remote-url (name)
  (git-link-chomp (git-link-exec (format "git config --get remote.%s.url" name))))

(defun git-link-relative-filename ()
  (let* ((filename (buffer-file-name))
	 (dir      (git-link-repo-root)))
    (if (and dir filename)
	(substring (file-truename filename)
		   (1+ (length dir))))))

(defun git-link-remote-host (remote-name)
  (let ((url (git-link-remote-url remote-name)))
    (if (string-match git-link-remote-regex url)
	(match-string 1 url))))

(defun git-link-remote-dir (remote-name)
  (let ((url (git-link-remote-url remote-name)))
    (if (string-match git-link-remote-regex url)
	(match-string 2 url))))

(defun git-link-github (hostname dirname filename branch commit start end)
  (format "https://github.com/%s/tree/%s/%s#%s"
	  dirname
	  (or branch commit)
	  filename
	  (if (and start end)
	      (format "L%s-L%s" start end)
	    (format "L%s" start))))

(defun git-link-gitorious (hostname dirname filename branch commit start end)
  (format "https://gitorious.org/%s/source/%s:%s#L%s"
	  dirname
	  commit
	  filename
	  start))

(defun git-link-bitbucket (hostname dirname filename branch commit start end)
  ;; ?at=branch-name
  (format "https://bitbucket.org/%s/src/%s/%s#cl-%s"
	  dirname
	  commit
	  filename
	  start))

(defun git-link (&optional prompt)
  "Create a URL representing the current buffer's location in its
GitHub/Bitbucket/Gitorious/... repository at the current line number
or active region. The URL will be added to the kill ring.

With a prefix argument prompt for the remote's name.
Defaults to \"origin\"."

  (interactive "P")
  (let* ((remote-name (if prompt (read-string "Remote: " nil nil git-link-default-remote)
			git-link-default-remote))
	 (remote-host (git-link-remote-host remote-name))
	 (filename    (git-link-relative-filename))
	 (branch      (git-link-current-branch))
	 (commit      (git-link-last-commit))
	 (handler     (nth 1 (assoc remote-host git-link-remote-alist)))
	 (lines       (if (and transient-mark-mode mark-active)           ;; change to use-region-p (>= 23)
			  (mapcar 'line-number-at-pos (list (region-beginning) (region-end)))
			(list (line-number-at-pos)))))

    (cond ((null filename)
	   (message "Buffer has no file"))
	  ((null remote-host)
	   (message "Unknown remote '%s'" remote-name))
	  ((and (null commit) (null branch))
	   (message "Not on a branch, and repo does not have commits"))
	  ;; functionp???
	  ((null handler)
	   (message "No handler for %s" remote-host))
	  ;; null ret val
	  ((kill-new
	    (funcall handler
		     remote-host
		     (git-link-remote-dir remote-name)
		     filename
		     branch
		     commit
		     (nth 0 lines)
		     (nth 1 lines)))))))

(provide 'git-link)
;;; --- git-link.el ends here
