;;; git-link.el --- Get the GitHub/Bitbucket/Gitorious URL for a buffer location

;; Author: Skye Shaw <skye.shaw@gmail.com>
;; Version: 0.1.0 (Unreleased)
;; Keywords: git
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

;; Create URLs for files and commits in GitHub/Bitbucket/Gitorious/...
;; repositories. `git-link' returns the URL for the current buffer's file
;; location at the current line number or active region. `git-link-commit'
;; returns the URL for a commit. URLs are added to the kill ring.
;;
;; With a prefix argument prompt for the remote's name. Defaults to "origin".

;;; Change Log:

;; 2015-02-05 - v0.1.0
;; * Added git-link-commit (Thanks Ryan Barrett)
;; * Added git-link-open-in-browser variable (Thanks Ryan Barrett)
;; * Use call-process instead of shell-command-to-string
;; * Use --short option when calling symbolic-ref (Thanks Steven Huwig)
;;
;; 2014-02-27 - v0.0.2
;; * Fix for buffers visiting files through symlinks (Bug #1, thanks Evgeniy Dolzhenko)

;;; Code:

(require 'thingatpt)

(defvar git-link-default-remote "origin" "Name of the remote branch to link to")

(defvar git-link-open-in-browser nil "If non-nil, also open link in browser")

(defvar git-link-remote-alist
  '(("github.com"    git-link-github)
    ("bitbucket.org" git-link-bitbucket)
    ("gitorious.org" git-link-gitorious))
  "Maps remote hostnames to a function capable of creating the appropriate file URL")

(defvar git-link-commit-remote-alist
  '(("github.com"    git-link-commit-github)
    ("bitbucket.org" git-link-commit-bitbucket)
    ("gitorious.org" git-link-commit-gitorious))
  "Maps remote hostnames to a function capable of creating the appropriate commit URL")

;; Matches traditional URL and scp style
;; This probably wont work for git remotes that aren't services
(defconst git-link-remote-regex "\\([-.[:word:]]+\\)[:/]\\([^/]+/[^/]+?\\)\\(?:\\.git\\)?$")

(defun git-link-chomp (s)
  (if (string-match "\\(\r?\n\\)+$" s)
      (replace-match "" t t s)
    s))

(defun git-link-exec (&rest args)
  (with-temp-buffer
    ;; swallow stderr and return an empty string on failure
    (apply 'call-process "git" nil (list (current-buffer) nil) nil args)
    (buffer-string)))

(defun git-link-last-commit ()
  (git-link-exec "--no-pager" "log" "-n1" "--pretty=format:%H"))

(defun git-link-current-branch ()
  (git-link-chomp (git-link-exec "symbolic-ref" "--short" "HEAD")))

(defun git-link-repo-root ()
  (git-link-chomp (git-link-exec "rev-parse" "--show-toplevel")))

(defun git-link-remote-url (name)
  (git-link-chomp (git-link-exec "config" "--get" (format "remote.%s.url" name))))

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

(defun git-link-remotes ()
  "Returns the list of remotes for this repository, or nil on error"
  (ignore-errors (process-lines "git" "remote")))

(defun git-link-read-remote ()
  (let ((remotes (git-link-remotes)))
    (if remotes
        (completing-read "Remote: "
                         remotes
                         nil
                         t
                         ""
                         nil
                         (if (member git-link-default-remote remotes)
                             git-link-default-remote
                           (car remotes)))
      git-link-default-remote)))

(defun git-link-get-region ()
  (save-restriction
    (widen)
    (save-excursion
      (let ((use-region (use-region-p)))
        (list
         (line-number-at-pos (when use-region (region-beginning)))
         (when use-region (line-number-at-pos (region-end))))))))

(defun git-link-github (hostname dirname filename branch commit start end)
  (format "https://%s/%s/tree/%s/%s#%s"
	  hostname
	  dirname
	  (or branch commit)
	  filename
	  (if (and start end)
	      (format "L%s-L%s" start end)
	    (format "L%s" start))))

(defun git-link-commit-github (hostname dirname commit)
  (format "https://%s/%s/commit/%s"
	  hostname
	  dirname
	  commit))

(defun git-link-gitorious (hostname dirname filename branch commit start end)
  (format "https://%s/%s/source/%s:%s#L%s"
	  hostname
	  dirname
	  commit
	  filename
	  start))

(defun git-link-commit-gitorious (hostname dirname commit)
  (format "https://%s/%s/commit/%s"
	  hostname
	  dirname
	  commit))

(defun git-link-bitbucket (hostname dirname filename branch commit start end)
  ;; ?at=branch-name
  (format "https://%s/%s/src/%s/%s#cl-%s"
	  hostname
	  dirname
	  commit
	  filename
	  start))

(defun git-link-commit-bitbucket (hostname dirname commit)
  ;; ?at=branch-name
  (format "https://%s/%s/commits/%s"
	  hostname
	  dirname
	  commit))

(defun git-link-new (link)
  (kill-new link)
  (if git-link-open-in-browser (browse-url link)))

;;;###autoload
(defun git-link (remote start end)
  "Create a URL representing the current buffer's location in its
GitHub/Bitbucket/Gitorious/... repository at the current line number
or active region. The URL will be added to the kill ring.

With a prefix argument prompt for the remote's name.
Defaults to \"origin\"."
  (interactive (let* ((remote (if current-prefix-arg
                                  (git-link-read-remote)
                                git-link-default-remote))
                      (region (git-link-get-region)))
                 (list remote (car region) (cadr region))))
  (let* ((remote-host (git-link-remote-host remote))
	 (filename    (git-link-relative-filename))
	 (branch      (git-link-current-branch))
	 (commit      (git-link-last-commit))
	 (handler     (cadr (assoc remote-host git-link-remote-alist))))

    (cond ((null filename)
	   (message "Buffer has no file"))
	  ((null remote-host)
	   (message "Unknown remote '%s'" remote))
	  ((and (null commit) (null branch))
	   (message "Not on a branch, and repo does not have commits"))
	  ;; functionp???
	  ((null handler)
	   (message "No handler for %s" remote-host))
	  ;; null ret val
	  ((git-link-new
	    (funcall handler
		     remote-host
		     (git-link-remote-dir remote)
		     filename
		     branch
		     commit
		     start
		     end))))))

;;;###autoload
(defun git-link-commit (remote)
  "Create a URL representing the commit for the hash under point
in the current buffer's GitHub/Bitbucket/Gitorious/...
repository. The URL will be added to the kill ring.

With a prefix argument prompt for the remote's name.
Defaults to \"origin\"."

  (interactive (list (if current-prefix-arg
                         (git-link-read-remote)
                       git-link-default-remote)))
  (let* ((remote-host (git-link-remote-host remote))
	 (commit      (word-at-point))
	 (handler     (cadr (assoc remote-host git-link-commit-remote-alist))))
    (cond ((null remote-host)
	   (message "Unknown remote '%s'" remote))
	  ((not (string-match "[a-z0-9]\\{7,40\\}" (or commit "")))
	   (message "Point is not on a commit hash"))
	  ;; functionp???
	  ((null handler)
	   (message "No handler for %s" remote-host))
	  ;; null ret val
	  ((git-link-new
	    (funcall handler
		     remote-host
		     (git-link-remote-dir remote)
		     commit))))))

(provide 'git-link)
;;; git-link.el ends here
