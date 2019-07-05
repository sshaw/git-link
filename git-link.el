;;; git-link.el --- Get the GitHub/Bitbucket/GitLab URL for a buffer location -*- lexical-binding: t -*-

;; Copyright (C) 2013-2019 Skye Shaw and others
;; Author: Skye Shaw <skye.shaw@gmail.com>
;; Version: 0.7.3
;; Keywords: git, vc, github, bitbucket, gitlab, sourcehut, convenience
;; URL: http://github.com/sshaw/git-link
;; Package-Requires: ((emacs "24.3"))

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

;; Create URLs for files and commits in GitHub/Bitbucket/GitLab/...
;; repositories.  `git-link' returns the URL for the current buffer's file
;; location at the current line number or active region.  `git-link-commit'
;; returns the URL for a commit.  URLs are added to the kill ring.
;;
;; With a prefix argument prompt for the remote's name.  Defaults to "origin".

;;; Change Log:

;; 2019-03-09 - v0.7.3
;; * Add support for sourcehut
;;
;; 2018-10-30 - v0.7.2
;; * Fix suffix stripping on remote path only if it ends in .git (Issue #58, thanks Marko Crnic)
;;
;; 2018-07-08 - v0.7.1
;; * Add support for vc-revision-other-window files (Issue #54)
;;
;; 2018-06-07 - v0.7.0
;; * Add support for Tramp (Issue #49, thanks Jürgen Hötzel)
;; * Fix various compiler warnings
;; * Fix differences between url-path-and-query across Emacs versions
;; * Require Emacs 24.3
;;
;; 2018-04-23 - v0.6.0
;; * Fix parsing of remotes with auth info (Issue #51)
;; * Removed remote regex in favor of url-parse
;;
;; 2017-06-03 - v0.5.1
;; * Add support for more magit modes
;;
;; 2017-06-01 - v0.5.0
;; * Add support for linking in dired and magit modes
;; * Add support for defcustom
;; * Change git-link-remote-regex to support more remote URL formats (Thanks Kaushal Modi)
;; * Change git-link-remote-alist to use regex matching (Thanks Kaushal Modi)
;; * Fix point on commit hash regex and support uppercase SHAs (Thanks Kaushal Modi!)
;; * Fix git-link-commit message so that SHA text is displayed without properties
;; * Enabled lexical-binding (Thanks Kaushal Modi!!)
;;
;; -- Note that v0.5.0 was released as "v0.5.0 (unreleased)"
;;
;; 2016-10-19 - v0.4.5
;; * Fix for branches containing reserved URLs characters (Issue #36)
;;
;; 2016-09-11 - v0.4.4
;; * Added support for git-link-homepage
;;
;; 2016-08-13 - v0.4.3
;; * Added support for git-timemachine (Issue #22, thanks Diego Berrocal)
;;
;; 2016-08-09 - v0.4.2
;; * Fix for URLs with ports (Issue #32)
;;
;; 2016-04-01 - v0.4.1
;; * Better handling for branches that have no explicit remote
;; * Better error messages
;;
;; 2016-02-16 - v0.4.0
;; * Try branch's tracking remote when other branch settings are not specified
;; * git-link-default-remote now defaults to nil
;;
;; 2015-09-21 - v0.3.0
;; * Support for setting branch and remote names via `git config`
;; * Added git-link-default-branch
;; * Removed some functions, use emacs "private" convention for others
;;
;; 2015-09-12 - v0.2.2
;; * Support for BitBucket's multiline format
;;
;; 2015-07-25 - v0.2.1
;; * Fix for BitBucket's new URL format (Thanks Ev Dolzhenko)
;; * Fix for GitLab's multiline format (Thanks Enrico Carlesso)
;;
;; 2015-06-05 - v0.2.0
;; * Deactivate mark after killing the link (Thanks Kaushal Modi)
;; * Support for GitLab (Thanks Swaroop C H)
;; * Use completing-read when prompting for remotes (Thanks Andrew Gwozdziewycz)
;; * Display URL in minibuffer when adding to kill ring (Thanks Andrew Gwozdziewycz)
;; * Added git-link-use-commit variable (Thanks Kaushal Modi)
;; * Fix for displaying link in minibuffer when interprogram-cut-function is set (Thanks Ric Lister)
;; * Fix to ignore point at beginning of line in regions (Thanks Kaushal Modi)
;; * Fix for narrow-to-region (Bug #10, thanks Andrew Gwozdziewycz)
;; * Fix to use remote hostname when constructing link URLs (Thanks David Hull)
;;
;; 2015-02-05 - v0.1.0
;; * Added git-link-commit (Thanks Ryan Barrett)
;; * Added git-link-open-in-browser variable (Thanks Ryan Barrett)
;; * Use call-process instead of shell-command-to-string
;; * Use --short option when calling symbolic-ref (Thanks Steven Huwig)
;;
;; 2014-02-27 - v0.0.2
;; * Fix for buffers visiting files through symlinks (Issue #1, thanks Evgeniy Dolzhenko)

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'thingatpt)
(require 'url-util)
(require 'url-parse)

(defgroup git-link nil
  "Get the GitHub/Bitbucket/GitLab URL for a buffer location"
  :prefix "git-link-"
  :link '(url-link :tag "Report a Bug" "https://github.com/sshaw/git-link/issues")
  :link '(url-link :tag "Homepage" "https://github.com/sshaw/git-link")
  :group 'convenience)

(eval-when-compile
  (defvar git-timemachine-revision))    ;; silence reference to free variable warning

(defcustom git-link-default-remote nil
  "Name of the remote to link to."
  :type 'string
  :group 'git-link)

(defcustom git-link-default-branch nil
  "Name of the branch to link to."
  :type 'string
  :group 'git-link)

(defcustom git-link-open-in-browser nil
  "If non-nil also open link in browser via `browse-url'."
  :type 'boolean
  :group 'git-link)

(defcustom git-link-use-commit nil
  "If non-nil use the latest commit's hash in the link instead of the branch name."
  :type 'boolean
  :group 'git-link)

(defcustom git-link-remote-alist
  '(("git.sr.ht" git-link-sourcehut)
    ("github" git-link-github)
    ("bitbucket" git-link-bitbucket)
    ("gitorious" git-link-gitorious)
    ("gitlab" git-link-gitlab))
  "Alist of host names and functions creating file links for those.
Each element looks like (REGEXP FUNCTION) where REGEXP is used to
match the remote's host name and FUNCTION is used to generate a link
to the file on remote host.

As an example, \"gitlab\" will match with both \"gitlab.com\" and
\"gitlab.example.com\"."
  :type '(alist :key-type string :value-type (group function))
  :group 'git-link)

(defcustom git-link-commit-remote-alist
  '(("git.sr.ht" git-link-commit-github)
    ("github" git-link-commit-github)
    ("bitbucket" git-link-commit-bitbucket)
    ("gitorious" git-link-commit-gitorious)
    ("gitlab" git-link-commit-github))
  "Alist of host names and functions creating commit links for those.
Each element looks like (REGEXP FUNCTION) where REGEXP is used to
match the remote's host name and FUNCTION is used to generate a link
to the commit on remote host.

As an example, \"gitlab\" will match with both \"gitlab.com\" and
\"gitlab.example.com\"."
  :type '(alist :key-type string :value-type (group function))
  :group 'git-link)

(defun git-link--exec(&rest args)
  (ignore-errors
    (with-temp-buffer
      (when (zerop (apply #'process-file "git" nil (current-buffer) nil args))
        (goto-char (point-min))
        (cl-loop until (eobp)
                 collect (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))
                 do (forward-line 1))))))

(defun git-link--get-config (name)
  (car (git-link--exec "config" "--get" name)))

(defun git-link--remotes ()
  (git-link--exec "remote"))

(defun git-link--last-commit ()
  (car (git-link--exec "--no-pager" "log" "-n1" "--pretty=format:%H")))

(defun git-link--commit ()
  (cond
   ((git-link--using-git-timemachine) (car git-timemachine-revision))
   ((git-link--using-magit-blame) (oref (magit-blame-chunk-at (point)) :orig-rev))
   (t (git-link--last-commit))))

(defun git-link--current-branch ()
  (car (git-link--exec "symbolic-ref" "--short" "HEAD")))

(defun git-link--repo-root ()
  (let ((dir (car (git-link--exec "rev-parse" "--show-toplevel"))))
    (if (file-remote-p default-directory)
	(concat (file-remote-p default-directory) dir)
      dir)))

(defun git-link--remote-url (name)
  (git-link--get-config (format "remote.%s.url" name)))

(defun git-link--branch-remote (branch)
  (git-link--get-config (format "branch.%s.remote" branch)))

(defun git-link--branch ()
  (or (git-link--get-config "git-link.branch")
      git-link-default-branch
      (git-link--current-branch)))

(defun git-link--remote ()
  (let* ((branch (git-link--current-branch))
	 (remote (or (git-link--get-config "git-link.remote")
		     git-link-default-remote
		     (git-link--branch-remote branch))))

    ;; Git defaults to "." if the branch has no remote.
    ;; If the branch has no remote we try master's, which may be set.
    (if (or (null remote)
	    (and (string= remote ".")
		 (not (string= branch "master"))))
	(setq remote (git-link--branch-remote "master")))

    (if (or (null remote) (string= remote "."))
	"origin"
      remote)))

(defun git-link--handler (alist str)
  "For an ALIST whose `car' (a regexp) matches STR, return cadr.

The ALIST consists of (REGEXP FN) list elements.
Valid ALISTs are `git-link-remote-alist',`git-link-commit-remote-alist'.

For the first ALIST element whose REGEXP matches with STR, FN is
returned.

Return nil,
- if STR does not match with REGEXP in any of the elements of ALIST, or
- if STR is not a string"
  (when (stringp str)
    (cadr (cl-find-if (lambda (lst)
                        (string-match-p (car lst) str))
                      alist))))

(defun git-link--parse-vc-revision (filename)
"If FILENAME appears to be from `vc-revision-other-window'
return (FILENAME . REVISION) otherwise nil."
  (when (and (string-match "\\(.+\\)\\.~\\([^~]+\\)~$" filename)
             (file-exists-p (match-string 1 filename)))
    (cons (match-string 1 filename)
          (match-string 2 filename))))

(defun git-link--relative-filename ()
  (let* ((filename (buffer-file-name))
	 (dir      (git-link--repo-root)))

    (when (null filename)
      (cond
       ((eq major-mode 'dired-mode)
        (setq filename (dired-file-name-at-point)))
       ((or
         (git-link--using-magit-blame)
         (string-match-p "^magit-" (symbol-name major-mode)))
        (setq filename (or
                        (and (fboundp 'magit-file-at-point)
                             (magit-file-at-point))
                        (and (fboundp 'magit-current-file)
                             (magit-current-file)))))))

    (if (and dir filename
             ;; Make sure filename is not above dir, e.g. "/foo/repo-root/.."
             (< (length dir) (length (file-truename filename))))
	(substring (file-truename filename)
		   (1+ (length dir))))))

(defun git-link--parse-remote (url)
  "Parse URL and return a list as (HOST DIR).  DIR has no leading slash or `git' extension."
  (let (host path)
    (unless (string-match "^[a-zA-Z0-9]+://" url)
      (setq url (concat "ssh://" url)))

    (setq url  (url-generic-parse-url url)
          ;; Normalize path.
          ;; If none, will nil on Emacs < 25. Later versions return "".
          path (or (car (url-path-and-query url)) "")
          host (url-host url))

    (when host
      (when (and (not (string= "/" path))
                 (not (string= ""  path)))
        (setq path (substring
                    (if (string-match "\\.git\\'" path)
                        (file-name-sans-extension path)
                      path)
                    1)))

      ;; Fix-up scp style URLs
      (when (string-match ":" host)
        (let ((parts (split-string host ":" t)))
          (setq host (car parts)
                path (concat (cadr parts) "/" path))))

      (list host path))))

(defun git-link--using-git-timemachine ()
  (and (boundp 'git-timemachine-revision)
       git-timemachine-revision))

(defun git-link--using-magit-blame ()
  magit-blame-mode)

(defun git-link--read-remote ()
  (let ((remotes (git-link--remotes))
	(current (git-link--remote)))
    (completing-read "Remote: "
		     remotes
		     nil
		     t
		     ""
		     nil
		     (if (member current remotes)
			 current
		       (car remotes)))))

(defun git-link--get-region ()
  (cond
   ((git-link--using-magit-blame) (list (oref (magit-blame-chunk-at (point)) :orig-line) nil))
   (buffer-file-name (save-restriction
                       (widen)
                       (save-excursion
                         (let* ((use-region (use-region-p))
                                (start (when use-region (region-beginning)))
                                (end   (when use-region (region-end)))
                                (line-start (line-number-at-pos start))
                                line-end)
                           (when use-region
                             ;; Avoid adding an extra blank line to the selection.
                             ;; This happens when point or mark is at the start of the next line.
                             ;;
                             ;; When selection is from bottom to top, exchange point and mark
                             ;; so that the `point' and `(region-end)' are the same.
                             (when (< (point) (mark))
                               (exchange-point-and-mark))
                             (when (= end (line-beginning-position))
                               ;; Go up and avoid the blank line
                               (setq end (1- end)))
                             (setq line-end (line-number-at-pos end))
                             (when (<= line-end line-start)
                               (setq line-end nil)))
                           (list line-start line-end)))))))

(defun git-link--new (link)
  (kill-new link)
  ;; prevent URL escapes from being interpreted as format strings
  (message (replace-regexp-in-string "%" "%%" link t t))
  (setq deactivate-mark t)
  (when git-link-open-in-browser
    (browse-url link)))

(defun git-link-gitlab (hostname dirname filename branch commit start end)
  (format "https://%s/%s/blob/%s/%s"
	  hostname
	  dirname
	  (or branch commit)
          (concat filename
                  (when start
                    (concat "#"
                            (if end
                                (format "L%s-%s" start end)
                              (format "L%s" start)))))))

(defun git-link-github (hostname dirname filename branch commit start end)
  (format "https://%s/%s/%s/%s/%s"
   hostname
   dirname
   (if (git-link--using-magit-blame) "blame" "blob")
   (or branch commit)
   (concat filename
                  (when start
                    (concat "#"
                            (if end
                                (format "L%s-L%s" start end)
                              (format "L%s" start)))))))

(defun git-link-commit-github (hostname dirname commit)
  (format "https://%s/%s/commit/%s"
	  hostname
	  dirname
	  commit))

(defun git-link-gitorious (hostname dirname filename _branch commit start _end)
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

(defun git-link-bitbucket (hostname dirname filename _branch commit start end)
  ;; ?at=branch-name
  (format "https://%s/%s/src/%s/%s"
          hostname
          dirname
          commit
          (if (string= "" (file-name-nondirectory filename))
              filename
            (concat filename
                    "#"
                    (file-name-nondirectory filename)
                    (when start
                      (if end
                          (format "-%s:%s" start end)
                        (format "-%s" start)))))))

(defun git-link-commit-bitbucket (hostname dirname commit)
  ;; ?at=branch-name
  (format "https://%s/%s/commits/%s"
	  hostname
	  dirname
	  commit))

(defun git-link--select-remote ()
  (if current-prefix-arg
      (git-link--read-remote)
    (git-link--remote)))

;;;###autoload
(defun git-link (remote start end)
  "Create a URL representing the current buffer's location in its
GitHub/Bitbucket/GitLab/... repository at the current line number
or active region. The URL will be added to the kill ring. If
`git-link-open-in-browser' is non-`nil' also call `browse-url'.

With a prefix argument prompt for the remote's name.
Defaults to \"origin\"."
  (interactive (let* ((remote (git-link--select-remote))
                      (region (git-link--get-region)))
                 (list remote (car region) (cadr region))))
  (let (filename branch commit handler remote-info (remote-url (git-link--remote-url remote)))
    (if (null remote-url)
        (message "Remote `%s' not found" remote)

      (setq remote-info (git-link--parse-remote remote-url)
            filename    (git-link--relative-filename)
            branch      (git-link--branch)
            commit      (git-link--commit)
            handler     (git-link--handler git-link-remote-alist (car remote-info)))

      (cond ((null filename)
             (message "Can't figure out what to link to"))
            ((null (car remote-info))
             (message "Remote `%s' contains an unsupported URL" remote))
            ((not (functionp handler))
             (message "No handler found for %s" (car remote-info)))
            ;; TODO: null ret val
            (t
             (let ((vc-revison (git-link--parse-vc-revision filename)))
               (when vc-revison
                 (setq filename (car vc-revison)
                       commit   (cdr vc-revison)))

               (git-link--new
                (funcall handler
                         (car remote-info)
                         (cadr remote-info)
                         filename
                         (if (or (git-link--using-git-timemachine) vc-revison git-link-use-commit)
                             nil
                           (url-hexify-string branch))
                         commit
                         start
                         end))))))))

;;;###autoload
(defun git-link-commit (remote)
  "Create a URL representing the commit for the hash under point
in the current buffer's GitHub/Bitbucket/GitLab/...
repository. The URL will be added to the kill ring.

With a prefix argument prompt for the remote's name.
Defaults to \"origin\"."

  (interactive (list (git-link--select-remote)))
  (let* (commit handler remote-info (remote-url (git-link--remote-url remote)))
    (if (null remote-url)
        (message "Remote `%s' not found" remote)

      (setq remote-info (git-link--parse-remote remote-url)
            commit (word-at-point)
            handler (git-link--handler git-link-commit-remote-alist (car remote-info)))

      (cond ((null (car remote-info))
             (message "Remote `%s' contains an unsupported URL" remote))
            ((not (string-match-p "[a-fA-F0-9]\\{7,40\\}" (or commit "")))
             (message "Point is not on a commit hash"))
            ((not (functionp handler))
             (message "No handler for %s" (car remote-info)))
            ;; null ret val
            ((git-link--new
              (funcall handler
                       (car remote-info)
                       (cadr remote-info)
                       (substring-no-properties commit))))))))

;;;###autoload
(defun git-link-homepage (remote)
  "Create a URL for the current buffer's REMOTE repository homepage.
The URL will be added to the kill ring.  If `git-link-open-in-browser'
is non-nil also call `browse-url'."

  (interactive (list (git-link--select-remote)))
  (let* ((remote-url (git-link--remote-url remote))
         (remote-info (when remote-url (git-link--parse-remote remote-url))))
    (if remote-info
	;;TODO: shouldn't assume https, need service specific handler like others
	(git-link--new (format "https://%s/%s" (car remote-info) (cadr remote-info)))
      (error  "Remote `%s' is unknown or contains an unsupported URL" remote))))

(provide 'git-link)
;;; git-link.el ends here
