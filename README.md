# git-link

Interactive Emacs functions that create URLs for files and commits in
GitHub/Bitbucket/Gitorious/... repositories. `git-link` returns the URL for the
current buffer's file location at the current line number or active region.
`git-link-commit` returns the URL for a commit. URLs are added to the kill ring.

## Usage

`M-x git-link` and `M-x git-link-commit`

With a prefix argument prompt for the remote's name. Defaults to `"origin"`.

### Settings

#### `git-link-default-remote`

Name of the remote branch to link to, defaults to `"origin"`.

#### `git-link-open-in-browser`

If non-`nil` also open link in browser via `browse-url`, defaults to `nil`.

#### `git-link-use-commit`

If non-`nil` use the latest commit's hash in the link instead of the branch name, defaults to `nil`.

### Supported Services

* [Bitbucket](http://bitbucket.com)
* [GitHub](http://github.com)
* [Gitorious](http://gitorious.org)

### Building Links and Adding Services

`git-link-remote-alist` and `git-link-commit-remote-alist` map remotes'
hostnames to a function capable of creating a URL on that host. To add (or
modify) how URLs are created for a given host add the appropriate function
objects to this lists.

If you use a self-hosted version of one of the supported services, you
can configure the link function alists for the hostname at which that
service is hosted.  For example, for a GitHub Enterprise instance at
`github.example.com`, you could add the following to your `.emacs` file.

    (eval-after-load "git-link"
      '(progn
        (add-to-list 'git-link-remote-alist
          '("github.example.com" git-link-github))
        (add-to-list 'git-link-commit-remote-alist
          '("github.example.com" git-link-commit-github))))

The `git-link` signature is:

`HOSTNAME DIRNAME FILENAME BRANCH COMMIT START END`

* `HOSTNAME` hostname of the remote
* `DIRNAME` directory portion of the remote
* `FILENAME` source file, relative to `DIRNAME`
* `BRANCH` active branch, may be `nil` if the repo's in "detached HEAD" state
* `COMMIT` SHA of the latest commit
* `START` starting line number
* `END`  ending line number, `nil` unless region is active

The `git-link-commit` signature is:

`HOSTNAME DIRNAME COMMIT`

* `HOSTNAME` hostname of the remote
* `DIRNAME` directory portion of the remote
* `COMMIT` SHA of the commit

### TODO

* Consolidate `git-link-*-alist`s
* `git-link-grep`
