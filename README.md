# git-link

Interactive Emacs functions that create URLs for files and commits in
GitHub/Bitbucket/Gitorious/... repositories. `git-link' returns the URL for the
current buffer's file location at the current line number or active region.
`git-commit-link' returns the URL for a commit. URLs are added to the kill ring.

## Usage

`M-x git-link` and `M-x git-commit-link`

With a prefix argument prompt for the remote's name. Defaults to `"origin"`.

### Default remote

The default remote name is `"origin"`. This can be changed by setting `git-link-default-remote`.

### Supported Services

* [Bitbucket](http://bitbucket.com)
* [GitHub](http://github.com)
* [Gitorious](http://gitorious.org)

### Building Links and Adding Services

`git-link-remote-alist` and `git-commit-link-remote-alist` maps remotes'
hostnames to a function capable of creating a URL on that host. To add (or
modify) how URLs are created for a given host add the appropriate function
objects to this lists.

The `git-link` signature is:

`HOSTNAME DIRNAME FILENAME BRANCH COMMIT START END`

* `HOSTNAME` hostname of the remote
* `DIRNAME` directory portion of the remote
* `FILENAME` source file, relative to `DIRNAME`
* `BRANCH` active branch, may be `nil` if the repo's in "detached HEAD" state
* `COMMIT` SHA of the latest commit
* `START` starting line number
* `END`  ending line number, `nil` unless region is active

The `git-commit-link` signature is:

`HOSTNAME DIRNAME COMMIT`

* `HOSTNAME` hostname of the remote
* `DIRNAME` directory portion of the remote
* `COMMIT` SHA of the commit
