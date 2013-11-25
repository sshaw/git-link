# git-link

An interactive Emacs function that creates a URL representing the current buffer's location
in its GitHub/Bitbucket/Gitorious/... repository at the current line number or active region.
The resulting URL will be added to the kill ring.

## Usage

`M-x git-link`

With a prefix argument prompt for the remote's name. Defaults to `"origin"`.

### Default remote

The default remote name is `"origin"`. This can be changed by setting `git-link-default-remote`.

### Supported Services

* [Bitbucket](http://bitbucket.com)
* [GitHub](http://github.com)
* [Gitorious](http://gitorious.org)

### Building Links and Adding Services

`git-link-remote-alist` maps remotes' hostnames to a function capable of
creating a URL on that host. To add (or modify) how URLs are created for a given host add
the appropriate function object to this list.

The function's signature is:

`HOSTNAME DIRNAME FILENAME BRANCH COMMIT START END`

* `HOSTNAME` hostname of the remote
* `DIRNAME` directory portion of the remote
* `FILENAME` source file, relative to `DIRNAME`
* `BRANCH` active branch, may be `nil` if the repo's in "detached HEAD" state
* `COMMIT` SHA of the latest commit
* `START` starting line number
* `END`  ending line number, `nil` unless region is active
