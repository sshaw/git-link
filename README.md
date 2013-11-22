# git-link

An interactive Emacs function that creates a URL representing the current buffer's location
in its GitHub/Bitbucket/... repository at the current line number or active region.
The resulting URL will be added to the kill ring.

## Usage

`M-x git-link`

With a prefix argument prompt for the remote's name. Defaults to "origin".

