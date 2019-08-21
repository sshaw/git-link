;; Tests for git-link

(require 'ert)
(require 'git-link)

(ert-deftest git-link--parse-remote-test ()
  (should (equal '("foo" "")
                 (git-link--parse-remote "foo"))))

  (should (equal '("github.com" "")
                 (git-link--parse-remote "https://github.com")))

  (should (equal '("github.com" "/")
                 (git-link--parse-remote "https://github.com/")))

  (should (equal '("github.com" "sshaw_/selfie_formatter")
                 (git-link--parse-remote "git@github.com:sshaw_/selfie_formatter.git")))

  (should (equal '("github.com" "ruby/ruby")
                 (git-link--parse-remote "https://github.com/ruby/ruby.git")))

  (should (equal '("github.com" "sshaw/copy-as-format")
                 (git-link--parse-remote "https://github.com:9999/sshaw/copy-as-format.git")))

  (should (equal '("orgmode.org" "org-mode")
                 (git-link--parse-remote "https://orgmode.org/org-mode.git")))

  (should (equal '("gitlab.com" "weshmashian/emacs.d")
                 (git-link--parse-remote "https://gitlab.com/weshmashian/emacs.d")))

  (should (equal '("foo-bar.github.com" "sshaw/foo/x")
                 (git-link--parse-remote "https://user:password@foo-bar.github.com/sshaw/foo/x.git")))

  (should (equal '("msazure.visualstudio.com" "project/_git/repo")
                 (git-link--parse-remote "msazure@vs-ssh.visualstudio.com:v3/msazure/project/repo")))

  (should (equal '("msazure.visualstudio.com" "DefaultCollection/project/_git/repo")
                 (git-link--parse-remote "https://msazure.visualstudio.com/DefaultCollection/project/_git/repo")))

  (should (equal '("dev.azure.com" "r-darwish/project/_git/repo")
                 (git-link--parse-remote "git@ssh.dev.azure.com:v3/r-darwish/project/repo")))

  (should (equal '("dev.azure.com" "r-darwish/project/_git/repo")
                 (git-link--parse-remote "https://r-darwish@dev.azure.com/r-darwish/project/_git/repo")))
