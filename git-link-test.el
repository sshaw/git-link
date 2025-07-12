;; Tests for git-link

(require 'ert)
(require 'git-link)

(ert-deftest git-link--parse-remote-test ()
  (should (equal '("foo" "")
                 (git-link--parse-remote "foo")))

  (should (equal '("github.com" "")
                 (git-link--parse-remote "https://github.com")))

  (should (equal '("github.com" "/")
                 (git-link--parse-remote "https://github.com/")))

  (should (equal '("github.com" "sshaw_/selfie_formatter")
                 (git-link--parse-remote "git@github.com:sshaw_/selfie_formatter.git")))

  (should (equal '("github.com" "ruby/ruby")
                 (git-link--parse-remote "https://github.com/ruby/ruby.git")))

  (should (equal '("github.com" "ruby/ruby")
                 (git-link--parse-remote "git+ssh://git@github.com/ruby/ruby.git")))

  (should (equal '("github.com" "sshaw/copy-as-format")
                 (git-link--parse-remote "https://github.com:9999/sshaw/copy-as-format.git")))

  (should (equal '("github.com" "ScreenStaring/Some-Thing")
                 (git-link--parse-remote "git@github.com:ScreenStaring/Some-Thing.git")))

  (should (equal '("orgmode.org" "org-mode")
                 (git-link--parse-remote "https://orgmode.org/org-mode.git")))

  (should (equal '("gitlab.com" "weshmashian/emacs.d")
                 (git-link--parse-remote "https://gitlab.com/weshmashian/emacs.d")))

  (should (equal '("codeberg.org" "takeonrules/emacs.d")
                 (git-link--parse-remote "https://codeberg.org/takeonrules/emacs.d")))

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

  (should (equal '("git.sv.gnu.org" "emacs")
                 (git-link--parse-remote "git://git.sv.gnu.org/emacs.git")))

  (should (equal '("git.savannah.gnu.org" "emacs")
                 (git-link--parse-remote "https://git.savannah.gnu.org/git/emacs.git")))

  (should (equal '("git.savannah.gnu.org" "emacs")
                 (git-link--parse-remote "ssh://git.savannah.gnu.org/srv/git/emacs.git")))

  (should (equal '("git.savannah.gnu.org" "emacs")
                 (git-link--parse-remote "git://git.savannah.gnu.org/emacs.git")))

  (should (equal '("us-west-2.console.aws.amazon.com" "codesuite/codecommit/repositories/TestRepo")
                 (git-link--parse-remote "ssh://git-codecommit.us-west-2.amazonaws.com/v1/repos/TestRepo")))

  (should (equal '("go.googlesource.com" "go")
                 (git-link--parse-remote "https://go.googlesource.com/go")))

  (should (equal '("bitbucket.org" "atlassianlabs/atlascode")
                 (git-link--parse-remote "https://bitbucket.org/atlassianlabs/atlascode.git")))

  (should (equal '("bitbucket.org" "atlassianlabs/atlascode")
                 (git-link--parse-remote "ssh://bitbucket.org:atlassianlabs/atlascode.git"))))

(ert-deftest git-link-bitbucket ()
  (should (equal "https://bitbucket.org/atlassian/atlascode/annotate/a-commit-hash/README.md#README.md-1"
                 (git-link-bitbucket "https://bitbucket.org" "atlassian/atlascode" "README.md" "_branch" "a-commit-hash" 1 nil)))

  (should (equal "https://bitbucket.org/atlassian/atlascode/annotate/a-commit-hash/README.md#README.md-1:33"
                 (git-link-bitbucket "https://bitbucket.org" "atlassian/atlascode" "README.md" "_branch" "a-commit-hash" 1 33)))

  (should (equal "https://bitbucket.org/atlassian/atlascode/src/a-commit-hash/.gitignore#.gitignore-1:33"
                 (git-link-bitbucket "https://bitbucket.org" "atlassian/atlascode" ".gitignore" "_branch" "a-commit-hash" 1 33))))

(ert-deftest git-link--should-render-via-bitbucket-annotate ()
  (should (equal "annotate"
                 (git-link--should-render-via-bitbucket-annotate "README.md")))

  (should (equal "src"
                 (git-link--should-render-via-bitbucket-annotate "a-cool-new-file.txt"))))

(ert-deftest git-link--web-host-test ()
  "Test git-link--web-host function with various scenarios."

  ;; Test with empty alist - should return the original host
  (let ((git-link-web-host-alist nil))
    (should (equal "https://github.com"
                   (git-link--web-host "github.com"))))

  ;; Test with alist entry without scheme - should prepend https://
  (let ((git-link-web-host-alist '(("github\\.com" . "web.github.com"))))
    (should (equal "https://web.github.com"
                   (git-link--web-host "github.com"))))

  ;; Test with alist entry that already has https scheme - should use as-is
  (let ((git-link-web-host-alist '(("ssh\\.gitlab\\.com" . "https://gitlab.com"))))
    (should (equal "https://gitlab.com"
                   (git-link--web-host "ssh.gitlab.com"))))

  ;; Test with alist entry that has http scheme - should use as-is
  (let ((git-link-web-host-alist '(("internal\\.git" . "http://internal.company.com"))))
    (should (equal "http://internal.company.com"
                   (git-link--web-host "internal.git"))))

  ;; Test with alist entry that has custom scheme - should use as-is
  (let ((git-link-web-host-alist '(("special\\.git" . "custom://special.example.com"))))
    (should (equal "custom://special.example.com"
                   (git-link--web-host "special.git"))))

  ;; Test when no match is found - should return original host with https:// prepended
  (let ((git-link-web-host-alist '(("gitlab\\.com" . "https://gitlab.com"))))
    (should (equal "https://bitbucket.org"
                   (git-link--web-host "bitbucket.org")))))

(ert-deftest git-link-integration-test ()
  "Test that handler functions work with URL schemes from git-link--web-host."

  ;; Test GitHub handler with scheme-enabled hostname
  (should (equal "https://github.com/user/repo/blob/master/file.txt"
                 (git-link-github "https://github.com" "user/repo" "file.txt" "master" "abc123" nil nil)))

  ;; Test GitHub handler with custom scheme
  (should (equal "http://internal.github.com/user/repo/blob/master/file.txt"
                 (git-link-github "http://internal.github.com" "user/repo" "file.txt" "master" "abc123" nil nil))))
