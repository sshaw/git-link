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
                 (git-link-bitbucket "bitbucket.org" "atlassian/atlascode" "README.md" "_branch" "a-commit-hash" 1 nil)))

  (should (equal "https://bitbucket.org/atlassian/atlascode/annotate/a-commit-hash/README.md#README.md-1:33"
                 (git-link-bitbucket "bitbucket.org" "atlassian/atlascode" "README.md" "_branch" "a-commit-hash" 1 33)))

  (should (equal "https://bitbucket.org/atlassian/atlascode/src/a-commit-hash/.gitignore#.gitignore-1:33"
                 (git-link-bitbucket "bitbucket.org" "atlassian/atlascode" ".gitignore" "_branch" "a-commit-hash" 1 33))))

(ert-deftest git-link--should-render-via-bitbucket-annotate ()
  (should (equal "annotate"
                 (git-link--should-render-via-bitbucket-annotate "README.md")))

  (should (equal "src"
                 (git-link--should-render-via-bitbucket-annotate "a-cool-new-file.txt"))))

(ert-deftest git-link--parse-vc-revision-test ()
  ;; Test vc-revision-other-window pattern (single tildes)
  (should (equal '("/tmp/git-link-test/test.txt" . "abc123")
                 (git-link--parse-vc-revision "/tmp/git-link-test/test.txt.~abc123~")))
  
  ;; Test that it returns nil for non-matching patterns
  (should (equal nil
                 (git-link--parse-vc-revision "/tmp/git-link-test/test.txt")))
  
  ;; Test that it returns nil for log-view pattern (double tildes)
  (should (equal nil
                 (git-link--parse-vc-revision "/tmp/git-link-test/test.txt.~~abc123~~"))))

(ert-deftest git-link--parse-log-view-revision-test ()
  ;; Test log-view-find-revision pattern (double tildes)
  (should (equal '("/tmp/git-link-test/test.txt" . "abc123")
                 (git-link--parse-log-view-revision "/tmp/git-link-test/test.txt.~~abc123~~")))
  
  ;; Test that it returns nil for non-matching patterns
  (should (equal nil
                 (git-link--parse-log-view-revision "/tmp/git-link-test/test.txt")))
  
  ;; Test that it returns nil for vc-revision pattern (single tildes)
  (should (equal nil
                 (git-link--parse-log-view-revision "/tmp/git-link-test/test.txt.~abc123~")))
  
  ;; Test with complex hash
  (should (equal '("/tmp/git-link-test/test.txt" . "1a2b3c4d5e6f7890abcdef")
                 (git-link--parse-log-view-revision "/tmp/git-link-test/test.txt.~~1a2b3c4d5e6f7890abcdef~~")))
  
  ;; Test that it handles filenames with dots
  (should (equal '("/tmp/git-link-test/file.with.dots.php" . "abc123")
                 (git-link--parse-log-view-revision "/tmp/git-link-test/file.with.dots.php.~~abc123~~"))))
