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

(ert-deftest git-link-github ()
  "Test git-link-github function."
  ;; Basic file link with branch
  (should (equal "https://github.com/user/repo/blob/master/file.txt"
                 (git-link-github "https://github.com" "user/repo" "file.txt" "master" "abc123" nil nil)))

  ;; File link with single line number
  (should (equal "https://github.com/user/repo/blob/master/file.txt#L10"
                 (git-link-github "https://github.com" "user/repo" "file.txt" "master" "abc123" 10 nil)))

  ;; File link with line range
  (should (equal "https://github.com/user/repo/blob/master/file.txt#L10-L20"
                 (git-link-github "https://github.com" "user/repo" "file.txt" "master" "abc123" 10 20)))

  ;; File link with commit instead of branch
  (should (equal "https://github.com/user/repo/blob/abc123/file.txt"
                 (git-link-github "https://github.com" "user/repo" "file.txt" nil "abc123" nil nil)))

  ;; Test with custom scheme
  (should (equal "http://internal.github.com/user/repo/blob/master/file.txt"
                 (git-link-github "http://internal.github.com" "user/repo" "file.txt" "master" "abc123" nil nil))))

(ert-deftest git-link-gitlab ()
  "Test git-link-gitlab function."
  ;; Basic file link with branch
  (should (equal "https://gitlab.com/user/repo/-/blob/master/file.txt"
                 (git-link-gitlab "https://gitlab.com" "user/repo" "file.txt" "master" "abc123" nil nil)))

  ;; File link with single line number
  (should (equal "https://gitlab.com/user/repo/-/blob/master/file.txt#L10"
                 (git-link-gitlab "https://gitlab.com" "user/repo" "file.txt" "master" "abc123" 10 nil)))

  ;; File link with line range
  (should (equal "https://gitlab.com/user/repo/-/blob/master/file.txt#L10-20"
                 (git-link-gitlab "https://gitlab.com" "user/repo" "file.txt" "master" "abc123" 10 20)))

  ;; File link with commit instead of branch
  (should (equal "https://gitlab.com/user/repo/-/blob/abc123/file.txt"
                 (git-link-gitlab "https://gitlab.com" "user/repo" "file.txt" nil "abc123" nil nil))))

(ert-deftest git-link-codeberg ()
  "Test git-link-codeberg function."
  ;; Basic file link with branch
  (should (equal "https://codeberg.org/user/repo/src/master/file.txt"
                 (git-link-codeberg "https://codeberg.org" "user/repo" "file.txt" "master" "abc123" nil nil)))

  ;; File link with single line number
  (should (equal "https://codeberg.org/user/repo/src/master/file.txt#L10"
                 (git-link-codeberg "https://codeberg.org" "user/repo" "file.txt" "master" "abc123" 10 nil)))

  ;; File link with line range
  (should (equal "https://codeberg.org/user/repo/src/master/file.txt#L10-L20"
                 (git-link-codeberg "https://codeberg.org" "user/repo" "file.txt" "master" "abc123" 10 20)))

  ;; File link with commit instead of branch
  (should (equal "https://codeberg.org/user/repo/src/commit/abc123/file.txt"
                 (git-link-codeberg "https://codeberg.org" "user/repo" "file.txt" nil "abc123" nil nil))))


(ert-deftest git-link-savannah ()
  "Test git-link-savannah function."
  ;; Basic file link with branch
  (should (equal "https://git.savannah.gnu.org/cgit/repo.git/tree/file.txt?h=master"
                 (git-link-savannah "https://git.savannah.gnu.org" "repo" "file.txt" "master" "abc123" nil nil)))

  ;; File link with single line number
  (should (equal "https://git.savannah.gnu.org/cgit/repo.git/tree/file.txt?h=master#n10"
                 (git-link-savannah "https://git.savannah.gnu.org" "repo" "file.txt" "master" "abc123" 10 nil)))

  ;; File link with commit instead of branch
  (should (equal "https://git.savannah.gnu.org/cgit/repo.git/tree/file.txt?h=abc123"
                 (git-link-savannah "https://git.savannah.gnu.org" "repo" "file.txt" nil "abc123" nil nil))))

(ert-deftest git-link-googlesource ()
  "Test git-link-googlesource function."
  ;; Basic file link with branch
  (should (equal "https://go.googlesource.com/go/+/master/file.txt"
                 (git-link-googlesource "https://go.googlesource.com" "go" "file.txt" "master" "abc123" nil nil)))

  ;; File link with single line number
  (should (equal "https://go.googlesource.com/go/+/master/file.txt#10"
                 (git-link-googlesource "https://go.googlesource.com" "go" "file.txt" "master" "abc123" 10 nil)))

  ;; File link with commit instead of branch
  (should (equal "https://go.googlesource.com/go/+/abc123/file.txt"
                 (git-link-googlesource "https://go.googlesource.com" "go" "file.txt" nil "abc123" nil nil))))

(ert-deftest git-link-azure ()
  "Test git-link-azure function."
  ;; Basic file link with branch
  (should (equal "https://dev.azure.com/project/_git/repo?path=%2Ffile.txt&version=GBmaster&line=&lineEnd=&lineStartColumn=1&lineEndColumn=9999&lineStyle=plain"
                 (git-link-azure "https://dev.azure.com" "project/_git/repo" "file.txt" "master" "abc123" nil nil)))

  ;; File link with single line number
  (should (equal "https://dev.azure.com/project/_git/repo?path=%2Ffile.txt&version=GBmaster&line=10&lineEnd=10&lineStartColumn=1&lineEndColumn=9999&lineStyle=plain"
                 (git-link-azure "https://dev.azure.com" "project/_git/repo" "file.txt" "master" "abc123" 10 nil)))

  ;; File link with line range
  (should (equal "https://dev.azure.com/project/_git/repo?path=%2Ffile.txt&version=GBmaster&line=10&lineEnd=20&lineStartColumn=1&lineEndColumn=9999&lineStyle=plain"
                 (git-link-azure "https://dev.azure.com" "project/_git/repo" "file.txt" "master" "abc123" 10 20)))

  ;; File link with commit instead of branch
  (should (equal "https://dev.azure.com/project/_git/repo?path=%2Ffile.txt&version=GCabc123&line=&lineEnd=&lineStartColumn=1&lineEndColumn=9999&lineStyle=plain"
                 (git-link-azure "https://dev.azure.com" "project/_git/repo" "file.txt" nil "abc123" nil nil))))

(ert-deftest git-link-sourcehut ()
  "Test git-link-sourcehut function."
  ;; Basic file link with branch
  (should (equal "https://git.sr.ht/~user/repo/tree/master/file.txt"
                 (git-link-sourcehut "https://git.sr.ht" "~user/repo" "file.txt" "master" "abc123" nil nil)))

  ;; File link with single line number
  (should (equal "https://git.sr.ht/~user/repo/tree/master/file.txt#L10"
                 (git-link-sourcehut "https://git.sr.ht" "~user/repo" "file.txt" "master" "abc123" 10 nil)))

  ;; File link with line range
  (should (equal "https://git.sr.ht/~user/repo/tree/master/file.txt#L10-20"
                 (git-link-sourcehut "https://git.sr.ht" "~user/repo" "file.txt" "master" "abc123" 10 20)))

  ;; File link with commit instead of branch
  (should (equal "https://git.sr.ht/~user/repo/tree/abc123/file.txt"
                 (git-link-sourcehut "https://git.sr.ht" "~user/repo" "file.txt" nil "abc123" nil nil))))

(ert-deftest git-link-sourcegraph ()
  "Test git-link-sourcegraph function."
  ;; Basic file link with branch
  (should (equal "https://sourcegraph.com/user/repo@master/-/blob/file.txt"
                 (git-link-sourcegraph "https://sourcegraph.com" "user/repo" "file.txt" "master" "abc123" nil nil)))

  ;; File link with single line number
  (should (equal "https://sourcegraph.com/user/repo@master/-/blob/file.txt#L10"
                 (git-link-sourcegraph "https://sourcegraph.com" "user/repo" "file.txt" "master" "abc123" 10 nil)))

  ;; File link with line range
  (should (equal "https://sourcegraph.com/user/repo@master/-/blob/file.txt#L10-20"
                 (git-link-sourcegraph "https://sourcegraph.com" "user/repo" "file.txt" "master" "abc123" 10 20)))

  ;; File link with commit instead of branch
  (should (equal "https://sourcegraph.com/user/repo@abc123/-/blob/file.txt"
                 (git-link-sourcegraph "https://sourcegraph.com" "user/repo" "file.txt" nil "abc123" nil nil))))

(ert-deftest git-link-codecommit ()
  "Test git-link-codecommit function."
  ;; Basic file link with branch
  (should (equal "https://us-west-2.console.aws.amazon.com/codesuite/codecommit/repositories/repo/browse/refs/heads/master/--/file.txt"
                 (git-link-codecommit "https://us-west-2.console.aws.amazon.com" "codesuite/codecommit/repositories/repo" "file.txt" "master" "abc123" nil nil)))

  ;; File link with single line number
  (should (equal "https://us-west-2.console.aws.amazon.com/codesuite/codecommit/repositories/repo/browse/refs/heads/master/--/file.txt?lines=10-10"
                 (git-link-codecommit "https://us-west-2.console.aws.amazon.com" "codesuite/codecommit/repositories/repo" "file.txt" "master" "abc123" 10 nil)))

  ;; File link with line range
  (should (equal "https://us-west-2.console.aws.amazon.com/codesuite/codecommit/repositories/repo/browse/refs/heads/master/--/file.txt?lines=10-20"
                 (git-link-codecommit "https://us-west-2.console.aws.amazon.com" "codesuite/codecommit/repositories/repo" "file.txt" "master" "abc123" 10 20)))

  ;; File link with commit instead of branch
  (should (equal "https://us-west-2.console.aws.amazon.com/codesuite/codecommit/repositories/repo/browse/refs/heads/abc123/--/file.txt"
                 (git-link-codecommit "https://us-west-2.console.aws.amazon.com" "codesuite/codecommit/repositories/repo" "file.txt" nil "abc123" nil nil))))

(ert-deftest git-link-integration-test ()
  "Test that handler functions work with URL schemes from git-link--web-host."

  ;; Test GitHub handler with scheme-enabled hostname
  (should (equal "https://github.com/user/repo/blob/master/file.txt"
                 (git-link-github "https://github.com" "user/repo" "file.txt" "master" "abc123" nil nil)))

  ;; Test GitHub handler with custom scheme
  (should (equal "http://internal.github.com/user/repo/blob/master/file.txt"
                 (git-link-github "http://internal.github.com" "user/repo" "file.txt" "master" "abc123" nil nil))))

;; Tests for commit functions
(ert-deftest git-link-commit-github ()
  "Test git-link-commit-github function."
  (should (equal "https://github.com/user/repo/commit/abc123"
                 (git-link-commit-github "https://github.com" "user/repo" "abc123"))))

(ert-deftest git-link-commit-bitbucket ()
  "Test git-link-commit-bitbucket function."
  (should (equal "https://bitbucket.org/user/repo/commits/abc123"
                 (git-link-commit-bitbucket "https://bitbucket.org" "user/repo" "abc123"))))

(ert-deftest git-link-commit-gitlab ()
  "Test git-link-commit-gitlab function."
  (should (equal "https://gitlab.com/user/repo/-/commit/abc123"
                 (git-link-commit-gitlab "https://gitlab.com" "user/repo" "abc123"))))

(ert-deftest git-link-commit-codeberg ()
  "Test git-link-commit-codeberg function."
  (should (equal "https://codeberg.org/user/repo/commit/abc123"
                 (git-link-commit-codeberg "https://codeberg.org" "user/repo" "abc123"))))



(ert-deftest git-link-commit-savannah ()
  "Test git-link-commit-savannah function."
  (should (equal "https://git.savannah.gnu.org/cgit/repo.git/commit/?id=abc123"
                 (git-link-commit-savannah "https://git.savannah.gnu.org" "repo" "abc123"))))

(ert-deftest git-link-commit-googlesource ()
  "Test git-link-commit-googlesource function."
  (should (equal "https://go.googlesource.com/go/+/abc123"
                 (git-link-commit-googlesource "https://go.googlesource.com" "go" "abc123"))))

(ert-deftest git-link-commit-sourcegraph ()
  "Test git-link-commit-sourcegraph function."
  (should (equal "https://sourcegraph.com/user/repo/-/commit/abc123"
                 (git-link-commit-sourcegraph "https://sourcegraph.com" "user/repo" "abc123"))))

(ert-deftest git-link-commit-codecommit ()
  "Test git-link-commit-codecommit function."
  (should (equal "https://us-west-2.console.aws.amazon.com/codesuite/codecommit/repositories/repo/commit/abc123"
                 (git-link-commit-codecommit "https://us-west-2.console.aws.amazon.com" "codesuite/codecommit/repositories/repo" "abc123"))))

;; Tests for homepage functions
(ert-deftest git-link-homepage-github ()
  "Test git-link-homepage-github function."
  (should (equal "https://github.com/user/repo"
                 (git-link-homepage-github "https://github.com" "user/repo"))))

(ert-deftest git-link-homepage-savannah ()
  "Test git-link-homepage-savannah function."
  (should (equal "https://git.savannah.gnu.org/cgit/repo.git/"
                 (git-link-homepage-savannah "https://git.savannah.gnu.org" "repo"))))

(ert-deftest git-link-homepage-codecommit ()
  "Test git-link-homepage-codecommit function."
  (should (equal "https://us-west-2.console.aws.amazon.com/codesuite/codecommit/repositories/repo/browse"
                 (git-link-homepage-codecommit "https://us-west-2.console.aws.amazon.com" "codesuite/codecommit/repositories/repo"))))

(ert-deftest git-link-interactive-simulation ()
  "Test interactive git-link function call with cursor at line 3 using real git repo."
  (let ((test-dir (make-temp-file "git-link-test" t)))
    (unwind-protect
        (let ((default-directory test-dir)
              git-link-add-to-kill-ring  ; Don't add to kill ring during test
              git-link-open-in-browser)  ; Don't open browser during test
          
          ;; Set up a real git repository
          (shell-command "git init")
          (shell-command "git config user.name 'Test User'")
          (shell-command "git config user.email 'test@example.com'")
          (shell-command "git remote add origin https://github.com/user/repo.git")
          
          ;; Create test file with content
          (with-temp-file (expand-file-name "test-file.txt" test-dir)
            (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n"))
          
          ;; Add and commit the file
          (shell-command "git add test-file.txt")
          (shell-command "git commit -m 'Initial commit'")
          
          ;; Create a buffer visiting the file and position cursor at line 3
          (with-current-buffer (find-file-noselect (expand-file-name "test-file.txt" test-dir))
            (goto-char (point-min))
            (forward-line 2) ; Move to line 3
            
            ;; Call git-link interactively
            (let ((result (git-link "origin" 3 nil))) ; Line 3, no end line
              ;; Verify the result is the complete expected URL
              (should (equal "https://github.com/user/repo/blob/master/test-file.txt#L3" result)))
            
            ;; Clean up buffer
            (kill-buffer)))
      
      ;; Clean up temporary directory
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))


(ert-deftest git-link-homepage-codeberg ()
  "Test git-link-homepage-codeberg function."
  (should (equal "https://codeberg.org/user/repo"
                 (git-link-homepage-codeberg "https://codeberg.org" "user/repo"))))
