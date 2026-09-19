# gemit

Generate conventional commit messages from staged diffs with Google
Gemini, from inside Magit. No `gptel` dependency -- besides Magit, only
Emacs built-ins are used.

`M-g` in a git-commit buffer inserts a generated message; `g` in the
magit-commit transient commits with one directly.

## Setup

Get a key at <https://aistudio.google.com/apikey>, then provide it one
of these ways (first one found wins):

1. `(setopt gemit-api-key "KEY")` -- or a file holding the key,
2. `export GEMINI_API_KEY_GEMIT=...`,
3. an auth-source entry (`~/.authinfo.gpg`):
   `machine generativelanguage.googleapis.com login apikey password <KEY>`,
4. nothing -- you are prompted once per Emacs session.

**Doom:**

```elisp
;; packages.el
(package! gemit :recipe (:host github :repo "nqminhuit/gemit"))

;; config.el
(setopt gemit-model "gemini-flash-lite-latest")
(with-eval-after-load 'magit (gemit-install))
```

**Vanilla:**

```elisp
(add-to-list 'load-path "/path/to/gemit")
(require 'gemit)
(with-eval-after-load 'magit (gemit-install))
```

## Privacy

Every generation sends your **entire staged diff** (file paths and
contents) to Google's Gemini API, plus the prompt. Unstaged changes,
history, and everything else on disk stay local. Review
`git diff --cached` before generating, and never stage secrets.

## Tests

```sh
emacs -Q --batch -L . -L test -l test/gemit-test.el -f ert-run-tests-batch-and-exit
```

Only pure helpers are tested; the network call and anything needing
live Magit buffers are stubbed or excluded (see `test/gemit-test.el`).
