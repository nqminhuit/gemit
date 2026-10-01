# gemit

Generate Conventional Commit messages from the staged diff inside Magit.
`M-g` in a git-commit buffer inserts a generated message; `g` in the
magit-commit transient commits with one directly. Besides Magit, gemit uses
only Emacs built-ins.

## Local-first setup

By default, gemit (`gemit-backend` = `auto`) tries a compatible llama.cpp
server first. For example, start one yourself with:

```sh
llama-server -m /path/to/model.gguf --host 127.0.0.1 --port 8080
```

The model must be loaded and the server ready before generation. Availability
means the server's `/health` endpoint returns JSON with `{"status":"ok"}`;
gemit does not start a server or download models. With no configured model,
gemit reads `/v1/models` and requires a single unambiguous model ID. For
multiple IDs, set an explicit alias.

Defaults and customization:

```elisp
(setopt gemit-backend 'auto) ; auto, local, or gemini
(setopt gemit-local-url "http://127.0.0.1:8080") ; server root URL
(setopt gemit-local-model nil) ; discover one loaded model, or set an alias
(setopt gemit-local-availability-timeout 2) ; seconds per health/discovery request
(setopt gemit-local-generation-timeout 120) ; seconds for generation
(setopt gemit-model "gemini-flash-lite-latest") ; Google model for Gemini
```

The URL is normalized to a single trailing slash. `local` never uses Gemini
and reports local errors without prompting. `gemini` bypasses the local check
and sends directly to Google. `auto` asks for confirmation on each local
failure; answering yes sends the **entire staged diff** to Gemini.

## Gemini setup

When using `auto` and accepting a cloud fallback, or using `gemini`, provide a
key from <https://aistudio.google.com/apikey> in one of these ways (first
one found wins):

The Gemini model defaults to `gemini-flash-lite-latest` and can be changed
with `gemit-model`.

1. `(setopt gemit-api-key "KEY")` -- or a file holding the key,
2. `export GEMINI_API_KEY_GEMIT=...`,
3. an auth-source entry (`~/.authinfo.gpg`):
   `machine generativelanguage.googleapis.com login apikey password <KEY>`,
4. nothing -- gemit prompts once per Emacs session. A key the API rejects is
   forgotten so the next attempt asks again.

**Doom:**

```elisp
;; packages.el
(package! gemit :recipe (:host github :repo "nqminhuit/gemit"))

;; config.el
(setopt gemit-backend 'auto)
(with-eval-after-load 'magit (gemit-install))
```

**Vanilla:**

```elisp
(add-to-list 'load-path "/path/to/gemit")
(require 'gemit)
(with-eval-after-load 'magit (gemit-install))
```

## Privacy

Generation sends the **entire staged diff**, including paths and contents, to
the endpoint for the selected backend. With the default `auto` backend, the
diff goes to Gemini only after an explicit yes to a per-request fallback
prompt; `gemit-backend` = `gemini` intentionally sends it directly. Local
generation sends the diff to `gemit-local-url`; a custom non-loopback URL is
not private or local just because the option is named `gemit-local-url`.
Unstaged changes, history, and other files are not sent. Review
`git diff --cached` and never stage secrets.

## Tests

```sh
emacs -Q --batch -L . -L test -l test/gemit-test.el \
  -f ert-run-tests-batch-and-exit
```

HTTP and timer behavior is tested with deterministic stubs; no model, live
Magit buffer, or network access is required.
