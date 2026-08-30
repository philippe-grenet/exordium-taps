# Code reviews in Emacs

Two related tools, both configured by this tap:

| | Reviews | Module | Entry point |
| --- | --- | --- | --- |
| **Part 1** | Pull requests on GitHub or GitHub Enterprise | `my-pr-review.el` (wraps [pr-review](https://github.com/blahgeek/emacs-pr-review)) | `M-x pr-review`, or `C-c M-v` from Forge |
| **Part 2** | Uncommitted local changes, typically written by a coding agent | `my-diff-review.el` (built on pr-review's internals) | `C-c M-l` |

Part 2 reuses Part 1's diff renderer, so both feel the same to drive: the same
`magit-section` buffer, the same `C-c C-c` to comment. Only the destination
differs — one posts to GitHub, the other exports markdown.

---

# Part 1 — Pull request reviews

## Why pr-review and not just Forge

Exordium ships `modules/init-forge.el`, which configures Magit's
[Forge](https://magit.vc/manual/forge/). Forge is good at browsing and
authoring, but it has no support for GitHub's *review* model:

| Capability                  | Forge          | pr-review        |
| --------------------------- | -------------- | ---------------- |
| Read PR body + discussion   | yes            | yes              |
| Approve / request changes   | yes (`/A`,`/R`)| yes (`C-c C-s`)  |
| Inline comment on a line    | **no**         | yes              |
| Reply to a review thread    | **no**         | yes              |
| Resolve / unresolve thread  | **no**         | yes              |
| Pending review batching     | **no**         | yes              |
| Notifications inbox         | yes            | yes              |

They are complementary. Keep Forge for browsing and creating PRs; use pr-review
for the actual review pass.

## Configuration

Already done, in `my-pr-review.el`:

- `use-package pr-review`, with `pr-review-forges-alist` built as
  `my/pr-review-extra-forges` followed by github.com. Anything a tap declares
  in `my/pr-review-extra-forges` therefore comes **first**, and pr-review uses
  the first entry as the default forge for `pr-review-notification` and
  `pr-review-search`.
- `my/pr-review-visit-pullreq`, bound to `C-c M-v` in `forge-topic-mode-map`
  and `magit-status-mode-map`: takes the PR at point in Forge and opens it in
  pr-review.
- `my/pr-review-use-buffer-font`, on `pr-review-mode-hook`: renders comment
  bodies in the buffer's own font. See [Fonts](#fonts) below.

`C-c M-v` was chosen because `C-c C-r` is already taken in both maps
(`forge-create-post`, `magit-next-reference`), and Exordium's own forge
bindings occupy `C-c M-{p,r,d,c}`.

## Authentication

pr-review authenticates through [ghub](https://magit.vc/manual/ghub/), which
reads a token from `~/.authinfo` (or `~/.authinfo.gpg`).

### 1. Create a token

On github.com: <https://github.com/settings/tokens> -> **Generate new token
(classic)**. Fine-grained tokens do not cover everything pr-review's GraphQL
calls need, so use a classic one.

Scopes -- exactly these three:

| Scope           | Needed for                                                       |
| --------------- | ---------------------------------------------------------------- |
| `repo`          | PR content, diffs, review threads, and all the GraphQL mutations |
| `read:org`      | org-owned repositories and reviewer lookups                      |
| `notifications` | `pr-review-notification`, which calls `GET /notifications`       |

Copy the token immediately; GitHub shows it once.

### 2. Tell git who you are

ghub reads the username from git, not from the token:

```sh
git config --global github.user YOUR_USERNAME
```

### 3. Store the token

```sh
touch ~/.authinfo && chmod 600 ~/.authinfo
```

Owner-only permissions matter -- this is a password in a plain text file. If
you have a GPG key set up, `~/.authinfo.gpg` is better; ghub reads either.

Open it in Emacs (`C-x C-f ~/.authinfo`, so the token never enters your shell
history) and add:

```
machine api.github.com login YOUR_USERNAME^emacs-pr-review password YOUR_TOKEN
```

**The machine is `api.github.com`**, not `github.com` — that is ghub's default
host for the `github` forge (`ghub-default-host-alist`).

And the login field is not your username. See below.

### Why the login is `USERNAME^PACKAGE`

The `login` field in `~/.authinfo` is normally an account name. ghub uses it as
a **composite key**: it glues the calling package's name onto your username and
searches for that (`ghub--ident`, `ghub.el`):

```elisp
(defun ghub--ident (username package)
  (format "%s^%s" username package))
```

and then looks the token up under that string:

```elisp
(let* ((user (ghub--ident username package))
       (token (ghub--auth-source-get :secret :host host :user user)))
```

So a plain `login YOUR_USERNAME` line is never consulted — nothing ever asks
for it. The `^` is just a separator ghub picked; netrc does not treat it
specially.

"Package" is the symbol the caller passes as `:auth` to `ghub-request`. It is a
namespace label, not something ghub validates:

| Caller    | `:auth`                                        | authinfo login        |
| --------- | ---------------------------------------------- | --------------------- |
| pr-review | `emacs-pr-review` (`pr-review-ghub-auth-name`) | `you^emacs-pr-review` |
| Forge     | `forge`                                        | `you^forge`           |

The point is that packages hold independent credentials: you can revoke or
rotate one without breaking the other, grant each only the scopes it needs, and
tell them apart in GitHub's token list. It also lets ghub create a token for a
package on demand with the right scopes.

So: **one line per package**, even against the same host. The same token value
in both is fine — use two different tokens only if you want to revoke them
independently.

```
machine api.github.com login YOUR_USERNAME^emacs-pr-review password YOUR_TOKEN
machine api.github.com login YOUR_USERNAME^forge           password YOUR_TOKEN
```

Get this wrong and nothing fails silently — ghub reports the exact login string
it wanted:

```
Required Github token ("you^emacs-pr-review" for "api.github.com") does not exist.
```

### 4. Verify, without printing the token

```sh
emacs --batch --eval '(progn
  (setq package-user-dir "~/.emacs.d/elpa-30.2")
  (package-initialize)
  (require (quote ghub))
  (princ (format "login: %s\n" (alist-get (quote login)
    (ghub-request "GET" "/user" nil :auth (quote emacs-pr-review))))))'
```

Should print your username.

### GitHub Enterprise


For a self-hosted instance, add it to `pr-review-forges-alist` by setting
`my/pr-review-extra-forges` from a tap's `before-init.el`, which Exordium
loads ahead of every tap's `after-init.el`:

```elisp
(defvar my/pr-review-extra-forges
  '(("github.example.com"
     . (github "github.example.com/api/v3" "your-username"))))
```

The first entry of the resulting alist is pr-review's default forge, so an
entry set this way wins over github.com.

The authinfo `machine` must then be the **API host, path included** --
`github.example.com/api/v3`. ghub tries that string verbatim and only then
falls back to the registered domain, which for a host like
`github.dept.example.com` computes to `example.com`, not the hostname. An
entry for the bare hostname silently fails to match, which is the single
easiest way to get this wrong.

If this checkout has an internal tap alongside it, the concrete host, token
scopes and verification snippets for that instance live there rather than
here.

## The mental model

Three keys do almost everything, and each is **context-sensitive** — it
dispatches on the `magit-section` under point:

| Key       | On a review thread | On a diff line          | Elsewhere              |
| --------- | ------------------ | ----------------------- | ---------------------- |
| `C-c C-c` | reply to thread    | add pending review note | comment on PR          |
| `C-c C-s` | resolve/unresolve  | submit pending review   | merge / close / reopen |
| `C-c C-e` | edit your reply    | —                       | edit description       |

So "where is point" is the whole interface.

### Everything else in `pr-review-mode`

| Key       | Command                              |
| --------- | ------------------------------------ |
| `C-c C-r` | refresh                              |
| `C-c C-v` | view file at that revision           |
| `C-c C-f` | jump to file in the diff             |
| `C-c C-d` | ediff the file                       |
| `C-c C-q` | request reviewers                    |
| `C-c C-l` | set labels                           |
| `C-c C-t` | toggle draft                         |
| `C-c C-j` | update reactions                     |
| `C-c C-o` | open in external browser             |

### In any input buffer

| Key       | Command       |
| --------- | ------------- |
| `C-c C-c` | **send**      |
| `C-c C-k` | abort         |
| `C-c @`   | mention user  |

### In `pr-review-notification`

| Key       | Command                  |
| --------- | ------------------------ |
| `RET`     | open PR                  |
| `C-c C-r` | mark read                |
| `C-c C-d` | mark for deletion        |
| `C-c C-u` | remove mark              |
| `C-c C-s` | execute marks            |
| `C-c C-t` | toggle read filter       |
| `C-c C-o` | open in browser          |

## Walkthrough: your own PR, being reviewed

Example: <https://github.com/owner/repo/pull/42>

**1. Open it**

```
M-x pr-review RET https://github.com/owner/repo/pull/42 RET
```

Or, from a Magit status or Forge topic buffer with the PR at point: `C-c M-v`.

**2. Orient**

It is a `magit-section` buffer, so the usual motions work: `n` / `p` between
sections, `TAB` to fold, `M-1`..`M-4` for global fold levels. Layout is
description -> timeline (comments, reviews, review threads) -> diff. `M-4`
collapses everything for a quick survey of who said what.

**3. Read a reviewer's review thread**

Threads render inline with their diff hunk (up to `pr-review-diff-hunk-limit`,
default 4 lines) and show a resolved/unresolved marker. Navigate with `n`.

**4. Reply**

Point on the thread -> `C-c C-c`. An input buffer opens, pre-populated with the
quoted comment. Type the response, then `C-c C-c` to send. It posts
immediately.

**5. Resolve**

Point still on the thread -> `C-c C-s`. Prompts `Really resolve this thread?`
-> `y`. Calls `resolveReviewThread` and refreshes. `C-c C-s` again on a
resolved thread unresolves it.

**6. After pushing a fix**

Push, then `C-c C-r` to refresh. New commits and newly-outdated threads
re-render.

**7. General comment**

Point anywhere outside a thread or the diff -> `C-c C-c` -> input buffer ->
`C-c C-c`.

**8. Fix the description**

Point on the description section -> `C-c C-e`. Opens the current body for
editing; `C-c C-c` to save.

**9. Merge when approved**

Point outside any thread/diff -> `C-c C-s` -> completing-read offers `MERGE` /
`REBASE` / `SQUASH`, plus `CLOSE` while the PR is open. Confirms before firing.

## Reviewing someone else's PR: the batching flow

1. Point on a diff line -> `C-c C-c` -> write the inline comment -> `C-c C-c`.
   It accumulates as a *pending* review thread, shown in the buffer.
2. Repeat for every comment.
3. `C-c C-s` once -> pick the event from `COMMENT` / `APPROVE` /
   `REQUEST_CHANGES` -> optional summary -> `C-c C-c`.

One review, all comments, same as the web UI.

## Caveats

- **You cannot approve your own PR.** `APPROVE` and `REQUEST_CHANGES` on your
  own PR are rejected by the GitHub API, not by Emacs. Use `COMMENT` if you
  want to batch inline annotations on your own PR.
- **Forge needs its own setup for GitHub Enterprise.** A `USERNAME^forge`
  authinfo line is necessary but not sufficient — stock `forge-alist` ships 13
  entries, all public forges, so a self-hosted host has to be added there too.
  pr-review is independent of this; it uses its own `pr-review-forges-alist`.

---

# Part 2 — Reviewing local changes

`my-diff-review.el`. Review uncommitted changes line by line — typically
changes a coding agent just wrote — then hand every comment back as markdown
for the agent to act on. Same idea as tuicr's local review, without leaving
Emacs and without the vim bindings.

No authentication, no network: this never talks to a forge.

## Open a review

```
C-c M-l          (M-x my/review-local-changes)
```

By default this reviews **all uncommitted changes** — staged, unstaged, and
untracked files. With a prefix argument (`C-u C-c M-l`) you pick:

| Choice                                    | `git diff`             |
| ----------------------------------------- | ---------------------- |
| uncommitted (staged + unstaged) — default | `HEAD`                 |
| unstaged only                             | (no args)              |
| staged only                               | `--cached`             |
| against a ref…                            | prompts for a revision |

**Untracked files are included** and diffed against `/dev/null`, so files the
agent created are reviewable too. The saved output file is excluded from that
list, so saving a review into the repository does not make the next review
include it.

## Keys

| Key       | Action                                                |
| --------- | ----------------------------------------------------- |
| `C-c C-c` | comment — see the scope table below                   |
| `C-c C-k` | delete the comment at point                           |
| `C-c C-w` | copy every comment as markdown to the kill ring       |
| `C-c C-s` | save every comment as markdown to a file              |
| `C-c C-r` | re-run the diff, keeping the comments already written |
| `C-c C-v` | visit the working-tree file at the line under point   |
| `C-c C-f` | jump to a file in the diff                            |
| `q`       | quit                                                  |

Everything inherited from `pr-review-mode` that would reach the GitHub API is
replaced or unbound, so nothing in this buffer can talk to a forge.

### `C-c C-c` dispatches on where point is

| Point is                         | You get                         | Exports as          |
| -------------------------------- | ------------------------------- | ------------------- |
| on a diff line                   | comment on that line            | `## file:line`      |
| with an active region            | comment on the whole range      | `## file:start-end` |
| on an existing comment           | edit it                         | —                   |
| on a file heading or hunk header | comment on the whole file       | `## file`           |
| in the buffer header             | comment on the whole change set | `## General`        |

The input buffer's header line always states the exact target, e.g. `Comment
on themes/color-theme-catppuccin.el:509-512.` Trust that over the region
highlight — selecting whole lines leaves point on the line *after* the last
one you want, as everywhere in Emacs, so the highlight looks one line longer
than the selection is.

### In the comment input buffer

| Key       | Action                                                      |
| --------- | ----------------------------------------------------------- |
| `C-c C-c` | **send**                                                    |
| `C-c C-k` | abort                                                       |
| `C-c C-i` | insert the lines being commented on as a `suggestion` block |

`C-c C-i` is opt-in rather than automatic: the export already quotes the
source, so prefilling it would put the same lines in the markdown twice. Use
it when you are proposing a concrete replacement.

## Walkthrough

1. Agent writes some code. Don't commit.
2. `C-c M-l` in any file of the repo.
3. Walk the diff with `n` / `p`. `TAB` folds, `M-4` collapses everything.
4. `C-c C-c` on anything worth commenting on. Repeat.
5. `C-c C-w` to copy, and paste into the agent's prompt.

Or `C-c C-s` to write the file, and tell the agent to read it — that is the
better route for a headless agent, and the basis for a future skill.

## What the export looks like

````markdown
# Code review comments

Reviewed: uncommitted (staged + unstaged) in `/path/to/repo/`

3 comments. Headings are `file:line` in the working tree, or `file` for a
comment on a whole file, or `General` for one on the whole change set.

## General

Split this into two commits.

## calc.py:9-13

```python
def scale(xs, k):
    out = []
    for x in xs:
        out.append(x * k)
    return out
```

This is just a list comprehension: `return [x * k for x in xs]`.
````

Comments are sorted for the reader, not in the order you wrote them:
review-level first, then by file; within a file, the file-level comment before
the line comments.

Fences are sized to their content — quoting a file that itself contains a
fence gets a longer one, so reviewing markdown does not produce broken
markdown. (The example above needs a four-backtick fence for exactly this
reason.)

## How it works, and what could break it

`my-diff-review.el` reuses three pr-review internals that have no dependency on
the GitHub API:

| Function | Role |
| --- | --- |
| `pr-review--insert-diff` | unified diff string -> rendered buffer, every line tagged with its path and line number |
| `pr-review-add-pending-review-thread` | reads that tag, opens an input buffer, renders the comment inline |
| `pr-review--pending-review-threads` | buffer-local list of those comments |

The GitHub-specific part is the submit path, which is simply never called; the
comment list is formatted as markdown instead. These are private (`--`)
functions, so a pr-review update could break this. The fallback is to vendor
`pr-review--insert-diff`, about 50 lines, into the tap.

Three non-obvious things the implementation has to do:

- **`git diff --no-prefix`.** magit always passes it, so magit's diff washer
  expects file names without `a/` and `b/`. Leaving them in makes every file
  heading render as `a/foo -> b/foo`.
- **The diff must not start at `point-min`.** The annotation loop in
  `pr-review--insert-diff` steps back one line before it starts, so a diff
  flush against the top of the buffer loses its first file name. The buffer
  header covers this.
- **Magit's section focus is disabled.** Magit highlights and repaints the
  section under point, which rewrote the faces of every line in the hunk —
  including comments rendered inside it, so moving point into a hunk made
  comments look like diff lines. `magit-section-highlight-current` is nil and
  the `painted` slot is unbound on every section after rendering. Selection
  highlighting stays, since it usefully shows what a region comment covers.

---

# Fonts

pr-review renders comment bodies as HTML through **shr** (Emacs's Simple HTML
Renderer, also used by eww and HTML mail). shr's `shr-text` face inherits
`variable-pitch`, which is spec'd as the generic family `"Sans Serif"` — a
name the platform resolves, and which on macOS can land on a *serif*.

Proportional prose is deliberate in shr: it keeps `shr-code` fixed pitch, so
code stays monospace, like a browser. But a review comment sits among code, so
`my/pr-review-use-buffer-font` remaps `shr-text` to the buffer's own font:

```elisp
(face-remap-set-base 'shr-text 'default)
```

Buffer-local on `pr-review-mode-hook`, so eww and HTML mail keep their
proportional text. `face-remap-set-base` rather than `face-remap-add-relative`
because the relative form would leave `shr-text`'s own `:inherit
variable-pitch-text` in play, giving the buffer's family at 1.1x size.

Not `shr-use-fonts nil`, which looks like the obvious switch but also flips shr
from pixel- to character-based measurement, while pr-review sets
`shr-indentation` in pixels.

The Catppuccin theme also styles the pr-review faces that appear inside a diff
(`pr-review-in-diff-pending-*`), which the package leaves uncoloured. See
`themes/color-theme-catppuccin.el`.
