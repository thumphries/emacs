# emacs
Aims to be a deterministic emacs configuration using git subtrees

## Usage

Install `stow`, `cask`, `emacs` and `make`, then run `./bootstrap.sh`.

Ideally we'd be able to stow the individual packages we want and let
bootstrap do the rest, but too lazy to do this for now (may change if
I re-add Proof General or agda-mode, installing those really sucks!)

## Subtrees

Git subtree doesn't store the upstream details anywhere, so we have to
manually provide them on the command line to pull. Luckily we almost
never want to update packages!

| package | pull |
| --- | --- |
| markdown-mode | `git subtree pull --prefix emacs/.emacs.d/markdown-mode https://github.com/defunkt/markdown-mode master --squash` |
| async | `git subtree pull --prefix emacs/.emacs.d/async https://github.com/jwiegley/emacs-async master --squash` |
| helm | `git subtree pull --prefix emacs/.emacs.d/helm https://github.com/emacs-helm/helm master --squash` |
| helm-ag | `git subtree pull --prefix emacs/.emacs.d/helm-ag https://github.com/syohex/emacs-helm-ag master --squash` |
| helm-swoop | `git subtree pull --prefix emacs/.emacs.d/helm-swoop https://github.com/ShingoFukuyama/helm-swoop master --squash` |
| dash | `git subtree pull --prefix emacs/.emacs.d/dash https://github.com/magnars/dash.el master --squash` |
| projectile | `git subtree pull --prefix emacs/.emacs.d/projectile https://github.com/bbatsov/projectile master --squash` |
| haskell-mode | `git subtree pull --prefix emacs/.emacs.d/haskell-mode https://github.com/haskell/haskell-mode master --squash` |
| org-journal | `git subtree pull --prefix emacs/.emacs.d/org-journal https://github.com/bastibe/org-journal master --squash` |
| scala-mode2 | `git subtree pull --prefix emacs/.emacs.d/scala-mode2 https://github.com/hvesalai/scala-mode2 master --squash` |
| yasnippet | `git subtree pull --prefix emacs/.emacs.d/yasnippet https://github.com/joaotavora/yasnippet master --squash` |
| haskell-snippets | `git subtree pull --prefix emacs/.emacs.d/haskell-snippets https://github.com/haskell/haskell-snippets master --squash` |
| handlebars-sgml-mode | `git subtree pull --prefix emacs/.emacs.d/handlebars-sgml-mode https://github.com/jacott/handlebars-sgml-mode master --squash` |
| spacemacs-theme | `git subtree pull --prefix emacs/.emacs.d/spacemacs-theme https://github.com/nashamri/spacemacs-theme master --squash` |
