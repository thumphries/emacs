# emacs
Aims to be a deterministic emacs configuration using git subtrees

## Usage

Install `stow`, `cask`, `emacs` and `make`, then run `./bootstrap.sh`.

## Subtrees

Git subtree doesn't store the upstream details anywhere, so we have to
manually provide them on the command line to pull. Luckily we almost
never want to update packages!

| package | pull |
| --- | --- |
| markdown-mode | `git subtree pull --prefix emacs/.emacs.d/markdown-mode https://github.com/defunkt/markdown-mode master --squash` |
| async | `git subtree pull --prefix emacs/.emacs.d/async https://github.com/jwiegley/emacs-async master --squash` |
| helm | `git subtree pull --prefix emacs/.emacs.d/helm https://github.com/emacs-helm/helm master --squash` |
| helm-swoop | `git subtree pull --prefix emacs/.emacs.d/helm-swoop https://github.com/ShingoFukuyama/helm-swoop master --squash` |
| dash | `git subtree pull --prefix emacs/.emacs.d/dash https://github.com/magnars/dash.el master --squash` |
| projectile | `git subtree pull --prefix emacs/.emacs.d/projectile https://github.com/bbatsov/projectile master --squash` |

