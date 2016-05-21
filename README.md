# emacs
Aims to be a deterministic emacs configuration using git subtrees

## Subtrees

Git subtree doesn't store the upstream details anywhere, so we have to
manually provide them on the command line to pull. Luckily we almost
never want to update packages!

### markdown-mode

```
git subtree add  --prefix emacs/.emacs.d/markdown-mode https://github.com/defunkt/markdown-mode master --squash
git subtree pull --prefix emacs/.emacs.d/markdown-mode https://github.com/defunkt/markdown-mode master --squash
```
