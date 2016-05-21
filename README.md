# emacs
Aims to be a deterministic emacs configuration using git subtrees

## Subtrees

### markdown-mode

```
git subtree add  --prefix emacs/.emacs.d/markdown-mode https://github.com/defunkt/markdown-mode/blob/master/markdown-mode.el master --squash
git subtree pull --prefix emacs/.emacs.d/markdown-mode https://github.com/defunkt/markdown-mode/blob/master/markdown-mode.el master --squash
```
