# emacs
Aims to be a deterministic emacs configuration using git subtrees

## Subtrees

Git subtree doesn't store the upstream details anywhere, so we have to
manually provide them on the command line to pull. Luckily we almost
never want to update packages!

### markdown-mode

```
git subtree pull --prefix emacs/.emacs.d/markdown-mode https://github.com/defunkt/markdown-mode master --squash
```

### async

```
git subtree pull --prefix emacs/.emacs.d/async https://github.com/jwiegley/emacs-async master --squash
```

### helm

```
git subtree pull --prefix emacs/.emacs.d/helm https://github.com/emacs-helm/helm master --squash
```
