# helm-ag2.el ![](https://github.com/syohex/emacs-helm-ag2/workflows/CI/badge.svg)

## Introduction

`helm-ag2.el` provides interfaces of [The Silver Searcher](https://github.com/ggreer/the_silver_searcher) with helm.


## Features

- Edit search result like [wgrep](https://github.com/mhayashi1120/Emacs-wgrep)


## Screenshot

![helm-ag2](image/helm-ag.png)


## Basic Usage

##### `helm-ag2`

Input search word with `ag` command. You can change search directory
with `C-u` prefix.

##### `helm-ag2-project-root`

Call `helm-ag2` at project root. `helm-ag2` seems directory as project root where
there is `.git`.


##### `helm-ag2-pop-stack`

Move to point before jump

##### `helm-ag2-clear-stack`

Clear context stack


## Enable helm-follow-mode by default

Please set `helm-follow-mode-persistent` to non-nil if you want to use `helm-follow-mode` by default. You must set it before loading `helm-ag2.el`.

``` lisp
(custom-set-variables
 '(helm-follow-mode-persistent t))
```


## Persistent action

You can see file content temporarily by persistent action(`C-j`).


## Search Tips of `helm-ag2`

##### Passing command line options and pattern

```
Pattern: -Gmd$ search_pattern
```

Command line options is `-Gmd$` and search pattern is `search_pattern`.
`helm-ag2` treats words which starts with `-` as command line option.

##### Pattern starts with `-`

```
Pattern: -- --count
```

Search pattern is `--count`.
`helm-ag2` treats words after `--` as search pattern.

##### Search meta characters as literal

`ag`(`ack`, `pt`) takes Perl compatible PCRE so that you need to escape meta characters
likes brackets, braces, asterisk, when you search them as literals.

##### Use short option

Don't use space between option and its value. For example `-tcpp` is ok, `-t cpp` is not ok.

##### Use long option

Please always use `=` separator for using long option. Don't use space as separator. For example `--ignore=pattern` is ok, `--ignore pattern` is not ok.

## Customize

##### `helm-ag2-base-command`(Default: `'("ag" "--nocolor" "--nogroup")`)

Base command of `ag`.

##### `helm-ag2-insert-at-point`(Default: `nil`)

Insert thing at point as default search pattern, if this value is `non nil`.
You can set the parameter same as `thing-at-point`(Such as `'word`, `symbol` etc).


##### `helm-ag2-edit-save`(Default: `t`)

Save buffers you edit at editing completed.

#### NOTE

`helm` removes `file-line` type feature from 1.6.9. So `helm-ag2-source-type` is no longer available.


## Keymap

`helm-ag2-map` are inherited by `helm-map`.

| Key              | Action                                                                     |
|:-----------------|:---------------------------------------------------------------------------|
| `C-c o`          | Open other window                                                          |
| `C-l`            | Search in parent directory                                                 |
| `C-c C-e`        | Switch to edit mode                                                        |
| `C-x C-s`        | Save ag results to buffer(Ask save buffer name if prefix key is specified) |
| `C-c C-f`        | Enable helm-follow-mode                                                    |
| `C-c >`, `right` | Move to next file                                                          |
| `C-c <`, `left`  | Move to previous file                                                      |
| `C-c ?`          | Show help message                                                          |


### Edit mode keymap

| Key       | Action           |
|:----------|:-----------------|
| `C-c C-c` | Commit changes   |
| `C-c C-k` | Abort            |
| `C-c C-d` | Mark delete line |
| `C-c C-u` | Unmark           |

You can use `next-error` and `previous-error` for seeing file content which
current line indicates.

### Saved buffer keymap

| Key   | Action                                        |
|:------|:----------------------------------------------|
| `RET` | Jump to current line position                 |
| `C-o` | Jump to current line position in other window |
| `g`   | Update result                                 |


## Sample Configuration

```lisp
(custom-set-variables
 '(helm-ag2-base-command '("ag" "--nocolor" "--nogroup" "--ignore-case"))
 '(helm-ag2-insert-at-point 'symbol)
 '(helm-ag2-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'")))
```

## ripgrep

You need to build `ripgrep` with `pcre` feature

```lisp
(custom-set-variables
 '(helm-ag-base-command '("rg" "--pcre2" "--color=never" "--vimgrep")))
```
