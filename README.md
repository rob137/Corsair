# Corsair

**Corsair** is an Emacs package that helps gather text to populate LLM prompts for [GPTel](https://github.com/karthink/gptel).

## Install Guide

Corsair extends GPTel, so ensure GPTel is already installed.

### Installing via MELPA

Once _Corsair_ is available on MELPA, you can install it directly using Emacs' package management system. To do so, add MELPA to your list of repositories (if not already added) by adding the following to your `init.el` or `.emacs` configuration:

```
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

Then, install Corsair with:

```
M-x package-refresh-contents
M-x package-install RET corsair RET
```

### Installing via use-package

If you're using use-package for package management, you can install Corsair directly from MELPA with the following configuration:

```
(use-package corsair
  :ensure t
  :defer t ; Optional: Load lazily
  :after gptel ; Ensure gptel is loaded before corsair
  )
```

### Installing via straight.el

You can also install Corsair using straight.el. Add the following to your Emacs configuration:

```
(use-package corsair
  :straight (:host github
             :repo "rob137/Corsair"
             :files ("corsair.el")) ; Load the specific file
  :defer t ; Optional: Load lazily
  :after gptel ; Ensure gptel is loaded before corsair
  )
```

## Features:

The following are _suggested_ keybindings to add Corsair with your workflow. You can add them to your configuration if you find them useful. See [corsair.el](./corsair.el) for more details.

- Open or switch to the GPTel chat buffer (`C-c g c`).
- Accumulate file paths, contents, and selected text to the chat buffer:
  - Current file path and contents (`C-c g a c`).
  - Current file name (`C-c g a n`).
  - Current full (absolute) file path (`C-c g a v`).
  - selected text (`C-c g a w`).
  - Clear the chat buffer (`C-c g a D`).
  - Open minibuffer with fido-style fuzzy matching to add file or folder contents (`C-c g f`).

Add the following to your emacs config to enable these keybindings:

```
(global-set-key (kbd "C-c g c") #'corsair-open-chat-buffer)                    ;; Open chat buffer
(global-set-key (kbd "C-c g a c") #'corsair-accumulate-file-path-and-contents) ;; Accumulate file path and contents
(global-set-key (kbd "C-c g a n") #'corsair-accumulate-file-name)              ;; Accumulate file name
(global-set-key (kbd "C-c g a v") #'corsair-accumulate-file-path)              ;; Accumulate file path
(global-set-key (kbd "C-c g a w") #'corsair-accumulate-selected-text)          ;; Accumulate selected text
(global-set-key (kbd "C-c g a D") #'corsair-drop-accumulated-buffer)           ;; Drop chat buffer
(global-set-key (kbd "C-c g f") #'corsair-insert-file-or-folder-contents)      ;; Insert file or folder contents
```
