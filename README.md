# Corsair.el

**Corsair.el** is an Emacs package that helps gather text for LLM prompts via [GPTel](https://github.com/karthink/gptel).

## Install Guide

Corsair.el extends GPTel, so ensure GPTel is already installed.

You can install **Corsair.el** using `use-package` with `straight.el` for package management. Add the following to your Emacs configuration:

```elisp
(use-package corsair
  :straight (:host github
             :repo "rob137/corsair"
             :files ("corsair.el")) ; Load the specific file
  :defer t ; Optional: Load lazily
  :after gptel ; Ensure gptel is loaded before corsair
  )
```

## Features:

See [corsair.el](./corsair.el) for the full list of bindings.

Features include:

1. **GPTel Chat Buffer Management**:
   - Open or switch to the GPTel chat buffer (`C-c g c`).
   - Accumulate file paths, contents, and selected text to the chat buffer:
     - Current file path and contents (`C-c g a c`).
     - Current file name (`C-c g a n`).
     - Current full (absolute) file path (`C-c g a v`).
     - selected text (`C-c g a w`).
     - Clear the chat buffer (`C-c g a D`).
     - Open minibuffer with fido-style fuzzy matching to add file or folder contents (`C-c g f`).
