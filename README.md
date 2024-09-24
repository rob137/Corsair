# Corsair.el

**Corsair.el** is an Emacs package that extends [GPTel](https://github.com/karthink/gptel) with functions for text accumulation along the lines of [Cursor](https://www.cursor.com/).

### Features:

1. **GPTel Chat Buffer Management**:
   - Open or switch to the GPTel chat buffer (`C-c g c`).
   - Accumulate file paths, contents, and selected text to the chat buffer:
     - Accumulate file path and contents (`C-c g a c`).
     - Accumulate file name (`C-c g a n`).
     - Accumulate selected text (`C-c g a w`).
     - Clear the chat buffer (`C-c g a D`).
2. **File Expansion**:
   - Expand `@path` to include files or directories within the project using fuzzy matching (`C-c g @`).
