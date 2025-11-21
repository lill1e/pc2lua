# pc2lua
A transpiler from [ParentheC](https://legacy.cs.indiana.edu/classes/b521/newpc/ParentheC.pdf) to [Lua](https://lua.org) written in [Racket](https://racket-lang.org)
## FAQ
### ParentheC, what is that?
ParentheC is a subset of racket, as noted in the linked paper above. In short, ParentheC is used to provide a mechanical way of translating from a Racket-like language to some other programming language.

## How to Use
1. Place `pc2lua.rkt` and some ParentheC file ending in `.pc` in the same directory.
2. Enter the Racket REPL by typing `racket` in your terminal.
3. Import `pc2lua` by entering:
```racket
(require "pc2lua.rkt")
```
4. Run `pc2lua` by entering:
```racket
(pc2lua "file.pc")
```
5. The output file should be named `file.lua` (or a current timestamp ending in `.lua` if the file already exists)
