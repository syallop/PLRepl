# PLRepl - experimental

A REPL for an experimental [Programming Language](https://github.com/syallop/PL) which currently uses a ['lispy'](https://github.com/syallop/PLLispy) syntax.

- [PL](https://github.com/syallop/PL) should describe the concepts of the language
- [PLLispy](https://github.com/syallop/PLLispy) should provide an overview of the syntax this REPL accepts

This REPL is primarily useful for development as most of the project is at
an unfinished and experimental phase.

## Building

This project is developed with Haskells [stack](https://docs.haskellstack.org/en/stable/README/).
`cabal` support is untested. 

A number of PL dependencies are pinned by the `stack.yaml`. Build with `stack
build`.

## Installing

If you with to install globally, run `stack install` to install the `pl`
executable to your global stack binaries directory.


## Executing

If installed globally, call `pl`. If built locally, call `stack exec pl`.


## Using

Launching the `pl` executable creates a [brick](https://hackage.haskell.org/package/brick)-based
command line interface split into several panes.

![Boolean And](https://github.com/syallop/PLRepl/blob/master/README/BooleanAnd.png) 
- The top-left `widget` is an [PLEditor](https://github.com/syallop/PLEditor)-based editor for inputing [lispy](https://github.com/syallop/PLLispy) source code.
- The bottom-left widget is an output text area that displays the result of parsing/ type checking code entered into the editor widget. 
- The right widget displays a context of the types currently defined and available to be mentioned in the editor.  

### Commands

Commands are entered by pressing keys, either a single key (such as an up
arrow), or multiple (such as the up arrow while holding the control key).
Most commands will be sent to whatever `widget` we consider in `focus`. Some
commands are `global` and affect the entire repl.

Below is an overview of some of the commands the repl understands.

| Key combination       | Targets        | Action |
| --------------------- | -------------- | ------ |
| `up`                  | focused        | Move the cursor up in the focused widget | 
| `ctrl + up`           | focused        | Make the current view within a widget taller |
| `down`                | focused        | Move the cursor down in the focused widget |
| `ctrl + down`         | focused        | Make the current view within a widget shorter |
| `left`                | focused        | Move the cursor left in the focused widget |
| `ctrl + left`         | focused        | Make the current view within a widget narrower |
| `right`               | focused        | Move the cursor right in the focused widget |
| `ctrl + right`        | focused        | Make the current view within a widget wider |
| `any other character` | editor         | Insert character into the editor widget |
| `HOME`                | editor         | Insert a random code sample into the editor |
| `DELETE/ BACKSPACE`   | editor         | Delete the current character in the editor widget |
| `ENTER`               | editor         | Insert a newline in the editor widget | 
| `INSERT`              | editor, output | Grab the text in the editor and run it through the configured repl. This usually means parsing, type-checking and evaluating. Output appears in the output widget. If successful, the editor widget is cleared | 
| `PAGE-UP`             | global         | Switch focus to the next widget |
| `PAGE-DOWN`           | global         | Switch focus to the previous widget |
| `ctrl + l`            | editor         | Clear all text from the editor widget |
| `ESC`                 | global         | Exit the program |

### Examples

Below are a couple of example functions, rendered with printing in a broken
state...

Boolean and:
![Boolean And](https://github.com/syallop/PLRepl/blob/master/README/BooleanAnd.png) 

Subtracting two from a natural number:
![Subtract Two](https://github.com/syallop/PLRepl/blob/master/README/SubTwo.png)

