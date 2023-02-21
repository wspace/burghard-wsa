# Whitespace Assembler

## Syntax

| Command                    | Explanation                                                                                                                                                   | Sample                           | Standard Whitespace |
| -------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------- | ------------------- |
| `push integer`             | push number on the stack                                                                                                                                      | `push 5`                         | Yes                 |
| `pushs string`             | push string on the stack with a termination NULL (can be used with the `prints`,`printsln` function from `io` library). String can be in `""` to allow spaces | `pushs "Hallo world"`            | Yes                 |
| `pop`                      | pop number from stack                                                                                                                                         | `pop`                            | Yes                 |
| `label name`               | label with name. name can be any string                                                                                                                       | `label hi`<br>`label asdsad`     | Yes                 |
| `doub`                     | dublicate item on stack                                                                                                                                       | `doub`                           | Yes                 |
| `swap`                     | swap top of stack                                                                                                                                             | `swap`                           | Yes                 |
| `add [integer]`            | add top of stack or add integer to top of stack                                                                                                               | `add`<br>`add 5`                 | Yes                 |
| `sub [integer]`            | " top of stack or " integer to top of stack                                                                                                                   | `sub`<br>`sub 5`                 | Yes                 |
| `mul [integer]`            | " top of stack or " integer to top of stack                                                                                                                   | `mul`<br>`mul 5`                 | Yes                 |
| `div [integer]`            | " top of stack or " integer to top of stack                                                                                                                   | `div`<br>`div 5`                 | Yes                 |
| `mod [integer]`            | " top of stack or " integer to top of stack                                                                                                                   | `mod`<br>`mod 5`                 | Yes                 |
| `store [integer]`          | store or store in space integer                                                                                                                               | `store`<br>`store 5`             | Yes                 |
| `retrive [integer]`        | retrive or retrive from integer                                                                                                                               | `retrive`<br>`retrive 5`         | Yes                 |
| `call name`                | call label                                                                                                                                                    | `call asd`                       | Yes                 |
| `jump name`                | jump                                                                                                                                                          | `jump asd`                       | Yes                 |
| `jumpz name`               | jump if zero                                                                                                                                                  | `jumpz asd`                      | Yes                 |
| `jumpn name`               | jump if negative                                                                                                                                              | `jumpn asd`                      | Yes                 |
| `jumpp name`               | jump if positive                                                                                                                                              | `jumpp asd`                      | Yes                 |
| `jumpnz name`              | jump negative or zero                                                                                                                                         | `jumpnz asd`                     | Yes                 |
| `jumppz name`              | jump positive or zero                                                                                                                                         | `jumppz asd`                     | Yes                 |
| `jumppn`,`jumpnp name`     | jump positive or negative, jump not null                                                                                                                      | `jumppn asd`                     | Yes                 |
| `include name`             | include file (without wsa)                                                                                                                                    | `include io`<br>`include memory` | Yes                 |
| `ret`                      | return from function                                                                                                                                          | `ret`                            | Yes                 |
| `exit`                     | exit from program                                                                                                                                             | `exit`                           | Yes                 |
| `outn`                     | out number                                                                                                                                                    | `outn`                           | Yes                 |
| `outc`                     | out char                                                                                                                                                      | `outc`                           | Yes                 |
| `inn`                      | in number to heap address from stack                                                                                                                          | `inn`                            | Yes                 |
| `inc`                      | in char to heap address from stack                                                                                                                            | `inc`                            | Yes                 |
| `test integer`             | test the top of stack with number. It dublicates the value before comparing, so it is NOT poped automaticaly                                                  | `test 5`                         | Yes                 |
| `valuestring _name value`  | define `_name` as `value`. so `_name` can be used everywhere a string is needed. `name` must start with `_`                                                   | `valuestring _hello "hi user"`   | Yes                 |
| `valueinteger _name value` | define `_name` as `value`. so `_name` can be used everywhere a integer is needed. `name` must start with `_`                                                  | `valueinteger _count 5`          | Yes                 |
| `ifoption name`            | include next only if option `name` is defined                                                                                                                 | `ifoption debug`                 | Yes                 |
| `elseoption`               | include next only if last `ifoption name` was not defined                                                                                                     | `elseoption`                     | Yes                 |
| `endoption`                | end `ifinclude` block                                                                                                                                         | `endoption`                      | Yes                 |
| `elseifoption name`        | combination of else and if                                                                                                                                    | `elseifoption debug2`            | Yes                 |
| `debug_printstack`         | print the stack of the interpreter. New Syntax, not compatible with other interpreters ! To enable it the option "extendedsyntax" must be enabled !           | `debug_printstack`               | **No**              |
| `debug_printheap`          | print the heap of the interpreter. New Syntax, not compatible with other interpreters ! To enable it the option "extendedsyntax" must be enabled !            | `debug_printheap`                | **No**              |

## functions:

|                                                       |                                                             |                                             |
| ----------------------------------------------------- | ----------------------------------------------------------- | ------------------------------------------- |
| `translateWSA filename extendedSyntax`                | translate WSA file to ws and pws files                      | `translateWSA "test" True`                  |
| `translateWSAOptions filename options extendedSyntax` | translate WSA file to ws and pws files with options enabled | `translateWSAoptions "test" ["debug"] True` |
| `getWSAOptions filename`                              | get the options that can be used in the wsa file            | `getWSAOptions "test"`                      |