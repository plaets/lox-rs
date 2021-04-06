# first part of craftinginterpreters finished

* written in rust
* garbage collector (sometimes?) thanks to [gcmodule](https://crates.io/crates/gcmodule), although i don't understand what should implement `Trace` and what shouldn't
* mixins(?): `class HasMixin ; Mixin1, Mixin2` - not very proud of the syntax but i was too lazy to add a new keyword, works with inheritance (`class Test < Inherited ; MixedIn`)
* subscript operator: `"test"[0]`
* lists: `[1, 2, 4, ["zxc", true, nil]][3][0]`
    + use `+` to merge two lists
* standard library
    + `class File`
        + `init(filename)` - open a file
        + `read()` - read the whole file
        + `read(n)` - read n bytes
        + `seek(n)` - seek to n
        + `size()` - file size
    + `len(o)` - length of a string or a list
    + `input()` - input
    + `exit(code)` - exit with code
    + `time()` - get current time in nanoseconds
    + check `src/native` for cool ass macros that will help you define new native classes and functions
* """test framework""" for the parser, i even wrote some tests!

## one day? after i finish the third part? maybe?

* imports
* dynamic native libraries
* ranges/slices
* error handling
* methods for native types 
* `+=` etc.
* big numbers/different number types for floating point/int
* break/continue (check `ch9-break`)
* anonymous functions (check `ch10-anon-fun`)
* vim syntax plugin?

