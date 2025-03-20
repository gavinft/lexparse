# lexparse

Small recursive descent parser and lexer project I'm currently working on. Not sure how far I'll go
with it, but the plan is that it could be used for game scripting and would have specific language
portions for things like game objects or components.

## Currently implemented

* Int, bool, string
* Expressions
* * Adding, subtracting, multiplying, dividing, modulo
* * Comparing (less than, less than or equal, equal, etc.)
* If/else if/else statement
* * If the if statement covers all cases (has an else clause) then it can return a value
* Variable assignment (`let var = expression`)
* * There's no reassignment yet, but with the current implementation you can just assign twice

## Example

```
let hello = 5
let word = "lmao"
let hello = hello + 1
print hello
print word

let var = if true then 5 else 6 fi
print "var:"
print var
```

## Usage

Put your program in a file called program.p
