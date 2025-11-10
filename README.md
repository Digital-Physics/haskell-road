# Haskell Library related to "The Haskell Road to Logic, Math and Programming" 📖

I will update this Repository with Haskell code as I go through the book. 


#
### Bayes Rule in Haskell
to compile it
```
ghc -o bayes Main.hs
```
 
to run it

```
./bayes
``` 
#
for the command line interactive interpreter, GHCi (Glasgow Haskell Compiler Interactive)
```
ghci
```

### Command,Shorthand,Description

:load, :l, "Load a Haskell file."
```
:l prime.hs
```

:type, :t, "Show the type of an expression (as a 'type judgement' of the form expression :: type or type scheme)."
```
:t divides 5
```

:info, :i,"Show information about a name"

```
:i rem
```

:quit, :q, Exit the GHCi interpreter.

```
:q
```

:help, :? , Display a list of all commands.

```
:?
```

:relaod, :r, reload current file that's been edited

```
:r
```

#
The following are reserved keywords and cannot be used to name functions (which always start with a lower-case letter, unlike Constructor identifiers for types):

```
case, class, data, default, deriving, do, else, if, import, in, infix, infixl, infixr, instance, let, module, newtype, of, then, type, where, -
```
#
9223372036854775807 = 2^63 - 1
```
Prelude> maxBound :: Int
```
-9223372036854775808
```
Prelude> minBound :: Int
```
### Character Maximum (Unicode)

In Haskell, the `Char` type represents a single **Unicode code point**.

The Prelude> command 

```
maxBound :: Char
``` 

returns the largest possible Unicode value:

* **Decimal:** 1,114,111
* **Hexadecimal/Unicode:** U+10FFFF

This value (U+10FFFF) is the highest possible code point defined by the Unicode standard, encompassing the full range of different ASCII characters, non-english chars, math symbols, emojis, and unassigned points.

#
converting to an infix operator instead of the standard prefix operator.
```
max 4 5
```

```
4 `max` 5
```
conversely, an infix operator is made a prefix operator by putting it in round brackets