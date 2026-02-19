# Disclaimer

This repository is a hobby project.
I've published it as art and as part of my portfolio of technical work.
Many (but not all!) of the ideas herein are compelling _per se_ and reflect my opinions, aethestics, craftsmanship, attention to detail, etc.

Part of my joy in this exercise has been avoiding pre-packaged solutions: it's a from-scratch tokenizer and parser.
There are some arguably good reasons for bespoke code here, but ultimately it's because I happened to enjoy fully confronting those lower-level considerations for this particular language.

It's nowhere near production quality.
Most likely never will be.
But if you think it's neat, please open Issues, PRs, etc!

# What is Matter

Matter is a human-readable serialization format, like JSON/YAML/etc.
It fixes what I don't like about them.

It's _not_ a configuration language (… … … yet?)--see [Nickel](https://github.com/nickel-lang/nickel) for that! :D

It's just a precisely structured language with carefully chosen syntax that users might prefer to read and write in various use cases.

See `doc/postulates.md` for my language design reasons.

# Syntax tree grammar

This specification excludes pins (see `doc/postulates.md`) for simplicity and implicitly allows whitespace anywhere.

```
matter =
    %atom
  |
    %bytes ("<>" %bytes)*
  |
    %decimal
  |
    ("_" | %textliteral %joiner)* %textliteral
  |
    %variant matter
  |
    "[" (matter | "{=" matter "=}")* "]"
  |
    "{>" matter ">}" matter
  |
    "(^" matter ")" "{<" matter "<}
```

# Token grammar

This specification only allows whitespace explicitly and uses `\1` as a capture group.

```
%ws = (" " | "\n" | "\r")+

%symbol = (ASCII 21 through 7E except 60)* (%ws|EOF)

%atom = "@" %symbol

%bytes = "0x" (%hexadecimaldigit %hexadecimaldigit)*

%decimal =
    ("+" | "-" | "") %decimaldigit+
    ("." %decimaldigit+)?
    (("e" | "E") ("+" | "-" | "") %decimaldigit+)?

%textliteral =
    "\"" [^"]* "\""
  |
    ("'" %decimaldigit{1,4} "'") [^\1]* \1

%joiner =
    "<" (
          ""
	|
          (%joinertext (%joinerescapes %joinertext)* %joinerescapes?)
        |
          (%joinerescapes (%joinertext %joinerescapes)* %joinertext?)
        ) ">"

%joinertext = [^>%]+

%joinerescapes = ("%" %validUtf8nibbles)+

%variant = "#" %symbol
```
