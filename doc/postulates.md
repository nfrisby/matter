# Matter Postulates

Matter is the result of my opinions and my aesthetics.
This document relates terse descriptions of those opinions and aesthetics to the parts of the Matter language that they influenced.

For the record, I think the ideas of Matter are elegant and useful, but the concrete syntax can be hard on the eyes, at least in a flat text editor.
Maybe in an alternative history where there were more ASCII symbols or a future where Unicode is easier to enter with keystrokes…

## Backtracking is burdensome: predictive language

Whatever characters follow some Matter cannot change its meaning.
Human readers and tokenizers merely need to lookahead at most one extra character.

- Any Matter starting with `@` is an atom.
- Any Matter starting with `0x` is bytes.
- Other than `0x`, any Matter starting with `+`, `-`, or a decimal digit is a decimal.
- Any Matter starting with `"`, `'`, or `_` is text.
- Any Matter starting with `#` is a variant.
- Any Matter starting with `[` a sequence.
- Any Matter starting with `{>` a meta term that will be immeidately followed by a normal term.
- Any Matter starting with `(^` is a normal term that will be immediately followed by a meta term, which will start with `) {<`.
- Any Matter starting with `{=` a meta term somewhere within a sequence.
- Other than `(^`, any Matter starting with `(` is merely a parenthesized term.

## Some text is code: atoms and variants

The most fundamental change in expressivity compared to well-established human-readable serialization languages (JSON, YAML, etc) is that Matter has explicit syntax for tagged unions.

- `#foo A` is a _variant_.
   - `#a [#href "https://xkcd.com/927/"]`
   - `#POSIXTime -2293241640`
   - and so on.
- `@bar` is an _atom_.
   - `@null`
   - `@NaN`
   - `@false`
   - etc.
- Matter considers `@foo` as semantically distinct from `#foo []`, rather than merely syntactic sugar.
  However, their exact relation is always ultimately determined by whoever is interpreting the Matter.

These constructs make it very obvious that the interpretation of Matter depends on how certain text is interpreted.
That's no different than any other serialization language, except that some might enforce case insensivity.

The common idiom for tagged unions in JSON, for example, is to include a distinguished property with a name like `"type"` or `"kind"` or some such.
Variants and atoms simply reify that pattern to a language construct and so shed the choice of a distinguished property name.

You can write Matter with a type property instead of variants.
In some use cases, it's preferable that the presence/absence of various subsets of properties determine the tag.
That same pattern can be used with Matter, when desired.
Another viable idiom is to eschew variants entirely and instead use atoms as in [S-expressions](https://en.wikipedia.org/wiki/S-expression): `[@* 2 [@+ 4 0.9]]`.

There is one unfortunate pitfall to atoms and variants!
Symbols can contain any visually-obvious character.
I don't know Unicode well enough to translate that requirement to existing predicates, so for now I'm limiting symbols just to ASCII.
There, the visually-obvious predicate only excludes control characters, whitespace (famously _invisible_), and the backtick ` (too easily conflated with the apostrophe ').
As a result, `[@bob]` is not well-formed Matter; `[@bob ]` is.
Woof.

Another quirk is that the symbol can be empty: `@` is well-formed, as is `# # # # # @`.
I wouldn't _encourage_ this sort of thing, but they're unambiguous and so allowed.

A geeky upshot I can't help but mention is that the path from the root of some Matter to each Matter subterm can be written as a sequence of self-describing symbols: `@[ @[ @( @{> @^) @[ @{= @{< @(^ @[ @{=`, for example.

## Some "code" is text: meta terms

Matter supports comments as a special instance of a more general mechanism: meta terms.
Matter itself imposes no constraint on meta terms except that their structure is also Matter.
A comment intended for humans to read is almost always going to be a meta term (ie "code") that is merely text.

```
{> "Careful! This is inches, not centimeters!" >} 17.3

(^ #USD 43000 ) {< #TODO "double-check this budget allocation" <} 

[
    @ubuntu
    @darwin
    {= {> #TODO "Disable until the CI machine is fixed." >} @windows =}
]
```

There are three meta terms in Matter.

- The meta term A refers to the term B in `{> A >} B`.
- The meta term B refers to the term A in `(^ A ) {< B <}`.
  The `(^` is called a "pin".
  The `{<<}` term is not allowed to exist without the preceding `(^`.
  The pin serves two purposes.
    - It prevents the ambiguity inherent in the (ill-formed) `#foo 3 {< "which?" <}`.
    - It gives forwarning of B, which can help when A itself is a visually large term.
- A `{==}` meta term refers to its own position within the containing sequence `[ A B {= "middle" =} C D ]`.
  One common use case might be to temporarily comment out an item in a list `[ A B {= C =} D ]`.

There is also a closing pin `^)` which is only allowed when there is a corresponding `{>>}` meta term, as in `{> A >} ( B ^)`.
Note the duality with `(^` and `{<<}`: the rule is that the later piece of syntax implies/requires the earlier piece of syntax.

- `{<<}` implies/requires `(^`.
- `^)` implies/requires `{>>}`.

To clarify, the following are all of the allowed mixtures of meta brackets, parens, and pins (where A and B and C are abbreviating _any_ Matter term).

- `{> A >}    B`
- `{> A >} (  B  )`
- `{> A >} (  B ^)`
- `{> A >} (^ B ^) {< C <}` (meow)
- `{> A >} (^ B  ) {< C <}`
- `        (^ B  ) {< C <}`
- `        (  B  )`

The semantics of `{> A >} (^ B ) {< C <}` is a meta term A that's referring to a meta term B that's referring to a term C.
The other nesting would be incorrect because the `(^` would be delimited from the `{<` and so neither would parse.

Meta terms seem elegant and simultaneously excessive.

- At the very least, they're a nice way to accommodate comments without whitespace/reformatting/etc possibly confusing which term the comment is about.
- I also imagine them as input commands to Matter (pre)processors, like formatting tools, macro expanders, etc.
- Lastly, I occasionally want my comments to have some apparent structure; with meta terms, that structure can simply be Matter.

## Navigation is important: well-established outer brackets

Except for text literals' quotes, all of the pairs of tokens that surround Matter terms have common pairs of ASCII brackets as their outermost characters.

- parens, with or without pins `()` `(^)` `(^^)` 
- meta terms `{>>}` `{==}` `{<<}`
- sequences `[]`
- joiners `<>`

Because of that choice, many text editors' features will apply.
For example, accelerator keystrokes will jump across the entire Matter term, from one well-established bracket character to the other.

## Escapes aren't literal: multiquotes

Having to escape quotes within text literals is a widespread pain among command-line users.
Matter completely forbids escapes in normal text literals.
They are allowed in _joiner text_, which will be discussed in another postulate below---but here's a sneak peek.

```
"He shouted " <"> "Hack the planet!" <"> "out the car window."

"He shouted " <%23> "Hack the planet!" <%23> "out the car window."
```

But Joiners are not the only option.
Matter also provides the quirky but lighterweight option to use an exotic quote for the text literal.
These are called _multiquotes_ in contrast to doublequotes.
A multiquote is 'A' 'AB' 'ABC' or 'ABCD' where A B C and D are any decimal digit.

```
'0'"He shouted "Hack the planet!" out the car window.'0'

'42'"'"'"''42'

'666'Quoth the Raven "Nevermore."'666'

'0000'I hear "I love you / I know" got 1'000'000 laughes in Switzerland.'0000'
```

There are 11110 different multiquotes.
Only an adverarial (and long!, 43210+ characters) text literal would contain all of them, and so at least one multiquote would be compatible for essentially any text literal an honest Matter user would want to write.

Rest assured that multiquotes are optional.
But they're an easy enough to provide the option to the user: a small burden on the tokenizer and arguably self-explanatory.

## Byte arrays are neither numbers nor text

In many languages `0xFF` can be written as an integer literal (equivalent to 255).

In Matter, `0xFF` is not a number, but rather instead a length-one byte array.
`0xBeeF` is length two, `0x` is length zero, and so on.

Sam Hughes lists some helpful reference tables at the brilliant https://github.com/qntm/base65536 repository.
In a UTF-8 encoded Matter file, hexadecimal stores bytes at 50% efficiency (ignoring the fixed `0x` overhead).
It's not great, but it's also not tremendous.
(UTF-16 is 25% efficient, but it's also very uncommon.)
Recall that Matter is a human-readable format, so bytes won't usually be that long anyway---it's likely just hashes or keys.
And if the user does need an onerous length of bytes, they can always fallback to #base64 "…" instead etc.

## Unordered is unreal: only sequences

Matter only supports sequences, no dictionaries/dicts/objects/maps/etc.
Especialy with variants, it's easy to express the same kind of data.

```
[#x 1 #y 2 #z -1]
```

At least one reason for this is that the order in which that sequence is written might itself have meaning, or perhaps improves legibility, etc.
The everything-is-ordered semantics discourages Matter users and tooling from disregarding/discarding that order prematurely.

## Some numbers are text: lax decimals

Matter permits leading zeros, trailing zeros, and unnecessary + signs in its decimals.
Matter permits decimals that are far beyond the precision and/or range of even the ubiquitous IEEE 754.

Humans read and write decimals in idiomatic ways, and therefore Matter preserves exactly what they wrote.

A medical professional might be legally required to write `1.30` instead of `1.3`.
A ticker tape might share that "The stock is +5 today."
An table of numbers might justify all exponents to three characters, 1.7e+03.
All of these are perfectly well-formed and unambiguous decimal fraction, but Matter users and tooling are discouraged from disregarding/discarding the _seemingly extraneous_ signs or zeros prematurely.

I do draw they line at fractions without any leading zeros, though :D.
`.5` is rejected as a probable typo---`0.5` isn't a heavy burden.
Also recall that all Matter decimals begin with a sign or a digit.

Tooling should carefully reject Matter input if the tooling is unable to almost surely interpret the given decimal how the user intended.
For example, tooling that interprets decimals as IEE 754 binary should reject a decimal that has different significant figures than the "shortest" rendering of the interpreted IEEE 754 binary back into decimal.
It's very likely that user did not realize that tool wouldn't be able to preserve exact decimal they wrote (eg they're unaware of IEEE 754 and that there are very few decimal fractions that it can represent _exactly_).

# Weaker Postulates

This section is more of the same, except that I'm less convinced of these postulates/language features.

## Layout isn't literal: joiners

The most familiar way to write a text "literal" that includes newlines is the escaped line feed character `\n`.

```
classic =
    "Hello,\nWorld!\n"
```

However, a text literal with escapes in it manifestly not _literal_.
Some languages do permit literal text to include newlines, but it's confusing and rare, for a good reason…

```
classic =
    "Hello,
World!
"
```

… the layout/indentation is jarring and confusing.
To compenstate, languages allow multi-line text "literals" to control indentation using awkward syntax and/or heuristics.
For example, Haskell permits the following (ignoring the multiquote).

```
haskell1 =
    "Flavors:\n\
    \  - Vanilla\n\
    \  - Chocolate\n\
    \"
```

The backslashes are clearly not literal syntax, but are necessarily to distinguish the layout of the expression itself from the indentation in the denoted text.
YAML famously has a disastrous amount of ways to do this: > to keep the discards the newlines, | to keep them, |- to skip the final newline, etc---it only gets worse from there.
Many familiar languages are either too restrictive or overloaded with options.

Matter uses a simple, powerful, and flexible mechanism called _(text) joiners_.
They allow the layout of the Matter term to differ from the layout of the text it denotes.

```
#joiners1 "Hello," <%0A> "World!"

#joiners2
        "Flavors:"
  <%0A> "  - Vanilla"
  <%0A> "  - Chocolate"
  <%0A> ""
```

Comparing `@joiners2` to the more idiomatic Haskell alternative helps it seem familiar.

```
haskell2 =
      "Flavors:\n"
   ++ "  - Vanilla\n"
   ++ "  - Chocolate\n"
```

Unlike Matter text literals, joiners are allowed to contain escapes, as seen above (0A is the ASCII code for the line feed character \n).
However, joiners are also allowed to contain anything else.
As a side-effect, joiners give the author the option compose their text in a way that emphasizes some substrings.

```
#joiners3
    "apples" <, > "oranges" <, and > "bananas"

#joiners4 "Hello" <,%0A > "World" <> "!"

#joiners5
        "Flavors:"
    <%0A  - > "Vanilla"
    <%0A  - > "Chocolate"
    <%0A> ""
```

Every joiner is composed of the following three parts: joiner delimiters, joiner text, and joiner escapes.

- Joiners begin with `<` and end with `>`.
- Each joiner text is free to contain any character except `>` and `%` (including all whitespace!).
- Each joiner escape is heralded by % and is merely the hexadecimal encoding of hte UTF-8 encoding of a Unicode code point.
  For reference, some common ones include the following.
    - horizontal tab \t is %09
    - linefeed \n is %0A
    - carriage return \r is %0D
    - double quote " is %23
    - single quote ' is %27
    - percent % is %25
    - closing angle bracket > is %3E
- Note that escapes might be multiple bytes, since they're _any_ UTF-8 character
  For example, the Yuan sign ¥ is %C2A5.
  The banana emoji 🍌 is %F09F8D8C.
- A Matter tokenizer should error out if the given nibbles do not denote a valid UTF-8 encoding of a valid Unicode code point.
  The necessary logic is much more tractable than novices expect.

Joiner text and joiner escapes can be freely composed so that any joiner is either empty (ie `<>`) or contains an alternation of non-empty joiner text and non-empty runs of joiner escapes, starting with either.
    - `<, >`
    - `<%0A *>`
    - `<;%0D%0A>`

## Line uniformity for sequences: juxtaposition instead of commas

When a sequence, joined text, or joined bytes is spread across multiple lines, the lines should not unnecessarily constrain each other.
For example, removing any item from the sequence should not require changing any remaining items' line.

The classic example that violates this is forbidding trailing commas in sequences.

```
"json": [
    P,
    Q,
  ]
```

In JSON, removing Q would require removing the comma from the line with P.

Matter sequences satisfy this postulate because they use juxtaposition instead of commas (like Nix, for example).
Removing any or all of X Y and Z would not require altering any remaining line.

```
#matter [
    X
    Y
    Z
  ]
```

The only way to satisfy this postulate in a language that requires commas and also disallows the trailing comma is to put commas on their own lines!
I will admit some fondness for this workaround (the extra visual space can improve legibility), but it seems too uneconomical to be the only way to satisfy the postulae---too much whitespace.

```
"json":
  [
    P
  ,
    Q
  ]
```

As a not insignificant bonus, it's nice to write row vectors `[ 1 2 3 ]` just as linear algebra would, instead of `[1, 2, 3]`.

## Line uniformity & Delimiters up front: suppressor

The line uniformty postulate also applies to joined text and joined byte arrays.

For bytes, the empty `0x` is a sufficient tool, because bytes joiners are always empty.

```
#matter 0x
     <> 0xAA
     <> 0xBB
     <> 0xCC
     <> 0xDD
```

For text, on the other hand, this postulate motivates the suppressor _.
The suppressor enables line uniformity in multiple ways, two of which are demonstrated here.

```
#matter _
   <, > "apples"
   <, > "oranges"
   <, and > _
   <, > "bananas"
```

All three fruits have a `<, >` prefix on their line, despite the fact that neither apples nor bananas are preceded by `, ` in the intepretation "apples, oranges, and bananas".


The suppressor and the joiners it suppresses could be avoided using the same whitespace-intensive alternative mentioned in the previous postulate.

```
#matter
    "apples"
  <, >
    "oranges"
  <, and >
    "bananas"
```

It's nice to still have this pattern an option, but the crucial point is that it's not required, since its syntactic overhead is linear in the length of the list.
The suppressor is instead typically only needed at the start and possibly the end.

```
#matter _
  < ,> "Kermit"
  < ,> "Piggy"
  < ,> "Fozzie"
  < ,> "Gonzo"
  < ,> "Animal"
  < ,> "Rowlf"
  < ,> "Scooter"
  <, and > _
  < ,> "Skeeter"
  <> "."
```

## Backtracking is burdensome & Delimiters up front: suppressor

I favored

```
#matter _
   <, > "apples"
   <, > "oranges"
   <, and > _
   <, > "bananas"
```

over

```
#matter
   "apples" <, >
   "oranges" <, >
   ! "" <, and >
   "bananas" <, >
   ! ""
```

, where ! is a hypothetical _backwards_ suppressor, for two reasons.

- The _ is seen before the joiner, so the reader knows they can ignore the joiner before they reach it.
  The ! on the other hand requires the reader to consider the joiner before any indication that it's suppressed.
  The ! would therefore incur some cognitive backtracking, which is burdensome.
  (TODO should _ be replaced with something more visually obvious? ___?)
- Delimiters should be up front, prefixes rather than suffixes.
  They're usually short, so this isn't inconvenient.
  It makes the context for the whole list easier to see at the start of the list, even if the payloads run past the end of the screen.
  It lets delimiters be vertically aligned even if the payloads are ragged.
