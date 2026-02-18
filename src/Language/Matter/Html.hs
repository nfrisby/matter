{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Matter.Html (module Language.Matter.Html) where

import Data.String (fromString)
import Data.Text qualified as T
import Language.Matter.Interpreter qualified as I
import Language.Matter.SyntaxTree
import Language.Matter.Tokenizer (MatterStream, Pos (MkPos), slice)
import Language.Matter.Tokenizer.Counting (forgetFour', valueFour)
import Prelude hiding (div, span)
import Text.Blaze.Html5 (Html, (!), div, span, toMarkup)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes as A hiding (span)

html :: forall inp sequ nesequ.
     (MatterStream inp, Functor sequ, Foldable sequ, Foldable nesequ)
 =>
    inp -> Matter sequ (Bytes X) Decimal Symbol (Text nesequ X) Pos -> Html
html inp =
    (\x -> H.docTypeHtml $ do H.head (H.style css); H.body x) . fold phi
  where
    phi = \case
        FlatF flt -> case flt of
            Atom s ->
                let symbol = I.symbolValueText $ I.interpretSymbol inp s
                in
                (span ! A.class_ (fromString $ "variant-" <> T.unpack symbol)) $ markup "@" <> (span ! A.class_ "atom") (toMarkup symbol)
            Bytes b ->
                bytes b
            Number n ->
                toMarkup (I.interpretDecimalAsText inp n)
            Text t ->
                text t
        VariantF s blz ->
            let symbol = I.symbolValueText $ I.interpretSymbol inp s
            in
            markup "#" <> (span ! A.class_ "variant") (toMarkup symbol) <+> (span ! A.class_ (fromString $ "variant-" <> T.unpack symbol)) blz
        SequenceF _l sequ _r ->
            (div ! A.class_ "sequ") $ (span ! A.class_ "markup delim") "[" <> (foldMap sequencePart sequ) <> (span ! A.class_ "markup delim") "]"
        MetaGtF _l blz1 _r (NoClosePin blz2) ->
            meta2 (meta "{>" <+> blz1 <+> meta ">}") <+> blz2
        MetaGtF _l1 blz1 _r1 (OnlyClosePin _l2 blz2 _r2) ->
            meta2 (meta "{>" <+> blz1 <+> meta ">}")
         <+>
            markup "(" <+> blz2 <+> markup "^)"
        MetaGtF _l1 blz1 _r1 (BothPins _l2 blz2 _r2 _l3 blz3 _r3) ->
            meta2 (meta "{>" <+> blz1 <+> meta ">}")
         <+>
            meta "(^" <+> blz2 <+> meta "^)"
         <+>
            meta2 (meta "{<" <+> blz3 <+> meta "<}")
        ParenF _l blz _r ->
            markup "(" <+> blz <+> markup ")"
        PinMetaLtF _l1 blz1 _r1 _l2 blz2 _r2 ->
            meta "(^" <+> blz1 <+> meta ")"
         <+>
            meta2 (meta "{<" <+> blz2 <+> meta "<}")
        
    bytes (BytesLit _blit l r more) =
        markup "0x" <> toMarkup (slice (l <> MkPos 2 2) r inp) <> moreBytes more
    moreBytes = \case
        NoMoreBytes ->
            mempty
        MoreBytes _p b ->
            " " <> markup "<" <> markup ">" <+> bytes b

    text = \case
        Suppressor _p jp j t ->
            markup "_" <+> markup "<" <+> joiner (jp <> MkPos 1 1) j <+> text t
        TextLit q _tlit l r more ->
            (span ! A.class_ clss) (toMarkup (slice l (l <> MkPos nl nl) inp))
         <>
            (span ! A.class_ "text") (toMarkup (slice (l <> MkPos nl nl) (r <> MkPos nr nr) inp))
         <>
            (span ! A.class_ clss) (toMarkup (slice (r <> MkPos nr nr) (r <> MkPos 1 1) inp))
         <> " " <>
            moreText more
          where
            nr = 1 - nl   -- skip last character of delimiter
            (clss, nl) = case q of
                DoubleQuote -> ("quote", 1)
                MultiQuote delim ->
                    ("quote multi", 2 + (valueFour . forgetFour') delim)
    moreText = \case
        NoMoreText ->
            mempty
        MoreText jp j t ->
            " " <+> markup "<" <> joiner (jp <> MkPos 1 1) j <+> text t
    joiner :: Pos -> Joiner nesequ tlit Pos j -> Html
    joiner l = \case
        NilJoiner _p ->
            markup ">"
        ConsJoinerText _tlit r j ->
            (span ! A.class_ "joiner-text") (toMarkup (slice l r inp)) <> joiner r j
        ConsJoinerEscapes escapes j ->
            foldr escape (\l' -> joiner l' j) escapes
          $ l
    escape sz k l =
        let !l' = l <> MkPos 1 1
            !n = 2 * valueFour sz
            !r = l' <> MkPos n n   -- skip % and nibbles
        in
        markup "%" <> (span ! A.class_ "joiner-utf8-hex") (toMarkup (slice l' r inp)) <> k r

(<+>) :: Html -> Html -> Html
l <+> r = l <> " " <> r

markup :: T.Text -> Html
markup = (span ! A.class_ "markup") . toMarkup

meta :: T.Text -> Html
meta = (span ! A.class_ "markup meta") . toMarkup

meta2 :: Html -> Html
meta2 = span ! A.class_ "meta2"

sequencePart :: SequencePart Pos Html -> Html
sequencePart = \case
    Item blz ->
        div blz
    MetaEQ _l blz _r ->
        div $ meta2 (meta "{=" <+> blz <+> meta "=}")

css :: Html
css =
    ".markup { color: blue; text-decoration: underline; }\n\
    \.markup.meta { color: pink; }\n\
    \.meta2 > .text { color: deepskyblue; }\n\
    \.meta2 { background-color: cornsilk; }\n\
    \.text { white-space: pre; }\n\
    \.joiner-text { white-space: pre; }\n\
    \.joiner-utf8-hex { color: purple; }\n\
    \.atom { color: orange; }\n\
    \.variant { color: green; }\n\
    \.quote { color: purple; }\n\
    \.quote.multi { vertical-align: super; }\n\
    \.sequ {\n\
    \    border: dotted 2px darkgray;\n\
    \    display: inline-flex;\n\
    \    align-items: center;\n\
    \    gap: 1em 1em;\n\
    \    flex-wrap: wrap;\n\
    \    margin: 0.5em;\n\
    \}\n\
    \.sequ > div {\n\
    \    flex: 1 1 content;;\n\
    \    padding: 1em;\n\
    \.sequ > span.delim {\n\
    \    flex: 0.1 10 auto;;\n\
    \    padding: 0.1em;\n\
    \}\n\
    \"
