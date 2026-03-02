{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | Emits comma-separated triplets L,R,F where L and R is a
-- left-closed interval of codepoints and F is a short string
-- categorizing that interval: Atom, Text, LoudPin, Whitespace,
-- NumberExponent, etc.
module Main (main) where

import Data.Text.Lazy.IO qualified as TL
import Language.Matter.Tokenizer

go :: SnocsResult -> IO()
go = \case
    SnocsToken l tk r x -> do
        putStrLn $ showHighlight l r tk
        go x
    SnocsError l r err -> 
        putStrLn $ showHighlight l r err
    SnocsDone x -> case eofTokenizer x of
        EofNothing ->
            pure ()
        EofJust tk ->
            putStrLn $ showHighlight (tokenizerStart x) (tokenizerCurrent x) tk
        EofError err ->
            putStrLn $ showHighlight (tokenizerStart x) (tokenizerCurrent x) err

{- TODO

- Flag to run as DBus service.

- Flag to show bytes instead of code points?

-}
main :: IO ()
main = do
    txt <- TL.getContents
    go $ snocsTokenizer startTokenizer txt

-----

-- | The left 'Pos' is always the right 'Pos' of the previous token,
-- but we include it in the output so that each line is coherent the
-- even if some lines have been filtered out
showHighlight :: Highlight x => Pos -> Pos -> x -> String
showHighlight l r x =
    ($ "") $ shows l' . (',' :) . shows r' . (',' :) . shows (highlight x)
  where
    l' = codePoints l
    r' = codePoints r + if bump x then 1 else 0

data Face =
    Atom
  |
    Bracket
  |
    Bytes
  |
    -- | A character that is always incorrect
    --
    -- Note that some 'EofError' data types are not in this class,
    -- such as a missing double quote.
    Error
  |
    Joiner
  |
    JoinerDelim
  |
    JoinerEscape
  |
    -- | Loud because it has huge semantic impact
    LoudMeta
  |
    -- | Loud because it's easy to miss, but is useful to see
    LoudPin
  |
    -- | Loud because it's easy to miss, but is useful to see
    LoudUnderscore
  |
    -- | The exponent part of a number
    NumberExponent
  |
    -- | The fractional part of a number
    NumberFraction
  |
    -- | The whole part of a number
    NumberWhole
  |
    -- | Text, including the quote
    --
    -- TODO have quote separately, as TextDelim
    Text
  |
    Variant
  |
    Whitespace
  deriving (Show)

class Highlight a where
    bump :: a -> Bool
    highlight :: a -> Face

instance Highlight Token where
    bump = \case
        OdToken tk -> bump tk
        SdToken tk -> bump tk
    highlight = \case
        OdToken tk -> highlight tk
        SdToken tk -> highlight tk

instance Highlight OdToken where
    bump _ = False
    highlight = \case
        OdWhitespace -> Whitespace
        OdAtom -> Atom
        OdVariant -> Variant
        OdOpenParen -> Bracket
        OdOpenJoiner -> JoinerDelim
        OdJoinerText -> Joiner
        OdBytes -> Bytes
        OdIntegerPart _ -> NumberWhole
        OdFractionPart -> NumberFraction
        OdExponentPart _ -> NumberExponent

instance Highlight SdToken where
    bump _ = True
    highlight = \case
        SdOpenSequ -> Bracket
        SdCloseSequ -> Bracket
        SdCloseParen -> Bracket
        SdUnderscore -> LoudUnderscore
        SdOpenPin -> LoudPin
        SdClosePin -> LoudPin
        SdOpenMeta _ -> LoudMeta
        SdCloseMeta _ -> LoudMeta
        SdDoubleQuotedString -> Text
        SdMultiQuotedString _ -> Text
        SdJoinerEscapedUtf8 _ -> JoinerEscape
        SdCloseJoiner -> JoinerDelim

instance Highlight EofError where
    bump _ = False
    highlight = \case
        EofNeedOrdering -> Error
        EofNeedRightParen -> Error
        EofNeedRightBrace -> Error
        EofNeedNibble -> Error
        EofNeedDigit _ -> Error
        EofDoubleQuotedString -> Text
        EofMultiQuotedString1 _ -> Text
        EofMultiQuotedString2 _ -> Text
        EofMultiQuotedString3 _ -> Text
        EofJoinerEscapedUtf8 _ -> JoinerEscape

instance Highlight SnocError where
    bump _ = True
    highlight _err = Error
