{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | The tokenization state machine
module Language.Matter.Tokenizer.StateMachine (

    -- * Tokens
    Token (..),
    OdToken (..),
    SdToken (..),

    -- * Input streams
    MatterStream (..),
    MunchResult (..),
    Pos (..),
    UnconsResult (..),
    posDiff,

    -- * Tokenizer states
    MultiQuotePartialMatch (..),
    St (..),
    Tokenizer (..),
    startTokenizer,
    tokenizerCurrent,
    tokenizerStart,

    -- * Tokenizer transitions
    EofError (..),
    EofResult (..),
    SnocError (..),
    SnocResult (..),
    SnocsResult (..),
    eofTokenizer,
    snocTokenizer,
    snocsTokenizer,

    -- * Accelerating loop transitions
    SetChar (..),
    loops,
    memberSetChar,

  ) where

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Word (Word32)
import Language.Matter.Tokenizer.Counting
import Language.Matter.Tokenizer.SetChar

{-

For reference, the following are all the ways a non-whitespace token
could begin. Each line contains a minimal example of all tokens that
could start with the same character. The lines are roughly grouped and
roughly ordered.

    @ At

    # Hash

    [ LeftBracket

    ] RightBracket

    ( (^  LeftParen

    ) RightParen

    ^) Caret

    {< {= {> LeftBrace

    < <} LeftAngle

    % Percent

    =} Equals

    >} RightAngle

    0x 0 Zero
    1 One
    2 Two
    3 Three
    4 Four
    5 Five
    6 Six
    7 Seven
    8 Eight
    9 Nine
    +1 Plus
    -1 Minus

    "" DoubleQuote

    '0''0' SingleQuote

    _ Underscore

-}

-- | A state of the tokenizer
--
-- This state does not track position, length, the input stream, etc.
data St =
    Start
  |
    MoreWhitespace
  |
    ManyIdAtom
  |
    ManyIdVariant
  |
    LeftParenSuccessor
  |
    CaretSuccessor
  |
    LeftBraceSuccessor
  |
    LeftAngleSuccessor
  |
    EqualsSuccessor
  |
    RightAngleSuccessor
  |
    ZeroSuccessor
  |
    ManyBytes
  |
    SomeDigits !Three
  |
    ManyDigits !Three
  |
    Exponent
  |
    DoubleQuotedString
  |
    -- | The delimiter so far
    MultiQuotedString1 !(Maybe' (Four' D10))
  |
    -- | The delimeter's digits
    MultiQuotedString2 !(Four' D10)
  |
    -- | The delimiter and the potential match so far
    MultiQuotedString3 !(MultiQuotePartialMatch D10)
  |
    -- | Whether this includes the @<@
    JoinerNotEscaped !Bool
  |
    JoinerEscapedUtf8 !(Maybe' Utf8Size)

data Utf8Size =
    Utf8Size1
  |
    Utf8Size2
  |
    Utf8Size3
  |
    -- | Whether 'SnocUtf8TooGreat' is still possible
    Utf8Size4 !Bool

valueUtf8Size :: Utf8Size -> Four
valueUtf8Size = \case
    Utf8Size1 -> Four1
    Utf8Size2 -> Four2
    Utf8Size3 -> Four3
    Utf8Size4 _ -> Four4

-- | A multiquote delimiter is a opening single quote, one to four
-- digits, and a closing single quote
data MultiQuotePartialMatch a =
    -- | The opening single quote is the only thing that has been matched so far
    MatchedNone !(Four' a)
  |
    -- | Matched some-but-not-all digits (ie one) of a delimiter that has two digits
    MatchedOneOf2 !a !a
  |
    -- | Matched some-but-not-all digits of a delimiter that has three digits
    MatchedSomeOf3 !a !a !a !Two
  |
    -- | Matched some-but-not-all digits of a delimiter that has four digits
    MatchedSomeOf4 !a !a !a !a !Three
  |
    -- | The closing single quote is the only thing left to match
    MatchedAll !(Four' a)

-----

deriving instance Show St
deriving instance Show Utf8Size
deriving instance Eq St
deriving instance Eq Utf8Size

deriving instance Show a => Show (MultiQuotePartialMatch a)
deriving instance Eq a => Eq (MultiQuotePartialMatch a)

-----

-- | An index into the input stream
--
-- In a UTF8 file, eg, this counts code points, not bytes.
newtype Pos = MkPos Word32
  deriving (Eq)

instance Show Pos where show (MkPos x) = 'P' : show x

posDiff :: Pos -> Pos -> Word32
posDiff (MkPos x) (MkPos y) = x - y

data Token =
    OdToken !OdToken
  |
    SdToken !SdToken
  deriving (Eq, Show)

data Tokenizer =
    MkTokenizer {-# UNPACK #-} !Pos {-# UNPACK #-} !Pos !St
  deriving (Eq, Show)

-- | The first character of the next token to be delimited, if any
tokenizerStart :: Tokenizer -> Pos
tokenizerStart (MkTokenizer start _cur _st) = start

-- | The position of the next input character to be provided by
-- 'snocTokenizer' or 'snocsTokenizer'
tokenizerCurrent :: Tokenizer -> Pos
tokenizerCurrent (MkTokenizer _start cur _st) = cur

-- | The freshly initialized tokenizer
startTokenizer :: Tokenizer
startTokenizer = MkTokenizer (MkPos 0) (MkPos 0) Start

data UnconsResult a = UnconsNothing | UnconsJust !Char !a

-- | Result along with how many were dropped
data MunchResult a = MkMunchResult !Word32 !a

class MatterStream a where
    -- | Advance one character
    uncons :: a -> UnconsResult a
    -- | Drop characters that satisfy 'memberSetChar'
    munch :: SetChar -> a -> MunchResult a

instance (a ~ Char) => MatterStream [a] where
    uncons = \case
        [] -> UnconsNothing
        c:s -> UnconsJust c s
    munch sc =
        go 0
      where
        go !acc = \case
            [] -> MkMunchResult acc []
            c:s ->
                if memberSetChar c sc then go (acc + 1) s else
                MkMunchResult acc (c:s)

instance MatterStream T.Text where
    uncons = maybe UnconsNothing (uncurry UnconsJust) . T.uncons
    munch sc txt =
        MkMunchResult
            (fromIntegral (T.length txt - T.length txt'))
            txt'
      where
        txt' = T.dropWhile (flip memberSetChar sc) txt

instance MatterStream TL.Text where
    uncons = maybe UnconsNothing (uncurry UnconsJust) . TL.uncons
    munch sc txt =
        MkMunchResult
            (fromIntegral (TL.length txt - TL.length txt'))
            txt'
      where
        txt' = TL.dropWhile (flip memberSetChar sc) txt

data SnocsResult =
    -- | A token, the position of its first character, and either the
    -- position of the character after the 'OdToken' or the
    -- position of final character of the 'SdToken'
    --
    -- The second 'Pos' is the first 'Pos' of the 'SnocsResult', but
    -- is immediately available without further evaluation.
    SnocsToken {-# UNPACK #-} !Pos !Token {-# UNPACK #-} !Pos SnocsResult
  |
    -- | The 'tokenizeStart' and 'tokenizerCurrent' when the error was
    -- recognized.
    SnocsError {-# UNPACK #-} !Pos {-# UNPACK #-} !Pos !SnocError
  |
    SnocsDone !Tokenizer
  deriving (Eq, Show)

snocsTokenizer :: MatterStream a => Tokenizer -> a -> SnocsResult
snocsTokenizer =
    \(MkTokenizer (MkPos start) (MkPos cur) st) -> go start cur st
  where
    go !start !cur !st = (. uncons) $ \case
        UnconsNothing ->
            SnocsDone $ MkTokenizer (MkPos start) (MkPos cur) st
        UnconsJust c s -> case snoc (MkPos start) (MkPos cur) st c of
            SnocEpsilon st' ->
                go' start     (cur + 1) st' s
            SnocOd tk st' ->
                SnocsToken (MkPos start) (OdToken tk) (MkPos cur)
              $ go'  cur      (cur + 1) st' s
            SnocSd tk st' ->
                SnocsToken (MkPos start) (SdToken tk) (MkPos cur)
              $ go' (cur + 1) (cur + 1) st' s
            SnocOdSd tk1 tk2 st' ->
                SnocsToken (MkPos start) (OdToken tk1) (MkPos cur)
              $ SnocsToken (MkPos cur) (SdToken tk2) (MkPos cur)
              $ go' (cur + 1) (cur + 1) st' s
            SnocError err ->
                SnocsError (MkPos start) (MkPos cur) err

    go' start cur st s = case loops st of
        Nothing ->
            go start cur st s
        Just sc ->
            let MkMunchResult n s' = munch sc s
            in
            go start (cur + n) st s'

{-# SPECIALIZE snocsTokenizer :: Tokenizer -> String  -> SnocsResult #-}
{-# SPECIALIZE snocsTokenizer :: Tokenizer -> T.Text  -> SnocsResult #-}
{-# SPECIALIZE snocsTokenizer :: Tokenizer -> TL.Text -> SnocsResult #-}

-----

data EofResult =
    -- | The EOF was immediately after an 'SdToken'
    EofNothing
  |
    -- | The EOF delimited an 'OdToken'
    --
    -- It begins at 'tokenizerStart' and ends one before
    -- 'tokenizerCurrent'.
    EofJust !OdToken
  |
    EofError !EofError
  deriving (Eq, Show)

-- | Other-delimited tokens
--
-- In other words, they would be valid without the next input but
-- might also be valid with the next input.
data OdToken =
    OdWhitespace
  |
    OdAtom
  |
    OdVariant
  |
    OdOpenParen
  |
    -- | Whether this token includes the @<@
    --
    -- Note that this token never includes the @>@.
    OdJoinerNotEscaped !Bool
  |
    OdBytes
  |
    OdIntegerPart
  |
    OdFractionPart
  |
    OdExponentPart
  deriving (Eq, Show)

data EofError =
    EofNeedOrdering
  |
    EofNeedRightParen
  |
    EofNeedRightBrace
  |
    EofNeedNibble
  |
    EofNeedDigit !Three
  |
    EofDoubleQuotedString
  |
    EofMultiQuotedString1 !(Maybe' (Four' D10))
  |
    EofMultiQuotedString2 !(Four' D10)
  |
    EofMultiQuotedString3 !(MultiQuotePartialMatch D10)
  |
    EofJoinerEscapedUtf8 !(Maybe' Four)
  |
    EofNeedExponent
  deriving (Eq, Show)

-- | There will never be another character
eofTokenizer :: Tokenizer -> EofResult
eofTokenizer (MkTokenizer start cur st) = eof start cur st

eof :: Pos -> Pos -> St -> EofResult
eof start cur = \case

    Start -> EofNothing

    MoreWhitespace -> EofJust OdWhitespace

    ManyIdAtom -> EofJust OdAtom

    ManyIdVariant -> EofJust OdVariant

    LeftParenSuccessor -> EofJust OdOpenParen

    CaretSuccessor -> EofError EofNeedRightParen

    EqualsSuccessor -> EofError EofNeedRightBrace

    RightAngleSuccessor -> EofError EofNeedRightBrace

    LeftBraceSuccessor -> EofError EofNeedOrdering

    LeftAngleSuccessor -> EofJust $ OdJoinerNotEscaped True

    ZeroSuccessor -> EofJust OdIntegerPart

    ManyBytes ->
        if odd (posDiff cur start)
        then EofError EofNeedNibble
        else EofJust OdBytes

    SomeDigits acc -> EofError $ EofNeedDigit acc

    ManyDigits acc -> EofJust $ case acc of
        Three1 -> OdIntegerPart
        Three2 -> OdFractionPart
        Three3 -> OdExponentPart

    Exponent -> EofError EofNeedExponent

    DoubleQuotedString -> EofError EofDoubleQuotedString

    MultiQuotedString1 acc -> EofError $ EofMultiQuotedString1 acc

    MultiQuotedString2 acc -> EofError $ EofMultiQuotedString2 acc

    MultiQuotedString3 acc -> EofError $ EofMultiQuotedString3 acc

    JoinerNotEscaped acc ->
        if cur == start
        then EofNothing   -- no zer-length tokens!
        else EofJust $ OdJoinerNotEscaped acc

    JoinerEscapedUtf8 acc -> EofError $ EofJoinerEscapedUtf8 (valueUtf8Size <$> acc)

-----

data SnocResult =
    -- | A state transition that does delimit or end a token
    SnocEpsilon !St
  |
    -- | The given characer delimited an 'OdToken'
    --
    -- It begins at 'tokenizerStart' and ends one before
    -- 'tokenizerCurrent'.
    SnocOd !OdToken !St
  |
    -- | The given character ended an 'SdToken'
    --
    -- It begins and at 'tokenizerCurrent'.
    SnocSd !SdToken !St
  |
    -- | The given character delimited an 'OdToken' and (both began
    -- and) ended an 'SdToken'
    --
    -- The 'OdToken' began at 'tokenizerStart' ands ends one before
    -- 'tokenizerCurrent'. The 'SdToken' begins and ends at
    -- 'tokenizerCurrent'.
    SnocOdSd !OdToken !SdToken !St
  |
    SnocError !SnocError
  deriving (Eq, Show)

-- | Self-delimited tokens
--
-- In other words, they do not require a lookahead to delimit.
data SdToken =
    SdOpenSeq
  |
    SdCloseSeq
  |
    SdCloseParen
  |
    SdUnderscore
  |
    SdOpenPin
  |
    SdClosePin
  |
    SdOpenMeta !Ordering
  |
    SdCloseMeta !Ordering
  |
    SdDoubleQuotedString
  |
    -- | The delimiter
    SdMultiQuotedString !(Four' D10)
  |
    -- | Whether this token also includes the @<@
    --
    -- Note that this token always includes the @>@.
    SdJoinerNotEscaped !Bool
  |
    -- | The number of bytes in the UTF8 encoding of the code point
    --
    -- TODO shoud this exclude the length, since 'Pos' carries that
    -- info?
    --
    -- TODO without a length argument, should adjacent escapes be
    -- combined into a single token?
    SdJoinerEscapedUtf8 !Four
  deriving (Eq, Show)

data SnocError =
    SnocNeedStart
  |
    SnocNeedNibble
  |
    -- | Whether we're in the integer part (of the mantissa), the
    -- fraction part (of the mantissa), or the exponent part
    SnocNeedDigit !Three
  |
    SnocNeedRightParen
  |
    SnocNeedOrdering
  |
    SnocNeedRightBrace
  |
    SnocNeedExponent
  |
    SnocNeedSingleQuote
  |
    SnocNeedMultiQuoteDigit
  |
    SnocNeedDigitOrSingleQuote
  |
    -- | The binary representation of the first nibble in any UTF8
    -- code point must not begin with 10.
    SnocBadUtf8Nibble1
  |
    -- | The binary representation of the second nibble in a 4 byte
    -- UTF8 code point must begin with 0.
    SnocBadUtf8Nibble2
  |
    -- | The binary representation of every nibble after the first
    -- byte in any UTF8 code point must begin with 10.
    SnocBadUtf8NibbleN
  |
    -- | The greatest UTF8 code point is U+10FFFF, which is encoded in
    -- UTF8 as F48FBFBF; this error means the user wrote something
    -- greater than that.
    SnocUtf8TooGreat
  deriving (Eq, Show)

snocTokenizer :: Tokenizer -> Char -> SnocResult
snocTokenizer (MkTokenizer start cur st) = snoc start cur st

snoc :: Pos -> Pos -> St -> Char -> SnocResult
snoc start cur = \case

    Start -> snocStart SnocEpsilon SnocSd

    -- loop
    MoreWhitespace -> \c ->
        if isWS c then SnocEpsilon MoreWhitespace else
        jumpStart OdWhitespace c

    -- loop
    ManyIdAtom -> \c ->
        if isID c then SnocEpsilon ManyIdAtom else
        jumpStart OdAtom c
        
    -- loop
    ManyIdVariant -> \c ->
        if isID c then SnocEpsilon ManyIdVariant else
        jumpStart OdVariant c
        
    LeftParenSuccessor -> \case
        '^' -> SnocSd SdOpenPin Start
        c -> jumpStart OdOpenParen c

    CaretSuccessor -> \case
        ')' -> SnocSd SdClosePin Start
        _ -> SnocError SnocNeedRightParen

    LeftBraceSuccessor -> \case
        '<' -> SnocSd (SdOpenMeta LT) Start
        '=' -> SnocSd (SdOpenMeta EQ) Start
        '>' -> SnocSd (SdOpenMeta GT) Start
        _ -> SnocError SnocNeedOrdering

    LeftAngleSuccessor -> \case
        '}' -> SnocSd (SdCloseMeta LT) Start
        '%' -> SnocOd (OdJoinerNotEscaped True) $ JoinerEscapedUtf8 Nothing'
        '>' -> SnocSd (SdJoinerNotEscaped True) Start
        _ -> SnocEpsilon (JoinerNotEscaped True)

    EqualsSuccessor -> \case
        '}' -> SnocSd (SdCloseMeta EQ) Start
        _ -> SnocError SnocNeedRightBrace

    RightAngleSuccessor -> \case
        '}' -> SnocSd (SdCloseMeta GT) Start
        _ -> SnocError SnocNeedRightBrace

    ZeroSuccessor -> \case
        'x' -> SnocEpsilon ManyBytes
        '.' -> SnocOd OdIntegerPart $ SomeDigits Three2
        'e' -> SnocOd OdIntegerPart Exponent
        'E' -> SnocOd OdIntegerPart Exponent
        _ -> SnocEpsilon (ManyDigits Three1)

    -- loop
    ManyBytes -> \c ->
        if isD16 c then SnocEpsilon ManyBytes else
        if odd (posDiff cur start)
        then SnocError SnocNeedNibble
        else jumpStart OdBytes c

    SomeDigits acc -> \c ->
        if isD10 c then SnocEpsilon $ ManyDigits acc else
        SnocError $ SnocNeedDigit acc

    -- loop
    ManyDigits acc -> \c ->
        let tk = case acc of
                Three1 -> OdIntegerPart
                Three2 -> OdFractionPart
                Three3 -> OdExponentPart
        in
        if isD10 c then SnocEpsilon $ ManyDigits acc else
        case (acc, c) of
            (Three1, '.') -> SnocOd tk $ SomeDigits Three2
            (Three1, 'e') -> SnocOd tk Exponent
            (Three1, 'E') -> SnocOd tk Exponent
            (Three2, 'e') -> SnocOd tk Exponent
            (Three2, 'E') -> SnocOd tk Exponent
            _ -> jumpStart tk c

    Exponent -> \case
        '+' -> SnocEpsilon $ SomeDigits Three3
        '-' -> SnocEpsilon $ SomeDigits Three3
        c | isD10 c -> SnocEpsilon $ ManyDigits Three3
        _ -> SnocError SnocNeedExponent

    -- loop
    DoubleQuotedString -> \case
        '"' -> SnocSd SdDoubleQuotedString Start
        _ -> SnocEpsilon DoubleQuotedString

    MultiQuotedString1 acc -> \case
        '\'' -> case acc of
            Nothing' -> SnocError SnocNeedMultiQuoteDigit
            Just' delim -> SnocEpsilon $ MultiQuotedString2 delim
        c | Just x <- parseD10 c -> case pushMultiQuoteDelim x acc of
            Nothing -> SnocError SnocNeedSingleQuote
            Just acc' -> SnocEpsilon $ MultiQuotedString1 $ Just' acc'
        _ -> SnocError $ case acc of
            Nothing' -> SnocNeedMultiQuoteDigit
            Just' delim -> case delim of
                Four4' _ _ _ _ -> SnocNeedSingleQuote
                _ -> SnocNeedDigitOrSingleQuote
        
    -- loop
    MultiQuotedString2 delim -> \case
        '\'' -> SnocEpsilon $ MultiQuotedString3 $ MatchedNone delim
        _ -> SnocEpsilon $ MultiQuotedString2 delim

    MultiQuotedString3 acc -> \case
        '\''
          | MatchedAll delim <- acc
         -> SnocSd (SdMultiQuotedString delim) Start
        c | Just x <- parseD10 c
          , Just acc' <- pushMultiQuotePartialMatch x acc
         -> SnocEpsilon $ MultiQuotedString3 acc'
        _ -> SnocEpsilon $ MultiQuotedString2 $ forgetMultiQuotePartialMatch acc

    -- loop
    JoinerNotEscaped acc -> \case
        '%' -> SnocOd (OdJoinerNotEscaped acc) (JoinerEscapedUtf8 Nothing')
        '>' -> SnocSd (SdJoinerNotEscaped acc) Start
        _ -> SnocEpsilon (JoinerNotEscaped acc)

    JoinerEscapedUtf8 acc -> (. parseD16) $ \case
        Nothing -> SnocError SnocNeedNibble
        Just x -> case acc of
            Nothing' -> case leadingBitCountPlus1 x of
                Five1 -> SnocEpsilon $ JoinerEscapedUtf8 $ Just' Utf8Size1
                Five2 -> SnocError SnocBadUtf8Nibble1
                Five3 -> SnocEpsilon $ JoinerEscapedUtf8 $ Just' Utf8Size2
                Five4 -> SnocEpsilon $ JoinerEscapedUtf8 $ Just' Utf8Size3
                Five5 -> SnocEpsilon $ JoinerEscapedUtf8 $ Just' $ Utf8Size4 True
            Just' size

              -- | the last nibble is always unconstrained; recall start is the Percent sign
              | posDiff cur start == 2 * valueFour (valueUtf8Size size)
             -> SnocSd (SdJoinerEscapedUtf8 (valueUtf8Size size)) $ JoinerNotEscaped False

              | odd (posDiff cur start)   -- Note that cur - start > 1 because acc is Just'
              , Five2 /= leadingBitCountPlus1 x
             -> SnocError SnocBadUtf8NibbleN

              | Utf8Size4 _ <- size
              , 2 == posDiff cur start   -- second nibble
              , Five1 /= leadingBitCountPlus1 x
             -> SnocError SnocBadUtf8Nibble2

              | Utf8Size4 True <- size
             -> case checkUtf8TooGreat (posDiff cur start) x of
                  Nothing' -> SnocError SnocUtf8TooGreat
                  Just' acc' -> SnocEpsilon $ JoinerEscapedUtf8 $ Just' $ Utf8Size4 acc'

              | otherwise -> SnocEpsilon (JoinerEscapedUtf8 acc)

-- | F48FBFBF is the greatest possible UTF8 encoding
checkUtf8TooGreat :: Word32 -> D16 -> Maybe' Bool
checkUtf8TooGreat nth c

  -- The second nibble is 0uvv, from the U+uvwxyz schema. The least 4
  -- byte UTF8 code point is U+010000 and the greatest is U+10FFFF. So
  -- uuuu is either 0 or 1. And vvvv must be 0 when u = 1.
  --
  -- So the second nibble is at most 4, is 0100.
  | 2 == nth = case compare c D16_4 of
        LT -> Just' False
        EQ -> Just' True
        GT -> Nothing'

  -- The third nibble is 10vv. So it must be 8 when u = 1.
  | 3 == nth, c > D16_8 = Nothing'

  | otherwise = Just' False

jumpStart :: OdToken -> Char -> SnocResult
{-# INLINE jumpStart #-}
jumpStart tk c = snocStart (SnocOd tk) (SnocOdSd tk) c

snocStart :: (St -> SnocResult) -> (SdToken -> St -> SnocResult) -> Char -> SnocResult
{-# INLINE snocStart #-}
snocStart snocNothing snocJust = \case
    c | isWS c -> snocNothing MoreWhitespace
    '@' -> snocNothing ManyIdAtom
    '#' -> snocNothing ManyIdVariant
    '[' -> snocJust SdOpenSeq Start
    ']' -> snocJust SdCloseSeq Start
    '(' -> snocNothing LeftParenSuccessor
    ')' -> snocJust SdCloseParen Start
    '^' -> snocNothing CaretSuccessor
    '{' -> snocNothing LeftBraceSuccessor
    '<' -> snocNothing LeftAngleSuccessor
    '=' -> snocNothing EqualsSuccessor
    '>' -> snocNothing RightAngleSuccessor
    '0' -> snocNothing ZeroSuccessor
    c | isD10 c -> snocNothing $ ManyDigits Three1
    '+' -> snocNothing $ SomeDigits Three1
    '-' -> snocNothing $ SomeDigits Three1
    '"' -> snocNothing DoubleQuotedString
    '\'' -> snocNothing $ MultiQuotedString1 Nothing'
    '_' -> snocJust SdUnderscore Start
    _ -> SnocError SnocNeedStart

-----

-- | Give up on the match, and reset to the full delimiter
forgetMultiQuotePartialMatch :: MultiQuotePartialMatch D10 -> Four' D10
forgetMultiQuotePartialMatch = \case
    MatchedNone delim -> delim
    MatchedOneOf2 a b -> Four2' a b
    MatchedSomeOf3 a b c _ -> Four3' a b c
    MatchedSomeOf4 a b c d _ -> Four4' a b c d
    MatchedAll delim -> delim

-- | Check if this digit is the next expected digit
pushMultiQuotePartialMatch :: D10 -> MultiQuotePartialMatch D10 -> Maybe (MultiQuotePartialMatch D10)
pushMultiQuotePartialMatch x = \case
    MatchedNone delim -> case delim of
        Four1' a -> if x /= a then Nothing else Just $ MatchedAll delim
        Four2' a b -> if x /= a then Nothing else Just $ MatchedOneOf2 a b
        Four3' a b c -> if x /= a then Nothing else Just $ MatchedSomeOf3 a b c Two1
        Four4' a b c d -> if x /= a then Nothing else Just $ MatchedSomeOf4 a b c d Three1
    MatchedOneOf2 a b ->
        if x /= b then Nothing else Just $ MatchedAll $ Four2' a b
    MatchedSomeOf3 a b c acc -> case acc of
        Two1 -> if x /= b then Nothing else Just $ MatchedSomeOf3 a b c Two2
        Two2 -> if x /= c then Nothing else Just $ MatchedAll $ Four3' a b c
    MatchedSomeOf4 a b c d acc -> case acc of
        Three1 -> if x /= b then Nothing else Just $ MatchedSomeOf4 a b c d Three2
        Three2 -> if x /= c then Nothing else Just $ MatchedSomeOf4 a b c d Three3
        Three3 -> if x /= d then Nothing else Just $ MatchedAll $ Four4' a b c d
    MatchedAll _ -> Nothing

-- | Add another if there's room
pushMultiQuoteDelim :: a -> Maybe' (Four' a) -> Maybe (Four' a)
pushMultiQuoteDelim x = \case
    Nothing' -> Just $ Four1' x
    Just' delim -> case delim of
        Four1' a -> Just $ Four2' a x
        Four2' a b -> Just $ Four3' a b x
        Four3' a b c -> Just $ Four4' a b c x
        Four4' _ _ _ _ -> Nothing

-----

-- | The loops from this state to itself
loops :: St -> Maybe SetChar
loops = \case

    MoreWhitespace -> Just setCharWS
    ManyIdAtom -> Just setCharID
    ManyIdVariant -> Just setCharID
    ManyBytes -> Just setCharD16
    ManyDigits _ -> Just setCharD10
    DoubleQuotedString -> Just $ setCharComplement1 '"'
    MultiQuotedString2 _ -> Just $ setCharComplement1 '\''
    JoinerNotEscaped _ -> Just $ setCharComplement2 '%' '>'

    _ -> Nothing
