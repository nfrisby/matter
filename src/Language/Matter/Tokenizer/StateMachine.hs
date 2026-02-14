{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | The tokenization state machine
module Language.Matter.Tokenizer.StateMachine (

    -- * Tokens
    OdToken (..),
    MaybeSign (..),
    SdToken (..),
    Sign (..),
    Token (..),

    -- * Input streams
    MatterStream (..),
    MunchResult (..),
    Pos (..),
    UnconsResult (..),
    defaultSlowMunch,
    posDiff,

    -- * Tokenizer states
    MultiQuotePartialMatch (..),
    NumberPart (..),
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

import Data.Text.Short qualified as TS
import Data.Text qualified as T
import Data.Text.Internal qualified as TI
import Data.Text.Internal.Encoding.Utf8 qualified as TI (utf8Length)
import Data.Text.Lazy qualified as TL
import Data.Text.Internal.Lazy qualified as TLI
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

data SomeOrMany = Some | Many

data NumberPart =
    IntegerPart !MaybeSign
  |
    FractionPart
  |
    ExponentPart !MaybeSign

-- | Try to add a + or - to the 'Digits' state
snocSign :: NumberPart -> Sign -> SnocResult
snocSign part sgn = case part of
    IntegerPart mbSign -> f IntegerPart mbSign
    FractionPart -> SnocError $ SnocNeedDigit FractionPart
    ExponentPart mbSign -> f ExponentPart mbSign
  where
    f g = \case
        NothingSign -> SnocEpsilon $ Digits Some $ g $ JustSign sgn
        JustSign{} -> SnocError SnocNoSign

-----

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
    Digits !SomeOrMany !NumberPart
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
    JoinerNotEscaped
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

deriving instance Show SomeOrMany
deriving instance Show NumberPart
deriving instance Show St
deriving instance Show Utf8Size
deriving instance Eq SomeOrMany
deriving instance Eq NumberPart
deriving instance Eq St
deriving instance Eq Utf8Size

deriving instance Show a => Show (MultiQuotePartialMatch a)
deriving instance Eq a => Eq (MultiQuotePartialMatch a)

-----

-- | An index into the input stream
data Pos = MkPos { codePoints, utf8Bytes :: !Word32 }
  deriving (Eq)

instance Monoid Pos where mempty = MkPos 0 0
instance Semigroup Pos where MkPos a b <> MkPos x y = MkPos (a + x) (b + y)

instance Show Pos where show x = 'P' : show (codePoints x)

charPos :: Char -> Pos
{-# INLINE charPos #-}
charPos c = MkPos {
    codePoints = 1
  ,
    utf8Bytes = fromIntegral $ TI.utf8Length c
  }

posDiff :: Pos -> Pos -> Word32
posDiff x y = codePoints x - codePoints y

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
startTokenizer = MkTokenizer mempty mempty Start

data UnconsResult inp = UnconsNothing | UnconsJust !Char !inp

-- | Result along with how many were dropped
data MunchResult inp = MkMunchResult !Pos !inp

-- | An input stream that can be segmented into Matter tokens
class MatterStream inp where
    -- | Advance one character
    uncons :: inp -> UnconsResult inp
    -- | Drop characters that satisfy 'memberSetChar'
    munch :: SetChar -> inp -> MunchResult inp

    sliceShort :: Pos -> Pos -> inp -> TS.ShortText
    slice :: Pos -> Pos -> inp -> T.Text

-- | An opt-in default for 'munch'
--
-- Does not call the 'munch' method. Calls once 'uncons' per
-- character.
defaultSlowMunch :: MatterStream inp => SetChar -> inp -> MunchResult inp
defaultSlowMunch sc =
    go mempty
  where
    go !acc inp = case uncons inp of
        UnconsNothing ->
            MkMunchResult acc inp
        UnconsJust c inp' ->
            if memberSetChar c sc then go (acc <> charPos c) inp' else
            MkMunchResult acc inp

instance (a ~ Char) => MatterStream [a] where
    uncons = \case
        [] -> UnconsNothing
        c:s -> UnconsJust c s
    munch = defaultSlowMunch

    sliceShort x y =
        TS.pack . take (fromIntegral $ codePoints y - codePoints x) . drop (fromIntegral $ codePoints x)
    slice x y =
        T.pack . take (fromIntegral $ codePoints y - codePoints x) . drop (fromIntegral $ codePoints x)

instance MatterStream T.Text where
    uncons = maybe UnconsNothing (uncurry UnconsJust) . T.uncons
    munch sc txt =
        MkMunchResult
            MkPos{codePoints = fromIntegral n, utf8Bytes = fromIntegral b}
            txt'
      where
        txt' = T.dropWhile (flip memberSetChar sc) txt

        n = T.length txt - T.length txt'
        b =
            let TI.Text _arr1 off1 _len1 = txt
                TI.Text _arr2 off2 _len2 = txt'
            in
            off2 - off1

    sliceShort x y = TS.fromText . slice x y
    slice x y =
        -- TODO use utf8Bytes instead of codePoints
        T.take (fromIntegral $ codePoints y - codePoints x) . T.drop (fromIntegral $ codePoints x)

instance MatterStream TL.Text where
    uncons = maybe UnconsNothing (uncurry UnconsJust) . TL.uncons
    munch sc =
        \txt -> go mempty txt
      where
        go !pos = \case
            TLI.Empty -> MkMunchResult pos TLI.Empty
            TLI.Chunk txt chunks ->
                let MkMunchResult pos' txt' = munch sc txt
                    pos'' = pos <> pos'
                in
                if T.null txt' then go pos'' chunks else
                MkMunchResult pos'' $ TLI.Chunk txt' chunks

    sliceShort x y = TS.fromText . slice x y
    slice x y =
        -- TODO use Builder.toLazyTextWith?
        TL.toStrict . TL.take (fromIntegral $ codePoints y - codePoints x) . TL.drop (fromIntegral $ codePoints x)

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

snocsTokenizer :: MatterStream inp => Tokenizer -> inp -> SnocsResult
snocsTokenizer =
    \(MkTokenizer start cur st) -> go start cur st
  where
    go !start !cur !st = (. uncons) $ \case
        UnconsNothing ->
            SnocsDone $ MkTokenizer start cur st
        UnconsJust c s -> case snoc start cur st c of
            SnocEpsilon st' ->
                go' start     (cur <> charPos c) st' s
            SnocOd tk st' ->
                SnocsToken start (OdToken tk) cur
              $ go'  cur      (cur <> charPos c) st' s
            SnocSd tk st' ->
                SnocsToken start (SdToken tk) cur
              $ go' (cur <> charPos c) (cur <> charPos c) st' s
            SnocOdSd tk1 tk2 st' ->
                SnocsToken start (OdToken tk1) cur
              $ SnocsToken cur   (SdToken tk2) cur
              $ go' (cur <> charPos c) (cur <> charPos c) st' s
            SnocError err ->
                SnocsError start cur err

    go' start cur st s = case loops st of
        Nothing ->
            go start cur st s
        Just sc ->
            let MkMunchResult n s' = munch sc s
            in
            go start (cur <> n) st s'

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
    OdOpenJoiner
  |
    OdJoinerText
  |
    OdBytes
  |
    OdIntegerPart !MaybeSign
  |
    OdFractionPart
  |
    OdExponentPart !MaybeSign
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
    EofNeedDigit !NumberPart
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

    LeftAngleSuccessor -> EofJust OdOpenJoiner

    ZeroSuccessor -> EofJust $ OdIntegerPart NothingSign

    ManyBytes ->
        if odd (posDiff cur start)
        then EofError EofNeedNibble
        else EofJust OdBytes

    Digits Some acc -> EofError $ EofNeedDigit acc

    Digits Many acc -> EofJust $ case acc of
        IntegerPart mbSgn -> OdIntegerPart mbSgn
        FractionPart -> OdFractionPart
        ExponentPart mbSgn -> OdExponentPart mbSgn

    DoubleQuotedString -> EofError EofDoubleQuotedString

    MultiQuotedString1 acc -> EofError $ EofMultiQuotedString1 acc

    MultiQuotedString2 acc -> EofError $ EofMultiQuotedString2 acc

    MultiQuotedString3 acc -> EofError $ EofMultiQuotedString3 acc

    JoinerNotEscaped ->
        if cur == start
        then EofNothing   -- no zero-length tokens!
        else EofJust OdJoinerText

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
    -- | The number of bytes in the UTF8 encoding of the code point
    --
    -- TODO shoud this exclude the length, since 'Pos' carries that
    -- info?
    --
    -- TODO without a length argument, should adjacent escapes be
    -- combined into a single token?
    SdJoinerEscapedUtf8 !Four
  |
    SdCloseJoiner
  deriving (Eq, Show)

data SnocError =
    SnocNeedStart
  |
    -- | + and - are not allowed to immediately follow a decimal nor
    -- hexadecimal digit
    SnocNoSign
  |
    SnocNeedNibble
  |
    SnocNeedDigit !NumberPart
  |
    SnocNeedRightParen
  |
    SnocNeedOrdering
  |
    SnocNeedRightBrace
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

    Start -> snocStart SignsOk SnocEpsilon SnocSd

    -- loop
    MoreWhitespace -> \c ->
        if isWS c then SnocEpsilon MoreWhitespace else
        jumpStart SignsOk OdWhitespace c

    -- loop
    ManyIdAtom -> \c ->
        if isID c then SnocEpsilon ManyIdAtom else
        jumpStart SignsOk OdAtom c
        
    -- loop
    ManyIdVariant -> \c ->
        if isID c then SnocEpsilon ManyIdVariant else
        jumpStart SignsOk OdVariant c
        
    LeftParenSuccessor -> \case
        '^' -> SnocSd SdOpenPin Start
        c -> jumpStart SignsOk OdOpenParen c

    CaretSuccessor -> \case
        ')' -> SnocSd SdClosePin Start
        _ -> SnocError SnocNeedRightParen

    LeftBraceSuccessor -> \case
        '<' -> SnocSd (SdOpenMeta LT) Start
        '=' -> SnocSd (SdOpenMeta EQ) Start
        '>' -> SnocSd (SdOpenMeta GT) Start
        _ -> SnocError SnocNeedOrdering

    LeftAngleSuccessor -> \case
        '}' -> SnocSd                (SdCloseMeta LT) Start
        '%' -> SnocOd   OdOpenJoiner                  (JoinerEscapedUtf8 Nothing')
        '>' -> SnocOdSd OdOpenJoiner SdCloseJoiner    Start
        _   -> SnocOd   OdOpenJoiner                  JoinerNotEscaped

    EqualsSuccessor -> \case
        '}' -> SnocSd (SdCloseMeta EQ) Start
        _ -> SnocError SnocNeedRightBrace

    RightAngleSuccessor -> \case
        '}' -> SnocSd (SdCloseMeta GT) Start
        _ -> SnocError SnocNeedRightBrace

    ZeroSuccessor -> \case
        'x' -> SnocEpsilon ManyBytes
        '.' -> SnocOd odInt $ Digits Some FractionPart
        'e' -> SnocOd odInt $ Digits Some $ ExponentPart NothingSign
        'E' -> SnocOd odInt $ Digits Some $ ExponentPart NothingSign
        c
          | isD10 c -> SnocEpsilon $ Digits Many $ IntegerPart NothingSign
          | otherwise -> jumpStart SignsNotOk odInt c
      where
        odInt = OdIntegerPart NothingSign

    -- loop
    ManyBytes -> \c ->
        if isD16 c then SnocEpsilon ManyBytes else
        if odd (posDiff cur start)
        then SnocError SnocNeedNibble
        else jumpStart SignsNotOk OdBytes c

    Digits Some acc -> \case
        '+' -> snocSign acc PosSign
        '-' -> snocSign acc NegSign
        c | isD10 c -> SnocEpsilon $ Digits Many acc
        _ -> SnocError $ SnocNeedDigit acc

    -- some
    Digits Many acc -> \c ->
        let tk = case acc of
                IntegerPart mbSgn -> OdIntegerPart mbSgn
                FractionPart -> OdFractionPart
                ExponentPart mbSgn -> OdExponentPart mbSgn
            exponentSt =
                Digits Some $ ExponentPart NothingSign
        in
        if isD10 c then SnocEpsilon $ Digits Many acc else
        case (acc, c) of
            (IntegerPart{}, '.') -> SnocOd tk $ Digits Some FractionPart
            (IntegerPart{}, 'e') -> SnocOd tk exponentSt
            (IntegerPart{}, 'E') -> SnocOd tk exponentSt
            (FractionPart, 'e') -> SnocOd tk exponentSt
            (FractionPart, 'E') -> SnocOd tk exponentSt
            _ -> jumpStart SignsNotOk tk c

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
        '\'' -> case acc of
            MatchedAll delim -> SnocSd (SdMultiQuotedString delim) Start
            _ -> SnocEpsilon $ MultiQuotedString3 $ MatchedNone $ forgetMultiQuotePartialMatch acc   -- TODO loop until /= '?
        c | Just x <- parseD10 c
          , Just acc' <- pushMultiQuotePartialMatch x acc
         -> SnocEpsilon $ MultiQuotedString3 acc'
        _ -> SnocEpsilon $ MultiQuotedString2 $ forgetMultiQuotePartialMatch acc

    -- loop
    JoinerNotEscaped -> \case
        '%' ->
            let f
                  | cur == start = SnocEpsilon   -- no zero-length tokens!
                  | otherwise    = SnocOd OdJoinerText
            in
            f $ JoinerEscapedUtf8 Nothing'
        '>' ->
            let f
                  | cur == start = SnocSd   -- no zero-length tokens!
                  | otherwise    = SnocOdSd OdJoinerText
            in
            f SdCloseJoiner Start
        _ -> SnocEpsilon JoinerNotEscaped

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
             -> SnocSd (SdJoinerEscapedUtf8 (valueUtf8Size size)) JoinerNotEscaped

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

data AreSignsOk = SignsNotOk | SignsOk

jumpStart :: AreSignsOk -> OdToken -> Char -> SnocResult
{-# INLINE jumpStart #-}
jumpStart flag tk c = snocStart flag (SnocOd tk) (SnocOdSd tk) c

snocStart :: AreSignsOk -> (St -> SnocResult) -> (SdToken -> St -> SnocResult) -> Char -> SnocResult
{-# INLINE snocStart #-}
snocStart flag snocNothing snocJust = \case
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
    c | isD10 c -> snocNothing $ Digits Many $ IntegerPart NothingSign
    '+' -> snocStartSign PosSign
    '-' -> snocStartSign NegSign
    '"' -> snocNothing DoubleQuotedString
    '\'' -> snocNothing $ MultiQuotedString1 Nothing'
    '_' -> snocJust SdUnderscore Start
    _ -> SnocError SnocNeedStart
  where
    snocStartSign sgn = case flag of
        SignsNotOk -> SnocError SnocNoSign
        SignsOk -> snocNothing $ Digits Some $ IntegerPart $ JustSign sgn

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
    Digits Many _ -> Just setCharD10
    DoubleQuotedString -> Just $ setCharComplement1 '"'
    MultiQuotedString2 _ -> Just $ setCharComplement1 '\''
    JoinerNotEscaped -> Just $ setCharComplement2 '%' '>'

    _ -> Nothing
