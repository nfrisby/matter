{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Language.Matter.Tokenizer.Tests (
    printWithPositions,
    tests,
  ) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, unless, when)
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Word (Word32)
import Language.Matter.Tokenizer
import Language.Matter.Tokenizer.Counting
import Language.Matter.Tokenizer.Debug qualified as Debug
import Language.Matter.Tokenizer.StateMachine (MultiQuotePartialMatch (..), NumberPart (..))
import System.Exit (exitFailure)

tests :: IO ()
tests = do
    count <- foldM
        (\acc x -> doesItPass x <&> \passed -> if passed then acc else acc + 1)
        (0 :: Word32)
        testCases
    unless (0 == count) $ do
        putStrLn $ show count ++ " failing test case" ++ if 1 == count then "" else "s" ++ "."
        exitFailure

testCases :: [TestCase]
testCases = [
    MkTestCase "" [] Done
  , MkTestCase "#foo 42 {< @bar <}"
        -- parser will reject this, but tokenizer should not
        [ 4#OdVariant, 5#OdWhitespace, 7#OdIntegerPart NothingSign, 8#OdWhitespace, 9#(SdOpenMeta LT)
        , 11#OdWhitespace, 15#OdAtom, 16#OdWhitespace, 17#(SdCloseMeta LT)
        ]
        Done
  , MkTestCase "()"
        [1#OdOpenParen, 1#SdCloseParen]
        Done
  , MkTestCase "(^)"
        -- parser will reject this, but tokenizer should not
        --
        -- The particular resolution of this ambiguity doesn't matter
        -- since parser will reject, but it's good to notice if it
        -- changes.
        [1#SdOpenPin, 2#SdCloseParen]
        Done
  , MkTestCase "(^^)"
        -- parser will reject this, but tokenizer should not
        [1#SdOpenPin, 3#SdClosePin]
        Done
  , MkTestCase "30"
        [2#OdIntegerPart NothingSign]
        Done
  , MkTestCase "#heading #degrees +37.3"
        [8#OdVariant,9#OdWhitespace,17#OdVariant,18#OdWhitespace,21#OdIntegerPart (JustSign PosSign),23#OdFractionPart]
        Done
  , MkTestCase "[1+2+3-4]"
        [0#SdOpenSequ]
        (Snoc 2 SnocNoSign)
  , MkTestCase "0.0"
        [1#OdIntegerPart NothingSign,3#OdFractionPart]
        Done
  , MkTestCase "0e+0"
        [1#OdIntegerPart NothingSign,4#OdExponentPart (JustSign PosSign)]
        Done
  , MkTestCase "-0e+0"
        [2#OdIntegerPart (JustSign NegSign),5#OdExponentPart (JustSign PosSign)]
        Done
  , MkTestCase "+00e00"
        [3#OdIntegerPart (JustSign PosSign),6#OdExponentPart NothingSign]
        Done
  , MkTestCase "+00E-007000000"
        [3#OdIntegerPart (JustSign PosSign),14#OdExponentPart (JustSign NegSign)]
        Done
  , MkTestCase ".5"
        []
        (Snoc 0 SnocNeedStart)
  , MkTestCase "e+3"
        []
        (Snoc 0 SnocNeedStart)
  , MkTestCase "7e"
        [1#OdIntegerPart NothingSign]
        (Eof 2 $ EofNeedDigit $ ExponentPart NothingSign)
  , MkTestCase "0."
        [1#OdIntegerPart NothingSign]
        (Eof 2 $ EofNeedDigit FractionPart)
  , MkTestCase "0.e"
        [1#OdIntegerPart NothingSign]
        (Snoc 2 $ SnocNeedDigit FractionPart)
  , MkTestCase "]"
        [0#SdCloseSequ]
        Done
  , MkTestCase "(^ @hi ){< '0'ok'0' <}"
        [ 1#SdOpenPin, 3#OdWhitespace, 6#OdAtom, 7#OdWhitespace, 7#SdCloseParen
        , 9#SdOpenMeta LT, 11#OdWhitespace, 18#SdMultiQuotedString (Four1' D10_0), 20#OdWhitespace, 21#SdCloseMeta LT]
        Done
  , MkTestCase "@["
        -- beware!
        [2#OdAtom]
        Done
  , MkTestCase "@("
        -- beware!
        [2#OdAtom]
        Done
  , MkTestCase "@{"
        -- beware!
        [2#OdAtom]
        Done
  , MkTestCase "@<"
        -- beware!
        [2#OdAtom]
        Done
  , MkTestCase "@null]"
        -- beware!
        [6#OdAtom]
        Done
  , MkTestCase "@uint8[32]"
        -- beware!
        [10#OdAtom]
        Done
  , MkTestCase "#array32 @uint8"
        -- more sensible
        [8#OdVariant, 9#OdWhitespace, 15#OdAtom]
        Done
  , MkTestCase "#array [@uint8 32]"
        -- even more sensible
        [6#OdVariant, 7#OdWhitespace, 7#SdOpenSequ, 14#OdAtom, 15#OdWhitespace, 17#OdIntegerPart NothingSign, 17#SdCloseSequ]
        Done
  , MkTestCase "#array [32 @uint8 ]"
        -- equally sensible
        [6#OdVariant, 7#OdWhitespace, 7#SdOpenSequ, 10#OdIntegerPart NothingSign, 11#OdWhitespace, 17#OdAtom, 18#OdWhitespace, 18#SdCloseSequ]
        Done
  , MkTestCase "#array [32 @uint8]"
        -- gotcha!
        [6#OdVariant, 7#OdWhitespace, 7#SdOpenSequ, 10#OdIntegerPart NothingSign, 11#OdWhitespace, 18#OdAtom]
        Done
  , MkTestCase "@][}{<>()\"!013_--sdf"
        [20#OdAtom]
        Done
  , MkTestCase "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
        [107#OdAtom]
        Done
  , MkTestCase "'451''451'"
        [9#SdMultiQuotedString (Four3' D10_4 D10_5 D10_1)]
        Done
  , MkTestCase "'0''1'"
        []
        (Eof 6 (EofMultiQuotedString3 (MatchedNone (Four1' D10_0))))
  , MkTestCase "'0''1'0'"
        [7#SdMultiQuotedString (Four1' D10_0)]
        Done
  , MkTestCase "'12'\"Who's calling?\", she asked. It was early '12.'12'"
        [53#SdMultiQuotedString (Four2' D10_1 D10_2)]
        Done
  , MkTestCase "\"\x2713\""
        -- Strings can include Unicode.
        [2#SdDoubleQuotedString]
        Done
  , MkTestCase ['\x2009']
        -- Unicode whitespace is not allowed as whitespace.
        []
    (Snoc 0 SnocNeedStart)
  , MkTestCase ['\t']
        -- Whitespace cannot be tabs.
        []
    (Snoc 0 SnocNeedStart)
  , MkTestCase "<%21>"
        [1#OdOpenJoiner,3#SdJoinerEscapedUtf8 Four1,4#SdCloseJoiner]
        Done
  , MkTestCase "<%C2A9>"
        [1#OdOpenJoiner,5#SdJoinerEscapedUtf8 Four2,6#SdCloseJoiner]
        Done
  , MkTestCase "<%E282AC>"
        [1#OdOpenJoiner,7#SdJoinerEscapedUtf8 Four3,8#SdCloseJoiner]
        Done
  , MkTestCase "<%F09F988A>"
        [1#OdOpenJoiner,9#SdJoinerEscapedUtf8 Four4,10#SdCloseJoiner]
        Done
  , MkTestCase "<%21ABOUT>"
        -- Looks wrong, but it's just a plain string ABOUT after the escape.
        [1#OdOpenJoiner,3#SdJoinerEscapedUtf8 Four1,9#OdJoinerText, 9#SdCloseJoiner]
        Done
  , MkTestCase "<%21"
        -- Caught a bug where we were emitting a zero-length OdJoinerNotEscaped False
        [1#OdOpenJoiner,3#SdJoinerEscapedUtf8 Four1]
        Done
  , MkTestCase "<%F0288c28"
        [1#OdOpenJoiner]
        (Snoc 4 SnocBadUtf8NibbleN)
  , MkTestCase "<%F48FBFBF"
        -- This is the UTF8 encoding of U+10FFFF, the greatest valid code point.
        [1#OdOpenJoiner,9#SdJoinerEscapedUtf8 Four4]
        Done
  , MkTestCase "<%F49FBFBF"
        -- This should be rejected, since it's > U+10FFFF
        [1#OdOpenJoiner]
        (Snoc 4 SnocUtf8TooGreat)
  , MkTestCase "<%F39FBFBF"
        -- But this is fine, since 3 < 4.
        [1#OdOpenJoiner,9#SdJoinerEscapedUtf8 Four4]
        Done
  , MkTestCase "<%F4AFBFBF"
        -- This should be rejected, since it's > U+10FFFF
        [1#OdOpenJoiner]
        (Snoc 4 SnocUtf8TooGreat)
  , MkTestCase "<%F4BFBFBF"
        -- This should be rejected, since it's > U+10FFFF
        [1#OdOpenJoiner]
        (Snoc 4 SnocUtf8TooGreat)
  , MkTestCase "<%F4CFBFBF"
        -- This should be rejected not because it's > U+10FFFF but
        -- rather because it violates the lower-level UTF8 schema.
        [1#OdOpenJoiner]
        (Snoc 4 SnocBadUtf8NibbleN)
  , MkTestCase "<\\n>"
        -- Backslash escapes are something the user might want to use as a joiner
        [1#OdOpenJoiner, 3#OdJoinerText, 3#SdCloseJoiner]
        Done
  , MkTestCase "<\n>"
        -- Literal newline is allowed
        [1#OdOpenJoiner, 2#OdJoinerText, 2#SdCloseJoiner]
        Done
  , MkTestCase "\n<>"
        -- Or can be avoided via joiners
        [1#OdWhitespace,2#OdOpenJoiner, 2#SdCloseJoiner]
        Done
  , MkTestCase "<,>"
        [1#OdOpenJoiner, 2#OdJoinerText, 2#SdCloseJoiner]
        Done
  , MkTestCase "<dangling"
        -- parser will reject this, but the tokenizer shouldn't.
        --
        -- For example, a syntax highlighter could benefit from this
        -- tokenization.
        [1#OdOpenJoiner, 9#OdJoinerText]
        Done
  , MkTestCase "[@NaN 0]"
        [0#SdOpenSequ, 5#OdAtom, 6#OdWhitespace, 7#OdIntegerPart NothingSign, 7#SdCloseSequ]
        Done
  , MkTestCase "#Z '7''salsa''7'"
        [2#OdVariant, 3#OdWhitespace, 15#SdMultiQuotedString (Four1' D10_7)]
        Done
  , MkTestCase "<%d0af"
        [1#OdOpenJoiner, 5#SdJoinerEscapedUtf8 Four2]
        Done
  , MkTestCase "<%25%25"
        [1#OdOpenJoiner, 3#SdJoinerEscapedUtf8 Four1, 6#SdJoinerEscapedUtf8 Four1]
        Done
  , MkTestCase "+0"
        [2#OdIntegerPart (JustSign PosSign)]
        Done
  , MkTestCase "++0"
        []
        (Snoc 1 SnocNoSign)
  , MkTestCase "0.+3"
        [1#OdIntegerPart NothingSign]
        (Snoc 2 $ SnocNeedDigit FractionPart)
  , MkTestCase "0e--0"
        [1#OdIntegerPart NothingSign]
        (Snoc 3 SnocNoSign)
  ]

-----

-- | The test case string, the expected tokens, and the expected
-- result
data TestCase = MkTestCase String [Tk] Res

-- | Token and the position at which it's delimited
data Tk = Tk Word32 Token

-- | Meager syntactic sugar to make it easy to read/write test cases
class C a where (#) :: Word32 -> a -> Tk
instance C OdToken where w # tk = Tk w (OdToken tk)
instance C SdToken where w # tk = Tk w (SdToken tk)

-- | The overall result of the tokenization
data Res = Done | Snoc Word32 SnocError | Eof Word32 EofError
  deriving (Show)

-- | Pair of expected and actual at the first mismatch
data Failure =
    Tk_Tk Word32 Token Word32 Pos Token Pos
  |
    Tk_SnocError Word32 Token Word32 Pos Pos SnocError
  |
    Tk_EofNothing Word32 Token Word32 Pos Pos
  |
    Tk_EofError Word32 Token Word32 Pos Pos EofError
  |
    Tk_EofJust Word32 Token Word32 Pos OdToken Pos
  |
    Result_Tk Word32 Res Pos Token Pos
  |
    Result_SnocsError Word32 Res Pos Pos SnocError
  |
    Done_EofJust Word32 Pos OdToken Pos
  |
    Done_EofError Word32 Pos Pos EofError
  |
    EofError_EofNothing Word32 Word32 EofError Pos Pos
  |
    EofError_EofJust Word32 Word32 EofError Pos OdToken Pos
  |
    EofError_EofError Word32 Word32 EofError Pos Pos EofError
  |
    SnocError_EofResult Word32 Word32 SnocError Pos Pos EofResult
  |
    SnocError_SnocError Word32 Word32 SnocError Pos Pos SnocError
  deriving (Show)

printWithPositions :: String -> IO ()
printWithPositions s = do
    when ('\r' `elem` s || '\n' `elem` s) $ putStrLn "(On the next line, \\r and \\n is are shown as spaces.)"
    putStrLn [ if '\n' == c || '\r' == c then ' ' else c | c <- s]
    let n = length s
    putStrLn $ take n $ cycle ['0' .. '9']
    when (n > 9) $ putStrLn $ take n $ replicate 10 ' ' ++ cycle (concat [ replicate 10 c | c <- ['1' .. '9'] ++ ['0']])
    when (n > 99) $ putStrLn $ take n $ replicate 100 ' ' ++ concat [ replicate 100 c | c <- ['1' .. '9']]

doesItPass :: TestCase -> IO Bool
doesItPass = \(MkTestCase s tks o) -> do
    let y =
                ((,) "Text" <$> go 0 tks o (snocsTokenizer startTokenizer s))
            <|>
                ((,) "String" <$> go 0 tks o (snocsTokenizer startTokenizer (T.pack s)))
    case y of
        Nothing -> pure True
        Just (ty, f) -> do
            putStrLn $ '#' : ' ' : replicate 30 '=' ++ " FAILURE " ++ replicate 30 '='
            printWithPositions s
            print f
            putStrLn $ ":: " ++ ty
            Debug.run [s]
            print f
            pure False
  where

    go acc (Tk cur tk : tks) o (SnocsToken start' tk' cur' rest)
      | acc == codePoints start'
      , tk == tk'
      , cur == codePoints cur'
      = let next = case tk of
                OdToken{} -> cur
                SdToken{} -> cur + 1
        in
        go next tks o rest
      | otherwise
      = Just $ Tk_Tk acc tk cur start' tk' cur'

    go acc (Tk cur tk : _) _o (SnocsError start' cur' err') =
        Just $ Tk_SnocError acc tk cur start' cur' err'

    go acc (Tk cur tk : _) _o (SnocsDone x) = case eofTokenizer x of
        EofNothing -> Just $ Tk_EofNothing acc tk cur (tokenizerStart x) (tokenizerCurrent x)
        EofError err' -> Just $ Tk_EofError acc tk cur (tokenizerStart x) (tokenizerCurrent x) err'
        EofJust tk'
          | acc == codePoints (tokenizerStart x)
          , cur == codePoints (tokenizerCurrent x)
          , tk == OdToken tk'
         -> Nothing
          | otherwise
         -> Just $ Tk_EofJust acc tk cur (tokenizerStart x) tk' (tokenizerCurrent x)

    go acc [] o (SnocsToken start' tk' cur' _rest) =
        Just $ Result_Tk acc o start' tk' cur'

    go acc [] (Snoc cur err) (SnocsError start' cur' err')

      | acc == codePoints start'
      , cur == codePoints cur'
      , err == err'
      = Nothing

      | otherwise
      = Just $ SnocError_SnocError acc cur err start' cur' err'

    go acc [] o@Done (SnocsError start' cur' err') =
        Just $ Result_SnocsError acc o start' cur' err'

    go acc [] o@Eof{} (SnocsError start' cur' err') =
        Just $ Result_SnocsError acc o start' cur' err'

    go acc [] Done (SnocsDone x) = case eofTokenizer x of
        EofNothing -> Nothing
        EofJust tk' -> Just $ Done_EofJust acc (tokenizerStart x) tk' (tokenizerCurrent x)
        EofError err' -> Just $ Done_EofError acc (tokenizerStart x) (tokenizerCurrent x) err'

    go acc [] (Eof cur err) (SnocsDone x) = case eofTokenizer x of
        EofNothing -> Just $ EofError_EofNothing acc cur err (tokenizerStart x) (tokenizerCurrent x)
        EofJust tk' -> Just $ EofError_EofJust acc cur err (tokenizerStart x) tk' (tokenizerCurrent x)
        EofError err'
          | acc == codePoints (tokenizerStart x)
          , cur == codePoints (tokenizerCurrent x)
          , err == err'
         -> Nothing
          | otherwise
         -> Just $ EofError_EofError acc cur err (tokenizerStart x) (tokenizerCurrent x) err'

    go acc [] (Snoc cur err) (SnocsDone x) =
        Just $ SnocError_EofResult acc cur err (tokenizerStart x) (tokenizerCurrent x) (eofTokenizer x)
