{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Matter.Parser.Tests (tests) where

import Control.Monad (foldM, unless)
import Control.Monad.Trans.Except (Except, throwE)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Vector (Vector)
import Data.Word (Word32)
import GHC.Exts (fromList)
import Language.Matter.Interpreter qualified as I
import Language.Matter.Parser qualified as P
import Language.Matter.Tokenizer
import Language.Matter.Tokenizer.Counting (Four (..))
import Language.Matter.Tokenizer.Pretty (prettyToken, toLazyText)
import Language.Matter.Tokenizer.Tests (printWithPositions)
import Language.Matter.SyntaxTree
import Language.Matter.SyntaxTree.Generator qualified as G
import Language.Matter.SyntaxTree.Pretty (pretty)
import System.Exit (exitFailure)
import System.Random (mkStdGen)
import System.Random.Stateful (getStdGen, runStateGen_)
import Test.QuickCheck qualified as QC

tests :: IO ()
tests = do
    count <- foldM
        (\acc x -> doesItPass x <&> \passed -> if passed then acc else acc + 1)
        (0 :: Word32)
        testCases
    unless (0 == count) $ do
        putStrLn $ show count ++ " failing test case" ++ if 1 == count then "" else "s" ++ "."
        exitFailure

    let f (sz, x) = do
            print sz
            g <- getStdGen
            -- TODO how to inject some unnecessary OdWhitespace tokens? Should not be adjacent to OdWhiteSpace.
            TL.putStrLn $ runStateGen_ g $ \g' -> toLazyText $ foldMap (prettyToken g') $ pretty (x :: Matter Vector (Bytes X) Decimal Symbol (Text NonEmpty X) P)
            putStrLn ""
    QC.sample' (QC.sized $ \sz -> (,) sz <$> G.generateMatter) >>= mapM_ f

    QC.quickCheckWith QC.stdArgs{QC.maxSuccess = 100000} prop_prettyThenParseIsSame

-----

testCases :: [TestCase]
testCases = [
    failing ""

  , failing "[@need-delimiting-space]"   -- the gotcha :sad-face:

  , passing "[@foo {= #bar #baz 42 =} ]"

  , passing "[{> @ >} 2 3 4]"

  , passing "\
        \'234'hello'234' <> '0'there!'0' <> _\n\
        \ <,> '0'apple'0'\n\
        \ <,> '0'bananas'0'\n\
        \ <and> _\n\
        \ <,> '0'cherries'0'\n\
        \"
  , failing "'0'hi'0' <>"
  , failing "'0'hi'0' <> 4"
  , failing "_"
  , passing "_ <> '0'ok'0'"

  , passing "+7"
  , passing "[4 +4 -4]"

  , passing "1"
  , passing "1e2"
  , passing "1.2"
  , passing "1.2e3"   -- TODO the parser ought to reject some token streams we can't generate with our tokenizer

  , passing "\"\" <%F09F988A> \"\""   -- smiley face emoji

  , passing "0x"
  , passing "0x1234"
  , failing "0x1234<>"
  , passing "0x1234<>0xCAFE"
  , passing "0x<>0x<>0x<>0x"
  , failing "0x<>0x< >0x<>0x"

  , passing "@"
  , passing "( @ )"
  , failing "( @ ^)"
  , failing "(^ @ )"
  , failing "(^ @ ^)"

  , failing "@ {< @no-pin <}"
  , failing "( @ ) {< @no-pin <}"
  , failing "( @ ^) {< @no-pin <}"
  , passing "(^ @ ) {< @ <}"
  , failing "(^ @ ^) {< @no-GT <}"

  , passing "{> 0 >} @"
  , passing "{> 0 >} ( @ )"
  , passing "{> 0 >} ( @ ^)"
  , failing "{> @no-LT >} (^ @ )"
  , failing "{> @no-LT >} (^ @ ^)"

  , failing "{> 0 >} @ {< @no-pin <}"
  , failing "{> 0 >} ( @ ) {< @no-pin <}"
  , failing "{> 0 >} ( @ ^) {< @no-pin <}"
  , passing "{> @ >} (^ @ ) {< @ <}"
  , passing "{> @ >} (^ @ ^) {< @ <}"

  , failing "(^ @ ) {< @ <} {< @ <}"
  , passing "(^(^ @ ){< @ <} ){< @ <}"
  , passing "{> @ >} {> @ >} @"

  , passing "[@NaN 0]"

  , passing "_<%d0af>'0''0'"

  , MkRoundTrip 0 $ Flat $ Text
      $ Suppressor MkP MkP
            (ConsJoinerEscapes (MkEscape MkP Four1 NE.:| [MkEscape MkP Four1]) $ NilJoiner MkP)
      $ TextLit DoubleQuote MkX MkP MkP
      $ NoMoreText

  , passing "_<%25%C398>\"\""

  , MkRoundTrip 0 $ Flat $ Text
      $ Suppressor MkP MkP
          (ConsJoinerEscapes (MkEscape MkP Four2 NE.:| [MkEscape MkP Four1]) (NilJoiner MkP))
      $ TextLit DoubleQuote MkX MkP MkP
      $ NoMoreText

    -- BytesAnno

  , passingAnd "0x" $ \_inp -> \case
        Flat (Bytes b) -> Just (P.bytesAnnoSize b, 0)
        _ -> Nothing

  , passingAnd "0x11223344556677" $ \_inp -> \case
        Flat (Bytes b) -> Just (P.bytesAnnoSize b, 7)
        _ -> Nothing

  , passingAnd "0x11 <> 0x2233 <> 0x <> 0x44" $ \_inp -> \case
        Flat (Bytes b) -> Just (P.bytesAnnoSize b, 4)
        _ -> Nothing

    -- TextAnno

  , passingAnd "\"\"" $ \_inp -> \case
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 0 0)
        _ -> Nothing

  , passingAnd "'0''0'" $ \_inp -> \case
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 0 0)
        _ -> Nothing

  , passingAnd "'7''7'" $ \_inp -> \case
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 0 0)
        _ -> Nothing

  , passingAnd "'23''23'" $ \_inp -> \case
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 0 0)
        _ -> Nothing

  , passingAnd "'931''931'" $ \_inp -> \case
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 0 0)
        _ -> Nothing

  , passingAnd "'8832''8832'" $ \_inp -> \case
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 0 0)
        _ -> Nothing

  , passingAnd "'8832'Banana'8832'" $ \_inp -> \case
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 6 6)
        _ -> Nothing

  , passingAnd "\"\" <banana> \"\"" $ \_inp -> \case
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 6 6)
        _ -> Nothing

  , passingAnd "\"\" <%F09F8D8C> \"\"" $ \_inp -> \case   -- ie 🍌
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 1 4)
        _ -> Nothing

  , passingAnd "\"\" <> \"euro\"" $ \_inp -> \case
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 4 4)
        _ -> Nothing

  , passingAnd "\"\" <%e282ac> \"\"" $ \_inp -> \case   -- ie €
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 1 3)
        _ -> Nothing

  , passingAnd "\"\" <%F09F8D8C> \"€\"" $ \_inp -> \case
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 2 7)
        _ -> Nothing

  , passingAnd "_ <%F09F8D8C> \"€\"" $ \_inp -> \case
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 1 3)
        _ -> Nothing

  , passingAnd "\"\" <%e282ac> \"🍌\"" $ \_inp -> \case
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 2 7)
        _ -> Nothing

  , passingAnd "_ <%e282ac> \"🍌\"" $ \_inp -> \case
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 1 4)
        _ -> Nothing

  , passingAnd "\"€\" <> _ <banana> \"🍌\"" $ \_inp -> \case
        Flat (Text txt) -> Just (P.textAnnoCounts txt, MkPos 2 7)
        _ -> Nothing

  , passingAnd "0x34" $ \inp -> \case
        Flat (Bytes bytes) ->
            Just (I.interpretBytes inp (Just $ P.bytesAnnoSize bytes) (P.bytesForget bytes), pure (fromList [0x34]))
        _ -> Nothing

  , passingAnd "0x34110f" $ \inp -> \case
        Flat (Bytes bytes) ->
            Just (I.interpretBytes inp (Just $ P.bytesAnnoSize bytes) (P.bytesForget bytes), pure (fromList [0x34, 0x11, 0x0f]))
        _ -> Nothing

  , passingAnd "@foo" $ \inp -> \case
        Flat (Atom s) ->
            Just (I.interpretSymbol inp s, I.shortTextSymbolValue "foo")
        _ -> Nothing

  , passingAnd "#foo []" $ \inp -> \case
        Variant s _ ->
            Just (I.interpretSymbol inp s, I.shortTextSymbolValue "foo")
        _ -> Nothing

  , passingAnd "@" $ \inp -> \case
        Flat (Atom s) ->
            Just (I.interpretSymbol inp s, I.shortTextSymbolValue "")
        _ -> Nothing

  , passingAnd "# []" $ \inp -> \case
        Variant s _ ->
            Just (I.interpretSymbol inp s, I.shortTextSymbolValue "")
        _ -> Nothing

  , passingAnd (show (negate 1 + toInteger (minBound :: Int))) $ \inp -> \case
        Flat (Number n) ->
            Just (I.unsafeInterpretDecimal inp n, throwE I.OutOfRange :: Except I.BadDecimal Int)
        _ -> Nothing

  , passingAnd (show (toInteger (minBound :: Int))) $ \inp -> \case
        Flat (Number n) ->
            Just (I.unsafeInterpretDecimal inp n, pure (minBound :: Int))
        _ -> Nothing

  , passingAnd "00000000000000000000000000000002937234045" $ \inp -> \case
        Flat (Number n) ->
            Just (I.unsafeInterpretDecimal inp n, pure (2937234045 :: Int))
        _ -> Nothing

  , passingAnd "2937234045.0000000000000000000000000000000" $ \inp -> \case
        Flat (Number n) ->
            Just (I.unsafeInterpretDecimal inp n, pure (2937234045 :: Int))
        _ -> Nothing

  , passingAnd "10000000000000.000000000000000001" $ \inp -> \case
        Flat (Number n) ->
            Just (I.unsafeInterpretDecimal inp n, throwE I.NotIntegral :: Except I.BadDecimal Int)
        _ -> Nothing

  , passingAnd "0.2937234045e10" $ \inp -> \case
        Flat (Number n) ->
            Just (I.unsafeInterpretDecimal inp n, pure (2937234045 :: Int))
        _ -> Nothing

  , passingAnd (show (toInteger (maxBound :: Int))) $ \inp -> \case
        Flat (Number n) ->
            Just (I.unsafeInterpretDecimal inp n, pure (maxBound :: Int))
        _ -> Nothing

  , passingAnd (show (1 + toInteger (maxBound :: Int))) $ \inp -> \case
        Flat (Number n) ->
            Just (I.unsafeInterpretDecimal inp n, throwE I.OutOfRange :: Except I.BadDecimal Int)
        _ -> Nothing

  ]

  ++ [ testIntegerNotIntegral (wlz, w, wtz) (flz, f, ftz) (elz, e)
     | wlz <- [0 .. 2]
     , w <- [0 .. 2]
     , wtz <- [0 .. 2]
     , flz <- [0 .. 2]
     , f <- [0 .. 2]
     , ftz <- [0 .. 2]
     , elz <- [0 .. 2]
     , e <- [-4 .. 4]
     ]

-- | Testing the 'Integer' instance of 'I.unsafeInterpretDecimal'
--
-- Leading zeros, how many whole digits, and trailing zeros
--
-- Leading zeros, how many fraction digits, and trailing zeros
--
-- Leading zeros of exponent and the actual exponent value
testIntegerNotIntegral :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int) -> TestCase
testIntegerNotIntegral (wlz, w, wtz) (flz, f, ftz) (elz, e) =
    passingAnd decstr $ \inp -> \case
        Flat (Number n) ->
            Just (I.unsafeInterpretDecimal inp n, expected)
        _ -> Nothing
  where
    decstr =
        (if null wstr then "0" else wstr)
     <>
        (if null fstr then "" else "." <> fstr)
     <>
        ("e" <> estr)

    wstr = replicate wlz '0' <> replicate w '1' <> replicate wtz '0'
    fstr = replicate flz '0' <> replicate f '1' <> replicate ftz '0'
    estr = (if e < 0 then "-" else "") <> replicate elz '0' <> show (abs e)

    fracEndsUpFrac = f > 0 && e - flz - f < 0
    wholeEndsUpFrac = w > 0 && w + wtz + e <= 0

    expected =
        if fracEndsUpFrac || wholeEndsUpFrac then throwE I.NotIntegral else
        pure $
            I.shiftInteger
                (read ("0" <> replicate w '1' <> replicate wtz '0' <> replicate flz '0' <> replicate f '1') :: Integer)
                (fromIntegral $ e - flz - f)

-----

data TestCase =
    forall a. (Show a, Eq a) => MkTestCase String (ParseResult -> Maybe (a, a))
  |
    MkRoundTrip Int (Matter Vector (Bytes X) Decimal Symbol (Text NonEmpty X) P)

failing :: String -> TestCase
failing s = MkTestCase s (\_ -> Nothing :: Maybe ((), ()))

passing :: String -> TestCase
passing s = MkTestCase s (\_ -> Just ((), ()))

passingAnd :: (Show a, Eq a) => String -> (String -> M -> Maybe (a, a)) -> TestCase
passingAnd s f = MkTestCase s $ \case
    ParseDone m -> f s m
    ParseStuck{} -> Nothing
    TokenizerError{} -> Nothing

doesItPass :: TestCase -> IO Bool
doesItPass = \case
    MkTestCase s mkExpected -> do
        let x = parseWhole s
            mbPair = mkExpected x
            expected = maybe False (uncurry (==)) mbPair
        let (itPasses, prefix, extra) = case x of
                ParseDone{} -> (expected, if expected then "PASS " else  "FAIL ", show mbPair)
                ParseStuck _ _ _ stk -> (not expected, if expected then "FAIL " else "PASS ", show (P.simplify stk))
                TokenizerError{} -> (False, "BAD ", "")
        unless itPasses $ do
            printWithPositions s
            putStr prefix
            print x
            putStrLn extra
        pure itPasses
    MkRoundTrip g m ->
        QC.quickCheckWithResult QC.stdArgs{QC.maxSuccess = 1} (QC.once $ prop_prettyThenParseIsSame' (g, m)) <&> \case
            QC.Success{} -> True
            QC.GaveUp{} -> False
            QC.Failure{} -> False
            QC.NoExpectedFailure{} -> False

-----

type M = Matter P.Sequ P.Bytes P.Decimal P.Symbol P.Text Pos

data ParseResult =
    ParseDone M
  |
    ParseStuck Pos (Maybe Token) Pos (P.Stk M)
  |
    TokenizerError Pos Pos (Either SnocError EofError)
  deriving (Show)

parseWhole :: MatterStream a => a -> ParseResult
parseWhole x =
    case P.snocs P.emptyStk x of
        P.SnocsDone stk k -> case eofTokenizer k of
            EofNothing -> case P.eof stk of
                Nothing -> ParseStuck (tokenizerStart k) Nothing (tokenizerCurrent k) stk
                Just m -> ParseDone m
            EofJust tk -> case P.snoc (tokenizerStart k) (tokenizerCurrent k) stk (OdToken tk) of
                Nothing -> ParseStuck (tokenizerStart k) (Just (OdToken tk)) (tokenizerCurrent k) stk
                Just stk' -> case P.eof stk' of
                    Nothing -> ParseStuck (tokenizerStart k) Nothing (tokenizerCurrent k) stk'
                    Just m -> ParseDone m
            EofError e -> TokenizerError (tokenizerStart k) (tokenizerCurrent k) (Right e)
        P.SnocsError l r e -> TokenizerError l r (Left e)
        P.SnocsStuck stk l tk r -> ParseStuck l (Just tk) r stk

-----

-- | Render random Matter to a 'TL.Text', then parse it, then require
-- equivalence at type 'Matter'.
prop_prettyThenParseIsSame :: QC.Property
prop_prettyThenParseIsSame =
    QC.propertyForAllShrinkShow gen shrnk noshow prop_prettyThenParseIsSame'
  where
    gen = liftA2 (,) (QC.choose (minBound, maxBound)) G.generateMatter
    shrnk (g, m) = [ (g, m') | m' <- G.shrinkMatter m]
    noshow _ = []

prop_prettyThenParseIsSame' :: (Int, Matter Vector (Bytes X) Decimal Symbol (Text NonEmpty X) P) -> QC.Property
prop_prettyThenParseIsSame' (g, m) =
    QC.counterexample ("m = " <> show m)
  $ QC.counterexample ("g = " <> show g)
  $ QC.counterexample ("txt = " <> TL.unpack txt)
  $ case parseWhole txt of
        ParseDone m' ->
            QC.counterexample ("m' = " <> show m')
          $ QC.counterexample "ParseDone"
          $ m == forget m'
        ParseStuck l mbTk r stk ->
            QC.counterexample ("stk = " <> show stk)
          $ QC.counterexample ("(l, mbTk, r) = " <> show (l, mbTk, r))
          $ QC.counterexample "ParseStuck"
          $ QC.property False
        TokenizerError l r e ->
            QC.counterexample ("(l, e, r) = " <> show (l, e, r))
          $ QC.counterexample "TokenizerError"
          $ QC.property False
  where
    forget =
        fold $ embed
             . mapFuns MkFuns {
                   sequFun = JustFun $ \(P.MkSequ _nitem _nmeta xs) -> xs
                 ,
                   bFun = JustFun $ \(P.MkBytes _w b) -> b
                 ,
                   nFun = NothingFun
                 ,
                   sFun = NothingFun
                 ,
                   tFun = JustFun $ \(P.MkText _counts t) -> t
                 ,
                   posFun = JustFun $ \MkPos{} -> MkP
                 }

    txt =
        runStateGen_ (mkStdGen g)
      $ \g' -> toLazyText
      $ foldMap (prettyToken g')
      $ pretty m
