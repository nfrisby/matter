{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Language.Matter.Parser.Tests (tests) where

import Control.Monad (foldM, unless)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Word (Word32)
import Language.Matter.Parser qualified as P
import Language.Matter.Tokenizer
import Language.Matter.Tokenizer.Counting (Four (..))
import Language.Matter.Tokenizer.Pretty (prettyToken, toLazyText)
import Language.Matter.Tokenizer.Tests (printWithPositions)
import Language.Matter.SyntaxTree
import Language.Matter.SyntaxTree.Generator (generateMatter, shrinkMatter)
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
            TL.putStrLn $ runStateGen_ g $ \g' -> toLazyText $ foldMap (prettyToken g') $ pretty (x :: Matter P NonEmpty [])
            putStrLn ""
    QC.sample' (QC.sized $ \sz -> (,) sz <$> generateMatter) >>= mapM_ f

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
      $ Suppressor MkP
            (ConsJoiner MkP MkP (MkEscape MkP Four1 NE.:| [MkEscape MkP Four1]) $ NilJoiner MkP MkP)
      $ TextLiteral DoubleQuote MkP MkP
      $ NoMoreText

  ]

data TestCase =
    MkTestCase String Bool
  |
    MkRoundTrip Int (Matter P NonEmpty [])

failing :: String -> TestCase
failing s = MkTestCase s False

passing :: String -> TestCase
passing s = MkTestCase s True

doesItPass :: TestCase -> IO Bool
doesItPass = \case
    MkTestCase s expected -> do
        let x = parseWhole s
        let (itPasses, prefix, extra) = case x of
                ParseDone{} -> (expected, if expected then "PASS " else "FAIL ", "")
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

type M = Matter Pos NonEmpty []

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
    gen = liftA2 (,) (QC.choose (minBound, maxBound)) generateMatter
    shrnk (g, m) = [ (g, m') | m' <- shrinkMatter m]
    noshow _ = []

prop_prettyThenParseIsSame' :: (Int, Matter P NonEmpty []) -> QC.Property
prop_prettyThenParseIsSame' (g, m) =
    QC.counterexample ("m = " <> show m)
  $ QC.counterexample ("g = " <> show g)
  $ QC.counterexample ("txt = " <> TL.unpack txt)
  $ case parseWhole txt of
        ParseDone m' ->
            QC.counterexample ("m' = " <> show m')
          $ QC.counterexample "ParseDone"
          $ m == forgetPos m'
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
    forgetPos = fold $ embed . mapPositions (\_ -> MkP)

    txt =
        runStateGen_ (mkStdGen g)
      $ \g' -> toLazyText
      $ foldMap (prettyToken g')
      $ pretty (m :: Matter P NonEmpty [])
