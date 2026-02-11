{-# LANGUAGE ImportQualifiedPost #-}

module Language.Matter.Parser.Tests (tests) where

import Control.Monad (foldM, unless)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word32)
import Language.Matter.Parser qualified as P
import Language.Matter.Tokenizer
import Language.Matter.Tokenizer.Tests (printWithPositions)
import Language.Matter.SyntaxTree qualified as ST
import Language.Matter.SyntaxTree.Generator (generateMatter)
import System.Exit (exitFailure)
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

    QC.sample' (QC.sized $ \sz -> (,) sz <$> generateMatter) >>= mapM_ (\(sz, x) -> do putStrLn ""; print sz; print (x :: ST.Matter ST.P NonEmpty []))

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

  , failing "4+4+4+4"
  , passing "[4+4+4+4]"

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
  ]

data TestCase = MkTestCase String Bool

failing :: String -> TestCase
failing s = MkTestCase s False

passing :: String -> TestCase
passing s = MkTestCase s True

doesItPass :: TestCase -> IO Bool
doesItPass (MkTestCase s expected) = do
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

-----

type M = ST.Matter Pos NonEmpty []

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
                Nothing -> case P.eof stk of
                    Nothing -> ParseStuck (tokenizerStart k) Nothing (tokenizerCurrent k) stk
                    Just m -> ParseDone m
                Just stk' -> case P.eof stk' of
                    Nothing -> ParseStuck (tokenizerStart k) Nothing (tokenizerCurrent k) stk'
                    Just m -> ParseDone m
            EofError e -> TokenizerError (tokenizerStart k) (tokenizerCurrent k) (Right e)
        P.SnocsError l r e -> TokenizerError l r (Left e)
        P.SnocsStuck stk l tk r -> ParseStuck l (Just tk) r stk
