{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.Text.Lazy.IO qualified as TL
import Language.Matter.Html (html)
import Language.Matter.Parser qualified as P
import Language.Matter.SyntaxTree
import Language.Matter.Tokenizer
import Text.Blaze.Html.Renderer.Text (renderHtml)

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

main :: IO ()
main = do
    txt <- TL.getContents
    case parseWhole txt of
        ParseDone m ->
            TL.putStrLn $ renderHtml $ html txt $ cnv m
        x@ParseStuck{} ->
            print x
        x@TokenizerError{} ->
            print x
  where
    cnv =
        fold
      $ embed . mapFuns MkFuns {
           sequFun = JustFun P.sequForget
         ,
           bFun = JustFun P.bytesForget
         ,
           nFun = NothingFun
         ,
           sFun = NothingFun
         ,
           tFun = JustFun P.textForget
         ,
           posFun = NothingFun
         }
