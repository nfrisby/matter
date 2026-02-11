{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | Helpers for debugging tokenization
module Language.Matter.Tokenizer.Debug (
    run,
  ) where

import Language.Matter.Tokenizer.StateMachine
import Data.Word (Word32)

data ABC =
    A Word32 Word32 St
  |
    B Word32 OdToken Word32
  |
    C Word32 SdToken Word32
  deriving (Show)

-- | A version of 'snocsTokenizer' that retains trace information
tokenizeWholeString :: String -> ([ABC], Either SnocError EofResult)
tokenizeWholeString =
    go [A 0 0 Start] startTokenizer
  where
    next acc1 start cur st =
        jump (A start (cur + 1) st : acc1) start (cur + 1) st

    jump acc1 start cur st s = case loops st of
        Nothing -> go acc1 (MkTokenizer (MkPos start) (MkPos cur) st) s
        Just sc ->
            let MkMunchResult n s' = munch sc s
            in
            go
                (if 0 == n then acc1 else A start (cur + n) st : acc1)
                (MkTokenizer (MkPos start) (MkPos (cur + n)) st)
                s'

    go !acc1 !x@(MkTokenizer (MkPos start) (MkPos cur) _st) = \case
        [] -> (reverse acc1, Right $ eofTokenizer x)
        c:s -> case snocTokenizer x c of
            SnocEpsilon st' ->
                next acc1 start cur st' s
            SnocOd tk st' ->
                next (B start tk (cur - 1) : acc1) cur cur st' s
            SnocSd tk st' ->
                next (C start tk cur : acc1) (cur + 1) cur st' s
            SnocOdSd tk1 tk2 st' ->
                next (C cur tk2 (cur + 1) : B start tk1 cur : acc1) (cur + 1) cur st' s
            SnocError err ->
                (reverse acc1, Left err)

run :: [String] -> IO ()
run ss = do
    flip mapM_ ss $ \s -> do
        putStrLn s
        let (x, y) = tokenizeWholeString s
        flip mapM_ x $ \case
            A start cur st -> putStrLn $ "--- " <> show (start, cur) <> " --- " <> show st
            B start tk final -> print (start, tk, final)
            C start tk final -> print (start, tk, final)
        print y
        putStrLn ""
        putStrLn ""
