{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Language.Matter.Parser.Tests qualified as P
import Language.Matter.Tokenizer.Tests qualified as T

main :: IO ()
main = do
  putStrLn "-------------------- Tokenizer --------------------"
  T.tests
  putStrLn "--------------------  Parser   --------------------"
  P.tests
