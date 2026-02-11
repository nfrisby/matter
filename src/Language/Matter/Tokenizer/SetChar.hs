{-# LANGUAGE LambdaCase #-}

-- | Set of characters type used by "Language.Matter.Tokenizer.StateMachine"
module Language.Matter.Tokenizer.SetChar (
    SetChar (..),
    memberSetChar,
    setCharComplement1,
    setCharComplement2,
    setCharD10,
    setCharD16,
    setCharID,
    setCharWS,
  ) where

-- | A compact representation of a set of characters
data SetChar =
    SetChar1ClosedInterval !Char !Char
  |
    SetChar2ClosedIntervals !Char !Char !Char !Char
  |
    SetChar3ClosedIntervals !Char !Char !Char !Char !Char !Char

memberSetChar :: Char -> SetChar -> Bool
memberSetChar c = \case
    SetChar1ClosedInterval lo hi ->
        f lo hi
    SetChar2ClosedIntervals lo1 hi1 lo2 hi2 ->
        f lo1 hi1 || f lo2 hi2
    SetChar3ClosedIntervals lo1 hi1 lo2 hi2 lo3 hi3 ->
        f lo1 hi1 || f lo2 hi2 || f lo3 hi3
  where
    f lo hi = lo <= c && c <= hi
  
setCharWS :: SetChar
setCharWS = SetChar3ClosedIntervals '\n' '\n' '\r' '\r' ' ' ' '

setCharID :: SetChar
setCharID = SetChar2ClosedIntervals '!' (pred '`') (succ '`') '~'

setCharD16 :: SetChar
setCharD16 = SetChar3ClosedIntervals '0' '9' 'A' 'F' 'a' 'f'

setCharD10 :: SetChar
setCharD10 = SetChar1ClosedInterval '0' '9'

setCharComplement1 :: Char -> SetChar
setCharComplement1 c
  | c == minBound = SetChar1ClosedInterval (succ minBound) maxBound
  | c == maxBound = SetChar1ClosedInterval minBound (pred maxBound)
  | otherwise     = SetChar2ClosedIntervals minBound (pred c) (succ c) maxBound

setCharComplement2 :: Char -> Char -> SetChar
setCharComplement2 c1 c2 =
    case compare c1 c2 of
        LT -> f c1 c2
        EQ -> setCharComplement1 c1
        GT -> f c2 c1
  where
    f lo hi =
        case (lo == minBound, hi == maxBound) of
            (True, True) -> SetChar1ClosedInterval (succ minBound) (pred maxBound)
            (True, False) -> SetChar2ClosedIntervals (succ minBound) (pred hi) (succ hi) maxBound
            (False, True) -> SetChar2ClosedIntervals minBound (pred lo) (succ lo) (pred maxBound)
            (False, False) -> SetChar3ClosedIntervals minBound (pred lo) (succ lo) (pred hi) (succ hi) maxBound
