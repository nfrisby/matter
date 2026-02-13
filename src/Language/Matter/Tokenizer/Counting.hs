{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Counting types used by "Language.Matter.Tokenizer.StateMachine"
module Language.Matter.Tokenizer.Counting (

    -- * Counting types
    Maybe' (..),
    Two (..),
    Three (..),
    Four (..),
    Four' (..),
    Five (..),
    valueFour,

    -- * Groups of characters
    D10 (..),
    D16 (..),
    isD10,
    isD16,
    isID,
    isWS,
    parseD10,
    parseD16,
    valueD10,
    valueD16,

    -- * UTF8 nibble
    leadingBitCountPlus1,

  ) where

import Data.Word (Word8, Word32)

data Two = Two1 | Two2
data Three = Three1 | Three2 | Three3
data Four = Four1 | Four2 | Four3 | Four4
data Five = Five1 | Five2 | Five3 | Five4 | Five5

valueFour :: Four -> Word32
valueFour = \case
    Four1 -> 1
    Four2 -> 2
    Four3 -> 3
    Four4 -> 4

data Maybe' a = Nothing' | Just' !a

instance Functor Maybe' where
   fmap f = \case
       Nothing' -> Nothing'
       Just' x -> Just' (f x)

data Four' a = Four1' !a | Four2' !a !a | Four3' !a !a !a | Four4' !a !a !a !a

data D10 = D10_0 | D10_1 | D10_2 | D10_3 | D10_4 | D10_5 | D10_6 | D10_7 | D10_8 | D10_9

data D16 =
    D16_0 | D16_1 | D16_2 | D16_3 | D16_4 | D16_5 | D16_6 | D16_7
  |
    D16_8 | D16_9 | D16_A | D16_B | D16_C | D16_D | D16_E | D16_F

-----

deriving instance Show Two
deriving instance Show Three
deriving instance Show Four
deriving instance Show Five
deriving instance Show D10
deriving instance Show D16

deriving instance Eq Two
deriving instance Eq Three
deriving instance Eq Four
deriving instance Eq Five
deriving instance Eq D10
deriving instance Eq D16

deriving instance Ord Two
deriving instance Ord Three
deriving instance Ord Four
deriving instance Ord Five
deriving instance Ord D10
deriving instance Ord D16

deriving instance Show a => Show (Maybe' a)
deriving instance Show a => Show (Four' a)

deriving instance Eq a => Eq (Maybe' a)
deriving instance Eq a => Eq (Four' a)

deriving instance Ord a => Ord (Maybe' a)
deriving instance Ord a => Ord (Four' a)

-----

-- | The unmistakable whitespace characters
--
-- Currently only space, carriage return, and newline.
--
-- TODO expand to suitable parts of Unicode.
isWS :: Char -> Bool
isWS = \case
    ' ' -> True
    '\r' -> True
    '\n' -> True
    _ -> False

-- | The unmistakable characters
--
-- Currently it's every visible ASCII character except backtick, to
-- avoid confusing with single quote.
--
-- TODO expand to suitable parts of Unicode.
isID :: Char -> Bool
isID c = '!' <= c && c <= '~' && c /= '`'

-- | Decimal digits
isD10 :: Char -> Bool
isD10 = \c -> case parseD10 c of
    Nothing -> False
    Just _ -> True

valueD10 :: D10 -> Word8
valueD10 = \case
    D10_0 -> 0
    D10_1 -> 1
    D10_2 -> 2
    D10_3 -> 3
    D10_4 -> 4
    D10_5 -> 5
    D10_6 -> 6
    D10_7 -> 7
    D10_8 -> 8
    D10_9 -> 9

parseD10 :: Char -> Maybe D10
parseD10 = \case
    '0' -> Just D10_0
    '1' -> Just D10_1
    '2' -> Just D10_2
    '3' -> Just D10_3
    '4' -> Just D10_4
    '5' -> Just D10_5
    '6' -> Just D10_6
    '7' -> Just D10_7
    '8' -> Just D10_8
    '9' -> Just D10_9
    _ -> Nothing

-- | Hexadecimal digits
isD16 :: Char -> Bool
isD16 = \c -> case parseD16 c of
    Nothing -> False
    Just _ -> True

valueD16 :: D16 -> Word8
valueD16 = \case
    D16_0 -> 0
    D16_1 -> 1
    D16_2 -> 2
    D16_3 -> 3
    D16_4 -> 4
    D16_5 -> 5
    D16_6 -> 6
    D16_7 -> 7
    D16_8 -> 8
    D16_9 -> 9
    D16_A -> 10
    D16_B -> 11
    D16_C -> 12
    D16_D -> 13
    D16_E -> 14
    D16_F -> 15

parseD16 :: Char -> Maybe D16
parseD16 = \case
    c | Just x <- parseD10 c -> Just $ case x of
        D10_0 -> D16_0
        D10_1 -> D16_1
        D10_2 -> D16_2
        D10_3 -> D16_3
        D10_4 -> D16_4
        D10_5 -> D16_5
        D10_6 -> D16_6
        D10_7 -> D16_7
        D10_8 -> D16_8
        D10_9 -> D16_9
    'A' -> Just D16_A
    'a' -> Just D16_A
    'B' -> Just D16_B
    'b' -> Just D16_B
    'C' -> Just D16_C
    'c' -> Just D16_C
    'D' -> Just D16_D
    'd' -> Just D16_D
    'E' -> Just D16_E
    'e' -> Just D16_E
    'F' -> Just D16_F
    'f' -> Just D16_F
    _ -> Nothing

-----

-- | Does this nibble have 0, 1, 2, 3, or 4 leading 1s in binary notation?
--
-- Cf <https://en.wikipedia.org/wiki/UTF-8#Description>
leadingBitCountPlus1 :: D16 -> Five
leadingBitCountPlus1 = \case
    -- 0xxx
    D16_0 -> Five1
    D16_1 -> Five1
    D16_2 -> Five1
    D16_3 -> Five1
    D16_4 -> Five1
    D16_5 -> Five1
    D16_6 -> Five1
    D16_7 -> Five1

    -- 10xx
    D16_8 -> Five2
    D16_9 -> Five2
    D16_A -> Five2
    D16_B -> Five2

    -- 110x
    D16_C -> Five3
    D16_D -> Five3

    -- 1110
    D16_E -> Five4

    -- 1111
    D16_F -> Five5
