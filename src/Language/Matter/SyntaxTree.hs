{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Matter.SyntaxTree (module Language.Matter.SyntaxTree) where

import Language.Matter.Tokenizer.Counting (D10, Four, Four')

data Matter pos seq neseq =
    Flat (Flat pos neseq)
  |
    Variant !pos !pos (Matter pos seq neseq)
  |
    Sequence !pos (seq (SequencePart pos seq neseq)) !pos
  |
    MetaGT !pos (Matter pos seq neseq) !pos (Matter pos seq neseq)
  |
    Paren !pos (Matter pos seq neseq) !pos !Pin
  |
    PinMetaLT !pos (Matter pos seq neseq) !pos !Pin !pos (Matter pos seq neseq) !pos

data Pin = NoPin | YesPin

-----

data Flat pos neseq =
    Atom !pos !pos
  |
    Bytes !pos !pos (MoreBytes pos)
  |
    Number !pos !pos !(MaybeFraction pos) !(MaybeExponent pos)
  |
    Text (Text pos neseq)

data MaybeFraction pos = NothingFraction | JustFraction !pos !pos

data MaybeExponent pos = NothingExponent | JustExponent !pos !pos

-----

data MoreBytes pos = NoMoreBytes | MoreBytes !pos !pos !pos (MoreBytes pos)

-----

data Text pos neseq =
    Suppressor !pos (Joiner pos neseq) (Text pos neseq)
  |
    TextLiteral !Quote !pos !pos (MoreText pos neseq)

data Quote =
    DoubleQuote
  |
    MultiQuote !(Four' D10)

data MoreText pos neseq = NoMoreText | MoreText (Joiner pos neseq) (Text pos neseq)

data Joiner pos neseq = NilJoiner !pos !pos | ConsJoiner !pos !pos (neseq (Escape pos)) (Joiner pos neseq)

data Escape pos = MkEscape !pos !Four

-----

data SequencePart pos seq neseq =
    Item !(Matter pos seq neseq)
  |
    MetaEQ !pos !(Matter pos seq neseq) !pos

-----

deriving instance (Show pos, Show (neseq (Escape pos)), Show (seq (SequencePart pos seq neseq))) => Show (Matter pos seq neseq)
deriving instance Show Pin
deriving instance (Show pos, Show (neseq (Escape pos))) => Show (Flat pos neseq)
deriving instance Show pos => Show (MaybeFraction pos)
deriving instance Show pos => Show (MaybeExponent pos)
deriving instance (Show pos, Show (neseq (Escape pos))) => Show (Text pos neseq)
deriving instance Show Quote
deriving instance (Show pos, Show (neseq (Escape pos))) => Show (MoreText pos neseq)
deriving instance (Show pos, Show (neseq (Escape pos))) => Show (Joiner pos neseq)
deriving instance Show pos => Show (Escape pos)
deriving instance Show pos => Show (MoreBytes pos)
deriving instance (Show pos, Show (neseq (Escape pos)), Show (seq (SequencePart pos seq neseq))) => Show (SequencePart pos seq neseq)
