{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Matter.SyntaxTree (module Language.Matter.SyntaxTree) where

import Data.Functor.Foldable qualified as F
import Language.Matter.Tokenizer.Counting (D10, Four, Four')

-- | Isomorph of @()@, easier on the eyes
data P = MkP

data Matter pos neseq seq =
    Flat (Flat pos neseq)
  |
    Variant !pos !pos (Matter pos neseq seq)
  |
    Sequence !pos (seq (SequencePart pos (Matter pos neseq seq))) !pos
  |
    MetaGT !pos (Matter pos neseq seq) !pos !(Pin' pos ((Matter pos neseq seq)))
  |
    Paren !pos (Matter pos neseq seq) !pos
  |
    PinMetaLT !pos (Matter pos neseq seq) !pos !Pin !pos (Matter pos neseq seq) !pos

data Pin' pos a = NoPin' a | YesPin' !pos a !pos

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

data SequencePart pos a =
    Item a
  |
    MetaEQ !pos a !pos

-----

deriving instance Show P
deriving instance (Show pos, Show (neseq (Escape pos)), Show (seq (SequencePart pos (Matter pos neseq seq)))) => Show (Matter pos neseq seq)
deriving instance Show Pin
deriving instance (Show pos, Show a) => Show (Pin' pos a)
deriving instance (Show pos, Show (neseq (Escape pos))) => Show (Flat pos neseq)
deriving instance Show pos => Show (MaybeFraction pos)
deriving instance Show pos => Show (MaybeExponent pos)
deriving instance (Show pos, Show (neseq (Escape pos))) => Show (Text pos neseq)
deriving instance Show Quote
deriving instance (Show pos, Show (neseq (Escape pos))) => Show (MoreText pos neseq)
deriving instance (Show pos, Show (neseq (Escape pos))) => Show (Joiner pos neseq)
deriving instance Show pos => Show (Escape pos)
deriving instance Show pos => Show (MoreBytes pos)
deriving instance (Show pos, Show a) => Show (SequencePart pos a)

deriving instance Functor (Pin' pos)
deriving instance Functor (SequencePart pos)

-----

-- | The 'F.Base' functor of 'Matter'
data MatterF pos neseq seq a =
    FlatF (Flat pos neseq)
  |
    VariantF !pos !pos a
  |
    SequenceF !pos (seq (SequencePart pos a)) !pos
  |
    MetaGtF !pos a !pos !(Pin' pos a)
  |
    ParenF !pos a !pos
  |
    PinMetaLtF !pos a !pos !Pin !pos a !pos

deriving instance Functor seq => Functor (MatterF pos neseq seq)

-- | Project one layer of 'MatterF'
project :: Matter pos neseq seq -> MatterF pos neseq seq (Matter pos neseq seq)
{-# INLINE project #-}
project = \case
    Flat flt -> FlatF flt
    Variant l r x -> VariantF l r x
    Sequence l xs r -> SequenceF l xs r
    MetaGT l x r y -> MetaGtF l x r y
    Paren l x r -> ParenF l x r
    PinMetaLT l1 x r1 pin l2 y r2 -> PinMetaLtF l1 x r1 pin l2 y r2

-- | Inverse of 'project'
embed :: MatterF pos neseq seq (Matter pos neseq seq) -> Matter pos neseq seq
{-# INLINE embed #-}
embed = \case
    FlatF flt -> Flat flt
    VariantF l r x -> Variant l r x
    SequenceF l xs r -> Sequence l xs r
    MetaGtF l x r y -> MetaGT l x r y
    ParenF l x r -> Paren l x r
    PinMetaLtF l1 x r1 pin l2 y r2 -> PinMetaLT l1 x r1 pin l2 y r2

type instance F.Base (Matter pos neseq seq) = MatterF pos neseq seq
instance Functor seq => F.Recursive (Matter pos neseq seq) where project = project
instance Functor seq => F.Corecursive (Matter pos neseq seq) where embed = embed

-- | Specialization of 'F.fold' from "Data.Functor.Foldable"
fold :: Functor seq => (MatterF pos neseq seq a -> a) -> Matter pos neseq seq -> a
{-# INLINE fold #-}
fold = F.fold

-- | Specialization of 'F.unfold' from "Data.Functor.Foldable"
unfold :: Functor seq => (a -> MatterF pos neseq seq a) -> a -> Matter pos neseq seq
unfold = F.unfold

mapSequence :: (seq (SequencePart pos a) -> seq' (SequencePart pos a)) -> MatterF pos neseq seq a -> MatterF pos neseq seq' a
{-# INLINE mapSequence #-}
mapSequence f = \case
    FlatF flt -> FlatF flt
    VariantF l r x -> VariantF l r x
    SequenceF l xs r -> SequenceF l (f xs) r
    MetaGtF l x r y -> MetaGtF l x r y
    ParenF l x r -> ParenF l x r
    PinMetaLtF l1 x r1 pin l2 y r2 -> PinMetaLtF l1 x r1 pin l2 y r2
