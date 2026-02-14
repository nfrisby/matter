{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Matter.SyntaxTree (module Language.Matter.SyntaxTree) where

import Data.Functor.Foldable qualified as F
import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl), TestEquality (testEquality))
import Language.Matter.Tokenizer.Counting (D10, Four, Four')

-- | Isomorph of @()@, easier on the eyes
data P = MkP

data Matter anno pos neseq seq =
    Flat !(Flat anno pos neseq)
  |
    Variant !pos !pos (Matter anno pos neseq seq)
  |
    Sequence !(SequenceAnno anno) !pos (seq (SequencePart pos (Matter anno pos neseq seq))) !pos
  |
    MetaGT !pos (Matter anno pos neseq seq) !pos !(ClosePin pos (Matter anno pos neseq seq))
  |
    Paren !pos (Matter anno pos neseq seq) !pos
  |
    PinMetaLT !pos (Matter anno pos neseq seq) !pos !pos (Matter anno pos neseq seq) !pos

data family SequenceAnno anno

data ClosePin pos a =
    NoClosePin a
  |
    OnlyClosePin !pos a !pos
  |
    BothPins !pos a !pos !pos a !pos

-----

data Flat anno pos neseq =
    Atom !pos !pos
  |
    Bytes !(BytesAnno anno) {-# UNPACK #-} !(Bytes pos)
  |
    Number !(NumberAnno anno) {-# UNPACK #-} !(Number pos)
  |
    Text !(TextAnno anno) (Text pos neseq)

data family BytesAnno anno
data family NumberAnno anno
data family TextAnno anno

-----

data Number pos = NumberLit !pos !pos !(MaybeFraction pos) !(MaybeExponent pos)

data MaybeFraction pos = NothingFraction | JustFraction !pos !pos

data MaybeExponent pos = NothingExponent | JustExponent !pos !pos

-----

data Bytes pos = BytesLit !pos !pos (MoreBytes pos)

data MoreBytes pos = NoMoreBytes | MoreBytes !pos !(Bytes pos)

-----

data Text pos neseq =
    forall j. Suppressor !pos !pos (Joiner pos neseq j) (Text pos neseq)
  |
    TextLit !Quote !pos !pos (MoreText pos neseq)

data Quote =
    DoubleQuote
  |
    MultiQuote !(Four' D10)

data MoreText pos neseq = NoMoreText | forall j. MoreText !pos (Joiner pos neseq j) (Text pos neseq)

data JOINER =
    -- | a joiner that can only be followed by escapes
    Je
  |
    -- | a joiner that can only be followed by text
    Jt

-- | a joiner, excluding the position of its @<@
--
-- The index of a 'Joiner' indicates what /preceded/ it.
data Joiner :: Type -> (Type -> Type) -> JOINER -> Type where
    NilJoiner         :: !pos                                         -> Joiner pos neseq j
    ConsJoinerText    :: !pos -> !pos          -> Joiner pos neseq Jt -> Joiner pos neseq Je
    ConsJoinerEscapes :: !(neseq (Escape pos)) -> Joiner pos neseq Je -> Joiner pos neseq Jt

data Escape pos = MkEscape !pos !Four

-----

data SequencePart pos a =
    Item a
  |
    MetaEQ !pos a !pos

-----

class (Show (BytesAnno anno), Show (NumberAnno anno), Show (SequenceAnno anno), Show (TextAnno anno)) => ShowAnno anno
class (Eq (BytesAnno anno), Eq (NumberAnno anno), Eq (SequenceAnno anno), Eq (TextAnno anno)) => EqAnno anno

deriving instance Show P
deriving instance (ShowAnno anno, Show pos, Show (neseq (Escape pos)), Show (seq (SequencePart pos (Matter anno pos neseq seq)))) => Show (Matter anno pos neseq seq)
deriving instance (Show pos, Show a) => Show (ClosePin pos a)
deriving instance (ShowAnno anno, Show pos, Show (neseq (Escape pos))) => Show (Flat anno pos neseq)
deriving instance Show pos => Show (Bytes pos)
deriving instance Show pos => Show (MoreBytes pos)
deriving instance Show pos => Show (Number pos)
deriving instance Show pos => Show (MaybeFraction pos)
deriving instance Show pos => Show (MaybeExponent pos)
deriving instance (Show pos, Show (neseq (Escape pos))) => Show (Text pos neseq)
deriving instance Show Quote
deriving instance (Show pos, Show (neseq (Escape pos))) => Show (MoreText pos neseq)
deriving instance (Show pos, Show (neseq (Escape pos))) => Show (Joiner pos neseq j)
deriving instance Show pos => Show (Escape pos)
deriving instance (Show pos, Show a) => Show (SequencePart pos a)

deriving instance Eq P
deriving instance (EqAnno anno, Eq pos, Eq (neseq (Escape pos)), Eq (seq (SequencePart pos (Matter anno pos neseq seq)))) => Eq (Matter anno pos neseq seq)
deriving instance (Eq pos, Eq a) => Eq (ClosePin pos a)
deriving instance (EqAnno anno, Eq pos, Eq (neseq (Escape pos))) => Eq (Flat anno pos neseq)
deriving instance Eq pos => Eq (Bytes pos)
deriving instance Eq pos => Eq (MoreBytes pos)
deriving instance Eq pos => Eq (Number pos)
deriving instance Eq pos => Eq (MaybeFraction pos)
deriving instance Eq pos => Eq (MaybeExponent pos)
instance (Eq pos, Eq (neseq (Escape pos))) => Eq (Text pos neseq) where
    Suppressor p1 p1' j1 txt1 == Suppressor p2 p2' j2 txt2 = p1 == p2 && p1' == p2' && joinerEquality j1 j2 && txt1 == txt2
    TextLit q1 l1 r1 more1 == TextLit q2 l2 r2 more2 = q1 == q2 && l1 == l2 && r1 == r2 && more1 == more2
    _ == _ = False
deriving instance Eq Quote
instance (Eq pos, Eq (neseq (Escape pos))) => Eq (MoreText pos neseq) where
    NoMoreText == NoMoreText = True
    MoreText p1 j1 txt1 == MoreText p2 j2 txt2 = p1 == p2 && joinerEquality j1 j2 && txt1 == txt2
    _ == _ = False
deriving instance (Eq pos, Eq (neseq (Escape pos))) => Eq (Joiner pos neseq j)
deriving instance Eq pos => Eq (Escape pos)
deriving instance (Eq pos, Eq a) => Eq (SequencePart pos a)

deriving instance Functor (ClosePin pos)
deriving instance Functor (SequencePart pos)

instance TestEquality (Joiner pos neseq) where
    testEquality = curry $ \case
        (NilJoiner{}        , NilJoiner{}        ) -> Nothing
        (ConsJoinerText{}   , ConsJoinerText{}   ) -> Just Refl
        (ConsJoinerEscapes{}, ConsJoinerEscapes{}) -> Just Refl
        _ -> Nothing

-- | Note that the arguments can have different indices
joinerEquality :: (Eq pos, Eq (neseq (Escape pos))) => Joiner pos neseq x -> Joiner pos neseq y -> Bool
joinerEquality = curry $ \case
    (NilJoiner p1                 , NilJoiner p2                 ) -> p1 == p2
    (ConsJoinerText l1 r1 j1      , ConsJoinerText l2 r2 j2      ) -> l1 == l2 && r1 == r2 && joinerEquality j1 j2
    (ConsJoinerEscapes escapes1 j1, ConsJoinerEscapes escapes2 j2) -> escapes1 == escapes2 && joinerEquality j1 j2
    _ -> False

-----

-- | The 'F.Base' functor of 'Matter'
data MatterF anno pos neseq seq a =
    FlatF (Flat anno pos neseq)
  |
    VariantF !pos !pos a
  |
    SequenceF !(SequenceAnno anno) !pos (seq (SequencePart pos a)) !pos
  |
    MetaGtF !pos a !pos !(ClosePin pos a)
  |
    ParenF !pos a !pos
  |
    PinMetaLtF !pos a !pos !pos a !pos

deriving instance Functor seq => Functor (MatterF anno pos neseq seq)

-- | Project one layer of 'MatterF'
project :: Matter anno pos neseq seq -> MatterF anno pos neseq seq (Matter anno pos neseq seq)
{-# INLINE project #-}
project = \case
    Flat flt -> FlatF flt
    Variant l r x -> VariantF l r x
    Sequence anno l xs r -> SequenceF anno l xs r
    MetaGT l x r y -> MetaGtF l x r y
    Paren l x r -> ParenF l x r
    PinMetaLT l1 x r1 l2 y r2 -> PinMetaLtF l1 x r1 l2 y r2

-- | Inverse of 'project'
embed :: MatterF anno pos neseq seq (Matter anno pos neseq seq) -> Matter anno pos neseq seq
{-# INLINE embed #-}
embed = \case
    FlatF flt -> Flat flt
    VariantF l r x -> Variant l r x
    SequenceF anno l xs r -> Sequence anno l xs r
    MetaGtF l x r y -> MetaGT l x r y
    ParenF l x r -> Paren l x r
    PinMetaLtF l1 x r1 l2 y r2 -> PinMetaLT l1 x r1 l2 y r2

type instance F.Base (Matter anno pos neseq seq) = MatterF anno pos neseq seq
instance Functor seq => F.Recursive (Matter anno pos neseq seq) where project = project
instance Functor seq => F.Corecursive (Matter anno pos neseq seq) where embed = embed

-- | Specialization of 'F.fold' from "Data.Functor.Foldable"
fold :: Functor seq => (MatterF anno pos neseq seq a -> a) -> Matter anno pos neseq seq -> a
{-# INLINE fold #-}
fold = F.fold

-- | Specialization of 'F.unfold' from "Data.Functor.Foldable"
unfold :: Functor seq => (a -> MatterF anno pos neseq seq a) -> a -> Matter anno pos neseq seq
unfold = F.unfold

mapSequence ::
    (seq (SequencePart pos a) -> seq' (SequencePart pos a))
 ->
    MatterF anno pos neseq seq a
 ->
    MatterF anno pos neseq seq' a
{-# INLINE mapSequence #-}
mapSequence f = \case
    FlatF flt -> FlatF flt
    VariantF l r x -> VariantF l r x
    SequenceF anno l xs r -> SequenceF anno l (f xs) r
    MetaGtF l x r y -> MetaGtF l x r y
    ParenF l x r -> ParenF l x r
    PinMetaLtF l1 x r1 l2 y r2 -> PinMetaLtF l1 x r1 l2 y r2

mapPositions ::
  forall anno pos pos' seq neseq a.
        (Functor seq, Functor neseq)
     =>
        (pos -> pos')
     ->
        MatterF anno pos neseq seq a
     ->
        MatterF anno pos' neseq seq a
{-# INLINE mapPositions #-}
mapPositions f = \case
    FlatF flt -> FlatF (flat flt)
    VariantF l r x -> VariantF (f l) (f r) x
    SequenceF anno l xs r -> SequenceF anno (f l) (fmap sequencePart xs) (f r)
    MetaGtF l x r y -> MetaGtF (f l) x (f r) (closePin y)
    ParenF l x r -> ParenF (f l) x (f r)
    PinMetaLtF l1 x r1 l2 y r2 -> PinMetaLtF (f l1) x (f r1) (f l2) y (f r2)
  where
    flat = \case
        Atom l r ->
            Atom (f l) (f r)
        Bytes anno (BytesLit l r more) ->
            Bytes anno $ BytesLit (f l) (f r) (moreBytes more)
        Number anno (NumberLit l r fpart epart) ->
            Number anno $ NumberLit (f l) (f r) (fractionPart fpart) (exponentPart epart)
        Text anno txt ->
            Text anno (text txt)

    moreBytes = \case
        NoMoreBytes -> NoMoreBytes
        MoreBytes p (BytesLit l r more) -> MoreBytes (f p) $ BytesLit (f l) (f r) (moreBytes more)

    fractionPart = \case
        NothingFraction -> NothingFraction
        JustFraction l r -> JustFraction (f l) (f r)

    exponentPart = \case
        NothingExponent -> NothingExponent
        JustExponent l r -> JustExponent (f l) (f r)

    text = \case
        Suppressor p1 p2 j txt -> Suppressor (f p1) (f p2) (joiner j) (text txt)
        TextLit q l r more -> TextLit q (f l) (f r) (moreText more)

    joiner :: Joiner pos neseq j -> Joiner pos' neseq j
    joiner = \case
        NilJoiner p -> NilJoiner (f p)
        ConsJoinerText l r j -> ConsJoinerText (f l) (f r) (joiner j)
        ConsJoinerEscapes escapes j -> ConsJoinerEscapes (fmap escape escapes) (joiner j)

    escape (MkEscape p sz) = MkEscape (f p) sz

    moreText = \case
        NoMoreText -> NoMoreText
        MoreText p j txt -> MoreText (f p) (joiner j) (text txt)

    sequencePart = \case
        Item x -> Item x
        MetaEQ l x r -> MetaEQ (f l) x (f r)

    closePin = \case
        NoClosePin y -> NoClosePin y
        OnlyClosePin l y r -> OnlyClosePin (f l) y (f r)
        BothPins l1 y1 r1 l2 y2 r2 -> BothPins (f l1) y1 (f r1) (f l2) y2 (f r2)

mapAnno ::
  forall anno anno' pos seq neseq a.
        Functor seq
     =>
        (BytesAnno anno -> BytesAnno anno')
     ->
        (NumberAnno anno -> NumberAnno anno')
     ->
        (SequenceAnno anno -> SequenceAnno anno')
     ->
        (TextAnno anno -> TextAnno anno')
     ->
        MatterF anno pos neseq seq a
     ->
        MatterF anno' pos neseq seq a
{-# INLINE mapAnno #-}
mapAnno f g h i = \case
    FlatF flt -> FlatF (flat flt)
    VariantF l r x -> VariantF l r x
    SequenceF anno l xs r -> SequenceF (h anno) l (fmap sequencePart xs) r
    MetaGtF l x r y -> MetaGtF l x r (closePin y)
    ParenF l x r -> ParenF l x r
    PinMetaLtF l1 x r1 l2 y r2 -> PinMetaLtF l1 x r1 l2 y r2
  where
    flat = \case
        Atom l r -> Atom l r
        Bytes anno bytes -> Bytes (f anno) bytes
        Number anno number -> Number (g anno) number
        Text anno txt -> Text (i anno) txt

    sequencePart = \case
        Item x -> Item x
        MetaEQ l x r -> MetaEQ l x r

    closePin = \case
        NoClosePin y -> NoClosePin y
        OnlyClosePin l y r -> OnlyClosePin l y r
        BothPins l1 y1 r1 l2 y2 r2 -> BothPins l1 y1 r1 l2 y2 r2
