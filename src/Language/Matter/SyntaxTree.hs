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
import Language.Matter.Tokenizer.Counting (D10, Four, Four', MaybeSign)

-- | Isomorph of @()@, easier on the eyes
--
-- For the @pos@ variable.
data P = MkP

-- | Isomorph of @()@, easier on the eyes
--
-- For the @blit@, @nlit@, @slit@, or @tlit@ variables.
data X = MkX

data Matter seq b n s t pos =
    Flat !(Flat b n s t pos)
  |
    Variant !(s pos) !pos !pos (Matter seq b n s t pos)
  |
    Sequence !pos (seq (SequencePart pos (Matter seq b n s t pos))) !pos
  |
    MetaGT !pos (Matter seq b n s t pos) !pos !(ClosePin pos (Matter seq b n s t pos))
  |
    Paren !pos (Matter seq b n s t pos) !pos
  |
    PinMetaLT !pos (Matter seq b n s t pos) !pos !pos (Matter seq b n s t pos) !pos

data Symbol slit pos = MkSymbol !slit

data ClosePin pos a =
    NoClosePin a
  |
    OnlyClosePin !pos a !pos
  |
    BothPins !pos a !pos !pos a !pos

-----

data Flat b n s t pos =
    Atom !(s pos) !pos !pos
  |
    Bytes !(b pos)
  |
    Number !(n pos)
  |
    Text !(t pos)

-----

data Number nlit pos = NumberLit !nlit !MaybeSign !pos !pos !(MaybeFraction pos) !(MaybeExponent pos)

data MaybeFraction pos = NothingFraction | JustFraction !pos !pos

data MaybeExponent pos = NothingExponent | JustExponent !MaybeSign !pos !pos

-----

data Bytes blit pos = BytesLit !blit !pos !pos (MoreBytes blit pos)

data MoreBytes blit pos = NoMoreBytes | MoreBytes !pos !(Bytes blit pos)

-----

data Text neseq tlit pos =
    forall j. Suppressor !pos !pos (Joiner neseq tlit pos j) (Text neseq tlit pos)
  |
    TextLit !Quote !tlit !pos !pos (MoreText neseq tlit pos)

data Quote =
    DoubleQuote
  |
    MultiQuote !(Four' D10)

data MoreText neseq tlit pos = NoMoreText | forall j. MoreText !pos (Joiner neseq tlit pos j) (Text neseq tlit pos)

data JOINER =
    -- | a joiner that can only be followed by escapes
    Je
  |
    -- | a joiner that can only be followed by text
    Jt

-- | a joiner, excluding the position of its @<@
--
-- The index of a 'Joiner' indicates what /preceded/ it.
data Joiner :: (Type -> Type) -> Type -> Type -> JOINER -> Type where
    NilJoiner         :: !pos                                              -> Joiner neseq tlit pos j
    ConsJoinerText    :: !tlit -> !pos -> !pos -> Joiner neseq tlit pos Jt -> Joiner neseq tlit pos Je
    ConsJoinerEscapes :: !(neseq (Escape pos)) -> Joiner neseq tlit pos Je -> Joiner neseq tlit pos Jt

data Escape pos = MkEscape !pos !Four

-----

data SequencePart pos a =
    Item a
  |
    MetaEQ !pos a !pos

-----

deriving instance Show P
deriving instance Show X
deriving instance (Show pos, Show (b pos), Show (n pos), Show (s pos), Show (t pos), Show (seq (SequencePart pos (Matter seq b n s t pos)))) => Show (Matter seq b n s t pos)
deriving instance (Show pos, Show slit) => Show (Symbol slit pos)
deriving instance (Show pos, Show a) => Show (ClosePin pos a)
deriving instance (Show pos, Show (b pos), Show (n pos), Show (s pos), Show (t pos)) => Show (Flat b n s t pos)
deriving instance (Show pos, Show blit) => Show (Bytes blit pos)
deriving instance (Show pos, Show blit) => Show (MoreBytes blit pos)
deriving instance (Show pos, Show nlit) => Show (Number nlit pos)
deriving instance Show pos => Show (MaybeFraction pos)
deriving instance Show pos => Show (MaybeExponent pos)
deriving instance (Show pos, Show (neseq (Escape pos)), Show tlit) => Show (Text neseq tlit pos)
deriving instance Show Quote
deriving instance (Show pos, Show (neseq (Escape pos)), Show tlit) => Show (MoreText neseq tlit pos)
deriving instance (Show pos, Show (neseq (Escape pos)), Show tlit) => Show (Joiner neseq tlit pos j)
deriving instance Show pos => Show (Escape pos)
deriving instance (Show pos, Show a) => Show (SequencePart pos a)

deriving instance Eq P
deriving instance Eq X
deriving instance (Eq pos, Eq (seq (SequencePart pos (Matter seq b n s t pos))), Eq (b pos), Eq (n pos), Eq (s pos), Eq (t pos)) => Eq (Matter seq b n s t pos)
deriving instance (Eq pos, Eq slit) => Eq (Symbol slit pos)
deriving instance (Eq pos, Eq a) => Eq (ClosePin pos a)
deriving instance (Eq pos, Eq (b pos), Eq (n pos), Eq (s pos), Eq (t pos)) => Eq (Flat b n s t pos)
deriving instance (Eq pos, Eq blit) => Eq (Bytes blit pos)
deriving instance (Eq pos, Eq blit) => Eq (MoreBytes blit pos)
deriving instance (Eq pos, Eq nlit) => Eq (Number nlit pos)
deriving instance Eq pos => Eq (MaybeFraction pos)
deriving instance Eq pos => Eq (MaybeExponent pos)
instance          (Eq pos, Eq (neseq (Escape pos)), Eq tlit) => Eq (Text neseq tlit pos) where
    Suppressor p1 p1' j1 txt1 == Suppressor p2 p2' j2 txt2 = p1 == p2 && p1' == p2' && joinerEquality j1 j2 && txt1 == txt2
    TextLit q1 tlit1 l1 r1 more1 == TextLit q2 tlit2 l2 r2 more2 = q1 == q2 && tlit1 == tlit2 && l1 == l2 && r1 == r2 && more1 == more2
    _ == _ = False
deriving instance Eq Quote
instance          (Eq pos, Eq (neseq (Escape pos)), Eq tlit) => Eq (MoreText neseq tlit pos) where
    NoMoreText == NoMoreText = True
    MoreText p1 j1 txt1 == MoreText p2 j2 txt2 = p1 == p2 && joinerEquality j1 j2 && txt1 == txt2
    _ == _ = False
deriving instance (Eq pos, Eq (neseq (Escape pos)), Eq tlit) => Eq (Joiner neseq tlit pos j)
deriving instance Eq pos => Eq (Escape pos)
deriving instance (Eq pos, Eq a) => Eq (SequencePart pos a)

deriving instance Functor (Symbol b)
deriving instance Functor (ClosePin pos)
deriving instance (Functor b, Functor n, Functor s, Functor t) => Functor (Flat b n s t)
deriving instance Functor (Bytes blit)
deriving instance Functor (MoreBytes blit)
deriving instance Functor (Number n)
deriving instance Functor MaybeFraction
deriving instance Functor MaybeExponent
instance          Functor neseq => Functor (Text neseq t) where
    fmap f = \case
        Suppressor p p' j txt -> Suppressor (f p) (f p') (mapPosJoiner f j) (fmap f txt)
        TextLit q tlit l r more -> TextLit q tlit (f l) (f r) (fmap f more)
instance          Functor neseq => Functor (MoreText neseq t) where
    fmap f = \case
        NoMoreText -> NoMoreText
        MoreText p j t -> MoreText (f p) (mapPosJoiner f j) (fmap f t)
deriving instance Functor Escape
deriving instance Functor (SequencePart pos)

instance          TestEquality (Joiner neseq tlit pos) where
    testEquality = curry $ \case
        (NilJoiner{}        , NilJoiner{}        ) -> Nothing
        (ConsJoinerText{}   , ConsJoinerText{}   ) -> Just Refl
        (ConsJoinerEscapes{}, ConsJoinerEscapes{}) -> Just Refl
        _ -> Nothing

-- | Note that the arguments can have different indices
joinerEquality :: (Eq pos, Eq (neseq (Escape pos)), Eq tlit) => Joiner neseq tlit pos x -> Joiner neseq tlit pos y -> Bool
joinerEquality = curry $ \case
    (NilJoiner p1                 , NilJoiner p2                 ) -> p1 == p2
    (ConsJoinerText t1 l1 r1 j1   , ConsJoinerText t2 l2 r2 j2   ) -> t1 == t2 && l1 == l2 && r1 == r2 && joinerEquality j1 j2
    (ConsJoinerEscapes escapes1 j1, ConsJoinerEscapes escapes2 j2) -> escapes1 == escapes2 && joinerEquality j1 j2
    _ -> False

mapPosJoiner :: Functor neseq => (pos -> pos') -> Joiner neseq tlit pos x -> Joiner neseq tlit pos' x
mapPosJoiner f = \case
    NilJoiner p -> NilJoiner (f p)
    ConsJoinerText t l r j -> ConsJoinerText t (f l) (f r) (mapPosJoiner f j)
    ConsJoinerEscapes escapes j -> ConsJoinerEscapes (fmap (fmap f) escapes) (mapPosJoiner f j)

-----

-- | The 'F.Base' functor of 'Matter'
data MatterF seq b n s t pos a =
    FlatF (Flat b n s t pos)
  |
    VariantF !(s pos) !pos !pos a
  |
    SequenceF !pos (seq (SequencePart pos a)) !pos
  |
    MetaGtF !pos a !pos !(ClosePin pos a)
  |
    ParenF !pos a !pos
  |
    PinMetaLtF !pos a !pos !pos a !pos

deriving instance Functor seq => Functor (MatterF seq b n s t pos)

-- | Project one layer of 'MatterF'
project :: Matter seq b n s t pos -> MatterF seq b n s t pos (Matter seq b n s t pos)
{-# INLINE project #-}
project = \case
    Flat flt -> FlatF flt
    Variant s l r x -> VariantF s l r x
    Sequence l xs r -> SequenceF l xs r
    MetaGT l x r y -> MetaGtF l x r y
    Paren l x r -> ParenF l x r
    PinMetaLT l1 x r1 l2 y r2 -> PinMetaLtF l1 x r1 l2 y r2

-- | Inverse of 'project'
embed :: MatterF seq b n s t pos (Matter seq b n s t pos) -> Matter seq b n s t pos
{-# INLINE embed #-}
embed = \case
    FlatF flt -> Flat flt
    VariantF s l r x -> Variant s l r x
    SequenceF l xs r -> Sequence l xs r
    MetaGtF l x r y -> MetaGT l x r y
    ParenF l x r -> Paren l x r
    PinMetaLtF l1 x r1 l2 y r2 -> PinMetaLT l1 x r1 l2 y r2

type instance F.Base (Matter seq b n s t pos) = MatterF seq b n s t pos
instance Functor seq => F.Recursive (Matter seq b n s t pos) where project = project
instance Functor seq => F.Corecursive (Matter seq b n s t pos) where embed = embed

-- | Specialization of 'F.fold' from "Data.Functor.Foldable"
fold :: Functor seq => (MatterF seq b n s t pos a -> a) -> Matter seq b n s t pos -> a
{-# INLINE fold #-}
fold = F.fold

-- | Specialization of 'F.unfold' from "Data.Functor.Foldable"
unfold :: Functor seq => (a -> MatterF seq b n s t pos a) -> a -> Matter seq b n s t pos
unfold = F.unfold

mapSequence ::
    (seq (SequencePart pos a) -> seq' (SequencePart pos a))
 ->
    MatterF seq  b n s t pos a
 ->
    MatterF seq' b n s t pos a
{-# INLINE mapSequence #-}
mapSequence f = \case
    FlatF flt -> FlatF flt
    VariantF s l r x -> VariantF s l r x
    SequenceF l xs r -> SequenceF l (f xs) r
    MetaGtF l x r y -> MetaGtF l x r y
    ParenF l x r -> ParenF l x r
    PinMetaLtF l1 x r1 l2 y r2 -> PinMetaLtF l1 x r1 l2 y r2

mapPositions ::
        (Functor seq, Functor b, Functor n, Functor s, Functor t)
     =>
        (pos -> pos')
     ->
        MatterF seq b n s t pos  a
     ->
        MatterF seq b n s t pos' a
{-# INLINE mapPositions #-}
mapPositions f = \case
    FlatF flt -> FlatF (fmap f flt)
    VariantF s l r x -> VariantF (fmap f s) (f l) (f r) x
    SequenceF l xs r -> SequenceF (f l) (fmap sequencePart xs) (f r)
    MetaGtF l x r y -> MetaGtF (f l) x (f r) (closePin y)
    ParenF l x r -> ParenF (f l) x (f r)
    PinMetaLtF l1 x r1 l2 y r2 -> PinMetaLtF (f l1) x (f r1) (f l2) y (f r2)
  where
    sequencePart = \case
        Item x -> Item x
        MetaEQ l x r -> MetaEQ (f l) x (f r)

    closePin = \case
        NoClosePin y -> NoClosePin y
        OnlyClosePin l y r -> OnlyClosePin (f l) y (f r)
        BothPins l1 y1 r1 l2 y2 r2 -> BothPins (f l1) y1 (f r1) (f l2) y2 (f r2)

mapBNST ::
        Functor seq
     =>
        (b pos -> b' pos)
     ->
        (n pos -> n' pos)
     ->
        (s pos -> s' pos)
     ->
        (t pos -> t' pos)
     ->
        MatterF seq b  n  s  t  pos a
     ->
        MatterF seq b' n' s' t' pos a
{-# INLINE mapBNST #-}
mapBNST fbytes fnum fsym ftext = \case
    FlatF flt -> FlatF (flat flt)
    VariantF s l r x -> VariantF (fsym s) l r x
    SequenceF l xs r -> SequenceF l (fmap sequencePart xs) r
    MetaGtF l x r y -> MetaGtF l x r (closePin y)
    ParenF l x r -> ParenF l x r
    PinMetaLtF l1 x r1 l2 y r2 -> PinMetaLtF l1 x r1 l2 y r2
  where
    flat = \case
        Atom s l r -> Atom (fsym s) l r
        Bytes b -> Bytes (fbytes b)
        Number n -> Number (fnum n)
        Text t -> Text (ftext t)

    sequencePart = \case
        Item x -> Item x
        MetaEQ l x r -> MetaEQ l x r

    closePin = \case
        NoClosePin y -> NoClosePin y
        OnlyClosePin l y r -> OnlyClosePin l y r
        BothPins l1 y1 r1 l2 y2 r2 -> BothPins l1 y1 r1 l2 y2 r2
