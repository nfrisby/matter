{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Matter.SyntaxTree (module Language.Matter.SyntaxTree) where

import Control.Category qualified as Cat
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
-- For the @blit@, @slit@, or @tlit@ variables.
data X = MkX

-- | A Matter term
--
-- @b@ @n@ @s@ and @t@ are bytes, number, symbol, and text,
-- respectively. See 'Bytes', 'Decimal', 'Symbol', and 'Text' for
-- suitable arguments that contain exactly the information resulting
-- from tokenization.
--
-- TODO should meta terms be able to have a different representation
-- than non-meta terms? What about meta meta terms, and so on?
data Matter sequ b n s t pos =
    -- | flat terms
    Flat !(Flat b n s t pos)
  |
    -- | #foo a
    Variant !(s pos) !pos !pos (Matter sequ b n s t pos)
  |
    -- | [ ... a b c ... ]
    Sequence !pos (sequ (SequencePart pos (Matter sequ b n s t pos))) !pos
  |
    -- | {> a >} and its referent
    MetaGT !pos (Matter sequ b n s t pos) !pos !(ClosePin pos (Matter sequ b n s t pos))
  |
    -- | ( a )
    Paren !pos (Matter sequ b n s t pos) !pos
  |
    -- | (^ a ) {< b <}
    PinMetaLT !pos (Matter sequ b n s t pos) !pos !pos (Matter sequ b n s t pos) !pos

-- | \@ or #
data Symbol slit pos = MkSymbol !slit

-- | The referent of {> a >} ...
data ClosePin pos a =
    -- | a
    NoClosePin a
  |
    -- | ( a ^)
    OnlyClosePin !pos a !pos
  |
    -- | (^ a ^) {< b <}
    BothPins !pos a !pos !pos a !pos

-----

-- | A Matter term that has no Matter subterms
data Flat b n s t pos =
    -- | \@
    Atom !(s pos) !pos !pos
  |
    -- | 0x and joiners
    Bytes !(b pos)
  |
    -- | 123
    Number !(n pos)
  |
    -- | "foo", '0'"Why?" she asked.'0', and joiners
    Text !(t pos)

-----

data Decimal pos = DecimalLit !MaybeSign !pos !pos !(MaybeFraction pos) !(MaybeExponent pos)

data MaybeFraction pos = NothingFraction | JustFraction !pos !pos

data MaybeExponent pos = NothingExponent | JustExponent !MaybeSign !pos !pos

-----

data Bytes blit pos = BytesLit !blit !pos !pos (MoreBytes blit pos)

data MoreBytes blit pos = NoMoreBytes | MoreBytes !pos !(Bytes blit pos)

-----

data Text nesequ tlit pos =
    forall j. Suppressor !pos !pos (Joiner nesequ tlit pos j) (Text nesequ tlit pos)
  |
    TextLit !Quote !tlit !pos !pos (MoreText nesequ tlit pos)

data Quote =
    DoubleQuote
  |
    MultiQuote !(Four' D10)

data MoreText nesequ tlit pos = NoMoreText | forall j. MoreText !pos (Joiner nesequ tlit pos j) (Text nesequ tlit pos)

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
    -- | end of joiner follows anything
    --
    -- TODO would it be useful to have @SingI j@ constraint?
    NilJoiner         :: !pos                                              -> Joiner nesequ tlit pos j
    -- | text only follows 'Je' and can only be followed by 'Jt'
    ConsJoinerText    :: !tlit -> !pos -> !pos -> Joiner nesequ tlit pos Jt -> Joiner nesequ tlit pos Je
    -- | some escapes only follow 'Jt' and can only be jollowed by 'Je'
    ConsJoinerEscapes :: !(nesequ (Escape pos)) -> Joiner nesequ tlit pos Je -> Joiner nesequ tlit pos Jt

data Escape pos =
    -- | How man bytes is this code point in its UTF8 encoding?
    MkEscape !pos !Four

-----

data SequencePart pos a =
    -- | not {= a =}
    Item a
  |
    -- | {= a =}
    MetaEQ !pos a !pos

-----

deriving instance Show P
deriving instance Show X
deriving instance (Show pos, Show (b pos), Show (n pos), Show (s pos), Show (t pos), Show (sequ (SequencePart pos (Matter sequ b n s t pos)))) => Show (Matter sequ b n s t pos)
deriving instance (Show pos, Show slit) => Show (Symbol slit pos)
deriving instance (Show pos, Show a) => Show (ClosePin pos a)
deriving instance (Show pos, Show (b pos), Show (n pos), Show (s pos), Show (t pos)) => Show (Flat b n s t pos)
deriving instance (Show pos, Show blit) => Show (Bytes blit pos)
deriving instance (Show pos, Show blit) => Show (MoreBytes blit pos)
deriving instance Show pos => Show (Decimal pos)
deriving instance Show pos => Show (MaybeFraction pos)
deriving instance Show pos => Show (MaybeExponent pos)
deriving instance (Show pos, Show (nesequ (Escape pos)), Show tlit) => Show (Text nesequ tlit pos)
deriving instance Show Quote
deriving instance (Show pos, Show (nesequ (Escape pos)), Show tlit) => Show (MoreText nesequ tlit pos)
deriving instance (Show pos, Show (nesequ (Escape pos)), Show tlit) => Show (Joiner nesequ tlit pos j)
deriving instance Show pos => Show (Escape pos)
deriving instance (Show pos, Show a) => Show (SequencePart pos a)

deriving instance Eq P
deriving instance Eq X
deriving instance (Eq pos, Eq (sequ (SequencePart pos (Matter sequ b n s t pos))), Eq (b pos), Eq (n pos), Eq (s pos), Eq (t pos)) => Eq (Matter sequ b n s t pos)
deriving instance (Eq pos, Eq slit) => Eq (Symbol slit pos)
deriving instance (Eq pos, Eq a) => Eq (ClosePin pos a)
deriving instance (Eq pos, Eq (b pos), Eq (n pos), Eq (s pos), Eq (t pos)) => Eq (Flat b n s t pos)
deriving instance (Eq pos, Eq blit) => Eq (Bytes blit pos)
deriving instance (Eq pos, Eq blit) => Eq (MoreBytes blit pos)
deriving instance Eq pos => Eq (Decimal pos)
deriving instance Eq pos => Eq (MaybeFraction pos)
deriving instance Eq pos => Eq (MaybeExponent pos)
instance          (Eq pos, Eq (nesequ (Escape pos)), Eq tlit) => Eq (Text nesequ tlit pos) where
    Suppressor p1 p1' j1 txt1 == Suppressor p2 p2' j2 txt2 = p1 == p2 && p1' == p2' && joinerEquality j1 j2 && txt1 == txt2
    TextLit q1 tlit1 l1 r1 more1 == TextLit q2 tlit2 l2 r2 more2 = q1 == q2 && tlit1 == tlit2 && l1 == l2 && r1 == r2 && more1 == more2
    _ == _ = False
deriving instance Eq Quote
instance          (Eq pos, Eq (nesequ (Escape pos)), Eq tlit) => Eq (MoreText nesequ tlit pos) where
    NoMoreText == NoMoreText = True
    MoreText p1 j1 txt1 == MoreText p2 j2 txt2 = p1 == p2 && joinerEquality j1 j2 && txt1 == txt2
    _ == _ = False
deriving instance (Eq pos, Eq (nesequ (Escape pos)), Eq tlit) => Eq (Joiner nesequ tlit pos j)
deriving instance Eq pos => Eq (Escape pos)
deriving instance (Eq pos, Eq a) => Eq (SequencePart pos a)

deriving instance Functor (Symbol b)
deriving instance Functor (ClosePin pos)
deriving instance (Functor b, Functor n, Functor s, Functor t) => Functor (Flat b n s t)
deriving instance Functor (Bytes blit)
deriving instance Functor (MoreBytes blit)
deriving instance Functor Decimal
deriving instance Functor MaybeFraction
deriving instance Functor MaybeExponent
instance          Functor nesequ => Functor (Text nesequ t) where
    fmap f = \case
        Suppressor p p' j txt -> Suppressor (f p) (f p') (mapPosJoiner f j) (fmap f txt)
        TextLit q tlit l r more -> TextLit q tlit (f l) (f r) (fmap f more)
instance          Functor nesequ => Functor (MoreText nesequ t) where
    fmap f = \case
        NoMoreText -> NoMoreText
        MoreText p j t -> MoreText (f p) (mapPosJoiner f j) (fmap f t)
deriving instance Functor Escape
deriving instance Functor (SequencePart pos)

instance          TestEquality (Joiner nesequ tlit pos) where
    testEquality = curry $ \case
        (NilJoiner{}        , NilJoiner{}        ) -> Nothing   -- SingI j could do better here
        (ConsJoinerText{}   , ConsJoinerText{}   ) -> Just Refl
        (ConsJoinerEscapes{}, ConsJoinerEscapes{}) -> Just Refl
        _ -> Nothing

-- | Note that the arguments can have different indices
joinerEquality :: (Eq pos, Eq (nesequ (Escape pos)), Eq tlit) => Joiner nesequ tlit pos x -> Joiner nesequ tlit pos y -> Bool
joinerEquality = curry $ \case
    (NilJoiner p1                 , NilJoiner p2                 ) -> p1 == p2
    (ConsJoinerText t1 l1 r1 j1   , ConsJoinerText t2 l2 r2 j2   ) -> t1 == t2 && l1 == l2 && r1 == r2 && joinerEquality j1 j2
    (ConsJoinerEscapes escapes1 j1, ConsJoinerEscapes escapes2 j2) -> escapes1 == escapes2 && joinerEquality j1 j2
    _ -> False

mapPosJoiner :: Functor nesequ => (pos -> pos') -> Joiner nesequ tlit pos x -> Joiner nesequ tlit pos' x
mapPosJoiner f = \case
    NilJoiner p -> NilJoiner (f p)
    ConsJoinerText t l r j -> ConsJoinerText t (f l) (f r) (mapPosJoiner f j)
    ConsJoinerEscapes escapes j -> ConsJoinerEscapes (fmap (fmap f) escapes) (mapPosJoiner f j)

-----

-- | The 'F.Base' functor of 'Matter'
data MatterF sequ b n s t pos a =
    FlatF (Flat b n s t pos)
  |
    VariantF !(s pos) !pos !pos a
  |
    SequenceF !pos (sequ (SequencePart pos a)) !pos
  |
    MetaGtF !pos a !pos !(ClosePin pos a)
  |
    ParenF !pos a !pos
  |
    PinMetaLtF !pos a !pos !pos a !pos
  deriving (Functor)

deriving instance (Eq (sequ (SequencePart pos a)), Eq (b pos), Eq (n pos), Eq (s pos), Eq (t pos), Eq pos, Eq a) => Eq (MatterF sequ b n s t pos a)
deriving instance (Show (sequ (SequencePart pos a)), Show (b pos), Show (n pos), Show (s pos), Show (t pos), Show pos, Show a) => Show (MatterF sequ b n s t pos a)

-- | Peel one layer of 'MatterF'
project :: Matter sequ b n s t pos -> MatterF sequ b n s t pos (Matter sequ b n s t pos)
{-# INLINE project #-}
project = \case
    Flat flt -> FlatF flt
    Variant s l r x -> VariantF s l r x
    Sequence l xs r -> SequenceF l xs r
    MetaGT l x r y -> MetaGtF l x r y
    Paren l x r -> ParenF l x r
    PinMetaLT l1 x r1 l2 y r2 -> PinMetaLtF l1 x r1 l2 y r2

-- | Inverse of 'project'
embed :: MatterF sequ b n s t pos (Matter sequ b n s t pos) -> Matter sequ b n s t pos
{-# INLINE embed #-}
embed = \case
    FlatF flt -> Flat flt
    VariantF s l r x -> Variant s l r x
    SequenceF l xs r -> Sequence l xs r
    MetaGtF l x r y -> MetaGT l x r y
    ParenF l x r -> Paren l x r
    PinMetaLtF l1 x r1 l2 y r2 -> PinMetaLT l1 x r1 l2 y r2

type instance F.Base (Matter sequ b n s t pos) = MatterF sequ b n s t pos
instance Functor sequ => F.Recursive (Matter sequ b n s t pos) where project = project
instance Functor sequ => F.Corecursive (Matter sequ b n s t pos) where embed = embed

-- | Specialization of 'F.fold' from "Data.Functor.Foldable"
fold :: Functor sequ => (MatterF sequ b n s t pos a -> a) -> Matter sequ b n s t pos -> a
{-# INLINE fold #-}
fold = F.fold

-- | Specialization of 'F.unfold' from "Data.Functor.Foldable"
unfold :: Functor sequ => (a -> MatterF sequ b n s t pos a) -> a -> Matter sequ b n s t pos
unfold = F.unfold

-----

infixr `MaybeFun`

-- | A function that might observably be @id@
data MaybeFun a b =
    (a ~ b) => NothingFun
  |
    JustFun (a -> b)

instance Cat.Category MaybeFun where
    id = NothingFun
    (.) = curry $ \case
        (NothingFun, NothingFun) -> NothingFun
        (f, g) -> JustFun $ maybeFun f . maybeFun g

maybeFun :: MaybeFun a b -> a -> b
maybeFun = \case
    NothingFun -> id
    JustFun f -> f

fmapMaybeFun :: Functor f => MaybeFun a b -> MaybeFun (f a) (f b)
fmapMaybeFun = \case
    NothingFun -> NothingFun
    JustFun f -> JustFun $ fmap f

data Funs sequ b n s t pos sequ' b' n' s' t' pos' a = MkFuns {
    sequFun :: sequ (SequencePart pos a) `MaybeFun` sequ' (SequencePart pos a)
  ,
    bFun :: b pos `MaybeFun` b' pos
  ,
    nFun :: n pos `MaybeFun` n' pos
  ,
    sFun :: s pos `MaybeFun` s' pos
  ,
    tFun :: t pos `MaybeFun` t' pos
  ,
    posFun :: pos `MaybeFun` pos'
  }

nothingFuns :: Funs sequ b n s t pos sequ b n s t pos a
nothingFuns = MkFuns NothingFun NothingFun NothingFun NothingFun NothingFun NothingFun

mapFuns ::
  forall sequ b n s t pos sequ' b' n' s' t' qos a.
        (Functor sequ', Functor b', Functor n', Functor s', Functor t')
     =>
        Funs sequ  b  n  s  t  pos
             sequ' b' n' s' t' qos a
     ->
        MatterF sequ  b  n  s  t  pos  a
     ->
        MatterF sequ' b' n' s' t' qos a
{-# INLINE mapFuns #-}
mapFuns funs =
    case funs of
        MkFuns NothingFun NothingFun NothingFun NothingFun NothingFun NothingFun -> id
        _ -> \case
            FlatF flt -> FlatF (flat flt)
            VariantF s l r x -> VariantF (symbol s) (pos l) (pos r) x
            SequenceF l xs r -> SequenceF (pos l) (sequenceParts $ sequ xs) (pos r)
            MetaGtF l x r y -> MetaGtF (pos l) x (pos r) (closePin y)
            ParenF l x r -> ParenF (pos l) x (pos r)
            PinMetaLtF l1 x r1 l2 y r2 -> PinMetaLtF (pos l1) x (pos r1) (pos l2) y (pos r2)
  where
    sequ   = maybeFun                             sequFun
    bytes  = maybeFun $ fmapMaybeFun posFun Cat.. bFun
    number = maybeFun $ fmapMaybeFun posFun Cat.. nFun
    symbol = maybeFun $ fmapMaybeFun posFun Cat.. sFun
    text   = maybeFun $ fmapMaybeFun posFun Cat.. tFun
    pos    = maybeFun                             posFun

    MkFuns {sequFun, bFun, nFun, sFun, tFun, posFun} = funs

    flat = case (bFun, nFun, sFun, tFun, posFun) of
        (NothingFun, NothingFun, NothingFun, NothingFun, NothingFun) -> id
        _ -> \case
            Atom s l r -> Atom (symbol s) (pos l) (pos r)
            Bytes b -> Bytes (bytes b)
            Number n -> Number (number n)
            Text t -> Text (text t)

    sequenceParts = case posFun of
        NothingFun -> id
        JustFun{} -> fmap $ \case
            Item x -> Item x
            MetaEQ l x r -> MetaEQ (pos l) x (pos r)

    closePin = case posFun of
        NothingFun -> id
        JustFun{} -> \case
            NoClosePin y -> NoClosePin y
            OnlyClosePin l y r -> OnlyClosePin (pos l) y (pos r)
            BothPins l1 y1 r1 l2 y2 r2 -> BothPins (pos l1) y1 (pos r1) (pos l2) y2 (pos r2)
