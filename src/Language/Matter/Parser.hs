{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Matter.Parser (
    MatterParse (..),

    Stk,
    emptyStk,
    pop,
    push,
    simplify,
    simplifyMaybe,

    SnocsResult (..),
    eof,
    snoc,
    snocs,
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Kind (Type)
import GHC.Show (showSpace)
import Language.Matter.Tokenizer (OdToken (..), Pos (..), SdToken (..), Token (..))
import Language.Matter.Tokenizer qualified as T
import Language.Matter.Tokenizer.Counting (Three (..), Four)
import Language.Matter.SyntaxTree qualified as ST

data FLAT =
    I | F
  |
    U | T | Jt1 | Jt2 | Jt3
  |
    B | Jb

class SingI a where singI :: a

data JoinerFollows :: FLAT -> Type where
  JoinerFollowsU   :: JoinerFollows U
  JoinerFollowsT   :: JoinerFollows T
  JoinerFollowsJt2 :: JoinerFollows Jt2

instance SingI (JoinerFollows U)   where singI = JoinerFollowsU
instance SingI (JoinerFollows T)   where singI = JoinerFollowsT
instance SingI (JoinerFollows Jt2) where singI = JoinerFollowsJt2

joinerFollowsOf :: SingI (JoinerFollows x) => Flat x -> JoinerFollows x
joinerFollowsOf _fstk = singI

data EscapeFollows :: FLAT -> Type where
  EscapeFollowsJt1 :: EscapeFollows Jt1
  EscapeFollowsJt2 :: EscapeFollows Jt2

instance SingI (EscapeFollows Jt1) where singI = EscapeFollowsJt1
instance SingI (EscapeFollows Jt2) where singI = EscapeFollowsJt2

escapeFollowsOf :: SingI (EscapeFollows x) => Flat x -> EscapeFollows x
escapeFollowsOf _fstk = singI

-- | Matter expressions that can't contain arbitrary matter expressions
--
-- This is a recursive type only because of joiners and (within one joiner) escapes.
data Flat :: FLAT -> Type where
    -- | 1
    IntegerPart :: Pos -> Pos -> Flat I
    -- | .2
    FractionPart :: Pos -> Pos -> Flat I -> Flat F

    -- | _
    Suppressor :: Pos -> Flat U   -- must be followed by a joiner
    -- | "hi"
    Text :: ST.Quote -> Pos -> Pos -> Flat T
    -- | <a> and a>
    SdJoiner :: SingI (JoinerFollows x) => Pos -> Pos -> Flat x -> Flat Jt3   -- must be followed by suppressor or text
    -- | <a and a
    OdJoiner :: SingI (JoinerFollows x) => Pos -> Pos -> Flat x -> Flat Jt1   -- must be followed by an escape
    -- | %21
    Escape :: SingI (EscapeFollows x) => Pos -> Four -> Flat x -> Flat Jt2    -- must be followed by escape or SdJoiner
    -- | _ after joiner
    MoreSuppressor :: Pos -> Flat Jt3 -> Flat U   -- must be followed by a joiner
    -- | "hi" after joiner
    MoreText :: ST.Quote -> Pos -> Pos -> Flat Jt3 -> Flat T

    -- | 0xAB
    Bytes :: Pos -> Pos -> Flat B
    -- | only <>
    BytesJoiner :: Pos -> Flat B -> Flat Jb   -- must be followed by bytes
    -- | 0xAB after joiner
    MoreBytes :: Pos -> Pos -> Flat Jb -> Flat B

-----

-- | Every frame except 'Flat' and 'Pin' must be followed by matter
--
-- The Maybe M arguments are only useful so that push can
-- succeed. Every function that simply ends with a call to 'push'
-- would have to be more complicated without these.
data Stk a =
    -- | @Nothing@ is initial state, @Just@ is accepting state
    Empty (Maybe a)   -- ^ never pops, since that'd be useless
  |
    forall x. Show (Flat x) => Flat (Flat x) (Stk a)   -- ^ might pop
  |
    -- | #
    OpenVariant Pos Pos (Stk a)   -- ^ won't pop
  |
    -- | [ a b c
    OpenSequence Pos (MatterSeq a (ST.SequencePart Pos a)) (Stk a)   -- ^ won't pop
  |
    -- | [ a b c {=
    Sequence_OpenMetaEQ Pos (MatterSeq a (ST.SequencePart Pos a)) Pos (Maybe a) (Stk a)   -- ^ @Just@ pops
  |
    -- | {> and {> a
    OpenMetaGT Pos (Maybe a) (Stk a)   -- ^ @Just@ pops
  |
    -- | {> a >}
    MetaGT_ Pos a Pos (Stk a)   -- ^ pops
  |
    -- | ( and ( a
    OpenParen Pos (Maybe a) (Stk a)   -- ^ @Just@ pops
  |
    -- | (^ and (^ a
    OpenPin Pos (Maybe a) (Stk a)   -- ^ @Just@ pops
  |
    -- | (^ a )
    PinParen Pos a Pos (Stk a)   -- ^ won't pop because must be followed by {<
  |
    -- | {> a >} (^ b ^)
    MetaGT_PinPin Pos a Pos Pos a Pos (Stk a)   -- ^ won't pop because must be followed by {<
  |
    -- | (^ a ) {< and (^ a ) {< b
    PinParen_OpenMetaLT Pos a Pos Pos (Maybe a) (Stk a)   -- ^ @Just@ pops
  |
    -- | {> a >} (^ b ^) {< and {> a >} (^ b ^) {< c
    MetaGT_PinPin_OpenMetaLT Pos a Pos Pos a Pos Pos (Maybe a) (Stk a)   -- ^ @Just@ pops

-- | A data type the parser knows how to construct
class MatterParse a where
    type MatterSeq a :: Type -> Type

    parseAlgebra :: ST.MatterF Pos NonEmpty (MatterSeq a) a -> a

    emptyMatterSeq :: MatterSeq a (ST.SequencePart Pos a)
    snocMatterSeq ::
        MatterSeq a (ST.SequencePart Pos a)
     ->
        ST.SequencePart Pos a
     ->
        MatterSeq a (ST.SequencePart Pos a)

emptyStk :: Stk a
emptyStk = Empty Nothing

instance MatterParse (ST.Matter Pos NonEmpty []) where
    type MatterSeq (ST.Matter Pos NonEmpty []) = []

    parseAlgebra = ST.embed . ST.mapSequence reverse

    emptyMatterSeq = []
    snocMatterSeq = flip (:)

snoc :: MatterParse a => Pos -> Pos -> Stk a -> Token -> Maybe (Stk a)
snoc l r = curry $ \case

    -- all number frames and number-continuation tokens
    (Flat (IntegerPart l1 r1) stk, OdToken OdFractionPart) ->
        Just $ Flat (FractionPart l r $ IntegerPart l1 r1) stk
    (Flat (FractionPart l2 r2 (IntegerPart l1 r1)) stk, OdToken OdExponentPart) ->
        push stk $ parseAlgebra $ ST.FlatF $ ST.Number l1 r1 (ST.JustFraction l2 r2) (ST.JustExponent l r)
    (Flat (IntegerPart l1 r1) stk, OdToken OdExponentPart) ->
        push stk $ parseAlgebra $ ST.FlatF $ ST.Number l1 r1 ST.NothingFraction (ST.JustExponent l r)

    (_stk, OdToken OdFractionPart) -> Nothing
    (_stk, OdToken OdExponentPart) -> Nothing

    -- OdJoiner and Escape frames
    (Flat (OdJoiner l1 r1 flt) stk, SdToken (SdJoinerEscapedUtf8 size)) ->
        Just $ Flat (Escape l size $ OdJoiner l1 r1 flt) stk
    (Flat OdJoiner{} _stk, _) -> Nothing
    (Flat (Escape l1 size flt) stk, SdToken (SdJoinerEscapedUtf8 size')) ->
        Just $ Flat (Escape l size' $ Escape l1 size flt) stk
    (Flat (Escape l1 size flt) stk, OdToken (OdJoinerNotEscaped False)) ->
        Just $ Flat (OdJoiner l r $ Escape l1 size flt) stk
    (Flat (Escape l1 size flt) stk, SdToken (SdJoinerNotEscaped Three3)) ->
        Just $ Flat (SdJoiner l r $ Escape l1 size flt) stk
    (Flat Escape{} _stk, _) -> Nothing

    (_stk, SdToken SdJoinerEscapedUtf8{}) -> Nothing
    (_stk, OdToken (OdJoinerNotEscaped False)) -> Nothing
    (_stk, SdToken (SdJoinerNotEscaped Three3)) -> Nothing


    -- Whitespace tokens, this MUST come after numbers and incomplete
    -- text joiners, since those disallow whitespace!
    (stk, OdToken OdWhitespace) -> Just stk

    -- Suppressor frames
    (Flat (Suppressor p1) stk, OdToken (OdJoinerNotEscaped True)) ->
        Just $ Flat (OdJoiner l r $ Suppressor p1) stk
    (Flat (Suppressor p1) stk, SdToken (SdJoinerNotEscaped _)) ->
        Just $ Flat (SdJoiner l r $ Suppressor p1) stk
    (Flat Suppressor{} _stk, _tk) -> Nothing
    (Flat (MoreSuppressor p1 flt) stk, OdToken (OdJoinerNotEscaped True)) ->
        Just $ Flat (OdJoiner l r $ MoreSuppressor p1 flt) stk
    (Flat (MoreSuppressor p1 flt) stk, SdToken (SdJoinerNotEscaped _)) ->
        Just $ Flat (SdJoiner l r $ MoreSuppressor p1 flt) stk
    (Flat MoreSuppressor{} _stk, _tk) -> Nothing

    -- Text frames
    (Flat (Text q l1 r1) stk, OdToken (OdJoinerNotEscaped True)) ->
        Just $ Flat (OdJoiner l r $ Text q l1 r1) stk
    (Flat (Text q l1 r1) stk, SdToken (SdJoinerNotEscaped _)) ->
        Just $ Flat (SdJoiner l r $ Text q l1 r1) stk
    (Flat (MoreText q l1 r1 flt) stk, OdToken (OdJoinerNotEscaped True)) ->
        Just $ Flat (OdJoiner l r $ MoreText q l1 r1 flt) stk
    (Flat (MoreText q l1 r1 flt) stk, SdToken (SdJoinerNotEscaped _)) ->
        Just $ Flat (SdJoiner l r $ MoreText q l1 r1 flt) stk

    (_stk, OdToken (OdJoinerNotEscaped True)) -> Nothing

    -- SdJoiner frames
    (Flat (SdJoiner l1 r1 flt) stk, SdToken SdUnderscore) ->
        Just $ Flat (MoreSuppressor l $ SdJoiner l1 r1 flt) stk
    (Flat (SdJoiner l1 r1 flt) stk, SdToken SdDoubleQuotedString) ->
        Just $ Flat (MoreText ST.DoubleQuote l r $ SdJoiner l1 r1 flt) stk
    (Flat (SdJoiner l1 r1 flt) stk, SdToken (SdMultiQuotedString delim)) ->
        Just $ Flat (MoreText (ST.MultiQuote delim) l r $ SdJoiner l1 r1 flt) stk
    (Flat SdJoiner{} _stk, _tk) -> Nothing

    -- All bytes frames
    (Flat (Bytes l1 r1) stk, SdToken (SdJoinerNotEscaped _)) ->
        if not $ emptyJoiner l r then Nothing else
        Just $ Flat (BytesJoiner l $ Bytes l1 r1) stk
    (Flat (BytesJoiner p1 flt) stk, OdToken OdBytes) ->
        Just $ Flat (MoreBytes l r $ BytesJoiner p1 flt) stk
    (Flat BytesJoiner{} _stk, _tk) -> Nothing
    (Flat (MoreBytes l1 r1 flt) stk, SdToken (SdJoinerNotEscaped _)) ->
        if not $ emptyJoiner l r then Nothing else
        Just $ Flat (BytesJoiner l $ MoreBytes l1 r1 flt) stk

    (_stk, SdToken (SdJoinerNotEscaped Three1)) -> Nothing
    (_stk, SdToken (SdJoinerNotEscaped Three2)) -> Nothing

    -- PinParen and PinPin frames
    (PinParen l1 m1 r1 stk, SdToken (SdOpenMeta LT)) ->
        Just $ PinParen_OpenMetaLT l1 m1 r1 l Nothing stk
    (MetaGT_PinPin l1 m1 r1 l2 m2 r2 stk, SdToken (SdOpenMeta LT)) ->
        Just $ MetaGT_PinPin_OpenMetaLT l1 m1 r1 l2 m2 r2 l Nothing stk
    (PinParen{}, _tk) -> Nothing
    (MetaGT_PinPin{}, _tk) -> Nothing

    (_stk, SdToken (SdOpenMeta LT)) -> Nothing

    ----------------------------------------
    -- Every case below this line must only use @'simplify' stk@,
    -- never just @stk@.
    --
    -- Sadly, if I were to enforce that with @(stk, tk) -> f (simplify
    -- stk) tk@, the pattern checker would consider @f@ partial.

    -- @
    (stk, OdToken OdAtom) ->
        push (simplify stk) $ parseAlgebra $ ST.FlatF $ ST.Atom l r
    -- 0
    (stk, OdToken OdIntegerPart) ->
        Just $ Flat (IntegerPart l r) $ simplify stk
    -- _
    (stk, SdToken SdUnderscore) ->
        Just $ Flat (Suppressor l) $ simplify stk
    -- "
    (stk, SdToken SdDoubleQuotedString) ->
        Just $ Flat (Text ST.DoubleQuote l r) $ simplify stk
    -- '
    (stk, SdToken (SdMultiQuotedString delim)) ->
        Just $ Flat (Text (ST.MultiQuote delim) l r) $ simplify stk
    -- 0x
    (stk, OdToken OdBytes) ->
        Just $ Flat (Bytes l r) $ simplify stk

    -- #
    (stk, OdToken OdVariant) ->
        Just $ OpenVariant l r $ simplify stk
    -- [ ]
    (stk, SdToken SdOpenSeq) ->
        Just $ OpenSequence l emptyMatterSeq $ simplify stk
    (stk, SdToken SdCloseSeq) ->
        case simplify stk of
            OpenSequence p1 acc stk' ->
                push stk' $ parseAlgebra $ ST.SequenceF p1 acc l
            _ -> Nothing
    -- {= =}
    (stk, SdToken (SdOpenMeta EQ)) -> do
        case simplify stk of
            OpenSequence p1 acc stk' ->
                Just $ Sequence_OpenMetaEQ p1 acc l Nothing stk'
            _ -> Nothing
    (stk, SdToken (SdCloseMeta EQ)) -> do
        case simplify stk of
            Sequence_OpenMetaEQ p1 acc p2 (Just m) stk' ->
                Just $ OpenSequence p1 (snocMatterSeq acc $ ST.MetaEQ p2 m l) stk'
            _ -> Nothing
    -- {> >}
    (stk, SdToken (SdOpenMeta GT)) ->
        Just $ OpenMetaGT l Nothing $ simplify stk
    (stk, SdToken (SdCloseMeta GT)) -> do
        case simplify stk of
            OpenMetaGT p1 (Just m) stk' ->
                Just $ MetaGT_ p1 m l stk'
            _ -> Nothing
    -- ( (^
    (stk, OdToken OdOpenParen) ->
        Just $ OpenParen l Nothing $ simplify stk
    (stk, SdToken SdOpenPin) ->
        Just $ OpenPin l Nothing $ simplify stk
    -- ) ^)
    (stk, SdToken SdCloseParen) -> do
        case simplify stk of
            OpenParen p1 (Just m) stk' ->
                push stk' $ parseAlgebra $ ST.ParenF p1 m l
            OpenPin p1 (Just m) stk'' ->
                Just $ PinParen p1 m l stk''
            _ -> Nothing
    (stk, SdToken SdClosePin) -> do
        case simplify stk of
            OpenParen p (Just m) (MetaGT_ l1 m' r1 stk') ->
                push stk' $ parseAlgebra $ ST.MetaGtF l1 m' r1 (ST.OnlyClosePin p m l)
            OpenPin p1 (Just m) (MetaGT_ l1 m' r1 stk') ->
                Just $ MetaGT_PinPin l1 m' r1 p1 m l stk'
            _ -> Nothing
    -- <}    note that {< is handled by the Pin frame above
    (stk, SdToken (SdCloseMeta LT)) -> do
        case simplify stk of
            PinParen_OpenMetaLT l1 m1 r1 p2 (Just m2) stk' ->
                push stk' $ parseAlgebra $ ST.PinMetaLtF l1 m1 r1 p2 m2 l
            MetaGT_PinPin_OpenMetaLT l1 m1 r1 l2 m2 r2 p3 (Just m3) stk' ->
                push stk' $ parseAlgebra $ ST.MetaGtF l1 m1 r1 (ST.BothPins l2 m2 r2 p3 m3 l)
            _ -> Nothing

emptyJoiner :: Pos -> Pos -> Bool
emptyJoiner (MkPos x) (MkPos y) = x + 1 == y

-----

simplify :: MatterParse a => Stk a -> Stk a
simplify stk = case simplifyMaybe stk of
    Nothing -> stk
    Just stk' -> stk'

simplifyMaybe :: MatterParse a => Stk a -> Maybe (Stk a)
simplifyMaybe stk = case pop stk of
    Nothing -> Nothing
    Just (m, stk') -> push stk' m

-----

pop :: MatterParse a => Stk a -> Maybe (a, Stk a)
pop = \case
    Empty{} -> Nothing
    Flat fstk stk -> do
        flt <- popFlat fstk
        Just (flt, stk)
    OpenVariant{} -> Nothing
    OpenSequence{} -> Nothing
    Sequence_OpenMetaEQ{} -> Nothing
    OpenMetaGT{} -> Nothing
    MetaGT_{} -> Nothing
    OpenParen{} -> Nothing
    OpenPin{} -> Nothing
    PinParen{} ->
        -- Note that this should not be simplifyMaybe to ST.Paren! (^
        -- ... ) It /must/ be /direclty/ followed by {<, and the logic
        -- for {< above doesn't call 'pop' or 'simplifyMaybe'.
        Nothing
    MetaGT_PinPin{} ->
        -- Note that this should not be simplifyMaybe to ST.Paren! (^
        -- ... ^) It /must/ be /direclty/ followed by {<, and the
        -- logic for {< above doesn't call 'pop' or 'simplifyMaybe'.
        Nothing
    PinParen_OpenMetaLT{} -> Nothing
    MetaGT_PinPin_OpenMetaLT{} -> Nothing
    
popFlat :: MatterParse a => Flat x -> Maybe a
popFlat = \case
    IntegerPart l r -> Just $ parseAlgebra $ ST.FlatF $ ST.Number l r ST.NothingFraction ST.NothingExponent
    FractionPart l2 r2 (IntegerPart l1 r1) -> Just $ parseAlgebra $ ST.FlatF $ ST.Number l1 r1 (ST.JustFraction l2 r2) ST.NothingExponent
    Suppressor{} -> Nothing
    x@Text{} -> Just $ parseAlgebra $ ST.FlatF $ ST.Text $ popT ST.NoMoreText x
    SdJoiner{} -> Nothing
    OdJoiner{} -> Nothing
    Escape{} -> Nothing
    MoreSuppressor{} -> Nothing
    x@MoreText{} -> Just $ parseAlgebra $ ST.FlatF $ ST.Text $ popT ST.NoMoreText x
    x@Bytes{} -> Just $ parseAlgebra $ ST.FlatF $ popBytes ST.NoMoreBytes x
    BytesJoiner{} -> Nothing
    x@MoreBytes{} -> Just $ parseAlgebra $ ST.FlatF $ popBytes ST.NoMoreBytes x

popT :: ST.MoreText Pos NonEmpty -> Flat T -> ST.Text Pos NonEmpty
popT acc = \case

    Text q l r -> ST.TextLiteral q l r acc
    MoreText q l r (SdJoiner l1 r1 fstk) -> case popJoinerFollows (ST.NilJoiner l1 r1) fstk of
        (j, fstk') -> popUT j (ST.TextLiteral q l r acc) fstk'

popUT :: ST.Joiner Pos NonEmpty -> ST.Text Pos NonEmpty -> Either (Flat U) (Flat T) -> ST.Text Pos NonEmpty
popUT j acc = \case

    Left (Suppressor p) -> ST.Suppressor p j acc
    Left (MoreSuppressor p (SdJoiner l1 r1 fstk)) -> case popJoinerFollows (ST.NilJoiner l1 r1) fstk of
        (j', fstk') -> popUT j' (ST.Suppressor p j acc) fstk'

    Right fstk -> popT (ST.MoreText j acc) fstk

popJoinerFollows ::
    SingI (JoinerFollows x)
 =>
    ST.Joiner Pos NonEmpty -> Flat x -> (ST.Joiner Pos NonEmpty, Either (Flat U) (Flat T))
popJoinerFollows j fstk = case joinerFollowsOf fstk of

    JoinerFollowsU -> (j, Left fstk)
    JoinerFollowsT -> (j, Right fstk)

    JoinerFollowsJt2 -> popJt2 [] j fstk

popJt2 :: [ST.Escape Pos] -> ST.Joiner Pos NonEmpty -> Flat Jt2 -> (ST.Joiner Pos NonEmpty, Either (Flat U) (Flat T))
popJt2 acc j (Escape p size fstk) = case (escapeFollowsOf fstk, fstk) of

    (EscapeFollowsJt2, fstk') -> popJt2 (ST.MkEscape p size : acc) j fstk'

    (EscapeFollowsJt1, OdJoiner l r fstk') ->
        popJoinerFollows (ST.ConsJoiner l r (NE.reverse (ST.MkEscape p size NE.:| acc)) j) fstk'

popBytes :: ST.MoreBytes Pos -> Flat B -> ST.Flat Pos NonEmpty
popBytes acc = \case
    Bytes l r -> ST.Bytes l r acc
    MoreBytes l r (BytesJoiner p fstk) -> popBytes (ST.MoreBytes p l r acc) fstk

-----

push :: MatterParse a => Stk a -> a -> Maybe (Stk a)
push = flip $ \m -> \case
    Empty mb ->
        case mb of
            Nothing -> Just $ Empty (Just m)
            Just{} -> Nothing
    Flat{} ->
        Nothing
    OpenVariant l r stk ->
        push stk $ parseAlgebra $ ST.VariantF l r m
    OpenSequence p acc stk ->
        Just $ OpenSequence p (snocMatterSeq acc $ ST.Item m) stk
    Sequence_OpenMetaEQ p1 acc p2 mb stk ->
        case mb of
            Nothing -> Just $ Sequence_OpenMetaEQ p1 acc p2 (Just m) stk
            Just{} -> Nothing
    OpenMetaGT p mb stk ->
        case mb of
            Nothing -> Just $ OpenMetaGT p (Just m) stk
            Just{} -> Nothing
    MetaGT_ l m' r stk ->
      push stk $ parseAlgebra $ ST.MetaGtF l m' r (ST.NoClosePin m)
    OpenParen p mb stk ->
        case mb of
            Nothing -> Just $ OpenParen p (Just m) stk
            Just{} -> Nothing
    OpenPin p mb stk ->
        case mb of
            Nothing -> Just $ OpenPin p (Just m) stk
            Just{} -> Nothing
    PinParen{} ->
        Nothing
    MetaGT_PinPin{} ->
        Nothing
    PinParen_OpenMetaLT l1 m' r1 l2 mb stk ->
        case mb of
            Nothing -> Just $ PinParen_OpenMetaLT l1 m' r1 l2 (Just m) stk
            Just{} -> Nothing
    MetaGT_PinPin_OpenMetaLT l1 m1 r1 l2 m2 r2 l3 mb stk ->
        case mb of
            Nothing -> Just $ MetaGT_PinPin_OpenMetaLT l1 m1 r1 l2 m2 r2 l3 (Just m) stk
            Just{} -> Nothing

-----

data SnocsResult a =
    SnocsDone (Stk a) T.Tokenizer
  |
    SnocsStuck (Stk a) Pos Token Pos
  |
    SnocsError Pos Pos T.SnocError

snocs :: (T.MatterStream inp, MatterParse a) => Stk a -> inp -> SnocsResult a
snocs =
    \stk x -> go stk $ T.snocsTokenizer T.startTokenizer x
  where
    go stk = \case
        T.SnocsToken l tk r k -> case snoc l r stk tk of
            Nothing -> SnocsStuck stk l tk r
            Just stk' -> go stk' k
        T.SnocsDone k -> SnocsDone stk k
        T.SnocsError start cur e -> SnocsError start cur e

eof :: MatterParse a => Stk a -> Maybe a
eof = (. simplify) $ \case
    Empty (Just m) -> Just m
    _ -> Nothing

-----

-- Copied from -ddump-deriv in a throwaway module where I simply
-- removed the type index from Flat.

instance (Show (MatterSeq a (ST.SequencePart Pos a)), Show a) => Show (Stk a) where
    showsPrec a_aIc (Empty b1_aId)
      = showParen
          (a_aIc >= 11)
          ((.)
             (showString "Empty ")
             (showsPrec 11 b1_aId))
    showsPrec a_aV9 (Flat b1_aVa b2_aVb)
      = showParen
          (a_aV9 >= 11)
          ((.)
             (showString "Flat ")
             ((.)
                (showsPrec 11 b1_aVa)
                ((.)
                   showSpace
                   (showsPrec 11 b2_aVb))))
    showsPrec
      a_aVc
      (OpenVariant b1_aVd b2_aVe b3_aVf)
      = showParen
          (a_aVc >= 11)
          ((.)
             (showString "OpenVariant ")
             ((.)
                (showsPrec 11 b1_aVd)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_aVe)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_aVf))))))
    showsPrec
      a_aVg
      (OpenSequence b1_aVh b2_aVi b3_aVj)
      = showParen
          (a_aVg >= 11)
          ((.)
             (showString "OpenSequence ")
             ((.)
                (showsPrec 11 b1_aVh)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_aVi)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_aVj))))))
    showsPrec
      a_aVk
      (Sequence_OpenMetaEQ b1_aVl b2_aVm b3_aVn b4_aVo b5_aVp)
      = showParen
          (a_aVk >= 11)
          ((.)
             (showString "Sequence_OpenMetaEQ ")
             ((.)
                (showsPrec 11 b1_aVl)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_aVm)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3_aVn)
                            ((.)
                               showSpace
                               ((.)
                                  (showsPrec 11 b4_aVo)
                                  ((.)
                                     showSpace
                                     (showsPrec 11 b5_aVp))))))))))
    showsPrec
      a_aVq
      (OpenMetaGT b1_aVr b2_aVs b3_aVt)
      = showParen
          (a_aVq >= 11)
          ((.)
             (showString "OpenMetaGT ")
             ((.)
                (showsPrec 11 b1_aVr)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_aVs)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_aVt))))))
    showsPrec
      a_aVu
      (MetaGT_ b1_aVv b2_aVw b3_aVx b4_aVy)
      = showParen
          (a_aVu >= 11)
          ((.)
             (showString "MetaGT_ ")
             ((.)
                (showsPrec 11 b1_aVv)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_aVw)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3_aVx)
                            ((.)
                               showSpace
                               (showsPrec 11 b4_aVy))))))))
    showsPrec
      a_aVz
      (OpenParen b1_aVA b2_aVB b3_aVC)
      = showParen
          (a_aVz >= 11)
          ((.)
             (showString "OpenParen ")
             ((.)
                (showsPrec 11 b1_aVA)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_aVB)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_aVC))))))
    showsPrec
      a_aVD
      (OpenPin b1_aVE b2_aVF b3_aVG)
      = showParen
          (a_aVD >= 11)
          ((.)
             (showString "OpenPin ")
             ((.)
                (showsPrec 11 b1_aVE)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_aVF)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_aVG))))))
    showsPrec
      a_aVH
      (PinParen b1_aVI b2_aVJ b3_aVK b4_aVL)
      = showParen
          (a_aVH >= 11)
          ((.)
             (showString "PinParen ")
             ((.)
                (showsPrec 11 b1_aVI)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_aVJ)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3_aVK)
                            ((.)
                               showSpace
                               (showsPrec 11 b4_aVL))))))))
    showsPrec
      a_aVM
      (MetaGT_PinPin b1_aVN b2_aVO b3_aVP b4_aVQ b5_aVR b6_aVS
                         b7_aVT)
      = showParen
          (a_aVM >= 11)
          ((.)
             (showString "MetaGT_PinPin ")
             ((.)
                (showsPrec 11 b1_aVN)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_aVO)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3_aVP)
                            ((.)
                               showSpace
                               ((.)
                                  (showsPrec 11 b4_aVQ)
                                  ((.)
                                     showSpace
                                     ((.)
                                        (showsPrec 11 b5_aVR)
                                        ((.)
                                           showSpace
                                           ((.)
                                              (showsPrec 11 b6_aVS)
                                              ((.)
                                                 showSpace
                                                 (showsPrec
                                                    11 b7_aVT))))))))))))))
    showsPrec
      a_aVU
      (PinParen_OpenMetaLT b1_aVV b2_aVW b3_aVX b4_aVY b5_aVZ b6_aW0)
      = showParen
          (a_aVU >= 11)
          ((.)
             (showString "PinParen_OpenMetaLT ")
             ((.)
                (showsPrec 11 b1_aVV)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_aVW)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3_aVX)
                            ((.)
                               showSpace
                               ((.)
                                  (showsPrec 11 b4_aVY)
                                  ((.)
                                     showSpace
                                     ((.)
                                        (showsPrec 11 b5_aVZ)
                                        ((.)
                                           showSpace
                                           (showsPrec 11 b6_aW0))))))))))))
    showsPrec
      a_aW1
      (MetaGT_PinPin_OpenMetaLT b1_aW2 b2_aW3 b3_aW4 b4_aW5 b5_aW6
                                    b6_aW7 b7_aW8 b8_aW9 b9_aWa)
      = showParen
          (a_aW1 >= 11)
          ((.)
             (showString "MetaGT_PinPin_OpenMetaLT ")
             ((.)
                (showsPrec 11 b1_aW2)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_aW3)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3_aW4)
                            ((.)
                               showSpace
                               ((.)
                                  (showsPrec 11 b4_aW5)
                                  ((.)
                                     showSpace
                                     ((.)
                                        (showsPrec 11 b5_aW6)
                                        ((.)
                                           showSpace
                                           ((.)
                                              (showsPrec 11 b6_aW7)
                                              ((.)
                                                 showSpace
                                                 ((.)
                                                    (showsPrec 11 b7_aW8)
                                                    ((.)
                                                       showSpace
                                                       ((.)
                                                          (showsPrec 11 b8_aW9)
                                                          ((.)
                                                             showSpace
                                                             (showsPrec
                                                                11 b9_aWa))))))))))))))))))

instance Show (Flat x) where
    showsPrec
      a_a2ln
      (IntegerPart b1_a2lo b2_a2lp)
      = showParen
          (a_a2ln >= 11)
          ((.)
             (showString "IntegerPart ")
             ((.)
                (showsPrec 11 b1_a2lo)
                ((.)
                   showSpace
                   (showsPrec 11 b2_a2lp))))
    showsPrec
      a_a2lq
      (FractionPart b1_a2lr b2_a2ls b3_a2lt)
      = showParen
          (a_a2lq >= 11)
          ((.)
             (showString "FractionPart ")
             ((.)
                (showsPrec 11 b1_a2lr)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2ls)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_a2lt))))))
    showsPrec
      a_a2lu
      (Suppressor b1_a2lv)
      = showParen
          (a_a2lu >= 11)
          ((.)
             (showString "Suppressor ")
             (showsPrec 11 b1_a2lv))
    showsPrec
      a_a2lw
      (Text b1_a2lx b2_a2ly b3_a2lz)
      = showParen
          (a_a2lw >= 11)
          ((.)
             (showString "Text ")
             ((.)
                (showsPrec 11 b1_a2lx)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2ly)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_a2lz))))))
    showsPrec
      a_a2lA
      (SdJoiner b1_a2lB b2_a2lC b3_a2lD)
      = showParen
          (a_a2lA >= 11)
          ((.)
             (showString "SdJoiner ")
             ((.)
                (showsPrec 11 b1_a2lB)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2lC)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_a2lD))))))
    showsPrec
      a_a2lE
      (OdJoiner b1_a2lF b2_a2lG b3_a2lH)
      = showParen
          (a_a2lE >= 11)
          ((.)
             (showString "OdJoiner ")
             ((.)
                (showsPrec 11 b1_a2lF)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2lG)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_a2lH))))))
    showsPrec
      a_a2lI
      (Escape b1_a2lJ b2_a2lK b3_a2lL)
      = showParen
          (a_a2lI >= 11)
          ((.)
             (showString "Escape ")
             ((.)
                (showsPrec 11 b1_a2lJ)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2lK)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_a2lL))))))
    showsPrec
      a_a2lM
      (MoreSuppressor b1_a2lN b2_a2lO)
      = showParen
          (a_a2lM >= 11)
          ((.)
             (showString "MoreSuppressor ")
             ((.)
                (showsPrec 11 b1_a2lN)
                ((.)
                   showSpace
                   (showsPrec 11 b2_a2lO))))
    showsPrec
      a_a2lP
      (MoreText b1_a2lQ b2_a2lR b3_a2lS
                                          b4_a2lT)
      = showParen
          (a_a2lP >= 11)
          ((.)
             (showString "MoreText ")
             ((.)
                (showsPrec 11 b1_a2lQ)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2lR)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3_a2lS)
                            ((.)
                               showSpace
                               (showsPrec 11 b4_a2lT))))))))
    showsPrec
      a_a2lU
      (Bytes b1_a2lV b2_a2lW)
      = showParen
          (a_a2lU >= 11)
          ((.)
             (showString "Bytes ")
             ((.)
                (showsPrec 11 b1_a2lV)
                ((.)
                   showSpace
                   (showsPrec 11 b2_a2lW))))
    showsPrec
      a_a2lX
      (BytesJoiner b1_a2lY b2_a2lZ)
      = showParen
          (a_a2lX >= 11)
          ((.)
             (showString "BytesJoiner ")
             ((.)
                (showsPrec 11 b1_a2lY)
                ((.)
                   showSpace
                   (showsPrec 11 b2_a2lZ))))
    showsPrec
      a_a2m0
      (MoreBytes b1_a2m1 b2_a2m2 b3_a2m3)
      = showParen
          (a_a2m0 >= 11)
          ((.)
             (showString "MoreBytes ")
             ((.)
                (showsPrec 11 b1_a2m1)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2m2)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_a2m3))))))
