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

module Language.Matter.Parser (module Language.Matter.Parser) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Kind (Type)
import GHC.Show (showSpace)
import Language.Matter.Tokenizer (OdToken (..), Pos (..), SdToken (..), Token (..))
import Language.Matter.Tokenizer qualified as T
import Language.Matter.Tokenizer.Counting (Four)
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

type M = ST.Matter Pos [] NonEmpty

-- | Every frame except 'Flat' and 'Pin' must be followed by matter
--
-- The Maybe M arguments are only useful so that push can
-- succeed. Every function that simply ends with a call to 'push'
-- would have to be more complicated without these.
data Stk m =
    -- | @Nothing@ is initial state, @Just@ is accepting state
    Empty (Maybe m)   -- ^ never pops, since that'd be useless
  |
    forall x. Show (Flat x) => Flat (Flat x) (Stk m)   -- ^ might pop
  |
    -- | #
    OpenVariant Pos Pos (Stk m)   -- ^ won't pop
  |
    -- | [ a b c
    OpenSequence Pos (Matters m) (Stk m)   -- ^ won't pop
  |
    -- | [ a b c {=
    SequenceOpenMetaEQ Pos (Matters m) Pos (Maybe m) (Stk m)   -- ^ @Just@ pops
  |
    -- | {> and {> a
    OpenMetaGT Pos (Maybe m) (Stk m)   -- ^ @Just@ pops
  |
    -- | {> a >}
    MetaGT_ Pos m Pos (Stk m)   -- ^ pops
  |
    -- | ( and ( a
    OpenParen Pos (Maybe m) (Stk m)   -- ^ @Just@ pops
  |
    -- | (^ and (^ a
    OpenPin Pos (Maybe m) (Stk m)   -- ^ @Just@ pops
  |
    -- | (^ a ) and (^ a ^)
    Pin Pos m Pos ST.Pin (Stk m)   -- ^ won't pop because must be followed by {<
  |
    -- | (^ a ) {< and (^ a ) {< b
    PinOpenMetaLT Pos m Pos ST.Pin Pos (Maybe m) (Stk m)   -- ^ @Just@ pops

class MatterParse m where
    data Matters m :: Type

    flat :: ST.Flat Pos NonEmpty -> m

    variant :: Pos -> Pos -> m -> m

    emptyMatters :: Matters m
    item :: Matters m -> m -> Matters m
    metaEQ :: Matters m -> Pos -> m -> Pos -> Matters m
    fromMatters :: Pos -> Matters m -> Pos -> m

    metaGT :: Pos -> m -> Pos -> m -> m
    paren :: Pos -> m -> Pos -> ST.Pin -> m
    pinMetaLT :: Pos -> m -> Pos -> ST.Pin -> Pos -> m -> Pos -> m

instance MatterParse (ST.Matter Pos [] NonEmpty) where
    newtype Matters _ = MkMatters [ST.SequencePart Pos [] NonEmpty]

    flat = ST.Flat

    variant = ST.Variant

    emptyMatters = MkMatters []
    item (MkMatters acc) m = MkMatters $ ST.Item m : acc
    metaEQ (MkMatters acc) p2 m l = MkMatters $ ST.MetaEQ p2 m l : acc
    fromMatters p1 (MkMatters acc) p2 =
        ST.Sequence p1 (reverse acc) p2

    metaGT = ST.MetaGT
    paren = ST.Paren
    pinMetaLT = ST.PinMetaLT

deriving instance Show (Matters (ST.Matter Pos [] NonEmpty))

snoc :: MatterParse m => Pos -> Pos -> Stk m -> Token -> Maybe (Stk m)
snoc l r = curry $ \case

    -- all number frames and number-continuation tokens
    (Flat (IntegerPart l1 r1) stk, OdToken OdFractionPart) ->
        Just $ Flat (FractionPart l r $ IntegerPart l1 r1) stk
    (Flat (FractionPart l2 r2 (IntegerPart l1 r1)) stk, OdToken OdExponentPart) ->
        push stk $ flat $ ST.Number l1 r1 (ST.JustFraction l2 r2) (ST.JustExponent l r)
    (Flat (IntegerPart l1 r1) stk, OdToken OdExponentPart) ->
        push stk $ flat $ ST.Number l1 r1 ST.NothingFraction (ST.JustExponent l r)

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
    (Flat (Escape l1 size flt) stk, SdToken (SdJoinerNotEscaped False)) ->
        Just $ Flat (SdJoiner l r $ Escape l1 size flt) stk
    (Flat Escape{} _stk, _) -> Nothing

    (_stk, SdToken SdJoinerEscapedUtf8{}) -> Nothing
    (_stk, OdToken (OdJoinerNotEscaped False)) -> Nothing
    (_stk, SdToken (SdJoinerNotEscaped False)) -> Nothing


    -- Whitespace tokens, this MUST come after numbers and incomplete
    -- text joiners, since those disallow whitespace!
    (stk, OdToken OdWhitespace) -> Just stk

    -- Suppressor frames
    (Flat (Suppressor p1) stk, OdToken (OdJoinerNotEscaped True)) ->
        Just $ Flat (OdJoiner l r $ Suppressor p1) stk
    (Flat (Suppressor p1) stk, SdToken (SdJoinerNotEscaped True)) ->
        Just $ Flat (SdJoiner l r $ Suppressor p1) stk
    (Flat Suppressor{} _stk, _tk) -> Nothing
    (Flat (MoreSuppressor p1 flt) stk, OdToken (OdJoinerNotEscaped True)) ->
        Just $ Flat (OdJoiner l r $ MoreSuppressor p1 flt) stk
    (Flat (MoreSuppressor p1 flt) stk, SdToken (SdJoinerNotEscaped True)) ->
        Just $ Flat (SdJoiner l r $ MoreSuppressor p1 flt) stk
    (Flat MoreSuppressor{} _stk, _tk) -> Nothing

    -- Text frames
    (Flat (Text q l1 r1) stk, OdToken (OdJoinerNotEscaped True)) ->
        Just $ Flat (OdJoiner l r $ Text q l1 r1) stk
    (Flat (Text q l1 r1) stk, SdToken (SdJoinerNotEscaped True)) ->
        Just $ Flat (SdJoiner l r $ Text q l1 r1) stk
    (Flat (MoreText q l1 r1 flt) stk, OdToken (OdJoinerNotEscaped True)) ->
        Just $ Flat (OdJoiner l r $ MoreText q l1 r1 flt) stk
    (Flat (MoreText q l1 r1 flt) stk, SdToken (SdJoinerNotEscaped True)) ->
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
    (Flat (Bytes l1 r1) stk, SdToken (SdJoinerNotEscaped True)) ->
        if not $ emptyJoiner l r then Nothing else
        Just $ Flat (BytesJoiner l $ Bytes l1 r1) stk
    (Flat (BytesJoiner p1 flt) stk, OdToken OdBytes) ->
        Just $ Flat (MoreBytes l r $ BytesJoiner p1 flt) stk
    (Flat BytesJoiner{} _stk, _tk) -> Nothing
    (Flat (MoreBytes l1 r1 flt) stk, SdToken (SdJoinerNotEscaped True)) ->
        if not $ emptyJoiner l r then Nothing else
        Just $ Flat (BytesJoiner l $ MoreBytes l1 r1 flt) stk

    (_stk, SdToken (SdJoinerNotEscaped True)) -> Nothing

    -- Pin frames
    (Pin l1 e r1 pin stk, SdToken (SdOpenMeta LT)) ->
        Just $ PinOpenMetaLT l1 e r1 pin l Nothing stk
    (Pin{}, _tk) -> Nothing

    (_stk, SdToken (SdOpenMeta LT)) -> Nothing

    -- @
    (stk, OdToken OdAtom) ->
        push stk $ flat $ ST.Atom l r
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
        Just $ OpenSequence l emptyMatters $ simplify stk
    (stk, SdToken SdCloseSeq) ->
        case simplify stk of
            OpenSequence p1 acc stk' ->
                push stk' $ fromMatters p1 acc l
            _ -> Nothing
    -- {= =}
    (stk, SdToken (SdOpenMeta EQ)) -> do
        case simplify stk of
            OpenSequence p1 acc stk' ->
                Just $ SequenceOpenMetaEQ p1 acc l Nothing stk'
            _ -> Nothing
    (stk, SdToken (SdCloseMeta EQ)) -> do
        case simplify stk of
            SequenceOpenMetaEQ p1 acc p2 (Just m) stk' ->
                Just $ OpenSequence p1 (metaEQ acc p2 m l) stk'
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
                push stk' $ paren p1 m l ST.NoPin
            OpenPin p1 (Just m) stk'' ->
                Just $ Pin p1 m l ST.NoPin stk''
            _ -> Nothing
    (stk, SdToken SdClosePin) -> do
        case simplify stk of
            OpenParen p1 (Just m) (MetaGT_ l1 m' r1 stk') ->
                push stk' $ metaGT l1 (paren p1 m l ST.YesPin) r1 m'
            OpenPin p1 (Just m) stk'@MetaGT_{} ->
                Just $ Pin p1 m l ST.YesPin stk'
            _ -> Nothing
    -- <}    note that {< is handled by the Pin frame above
    (stk, SdToken (SdCloseMeta LT)) -> do
        case simplify stk of
            PinOpenMetaLT l1 m r1 pin p2 (Just m') stk' ->
                push stk' $ pinMetaLT l1 m r1 pin p2 m' l
            _ -> Nothing

emptyJoiner :: Pos -> Pos -> Bool
emptyJoiner (MkPos x) (MkPos y) = x + 1 == y

-----

simplify :: MatterParse m => Stk m -> Stk m
simplify stk = case simplifyMaybe stk of
    Nothing -> stk
    Just stk' -> stk'

simplifyMaybe :: MatterParse m => Stk m -> Maybe (Stk m)
simplifyMaybe stk = case pop stk of
    Nothing -> Nothing
    Just (m, stk') -> push stk' m

-----

pop :: MatterParse m => Stk m -> Maybe (m, Stk m)
pop = \case
    Empty{} -> Nothing
    Flat fstk stk -> do
        flt <- popFlat fstk
        Just (flt, stk)
    OpenVariant{} -> Nothing
    OpenSequence{} -> Nothing
    SequenceOpenMetaEQ{} -> Nothing
    OpenMetaGT{} -> Nothing
    MetaGT_{} -> Nothing
    OpenParen{} -> Nothing
    OpenPin{} -> Nothing
    Pin{} ->
        -- Note that this should not be simplifyMaybe to ST.Paren! (^ ... )
        -- /must/ be /direclty/ followed by {<, and the logic for {<
        -- above doesn't call 'pop' or 'simplifyMaybe'.
        Nothing
    PinOpenMetaLT{} -> Nothing
    
popFlat :: MatterParse m => Flat x -> Maybe m
popFlat = \case
    IntegerPart l r -> Just $ flat $ ST.Number l r ST.NothingFraction ST.NothingExponent
    FractionPart l2 r2 (IntegerPart l1 r1) -> Just $ flat $ ST.Number l1 r1 (ST.JustFraction l2 r2) ST.NothingExponent
    Suppressor{} -> Nothing
    x@Text{} -> Just $ flat $ ST.Text $ popT ST.NoMoreText x
    SdJoiner{} -> Nothing
    OdJoiner{} -> Nothing
    Escape{} -> Nothing
    MoreSuppressor{} -> Nothing
    x@MoreText{} -> Just $ flat $ ST.Text $ popT ST.NoMoreText x
    x@Bytes{} -> Just $ flat $ popBytes ST.NoMoreBytes x
    BytesJoiner{} -> Nothing
    x@MoreBytes{} -> Just $ flat $ popBytes ST.NoMoreBytes x

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

push :: MatterParse m => Stk m -> m -> Maybe (Stk m)
push = flip $ \m -> \case
    Empty mb ->
        case mb of
            Nothing -> Just $ Empty (Just m)
            Just{} -> Nothing
    Flat{} ->
        Nothing
    OpenVariant l r stk ->
        push stk $ variant l r m
    OpenSequence p acc stk ->
        Just $ OpenSequence p (item acc m) stk
    SequenceOpenMetaEQ p1 acc p2 mb stk ->
        case mb of
            Nothing -> Just $ SequenceOpenMetaEQ p1 acc p2 (Just m) stk
            Just{} -> Nothing
    OpenMetaGT p mb stk ->
        case mb of
            Nothing -> Just $ OpenMetaGT p (Just m) stk
            Just{} -> Nothing
    MetaGT_ l m' r stk ->
      push stk $ metaGT l m' r m
    OpenParen p mb stk ->
        case mb of
            Nothing -> Just $ OpenParen p (Just m) stk
            Just{} -> Nothing
    OpenPin p mb stk ->
        case mb of
            Nothing -> Just $ OpenPin p (Just m) stk
            Just{} -> Nothing
    Pin{} ->
        Nothing
    PinOpenMetaLT l1 m' r1 pin l2 mb stk ->
        case mb of
            Nothing -> Just $ PinOpenMetaLT l1 m' r1 pin l2 (Just m) stk
            Just{} -> Nothing

-----

data SnocsResult m =
    SnocsDone (Stk m) T.Tokenizer
  |
    SnocsStuck (Stk m) Pos Token Pos
  |
    SnocsError Pos Pos T.SnocError

snocs :: (T.MatterStream a, MatterParse m) => Stk m -> a -> SnocsResult m
snocs =
    \stk x -> go stk $ T.snocsTokenizer T.startTokenizer x
  where
    go stk = \case
        T.SnocsToken l tk r k -> case snoc l r stk tk of
            Nothing -> SnocsStuck stk l tk r
            Just stk' -> go stk' k
        T.SnocsDone k -> SnocsDone stk k
        T.SnocsError start cur e -> SnocsError start cur e

eof :: MatterParse m => Stk m -> Maybe m
eof = (. simplify) $ \case
    Empty (Just m) -> Just m
    _ -> Nothing

-----

-- Copied from -ddump-deriv in a throwaway module where I simply
-- removed the type index from Flat.

instance (Show (Matters m), Show m) => Show (Stk m) where
    showsPrec p (Empty x)
      = showParen
          (p >= 11)
          ((.)
             (showString "Empty ")
             (showsPrec 11 x))
    showsPrec
      a_a2kw
      (Flat b1_a2kx b2_a2ky)
      = showParen
          (a_a2kw >= 11)
          ((.)
             (showString "Flat ")
             ((.)
                (showsPrec 11 b1_a2kx)
                ((.)
                   showSpace
                   (showsPrec 11 b2_a2ky))))
    showsPrec
      a_a2kE
      (OpenVariant b1_a2kF b2_a2kG b3_a2kH)
      = showParen
          (a_a2kE >= 11)
          ((.)
             (showString "OpenVariant ")
             ((.)
                (showsPrec 11 b1_a2kF)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2kG)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_a2kH))))))
    showsPrec
      a_a2kI
      (OpenSequence b1_a2kJ b2_a2kK b3_a2kL)
      = showParen
          (a_a2kI >= 11)
          ((.)
             (showString "OpenSequence ")
             ((.)
                (showsPrec 11 b1_a2kJ)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2kK)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_a2kL))))))
    showsPrec
      a_a2kM
      (SequenceOpenMetaEQ b1_a2kN b2_a2kO
                                                    b3_a2kP b4_a2kQ b5_a2kR)
      = showParen
          (a_a2kM >= 11)
          ((.)
             (showString "SequenceOpenMetaEQ ")
             ((.)
                (showsPrec 11 b1_a2kN)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2kO)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3_a2kP)
                            ((.)
                               showSpace
                               ((.)
                                  (showsPrec 11 b4_a2kQ)
                                  ((.)
                                     showSpace
                                     (showsPrec 11 b5_a2kR))))))))))
    showsPrec
      a_a2kS
      (OpenMetaGT b1_a2kT b2_a2kU b3_a2kV)
      = showParen
          (a_a2kS >= 11)
          ((.)
             (showString "OpenMetaGT ")
             ((.)
                (showsPrec 11 b1_a2kT)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2kU)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_a2kV))))))
    showsPrec
      a_a2kW
      (MetaGT_ b1_a2kX b2_a2kY b3_a2kZ b4_a2l0)
      = showParen
          (a_a2kW >= 11)
          ((.)
             (showString "MetaGT_ ")
             ((.)
                (showsPrec 11 b1_a2kX)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2kY)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3_a2kZ)
                            ((.)
                               showSpace
                               (showsPrec 11 b4_a2l0))))))))
    showsPrec
      a_a2l1
      (OpenParen b1_a2l2 b2_a2l3 b3_a2l4)
      = showParen
          (a_a2l1 >= 11)
          ((.)
             (showString "OpenParen ")
             ((.)
                (showsPrec 11 b1_a2l2)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2l3)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_a2l4))))))
    showsPrec
      a_a2l5
      (OpenPin b1_a2l6 b2_a2l7 b3_a2l8)
      = showParen
          (a_a2l5 >= 11)
          ((.)
             (showString "OpenPin ")
             ((.)
                (showsPrec 11 b1_a2l6)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2l7)
                      ((.)
                         showSpace
                         (showsPrec 11 b3_a2l8))))))
    showsPrec
      a_a2l9
      (Pin b1_a2la b2_a2lb b3_a2lc b4_a2ld
                                     b5_a2le)
      = showParen
          (a_a2l9 >= 11)
          ((.)
             (showString "Pin ")
             ((.)
                (showsPrec 11 b1_a2la)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2lb)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3_a2lc)
                            ((.)
                               showSpace
                               ((.)
                                  (showsPrec 11 b4_a2ld)
                                  ((.)
                                     showSpace
                                     (showsPrec 11 b5_a2le))))))))))
    showsPrec
      a_a2lf
      (PinOpenMetaLT b1_a2lg b2_a2lh b3_a2li
                                               b4_a2lj b5_a2lk b6_a2ll b7_a2lm)
      = showParen
          (a_a2lf >= 11)
          ((.)
             (showString "PinOpenMetaLT ")
             ((.)
                (showsPrec 11 b1_a2lg)
                ((.)
                   showSpace
                   ((.)
                      (showsPrec 11 b2_a2lh)
                      ((.)
                         showSpace
                         ((.)
                            (showsPrec 11 b3_a2li)
                            ((.)
                               showSpace
                               ((.)
                                  (showsPrec 11 b4_a2lj)
                                  ((.)
                                     showSpace
                                     ((.)
                                        (showsPrec 11 b5_a2lk)
                                        ((.)
                                           showSpace
                                           ((.)
                                              (showsPrec 11 b6_a2ll)
                                              ((.)
                                                 showSpace
                                                 (showsPrec
                                                    11 b7_a2lm))))))))))))))
  
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
