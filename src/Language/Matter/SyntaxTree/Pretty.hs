{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

module Language.Matter.SyntaxTree.Pretty (module Language.Matter.SyntaxTree.Pretty) where

import Data.DList (DList)
import Data.DList qualified as DList
import Data.DList.DNonEmpty (DNonEmpty)
import Data.DList.DNonEmpty qualified as DNE
import Language.Matter.SyntaxTree
import Language.Matter.Tokenizer (OdToken (..), SdToken (..), Token (..))

od :: OdToken -> DNonEmpty Token
od = DNE.singleton . OdToken

sd :: SdToken -> DNonEmpty Token
sd = DNE.singleton . SdToken

sd' :: SdToken -> DList Token
sd' = DList.singleton . SdToken

od' :: OdToken -> DList Token
od' = DList.singleton . OdToken

-----

toDList :: DNonEmpty a -> DList a
toDList x = DList.singleton (DNE.head x) <> DNE.tail x

appendDList :: DNonEmpty a -> DList a -> DNonEmpty a
appendDList x y = DNE.head x DNE.:| (DNE.tail x <> y)

consDList :: a -> DList a -> DNonEmpty a
consDList x y = x DNE.:| y

-----

pretty :: (Functor seq, Foldable seq, Foldable neseq) => Matter anno pos neseq seq -> DNonEmpty Token
pretty =
    fold phi
  where
    phi = \case
        FlatF flt -> prettyFlat flt
        VariantF _anno _l _r x -> od OdVariant <> x
        SequenceF _anno _l xs _r -> sd SdOpenSeq <> foldr (\x acc -> prettySequencePart x <> acc) (sd SdCloseSeq) xs
        MetaGtF _l x _r y ->
            let y' = case y of
                    NoClosePin y1 -> y1
                    OnlyClosePin _l y1 _r -> od OdOpenParen <> y1 <> sd SdClosePin
                    BothPins _l1 y1 _r1 _l2 y2 _r2 -> sd SdOpenPin <> y1 <> sd SdClosePin <> sd (SdOpenMeta LT) <> y2 <> sd (SdCloseMeta LT)
            in
            sd (SdOpenMeta GT) <> x <> sd (SdCloseMeta GT) <> y'
        ParenF _l x _r -> od OdOpenParen <> x <> sd SdCloseParen
        PinMetaLtF _l1 x _r1 _l2 y _r2 ->
            sd SdOpenPin <> x <> sd SdCloseParen <> sd (SdOpenMeta LT) <> y <> sd (SdCloseMeta LT)

prettySequencePart :: SequencePart pos (DNonEmpty Token) -> DNonEmpty Token
prettySequencePart = \case
    Item x -> x
    MetaEQ _l x _r -> sd (SdOpenMeta EQ) <> x <> sd (SdCloseMeta EQ)

prettyFlat :: Foldable neseq => Flat anno pos neseq -> DNonEmpty Token
prettyFlat = \case
    Atom _anno _l _r -> od OdAtom
    Bytes _anno (BytesLit _l _r more) -> OdToken OdBytes `consDList` prettyMoreBytes more
    Number _anno (NumberLit mbSign _l _r fractionPart exponentPart) ->
        let x = case fractionPart of
                NothingFraction -> mempty
                JustFraction _l _r -> od' OdFractionPart
            y = case exponentPart of
                NothingExponent -> mempty
                JustExponent mbSign' _l _r -> od' (OdExponentPart mbSign')
        in
        OdToken (OdIntegerPart mbSign) `consDList` (x <> y)
    Text _anno txt -> prettyText txt

prettyMoreBytes :: MoreBytes pos -> DList Token
prettyMoreBytes = \case
    NoMoreBytes -> mempty
    MoreBytes _p (BytesLit _l _r more) -> od' OdOpenJoiner <> sd' SdCloseJoiner <> od' OdBytes <> prettyMoreBytes more

prettyText :: Foldable neseq => Text pos neseq -> DNonEmpty Token
prettyText = \case
    Suppressor _p _p' j txt -> sd SdUnderscore <> od OdOpenJoiner <> prettyJoiner j <> prettyText txt
    TextLit q _l _r more -> prettyQuote q `appendDList` prettyMoreText more

prettyJoiner :: Foldable neseq => Joiner pos neseq j -> DNonEmpty Token
prettyJoiner = \case
    NilJoiner _p -> sd SdCloseJoiner
    ConsJoinerText _l _r j -> od OdJoinerText <> prettyJoiner j
    ConsJoinerEscapes escapes j -> foldr (\e acc -> prettyEscape e <> acc) (prettyJoiner j) escapes

prettyEscape :: Escape pos -> DNonEmpty Token
prettyEscape (MkEscape _p sz) = sd (SdJoinerEscapedUtf8 sz)

prettyQuote :: Quote -> DNonEmpty Token
prettyQuote = \case
    DoubleQuote -> sd SdDoubleQuotedString
    MultiQuote sz -> sd $ SdMultiQuotedString sz

prettyMoreText :: Foldable neseq => MoreText pos neseq -> DList Token
prettyMoreText = \case
    NoMoreText -> mempty
    MoreText _p j txt -> toDList $ od OdOpenJoiner <> prettyJoiner j <> prettyText txt
