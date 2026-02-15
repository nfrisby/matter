{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Language.Matter.SyntaxTree.Generator (
    generateMatter,
    shrinkMatter,
  ) where

import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Language.Matter.SyntaxTree
import Language.Matter.Tokenizer.Counting (D10 (..), Four (..), Four' (..), MaybeSign (..), Sign (..))
import System.Random.SplitMix (mkSMGen)
import Math.Combinat.Partitions.Integer qualified as Combinat
import Test.QuickCheck (shrinkList)
import Test.QuickCheck.Gen qualified as QC

frequency :: NonEmpty (Int, QC.Gen a) -> QC.Gen a
frequency =
    QC.frequency . NE.toList

oneof :: NonEmpty (QC.Gen a) -> QC.Gen a
oneof =
    QC.oneof . NE.toList

sizeHalves :: Int -> QC.Gen (Int, Int)
sizeHalves sz = do
    if sz < 3 then pure (1, 1) else do
        l <- QC.choose (1, sz - 1)
        pure (l, sz - l)

sizeParts :: Int -> QC.Gen [Int]
sizeParts sz = do
    g <- fmap mkSMGen $ QC.chooseUpTo maxBound
    QC.shuffle $ Combinat.fromPartition $ fst $ Combinat.randomPartition (max 0 sz) g

-----

-- | Generates a 'Matter' that strictly respects 'QC.sized'
generateMatter :: QC.Gen (Matter Vector (Bytes X) (Number X) (Symbol X) (Text NonEmpty X) P)
generateMatter =
    QC.sized $ \sz ->
        if sz < 1 then pure (Sequence MkP V.empty MkP) else   -- the smallest visual /obvious/ option
        frequency $
            (  ( 5,          generateSequence )            NE.:|)

          $ ([ (20, Flat <$> generateNumber   ) | sz < 4 ] ++)
          $ ([ (20, Flat <$> generateAtom     ) | sz < 2 ] ++)

          $ ([ (10, Flat <$> generateText     ) | sz < 10 ] ++)
          $ ([ ( 3, Flat <$> generateBytes    ) | sz < 10 ] ++)

          $ ([ (10,          generateVariant  ) | 2 < sz ] ++)
          $ ([ ( 1,          generateParen    ) | 2 < sz ] ++)
          $ ([ ( 1,          generateMetaGT   ) | 3 < sz ] ++)
          $ ([ ( 1,          generatePinMetaLT) | 3 < sz ] ++)
          $ []

generateSequence :: QC.Gen (Matter Vector (Bytes X) (Number X) (Symbol X) (Text NonEmpty X) P)
generateSequence =
    QC.sized $ \sz -> do
        szs <- sizeParts $ max 1 sz - 1
        xs <- traverse (flip QC.resize generateSequencePart) $ V.fromList szs
        pure $ Sequence MkP xs MkP

generateSequencePart :: QC.Gen (SequencePart P (Matter Vector (Bytes X) (Number X) (Symbol X) (Text NonEmpty X) P))
generateSequencePart =
    QC.sized $ \sz ->
        frequency $
            (  (10,        Item              <$> QC.resize  sz      generateMatter)            NE.:|)
          $ ([ ( 1, (\x -> MetaEQ MkP x MkP) <$> QC.resize (sz - 2) generateMatter) | 2 < sz ] ++)
          $ []

generateAtom :: QC.Gen (Flat (Bytes X) (Number X) (Symbol X) (Text NonEmpty X) P)
generateAtom =
    pure $ Atom (MkSymbol MkX) MkP MkP

generateNumber :: QC.Gen (Flat (Bytes X) (Number X) (Symbol X) (Text NonEmpty X) P)
generateNumber =
    QC.sized $ \sz ->
        (\mbSgn (fpart, epart) -> Number $ NumberLit MkX mbSgn MkP MkP fpart epart)
    <$> genMbSgn
    <*> case compare sz 2 of
            LT -> pure (NothingFraction, NothingExponent)
            EQ -> oneof $ pure (JustFraction MkP MkP, NothingExponent) NE.:| [(,) NothingFraction <$> expo]
            GT -> (,) (JustFraction MkP MkP) <$> expo
  where
    genMbSgn =
        frequency $ (4, pure $ JustSign NegSign) NE.:| [(5, pure NothingSign), (1, pure $ JustSign PosSign)]

    expo = (\mbSgn -> JustExponent mbSgn MkP MkP) <$> genMbSgn

generateBytes :: QC.Gen (Flat (Bytes X) (Number X) (Symbol X) (Text NonEmpty X) P)
generateBytes =
    QC.sized $ \sz ->
        pure $ Bytes $ BytesLit MkX MkP MkP $ foldr ($) NoMoreBytes $ replicate (max 1 sz - 1) $ MoreBytes MkP . BytesLit MkX MkP MkP

generateText :: QC.Gen (Flat (Bytes X) (Number X) (Symbol X) (Text NonEmpty X) P)
generateText = Text <$> generateText'

generateText' :: QC.Gen (Text NonEmpty X P)
generateText' =
    text
  where
    text =
        QC.sized $ \sz ->
            if sz < 2 then lit NoMoreText else
            frequency
              $  ((1, QC.resize (max 1 sz - 1) moreText >>= \(MkSomeJoiner j, txt) -> lit (MoreText MkP j txt))            NE.:|)
              $ ([(1, QC.resize (      sz - 1) moreText <&> \(MkSomeJoiner j, txt) -> Suppressor MkP MkP j txt) | sz <= 3] ++)
              $ []

    lit more =
        generateQuote <&> \q -> TextLit q MkX MkP MkP more

    moreText =
        QC.sized $ \sz ->
            if sz < 2 then lit NoMoreText <&> \txt -> (MkSomeJoiner (NilJoiner MkP), txt) else do
                jsz <-
                    frequency
                      $  ((90,                         pure 1  )            NE.:|)
                      $ ([( 9, QC.choose ( 2, min (sz - 1)  3) ) |  2 < sz] ++)
                      $ ([( 1, QC.choose (10, min (sz - 1) 20) ) | 10 < sz] ++)
                      $ []
                (,) <$> QC.resize jsz joiner <*> QC.resize (sz - jsz) text

    joiner =
        QC.sized $ \sz ->
            if sz < 2 then pure (MkSomeJoiner $ NilJoiner MkP) else do
            frequency $ (9, MkSomeJoiner <$> joinerText) NE.:| [(1, MkSomeJoiner <$> joinerEscapes)]

    joinerText =
        QC.sized $ \sz ->
            if sz < 2 then pure (NilJoiner MkP) else do
            ConsJoinerText MkX MkP MkP <$> QC.resize (sz - 1) joinerEscapes

    joinerEscapes =
        QC.sized $ \sz ->
            if sz < 2 then pure (NilJoiner MkP) else do
                n <-
                    frequency
                      $  ((90,                        pure 1  )           NE.:|)
                      $ ([( 9,                        pure 2  ) | 2 < sz] ++)
                      $ ([( 1, QC.choose (5, min (sz - 1) 10) ) | 5 < sz] ++)
                      $ []
                escapes <- (NE.:|) <$> escape <*> QC.vectorOf (n - 1) escape
                ConsJoinerEscapes escapes <$> QC.resize (sz - n) joinerText

    escape = MkEscape MkP <$> generateFour

data SomeJoiner = forall j. MkSomeJoiner !(Joiner NonEmpty X P j)

generateQuote :: QC.Gen Quote
generateQuote =
    frequency $ (9, pure DoubleQuote) NE.:| [(1, MultiQuote <$> generateFour'D10)]

generateFour :: QC.Gen Four
generateFour =
    oneof $ fmap pure $ Four1 NE.:| [Four2, Four3, Four4]

generateFour'D10 :: QC.Gen (Four' D10)
generateFour'D10 =
    oneof $ (Four1' <$> d10) NE.:| [Four2' <$> d10 <*> d10, Four3' <$> d10 <*> d10 <*> d10, Four4' <$> d10 <*> d10 <*> d10 <*> d10]
  where
    d10 = oneof $ fmap pure $ D10_0 NE.:| [D10_1,D10_2,D10_3,D10_4,D10_5,D10_6,D10_7,D10_8,D10_9]

generateVariant :: QC.Gen (Matter Vector (Bytes X) (Number X) (Symbol X) (Text NonEmpty X) P)
generateVariant =
    QC.sized $ \sz -> do
        n <- frequency $
            (  (30,                pure 1)            NE.:|)
          $ ([ ( 5,                pure 2) | 2 < sz ] ++)
          $ ([ ( 3,                pure 3) | 3 < sz ] ++)
          $ ([ ( 1, QC.choose (4, sz - 1)) | 4 < sz ] ++)
          $ []
        x <- QC.resize (max n sz - n) generateMatter
        pure $ foldr ($) x $ replicate n $ Variant (MkSymbol MkX) MkP MkP

generateParen :: QC.Gen (Matter Vector (Bytes X) (Number X) (Symbol X) (Text NonEmpty X) P)
generateParen =
    QC.sized $ \sz -> do
        x <- QC.resize (max 1 sz - 1) generateMatter
        pure $ Paren MkP x MkP

generateMetaGT :: QC.Gen (Matter Vector (Bytes X) (Number X) (Symbol X) (Text NonEmpty X) P)
generateMetaGT =
    QC.sized $ \sz -> do
        (l, r) <- sizeHalves $ max 1 sz - 1
        x <- QC.resize l generateMatter
        y <- frequency $
            (  (10  ,      NoClosePin              <$> QC.resize  r      generateMatter)            NE.:|)
          $ ([ ( 1, (\y -> OnlyClosePin MkP y MkP) <$> QC.resize (r - 1) generateMatter) | r <= 2 ] ++)
          $ ([ ( 1, do
                   (rl, rr) <- sizeHalves $ r - 2
                   yx <- QC.resize rl generateMatter
                   yy <- QC.resize rr generateMatter
                   pure $ BothPins MkP yx MkP MkP yy MkP                               ) | r <= 4 ] ++)
          $ []
        pure $ MetaGT MkP x MkP y

generatePinMetaLT :: QC.Gen (Matter Vector (Bytes X) (Number X) (Symbol X) (Text NonEmpty X) P)
generatePinMetaLT =
    QC.sized $ \sz -> do
        (l, r) <- sizeHalves $ max 2 sz - 2
        x <- QC.resize l generateMatter
        y <- QC.resize r generateMatter
        pure $ PinMetaLT MkP x MkP MkP y MkP

-----

-- | TODO very incomplete
shrinkMatter :: Matter Vector (Bytes X) (Number X) (Symbol X) (Text nesequ X) P -> [Matter Vector (Bytes X) (Number X) (Symbol X) (Text nesequ X) P]
shrinkMatter = \case
    Flat flt -> map Flat $ flat flt
    Variant s l r x -> x  : [Variant s l r x' | x' <- shrinkMatter x]
    Sequence l xs r ->
        (case V.toList xs of [Item x] -> (x :); [MetaEQ _l x _r] -> (x :); _ -> id)
      $ [ Sequence l (V.fromList xs') r
        | xs' <- shrinkList sequencePart (V.toList xs)
        ]
    MetaGT _l x _r y -> x : case y of
        NoClosePin y1 -> [y1]
        OnlyClosePin _l1 y1 _r1 -> [y1]
        BothPins _l1 y1 _r1 _l2 y2 _r2 -> [y1, y2]
    Paren l x r -> x : [Paren l x' r | x' <- shrinkMatter x]
    PinMetaLT l1 x r1 l2 y r2 ->
        x : y : [ PinMetaLT l1 x' r1 l2 y r2 | x' <- shrinkMatter x ] ++ [ PinMetaLT l1 x r1 l2 y' r2 | y' <- shrinkMatter y ]
  where
    flat = \case
        Atom{} -> []
        Bytes (BytesLit _blit _l _r NoMoreBytes) -> []
        Bytes (BytesLit _blit _l _r (MoreBytes _p (BytesLit _blit2 l r more))) -> [Bytes $ BytesLit MkX l r more]
        Number (NumberLit _nlit mbSign l r fpart epart) -> map Number $ case (fpart, epart) of
            (NothingFraction, NothingExponent) -> []
            (JustFraction{}, NothingExponent) -> [NumberLit MkX mbSign l r NothingFraction epart]
            (NothingFraction, JustExponent{}) -> [NumberLit MkX mbSign l r fpart NothingExponent]
            (JustFraction{}, JustExponent{}) -> [NumberLit MkX mbSign l r fpart NothingExponent, NumberLit MkX mbSign l r NothingFraction epart]
        Text{} -> []

    sequencePart = \case
        Item x -> map Item $ shrinkMatter x
        MetaEQ l x r -> [MetaEQ l x' r | x' <- shrinkMatter x]
