{-# LANGUAGE ImportQualifiedPost #-}

module Language.Matter.SyntaxTree.Generator (module Language.Matter.SyntaxTree.Generator) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Language.Matter.SyntaxTree
import System.Random.SplitMix (mkSMGen)
import Math.Combinat.Partitions.Integer qualified as Combinat
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
generateMatter :: QC.Gen (Matter P NonEmpty [])
generateMatter =
    QC.sized $ \sz ->
        if sz < 1 then pure (Sequence MkP [] MkP) else   -- the smallest visual /obvious/ option
        frequency $
            (  ( 5,          generateSequence )            NE.:|)

          $ ([ (20, Flat <$> generateNumber   ) | sz < 4 ] ++)
          $ ([ (20, Flat <$> generateAtom     ) | sz < 2 ] ++)

          $ ([ ( 3, Flat <$> generateBytes    ) | sz < 10 ] ++)

          -- TODO generateText :grimace:

          $ ([ (10,          generateVariant  ) | 2 < sz ] ++)
          $ ([ ( 1,          generateParen    ) | 2 < sz ] ++)
          $ ([ ( 1,          generateMetaGT   ) | 3 < sz ] ++)
          $ ([ ( 1,          generatePinMetaLT) | 3 < sz ] ++)
          $ []

generateSequence :: QC.Gen (Matter P NonEmpty [])
generateSequence =
    QC.sized $ \sz -> do
        szs <- sizeParts $ max 1 sz - 1
        xs <- mapM (flip QC.resize generateSequencePart) szs
        pure $ Sequence MkP xs MkP

generateSequencePart :: QC.Gen (SequencePart P (Matter P NonEmpty []))
generateSequencePart =
    QC.sized $ \sz ->
        frequency $
            (  (10,        Item              <$> QC.resize  sz      generateMatter)            NE.:|)
          $ ([ ( 1, (\x -> MetaEQ MkP x MkP) <$> QC.resize (sz - 2) generateMatter) | 2 < sz ] ++)
          $ []

generateAtom :: QC.Gen (Flat P NonEmpty)
generateAtom =
    pure $ Atom MkP MkP

generateNumber :: QC.Gen (Flat P NonEmpty)
generateNumber =
    QC.sized $ \sz ->
        fmap (uncurry (Number MkP MkP))
      $ case compare sz 2 of
            LT -> pure (NothingFraction, NothingExponent)
            EQ -> oneof $ pure (JustFraction MkP MkP, NothingExponent) NE.:| [pure (NothingFraction, JustExponent MkP MkP)]
            GT -> pure (JustFraction MkP MkP, JustExponent MkP MkP)

generateBytes :: QC.Gen (Flat P NonEmpty)
generateBytes =
    QC.sized $ \sz ->
        pure $ Bytes MkP MkP $ foldr ($) NoMoreBytes $ replicate (max 1 sz - 1) $ MoreBytes MkP MkP MkP

generateVariant :: QC.Gen (Matter P NonEmpty [])
generateVariant =
    QC.sized $ \sz -> do
        n <- frequency $
            (  (30,                pure 1)            NE.:|)
          $ ([ ( 5,                pure 2) | 2 < sz ] ++)
          $ ([ ( 3,                pure 3) | 3 < sz ] ++)
          $ ([ ( 1, QC.choose (4, sz - 1)) | 4 < sz ] ++)
          $ []
        x <- QC.resize (max n sz - n) generateMatter
        pure $ foldr ($) x $ replicate n $ Variant MkP MkP

generateParen :: QC.Gen (Matter P NonEmpty [])
generateParen =
    QC.sized $ \sz -> do
        x <- QC.resize (max 1 sz - 1) generateMatter
        pure $ Paren MkP x MkP

generateMetaGT :: QC.Gen (Matter P NonEmpty [])
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

generatePinMetaLT :: QC.Gen (Matter P NonEmpty [])
generatePinMetaLT =
    QC.sized $ \sz -> do
        (l, r) <- sizeHalves $ max 2 sz - 2
        x <- QC.resize l generateMatter
        y <- QC.resize r generateMatter
        pure $ PinMetaLT MkP x MkP MkP y MkP
