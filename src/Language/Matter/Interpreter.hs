{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}

module Language.Matter.Interpreter (module Language.Matter.Interpreter) where

import Control.Monad.ST (runST)
import Data.Bits ((.|.), (.>>.))
import Data.Primitive.ByteArray qualified as BA
import Data.Text.Internal qualified as TI
import Data.Text.Short qualified as TS
import Data.Word (Word8)
import GHC.Exts qualified as GHC
import GHC.Word qualified as GHC

import Language.Matter.Parser (Anno, bytesAnnoSize)
import Language.Matter.SyntaxTree
import Language.Matter.Tokenizer (MatterStream (slice, sliceShort), Pos (..))

-- | Useful both for 'Atom' and 'Variant'
interpretSymbol :: MatterStream inp => inp -> Pos -> Pos -> TS.ShortText
interpretSymbol inp x y =
    -- skip the @ or the #
    sliceShort (x <> MkPos 1 1) y inp

interpretBytes :: MatterStream inp => inp -> Maybe (BytesAnno Anno) -> Bytes Pos -> Either String BA.ByteArray
interpretBytes inp mbAnno top =
    runST $ do
        marr <- BA.newByteArray (fromIntegral n)
        let go !i = \case
                BytesLit l r more -> do
                    let txt@(TI.Text arr off len) = slice l r inp
                    mx <- go2 i arr (off + 2) 0 0 (len - 2)   -- skip 0x
                    if mx == maxBound then pure (Just (l, r, txt)) else do
                        case more of
                            NoMoreBytes -> pure Nothing
                            MoreBytes _p bytes -> go i bytes
            go2 !i !arr !base !acc !j !m
              | fromIntegral i >= n = pure maxBound
              | j >= m = pure acc
              | otherwise = do
                let b1 = aix hi $ BA.indexByteArray arr $ base     + j
                    b2 = aix lo $ BA.indexByteArray arr $ base + 1 + j
                BA.writeByteArray marr (i + (j .>>. 1)) (b1 .|. b2)
                go2 i arr base (acc `max` b1 `max` b2) (j + 2) m
        go 0 top >>= \case
            Just (l, r, txt) -> pure $ Left $ "Bad bytes literal " ++ show (l, r, txt)
            Nothing -> Right <$> BA.unsafeFreezeByteArray marr
                -- TODO why isn't there a @createByteArrayMaybe :: Int -> (forall s. MutableByteArray s -> ST s (Maybe e)) -> Either e ByteArray@?
  where
    n = case mbAnno of
        Just anno -> bytesAnnoSize anno
        Nothing ->
            let count !acc = \case
                    BytesLit l r more ->
                        count2 (acc + codePoints r - codePoints l - 2) more
                count2 !acc = \case
                    NoMoreBytes -> acc
                    MoreBytes _p b -> count acc b
            in
            count 0 top

    -- copied from base16-bytestring-1.0.2.0:Data.ByteString.Base16.Internal
    !lo = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\xff\xff\xff\xff\xff\xff\xff\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

    !hi = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x10\x20\x30\x40\x50\x60\x70\x80\x90\xff\xff\xff\xff\xff\xff\xff\xa0\xb0\xc0\xd0\xe0\xf0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xa0\xb0\xc0\xd0\xe0\xf0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

    aix :: GHC.Addr# -> Word8 -> Word8
    {-# INLINE aix #-}
    aix table w =
        GHC.W8# (GHC.indexWord8OffAddr# table i)
      where
        !(GHC.I# i) = fromIntegral w

interpretBytesLit :: MatterStream inp => inp -> Pos -> Pos -> Either String BA.ByteArray
interpretBytesLit inp l r =
    GHC.inline interpretBytes inp Nothing $ BytesLit l r NoMoreBytes

-- TODO interpretNumber

-- TODO interpretText

-- TODO interpretTextLit
