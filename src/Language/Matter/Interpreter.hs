{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Matter.Interpreter (
    -- * Symbol
    SymbolValue,   -- opaque
    interpretSymbol,
    shortTextSymbolValue,
    symbolValueText,
    textSymbolValue,

    -- * Bytes
    interpretBytes,
    interpretBytesLit,

    -- * Paths
    Path (EmptyPath, SnocPath),
    Turn (..),
    TurnX (..),
    pathLength,
    pathMap,
    pathedFmap,
    pathedFold,
  ) where

import Control.Monad.ST (runST)
import Control.Monad.Trans.State qualified as State
import Data.Bits ((.|.), (.>>.))
import Data.Function (on)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Primitive.ByteArray qualified as BA
import Data.Text qualified as T
import Data.Text.Internal qualified as TI
import Data.Text.Short qualified as TS
import Data.Word (Word8, Word32)
import GHC.Exts qualified as GHC
import GHC.Word qualified as GHC

import Language.Matter.SyntaxTree
import Language.Matter.Tokenizer (MatterStream (slice), Pos (..))

data SymbolValue =
      -- | Preferable when creating them programmatically
      ShortTextSV !TS.ShortText
    |
      -- | Preferable when slicing them from a source file
      TextSV !T.Text

shortTextSymbolValue :: TS.ShortText -> SymbolValue
shortTextSymbolValue = ShortTextSV

textSymbolValue :: T.Text -> SymbolValue
textSymbolValue = TextSV

symbolValueText :: SymbolValue -> T.Text
{-# INLINE symbolValueText #-}
symbolValueText = \case
    ShortTextSV t -> TS.toText t
    TextSV t -> t

instance Eq SymbolValue where (==) = (==) `on` symbolValueText
instance Ord SymbolValue where compare = compare `on` symbolValueText
instance Show SymbolValue where
    showsPrec p = \case
        ShortTextSV t -> showsPrec p t
        TextSV t -> showsPrec p t

-- | Useful both for 'Atom' and 'Varinant'
interpretSymbol :: MatterStream inp => inp -> Symbol Pos -> SymbolValue
interpretSymbol inp (MkSymbol l r) =
    -- skip the @ or the #
    TextSV $ slice (l <> MkPos 1 1) r inp

----

interpretBytes :: MatterStream inp => inp -> Maybe Word32 -> Bytes blit Pos -> Either String BA.ByteArray
interpretBytes inp mbByteSize top =
    runST $ do
        marr <- BA.newByteArray (fromIntegral n)
        let go !i = \case
                BytesLit _blit l r more -> do
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
    n = case mbByteSize of
        Just byteSize -> byteSize
        Nothing ->
            let count !acc = \case
                    BytesLit _blit l r more ->
                        count2 (acc + codePoints r - codePoints l - 2) more   -- skip 0x
                count2 !acc = \case
                    NoMoreBytes -> acc
                    MoreBytes _p b -> count acc b
            in
            div (count 0 top) 2   -- {- 2 nibbles per byte -}

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
    GHC.inline interpretBytes inp Nothing $ BytesLit MkX l r NoMoreBytes

-- TODO interpretNumber

-- TODO interpretText

-- TODO interpretTextLit

-----

-- | The sequence of turns that reached some subtree of a 'Matter'
data Path =
    EmptyPath
  |
    RawSnocPath !Path !(Map TurnX Path) !Turn
  deriving (Eq, Show)

-- | Hides the cached value of 'pathMap'
pattern SnocPath :: Path -> Turn -> Path
pattern SnocPath path turn <-
    RawSnocPath path _m turn
  where
    SnocPath path turn = RawSnocPath path (pathMap path) turn

{-# COMPLETE EmptyPath, SnocPath #-}

-- | Each possible turn when descending into a 'Matter' value
data Turn =
    -- | #foo A
    VariantTurn {-# UNPACK #-} !SymbolValue
  |
    -- | [ ... A ... ], with a count of predecessors (aka index)
    ItemTurn !Word32 !Word32
  |
    -- | {= A =}, with a count of predecessors (aka index)
    MetaEqTurn !Word32 !Word32
  |
    -- | ( A )
    --
    -- TODO is this turn useless?
    ParenTurn
  |
    -- | The A of @{> A >} B@
    MetaGtTurn
  |
    -- | The B of @{> A >} B@
    ReferentGtTurn
  |
    -- | The A of @(^ A ) {< B <}@
    ReferentLtTurn
  |
    -- | The B of @(^ A ) {< B <}@
    MetaLtTurn
  deriving (Eq, Ord, Show)

-- | Either a specific 'Turn', or a whole family of 'Turns'
data TurnX =
    TurnX !Turn
  |
    VariantTurnX
  |
    ItemTurnX
  |
    MetaTurnX
  |
    ReferentTurnX
  deriving (Eq, Ord, Show)

turnX :: Turn -> Maybe TurnX
{-# INLINE turnX #-}
turnX = \case
    VariantTurn{} -> Just VariantTurnX
    ItemTurn{} -> Just ItemTurnX
    MetaEqTurn{} -> Just MetaTurnX
    ParenTurn -> Nothing
    MetaGtTurn -> Just MetaTurnX
    ReferentGtTurn -> Just ReferentTurnX
    ReferentLtTurn -> Just ReferentTurnX
    MetaLtTurn -> Just MetaTurnX

-- | The deepest predecessor 'Path' that matches the given 'TurnX'
pathMap :: Path -> Map TurnX Path
pathMap = \case
    EmptyPath ->
        Map.empty
    RawSnocPath path m turn ->
        Map.insert (TurnX turn) path
      $ maybe id (flip Map.insert path) (turnX turn)
      $ m

-- | The number of turns in the path
--
-- Equal to the number of 'SnocPath' constructors.
--
-- It is potentially /greater/ than the number of data constructors of
-- type 'Matter', since an occurrence of 'BothPins' also adds its own
-- 'SnocPath' constructor.
pathLength :: Path -> Int
pathLength = Map.size . pathMap

-- | Like 'fmap' but also applies the appropriate 'Turn's to the 'Path'
--
-- 'SnocPath' will hav been applied to each position. Note that the
-- paths to the 'BothPins' arguments involve two turns, so 'SnocPath'
-- has been called twice for each.
pathedFmap ::
    Traversable sequ
 =>
    (s pos -> SymbolValue)
 ->
    Path
 ->
    (Path -> a -> a')
 ->
    MatterF sequ b n s t pos a
 ->
    MatterF sequ b n s t pos a'
{-# INLINE pathedFmap #-}
pathedFmap symbol path f = \case
    FlatF flt -> FlatF flt
    VariantF s x -> VariantF s $ flip f x $ SnocPath path $ VariantTurn $ symbol s
    SequenceF p1 xs p2 -> SequenceF p1 (go xs) p2
    MetaGtF p1 x p2 y ->
        MetaGtF p1 (flip f x $ SnocPath path MetaGtTurn) p2 $ case y of
            NoClosePin y' ->
                NoClosePin (flip f y' $ SnocPath path ReferentGtTurn)
            OnlyClosePin p3 y' p4 ->
                OnlyClosePin p3 (flip f y' $ SnocPath path ReferentGtTurn) p4
            BothPins p3 y1 p4 p5 y2 p6->
                BothPins
                   p3 (flip f y1 $ SnocPath path ReferentGtTurn `SnocPath` ReferentLtTurn) p4
                   p5 (flip f y2 $ SnocPath path ReferentGtTurn `SnocPath` MetaLtTurn    ) p6
    ParenF p1 x p2 -> ParenF p1 (flip f x $ SnocPath path ParenTurn) p2
    PinMetaLtF p1 x p2 p3 y p4 ->
        PinMetaLtF
           p1 (flip f x $ SnocPath path ReferentLtTurn) p2
           p3 (flip f y $ SnocPath path MetaLtTurn    ) p4
  where
    go xs = traverse sequencePart xs `State.evalState` MkW32W32 0 0
    sequencePart part = case part of
        Item x -> (\e -> Item (f e x)) <$> item
        MetaEQ l x r -> (\e -> MetaEQ l (f e x) r) <$> metaEQ
    item = State.state $ \(MkW32W32 nitem nmeta) -> (path `SnocPath` ItemTurn nitem nmeta, MkW32W32 (nitem + 1) nmeta)
    metaEQ = State.state $ \(MkW32W32 nitem nmeta) -> (path `SnocPath` MetaEqTurn nitem nmeta, MkW32W32 nitem (nmeta + 1))

-- | NOT EXPORTED
data W32W32 = MkW32W32 !Word32 !Word32

-- | Like 'fold' but also applies the appropriate 'Turn's to the 'Path'
pathedFold ::
    Traversable sequ
 =>
    (s pos -> SymbolValue)
 ->
    (Path -> MatterF sequ b n s t pos a -> a)
 ->
    Path
 ->
    Matter sequ b n s t pos
 ->
    a
{-# INLINE pathedFold #-}
pathedFold slit phi =
    go
  where
    go !path = phi path . pathedFmap slit path go . project
