{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Matter.Interpreter (
    -- * Symbol
    SymbolValue,   -- opaque
    interpretSymbol,
    shortTextSymbolValue,
    symbolValueText,
    textSymbolValue,

    -- * Bytes
    BadBytes (..),
    interpretBytes,
    interpretBytesLit,

    -- * Decimal
    BadDecimal (..),
    InterpretDecimal (..),
    interpretDecimalAsText,
    unsafeInterpretDecimal,

    -- * Text
    interpretText,
    interpretTextLit,

    -- * Paths
    Path (EmptyPath, SnocPath),
    Turn (..),
    TurnX (..),
    pathLength,
    pathMap,
    pathedFmap,
    pathedFold,

    -- * Utilities
    shiftInteger,

  ) where

import Control.Monad (when)
import Control.Monad.ST (runST)
import Control.Monad.Trans.Except (Except, throwE)
import Control.Monad.Trans.State qualified as State
import Data.Bits ((.|.), (.>>.))
import Data.ByteString.Base16 qualified as B16 (decode)
import Data.Double.Conversion.Text (toShortest)
import Data.Function (on)
import Data.Integer.Conversion (textToInteger)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Primitive.ByteArray qualified as BA
import Data.Text qualified as T
import Data.Text.Encoding qualified as T (decodeUtf8', encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Internal qualified as TI
import Data.Text.Read qualified as TR
import Data.Text.Short qualified as TS
import Data.Word (Word8, Word32)
import GHC.Exts qualified as GHC
import GHC.Word qualified as GHC

import Language.Matter.SyntaxTree
import Language.Matter.Tokenizer (MatterStream (slice), MaybeSign (..), Pos (..), Sign (..))
import Language.Matter.Tokenizer.Counting (forgetFour', valueFour)

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

data BadBytes =
      MkBadBytes !Pos !Pos
  deriving (Eq, Show)

interpretBytes :: MatterStream inp => inp -> Maybe Word32 -> Bytes blit Pos -> Except BadBytes BA.ByteArray
interpretBytes inp mbByteSize top =
    runST $ do
        marr <- BA.newByteArray (fromIntegral n)
        let go !i = \case
                BytesLit _blit l r more -> do
                    let TI.Text arr off len = slice l r inp
                    mx <- go2 i arr (off + 2) 0 0 (len - 2)   -- skip 0x
                    if mx == maxBound then pure $ Just $ MkBadBytes l r else do
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
            Just exn -> pure $ throwE exn
            Nothing -> pure <$> BA.unsafeFreezeByteArray marr
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

interpretBytesLit :: MatterStream inp => inp -> Pos -> Pos -> Except BadBytes BA.ByteArray
interpretBytesLit inp l r =
    GHC.inline interpretBytes inp Nothing $ BytesLit MkX l r NoMoreBytes

-----

interpretDecimalAsText :: MatterStream inp => inp -> Decimal Pos -> T.Text
interpretDecimalAsText inp (DecimalLit _mbSign l wr fpart epart) =
    slice l r inp
  where
    r = case epart of
        JustExponent _mbSign' er -> er
        NothingExponent -> case fpart of
            JustFraction fr -> fr
            NothingFraction -> wr

-- | ASSUMPTION: The 'Pos' values in the 'Decimal' are correct for
-- @inp@.
unsafeInterpretDecimal :: (MatterStream inp, InterpretDecimal a) => inp -> Decimal Pos -> Except BadDecimal a
unsafeInterpretDecimal inp decimal@(DecimalLit _mbSign _l wr fpart epart) =
    let DecimalLit mbSign l r _fpart _epart = decimal
        wdigitsWithTrailingZeros = T.dropWhile (== '0') $ slice (l <> signWidth mbSign) r inp
        wdigits = T.dropWhileEnd (== '0') wdigitsWithTrailingZeros
        wshift = T.length wdigitsWithTrailingZeros - T.length wdigits
    in
    unsafeInterpretDecimalParts
        (interpretDecimalAsText inp decimal)
        mbSign wdigits wshift
        fdigits
        esign edigits
  where
    signWidth :: MaybeSign -> Pos
    signWidth = \case
        NothingSign -> MkPos 0 0
        JustSign{} -> MkPos 1 1

    (fr, fdigits) = case fpart of
        NothingFraction -> (wr, T.empty)
        JustFraction r ->
            (,) r
          $ T.dropWhileEnd (== '0')
          $ slice (wr <> MkPos 1 1) r inp   -- skip .

    (esign, edigits) = case epart of
        NothingExponent -> (NothingSign, T.empty)
        JustExponent mbSign r ->
           (
             mbSign
           ,
             T.dropWhile (== '0') $ slice (fr <> MkPos 1 1 <> signWidth mbSign) r inp
           )   -- skip E and sign

data BadDecimal =
    NotIntegral
  |
    OutOfRange
  |
    -- | This decimal (first argument) has the same closest 'Double'
    -- (second argument) and has fewer significant figures (ie
    -- ignoring leading zeros and also ignoring trailing zeroes in the
    -- case of E-notation)
    WasNotShortest !T.Text !Double
  deriving (Eq, Show)

class InterpretDecimal a where
    -- | The entire decimal, the whole parts' sign and digits without
    -- leading zeros and without trailing zeros, how many trailing
    -- zeros the whole parts had, fraction part's digits without
    -- trailing zeros, and exponent part's sign and digits.
    unsafeInterpretDecimalParts ::
        T.Text
     ->
        MaybeSign -> T.Text -> Int
     ->
        T.Text
     ->
        MaybeSign -> T.Text
     ->
        Except BadDecimal a

applySign :: Num a => MaybeSign -> a -> a
applySign = \case
    NothingSign -> id
    JustSign NegSign -> negate
    JustSign PosSign -> id

getInteger :: MaybeSign -> T.Text -> Integer
getInteger mbSign txt =
    applySign mbSign $ textToInteger txt

-- | If this shift is too negative, this will silently discard the
-- least significant digits.
--
-- Use 'integralChecks' to avoid that.
shiftInteger :: Integer -> Integer -> Integer
shiftInteger x e = case compare e 0 of
    LT -> div x (10 ^ negate e)
    EQ -> x
    GT -> x * 10 ^ e

-- | Short-circuits if the decimal is 0 (regardless of exponent) or if
-- it's a fraction.
--
-- TODO the exponent type should really be limited to Int
integralChecks :: Num a => T.Text -> Int -> T.Text -> Integer -> Maybe (Except BadDecimal a)
integralChecks wdigits wshift fdigits e
  | T.null wdigits, T.null fdigits = Just $ pure 0
  | not (T.null fdigits) && fromIntegral (T.length fdigits) > e = Just $ throwE NotIntegral
  | fromIntegral (T.length wdigits + wshift) <= negate e = Just $ throwE NotIntegral
  | otherwise = Nothing

-- | ASSUMPTION: 'integralChecks' etc
veryUnsafeInteger ::
    MaybeSign -> T.Text -> Int
 ->
    T.Text
 ->
    MaybeSign -> T.Text
 ->
    Integer
veryUnsafeInteger wsign wdigits wshift fdigits esign edigits =
    (getInteger wsign wdigits `shiftInteger` (e + fromIntegral wshift))
  +
    (getInteger NothingSign fdigits `shiftInteger` (e - fcount))
  where
    e = getInteger esign edigits
    fcount = fromIntegral $ T.length fdigits

instance InterpretDecimal Integer where
    unsafeInterpretDecimalParts _entire wsign wdigits wshift fdigits esign edigits
      | Just m <- integralChecks wdigits wshift fdigits e = m
      | otherwise =
        pure $ veryUnsafeInteger wsign wdigits wshift fdigits esign edigits
      where
        e = getInteger esign edigits

instance InterpretDecimal Int where
    unsafeInterpretDecimalParts _entire wsign wdigits wshift fdigits esign edigits
      | Just m <- integralChecks wdigits wshift fdigits e = m
      | 19 < T.length wdigits + wshift + T.length fdigits = throwE OutOfRange   -- minBound and maxBound have 19 digits
      | otherwise = do
          -- TODO optimize
          let i = veryUnsafeInteger wsign wdigits wshift fdigits esign edigits
          when (i < toInteger (minBound :: Int)) $ throwE OutOfRange
          when (i > toInteger (maxBound :: Int)) $ throwE OutOfRange
          pure $ fromInteger i
      where
        e = getInteger esign edigits

-- | There are many pitfalls when failing to distinguish between
-- decimal fractions and IEEE 754 binary floating point numbers. This
-- instance takes a mildly confusing approach to avoid such surprises.
--
-- Specifically, it fails with 'WasNotShortest' whenever there exists
-- a shorter decimal (ie fewer significant digits, ie ignoring leading
-- and trailing zeros) that would yield the same 'Double'. Note that
-- over 40 years after IEEE 754 was first established, it is still
-- quite rare for standard libraries to render floating points as the
-- "shortest" decimal! So this is likely going to cause unpleasant
-- friction. However, it seems worse to interpret a Matter decimal as
-- a 'Double' that would not be rendered as that same decimal by the
-- strict interpretation of the intuitive requirements that have
-- motivated every non-trivial binary-to-decimal floating point
-- rendering algorithm.
--
-- See algorithms such as Errol and Ryu for mapping a 'Double' to the
-- actual ideal decimal. Grisu3 is also an option, if configured to
-- use a falls back algorithm when it detects its output is not
-- actually the shortest.
--
-- This implementation invokes the @double-conversion@ package's
-- bindings to <https://github.com/google/double-conversion>, which
-- seems to be maintained by Florian Loitsch, the author of the Grisu
-- family. I've seen elsewhere that Grisu3 is not itself fully
-- correct, but however can be configured to detect when it's
-- incorrect and fall back to a slower, correct algorithm (eg
-- Dragon4). I'm unsure if the @double-conversion@ Haskell package is
-- indeed configured that way!
--
-- One very appealing option is this PR
-- <https://github.com/haskell/bytestring/pull/365>, in which Lawrence
-- Wu implements Ryu in Haskell (!), but then---unfortunately for my
-- priorities---fudges the output to match the non-Ryu behaviors of
-- the @base@ package's 'show' at type 'Double'. But the code in that
-- PR nicely explains how to remove that fudge /and/ is BSD3, so I
-- will likely eventually copy it into this package and get a nice Ryu
-- implemenation that way.
instance InterpretDecimal Double where
    unsafeInterpretDecimalParts entire _wsign wdigits wshift fdigits _esign _edigits =
        -- TODO check number of sigfigs first, to ensure the String is
        -- small

        -- Surprise! Both 'TR.rational' and the @readExponentialLimit@
        -- function in @bytestring-lexing@ lose precision if the
        -- mantissa has trailing zeros. Even if the mantissa is a
        -- whole number.
        --
        -- Try 0.1000700 vs 0.10007. Or 3000e-60 vs 3e-57. Hell, 3e-57
        -- doesn't end up as 3e-57, even without trailing zeros!
        --
        -- To avoid this, we parse into 'Rational' and them map to
        -- 'Double'. If I had more mastery over the 'Fractional'
        -- class, perhaps this /surprise/ would have been obvious from
        -- the type signatures :thinking-face:.
        --
        -- TODO would going through 'Scientific' somehow be
        -- preferable?
        --
        -- TODO surely there's a package that provides the
        -- well-optimized /exact/ parsing function, right?
        case (\(x, txt) -> (fromRational x, txt)) <$> TR.rational entire of
          Left s -> error $ "Data.Text.Read.decimal @Fractional " <> s
          Right (dbl, leftovers)
            | not (T.null leftovers) ->
                  error $ "Data.Text.Read.rational did not consume whole decimal "
                    <> T.unpack leftovers <> " from " <> T.unpack entire
            | (1/0) == dbl -> throwE OutOfRange
            | (-1/0) == dbl -> throwE OutOfRange
            | Just x <- check dbl ->
                  throwE x
            | otherwise ->
                  pure dbl
      where
        -- TODO optimize this
        --
        -- Ryu, eg, computes (mantissa, exponent) as an integer pair,
        -- which I could then probably much more efficiently compare
        -- to @wdigits <> fdigits@?
        check dbl =
            let shortest = toShortest $ abs dbl

                wf = T.takeWhile (/= 'e') shortest
                w  = T.takeWhile (/= '.') wf
                f  = T.drop (T.length w + 1) wf

                sameSigFigs = (==) `on` T.dropAround (== '0')

                given = wdigits <> T.replicate wshift (T.pack "0") <> fdigits
            in
            if given `sameSigFigs` (w <> f) then Nothing else
            Just $ WasNotShortest shortest dbl

-- TODO instance InterpretDecimal Scientific, which would only
-- necessarily lose leading zeros in whole and exponent
--
-- TODO etc?

-----

interpretText :: forall inp nesequ tlit.
    (MatterStream inp, Foldable nesequ)
 =>
    inp -> Maybe Word32 -> Text nesequ tlit Pos -> TL.Text
interpretText inp mbSz =
    f . text
  where
    f = case mbSz of
        Just sz -> TB.toLazyTextWith (fromIntegral sz)
        Nothing -> TB.toLazyText

    text = \case
        Suppressor _p _jp _j txt -> text txt
        TextLit q _tlit l r more ->
            moreText (TB.fromText (interpretTextLit inp q l r)) more

    {-# INLINE moreText #-}
    moreText acc = \case
        NoMoreText -> acc
        MoreText jp j txt -> acc <> joiner txt (jp <> MkPos 1 1) j   -- skip <

    joiner :: Text nesequ tlit Pos -> Pos -> Joiner nesequ tlit Pos j -> TB.Builder
    joiner txt l = \case
        NilJoiner _r ->
            text txt
        ConsJoinerText _tlit r j' ->
            TB.fromText (slice l r inp) <> joiner txt r j'
        ConsJoinerEscapes escapes j' ->
            foldr escape (\l' -> joiner txt l' j') escapes
          $ l

    escape sz k l =
        let !l' = l <> MkPos 1 1   -- skip %
            !n = 2 * valueFour sz   -- nibbles
            !r = l' <> MkPos n n
            !hextxt = slice l' r inp
        in
        -- TODO there's probably much simpler logic for this since we
        -- have to go just one code point at a time
        case B16.decode (T.encodeUtf8 hextxt) of
            Left s -> error $ "impossible! base16 escape " <> T.unpack hextxt <> " " <> s
            Right bs -> case T.decodeUtf8' bs of
                Left exn -> error $ "impossible! utf8 escape " <> T.unpack hextxt <> " " <> show exn
                Right txt' -> TB.fromText txt' <> k r

interpretTextLit :: MatterStream inp => inp -> Quote -> Pos -> Pos -> T.Text
interpretTextLit inp q l r =
    slice (l <> MkPos nl nl) (r <> MkPos nr nr) inp
  where
    nr = 1 - nl   -- skip last character of delimiter
    nl = case q of
        DoubleQuote -> 1
        MultiQuote delim ->
            2 + (valueFour . forgetFour') delim

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
