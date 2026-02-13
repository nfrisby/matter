{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Language.Matter.Tokenizer.Pretty (Builder, prettyToken, toLazyText) where

import Data.String (IsString, fromString)
import Data.Vector qualified as V
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TL
import Language.Matter.Tokenizer
import Language.Matter.Tokenizer.Counting (D10 (..), Four (..), Four' (..), Three (..))
import System.Random.Stateful qualified as R

pick :: (R.StatefulGen g m, ?g :: g) => TL.Builder -> [TL.Builder] -> Builder m
{-# INLINE pick #-}
pick a xs = MkBuilder $ \s -> (# do
    let v = V.fromList xs
    i <- R.uniformWord64R (fromIntegral $ V.length v) ?g
    pure $ if i == 0 then a else v V.! (fromIntegral i - 1), s #)

data St =
    -- | Unconstrained
    Any
  |
    -- | Previous was 0 or 0x
    Digit
  |
    -- | Previous was @ or #
    Id

-- | Carries state so that 'prettyToken' can insert 'OdWhitespace'
-- when necessary
newtype Builder m = MkBuilder (St -> (# m TL.Builder, St #))
instance Applicative m => IsString (Builder m) where fromString = \x -> MkBuilder $ \s -> (# pure (fromString x), s #)
instance Applicative m => Monoid (Builder m) where mempty = MkBuilder $ \s -> (# pure mempty, s #)
instance Applicative m => Semigroup (Builder m) where
  MkBuilder f <> MkBuilder g = MkBuilder $ \s ->
      let (# x, s' #) = f s
          (# y, s'' #) = g s'
      in
      (# liftA2 (<>) x y, s'' #)

toLazyText :: Functor m => Builder m -> m TL.Text
toLazyText (MkBuilder f) =
    let (# m, _s #) = f Any
    in
    TL.toLazyText <$> m

-----

wrap :: Applicative m => (Token -> Builder m) -> Token -> Builder m
{-# INLINE wrap #-}
wrap f = \tk -> MkBuilder $ \s ->
    let MkBuilder g = case s of
            Any -> f tk
            Digit -> (if cantFollowDigit tk then f (OdToken OdWhitespace) else mempty) <> f tk
            Id -> (if cantFollowId tk then f (OdToken OdWhitespace) else mempty) <> f tk
        (# m, _s #) = g s
    in
    (# m, after tk #)
  where
    cantFollowDigit = \case
        OdToken OdIntegerPart -> True
        OdToken OdBytes -> True
        _ -> False

    cantFollowId = \case
        OdToken OdWhitespace -> False
        _ -> True

after :: Token -> St
{-# INLINE after #-}
after = \case
    OdToken od -> case od of
        OdWhitespace -> Any
        OdAtom -> Id
        OdVariant -> Id
        OdOpenParen -> Any
        OdJoinerNotEscaped{} -> Any
        OdBytes -> Digit
        OdIntegerPart -> Digit
        OdFractionPart -> Digit
        OdExponentPart -> Digit
    SdToken{} -> Any

-- | Inserts an 'OdWhitespace' whenever necessary
prettyToken :: R.StatefulGen g m => g -> Token -> Builder m
prettyToken g = let ?g = g in wrap $ \case
    OdToken od -> case od of
        OdWhitespace -> pick " " ["    ", "\n", "  \n"]
        OdAtom -> "@" <> pick "A" ["B", "C", "true", "False", "null", "NaN"]
        OdVariant -> "#" <> pick "X" ["Y", "Z", "red", "Blue", "NaN"]
        OdOpenParen -> "("
        OdJoinerNotEscaped start ->
            (if start then "<" else "") <> pick "" ["join", "\t", ","]
        OdBytes -> "0x" <> pick "" ["00", "AB", "a1973b", "FEEB", "0103", "0000", "123456"]
        OdIntegerPart -> integerPart
        OdFractionPart -> "." <> pick "0" ["123", "100", "000", "00900"]
        OdExponentPart -> pick "e" ["E"] <> integerPart
    SdToken sd -> case sd of
        SdOpenSeq -> "["
        SdCloseSeq -> "]"
        SdCloseParen -> ")"
        SdUnderscore -> "_"
        SdOpenPin -> "(^"
        SdClosePin -> "^)"
        SdOpenMeta o -> case o of
            LT -> "{<"
            EQ -> "{="
            GT -> "{>"
        SdCloseMeta o -> case o of
            LT -> "<}"
            EQ -> "=}"
            GT -> ">}"
        SdDoubleQuotedString -> "\"" <> pick "" ["foo", "bar", "\n", "Hello there, Rabbit."] <> "\""
        SdMultiQuotedString four' ->
            let delim = case four' of
                    Four1' d1 -> digit d1
                    Four2' d1 d2 -> digit d1 <> digit d2
                    Four3' d1 d2 d3 -> digit d1 <> digit d2 <> digit d3
                    Four4' d1 d2 d3 d4 -> digit d1 <> digit d2 <> digit d3 <> digit d4
                q = "'" <> delim <> "'"
            in
            q <> pick "" ["foo", "bar", "\n", "Hello there, Rabbit.", "Quoth the Raven \"Nevermore.\"", "a'a", "'salsa'"] <> q
        SdJoinerNotEscaped acc ->
            -- TODO we're letting Three2 be "<>" sometimes, which is
            -- contrary to its stated semantics. But that's is
            -- currently the only way that "Generator" would ever
            -- yield a <> in a Text.
            (if Three3 /= acc then "<" else "") <> (if Three1 == acc then "" else pick "" ["join", "\t", ","]) <> ">"
        SdJoinerEscapedUtf8 four -> case four of
            Four1 -> pick "%46" ["%09", "%0a", "%25", "%9C"]
            Four2 -> pick "%C398" ["%C2B1", "%CF80", "%d0af"]
            Four3 -> pick "%E299bf" ["%E29a80", "%E2998B", "%E2A88C", "%E0B8BF", "%EFb4bF"]
            Four4 -> pick "%F09F8084" ["%F09f82a1", "%F09F8E83", "%F09FA4AA"]

integerPart :: (R.StatefulGen g m, ?g :: g) => Builder m
integerPart = pick "" ["+", "-"] <> pick "0" ["123", "100", "000", "00900"]

digit :: Applicative m => D10 -> Builder m
digit = \case
    D10_0 -> "0"
    D10_1 -> "1"
    D10_2 -> "2"
    D10_3 -> "3"
    D10_4 -> "4"
    D10_5 -> "5"
    D10_6 -> "6"
    D10_7 -> "7"
    D10_8 -> "8"
    D10_9 -> "9"
