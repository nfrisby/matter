-- | The tokens and tokenization
module Language.Matter.Tokenizer (

    -- * Tokens
    Token (..),
    OdToken (..),
    SdToken (..),

    -- * Input streams
    MatterStream (..),
    MunchResult (..),
    Pos (..),
    UnconsResult (..),
    posDiff,

    -- * Tokenizer states
    Tokenizer (..),
    startTokenizer,
    tokenizerCurrent,
    tokenizerStart,

    -- ** Tokenizer transitions
    EofError (..),
    EofResult (..),
    SnocError (..),
    SnocResult (..),
    SnocsResult (..),
    eofTokenizer,
    snocTokenizer,
    snocsTokenizer,

  ) where

import Language.Matter.Tokenizer.StateMachine
