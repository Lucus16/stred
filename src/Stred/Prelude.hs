module Stred.Prelude
  ( module Exports
  , readMaybe
  , try
  ) where

import Control.Applicative as Exports ((<|>))
import Control.Monad as Exports (guard)
import Control.Monad.IO.Class as Exports (liftIO)
import Data.ByteString as Exports (ByteString)
import Data.Fix as Exports (Fix (..))
import Data.Foldable as Exports (traverse_)
import Data.Function as Exports (applyWhen)
import Data.List.NonEmpty as Exports (NonEmpty (..), nonEmpty)
import Data.Map as Exports (Map)
import Data.Maybe as Exports (fromMaybe)
import Data.Sequence as Exports (Seq)
import Data.Set as Exports (Set)
import Data.String as Exports (IsString (..))
import Data.Tape as Exports (Tape)
import Data.Text as Exports (Text)
import Data.Traversable as Exports (for)
import Data.Word as Exports (Word16, Word32, Word64, Word8)
import Prelude as Exports hiding (head, init, last, tail)

import Data.Text qualified as Text
import Text.Read qualified as String

readMaybe :: (Read a) => Text -> Maybe a
readMaybe = String.readMaybe . Text.unpack

try :: (a -> Maybe a) -> a -> a
try f x = fromMaybe x (f x)
