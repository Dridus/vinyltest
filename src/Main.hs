module Main where

import ClassyPrelude hiding (Identity(Identity))
import Data.Aeson (Object, Value(Object), encode, toJSON)
import qualified Data.Aeson.BetterErrors as ABE
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (Rec((:&), RNil), rmap)
import Data.Vinyl.Functor (Identity(Identity))
import Frames ((:->)(Col, getCol), (&:))
import GHC.TypeLits (KnownSymbol, symbolVal)

type Name = "name" :-> Text

type Person = '[Name]

newtype ToJson a = ToJson { unToJson :: a -> Value }
newtype FromJson e a = FromJson { unFromJson :: ABE.Parse e a }
data ToFromJson e a = ToFromJson
  { toJson   :: a -> Value
  , fromJson :: ABE.Parse e a
  }

class RecToJson rs where
  recToJson' :: Rec ToJson rs -> Rec Identity rs -> Object

class RecFromJson rs where
  recFromJson' :: Rec (FromJson e) rs -> (ABE.Parse e (Rec Identity rs))

instance (KnownSymbol s, RecToJson rs) => RecToJson ((s :-> a) ': rs) where
  recToJson' (ToJson aToJson :& fs) (Identity a :& as) =
    insertMap (pack $ symbolVal (Proxy :: Proxy s)) (aToJson a) $
      recToJson' fs as

instance (KnownSymbol s, RecFromJson rs) => RecFromJson ((s :-> a) ': rs) where
  recFromJson' (FromJson aFromJson :& fs) =
    (:&)
      <$> ABE.key (pack $ symbolVal (Proxy :: Proxy s)) (Identity <$> aFromJson)
      <*> recFromJson' fs

instance RecToJson '[] where
  recToJson' _ = const mempty

instance RecFromJson '[] where
  recFromJson' _ = pure RNil

recToJson :: RecToJson rs => Rec ToJson rs -> Rec Identity rs -> Value
recToJson = map Object . recToJson'

recFromJson :: RecFromJson rs => Rec (FromJson e) rs -> ABE.Parse e (Rec Identity rs)
recFromJson = recFromJson'

recToFromJson :: (RecToJson rs, RecFromJson rs) => Rec (ToFromJson e) rs -> ToFromJson e (Rec Identity rs)
recToFromJson format =
  ToFromJson
    (recToJson . rmap (ToJson . toJson) $ format)
    (recFromJson . rmap (FromJson . fromJson) $ format)

class DefaultToFromJson a where
  defaultToFromJson :: ToFromJson e (s :-> a)

instance DefaultToFromJson Text where
  defaultToFromJson = textJson

class DefaultRecToFromJson rs where
  defaultRecToFromJson :: Rec (ToFromJson e) rs

instance (DefaultToFromJson a, DefaultRecToFromJson rs) => DefaultRecToFromJson ((s :-> a) ': rs) where
  defaultRecToFromJson = defaultToFromJson :& defaultRecToFromJson

instance DefaultRecToFromJson '[] where
  defaultRecToFromJson = RNil

textJson :: ToFromJson e (s :-> Text)
textJson = ToFromJson (toJSON . getCol) (Col <$> ABE.asText)

personJson :: ToFromJson e (Rec Identity Person)
personJson = recToFromJson $ textJson :& RNil

main :: IO ()
main = do
  let alice = "Alice" &: RNil :: Rec Identity Person
  let json = toJson personJson alice
  putStrLn . toStrict . decodeUtf8 . encode $ json
