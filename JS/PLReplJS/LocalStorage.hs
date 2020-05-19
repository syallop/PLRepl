{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : PLReplJS.LocalStorage
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Implementation of PL.Store backed by Web DOM LocalStorage.
-}

module PLReplJS.LocalStorage
  ( LocalStorageStore ()
  , newLocalStorageStore
  , newSimpleLocalStorageStore

  , storeInLocalStorage
  , lookupFromLocalStorage

  -- Util
  , qualifiedKeyName
  , serializeJSString
  , deserializeJSString
  )
  where

import qualified JavaScript.Web.Storage as JS
import qualified Data.JSString as JS

import PL.Store
import PL.Serialize

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import qualified Data.Set as Set

-- | Store key-values in the Web DOM LocalStorage.
data LocalStorageStore k v = LocalStorageStore
  { _keyPath             :: k -> JS.JSString
  , _serializeJSString   :: v -> JS.JSString
  , _deserializeJSString :: JS.JSString -> Maybe v
  , _valuesEqual         :: v -> v -> Bool

  , _storage   :: JS.Storage
  }

instance Show (LocalStorageStore k v) where
  show _ = "LocalStorageStore"

-- | Create a LocalStorageStore in the Web DOM LocalStorage.
newLocalStorageStore
  :: (k -> JS.JSString)
  -> (v -> JS.JSString)
  -> (JS.JSString -> Maybe v)
  -> (v -> v -> Bool)
  -> LocalStorageStore k v
newLocalStorageStore keyPath serializeJSString deserializeJSString valuesEqual = LocalStorageStore
  { _keyPath             = keyPath
  , _serializeJSString   = serializeJSString
  , _deserializeJSString = deserializeJSString
  , _valuesEqual         = valuesEqual
  , _storage = JS.localStorage
  }

newSimpleLocalStorageStore
  :: (Serialize k, Serialize v, Ord v, Eq v)
  => Text
  -> LocalStorageStore k v
newSimpleLocalStorageStore namespace = LocalStorageStore
  { _keyPath             = qualifiedKeyName namespace
  , _serializeJSString   = serializeJSString
  , _deserializeJSString = deserializeJSString
  , _valuesEqual         = (==)
  , _storage = JS.localStorage
  }

instance Ord v => Store LocalStorageStore k v where
  store  = storeInLocalStorage
  lookup = lookupFromLocalStorage

-- | Store a key-value association in the Web DOM LocalStorage.
storeInLocalStorage
  :: Ord v
  => LocalStorageStore k v
  -> k
  -> v
  -> IO (Maybe (LocalStorageStore k v, StoreResult v))
storeInLocalStorage localStorage key value = do
  let qualifiedKey = _keyPath localStorage key

  -- Check whether a value is already stored at this key.
  -- If it contains the same content, we don't need to do anything
  -- If it differs, we replace and return the old valu
  mJSString <- JS.getItem qualifiedKey (_storage localStorage)
  case mJSString of
    Nothing
      -> do JS.setItem qualifiedKey (_serializeJSString localStorage value) (_storage localStorage)
            pure $ Just (localStorage, Successfully)

    Just jsString
      -> case _deserializeJSString localStorage jsString of
           Nothing
             -> error $ mconcat [ "When attempting to store a key-value in the LocalStorage store we encountered an existing value which did not deserialize as expected. This could indicate:\n"
                                , "- Serialization does not round trip correctly\n"
                                , "- The localstorage has been tampered with\n"
                                , "- We have a hash collision\n"
                                , "\n"
                                , "The file in question is at: "
                                , JS.unpack qualifiedKey
                                ]
           Just existingValue
             | _valuesEqual localStorage existingValue value
              -> pure $ Just (localStorage, AlreadyStored)

             | otherwise
              -> do JS.setItem qualifiedKey (_serializeJSString localStorage value) (_storage localStorage)
                    pure $ Just (localStorage, Overwritten $ Set.fromList [existingValue])

-- | Read a value by consulting the Web DOM LocalStorage.
lookupFromLocalStorage
  :: LocalStorageStore k v
  -> k
  -> IO (Maybe (LocalStorageStore k v, v))
lookupFromLocalStorage localStorage key = do
  let qualifiedKey = _keyPath localStorage key
  mJSString <- JS.getItem qualifiedKey (_storage localStorage)
  case mJSString of
    Nothing
      -> pure Nothing

    Just jsString
      -> case _deserializeJSString localStorage jsString of
           Nothing
             -> error "Could not deserialize value to expected type in localstorage"

           Just value
             -> pure $ Just (localStorage, value)


-- | Compute a keys name in the LocalStorage by prefixing the namespace.
qualifiedKeyName
  :: Serialize k
  => Text
  -> k
  -> JS.JSString
qualifiedKeyName namespace key = mconcat
  [ JS.pack . Text.unpack $ namespace
  , JS.pack "/"
  , serializeJSString key
  ]

-- | Take a trip through every string type to Serialize to a JSString.
serializeJSString :: Serialize s => s -> JS.JSString
serializeJSString = JS.pack . Text.unpack . decodeUtf8 . serialize
-- TODO: Don't.

-- | Take a trip through every string type to Deserialize from a JSString.
deserializeJSString :: Serialize s => JS.JSString -> Maybe s
deserializeJSString = deserialize . encodeUtf8 . Text.pack . JS.unpack
-- TODO: Don't.

