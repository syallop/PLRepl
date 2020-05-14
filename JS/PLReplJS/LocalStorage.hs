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
  , storeInLocalStorage
  , lookupFromLocalStorage
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

-- | Store key-values under a namespace in the Web DOM LocalStorage.
data LocalStorageStore k v = LocalStorageStore
  { _namespace :: Text
  , _storage   :: JS.Storage
  }

instance Show (LocalStorageStore k v) where
  show (LocalStorageStore namespace _) = "LocalStorageStore/"<> Text.unpack namespace

-- | Create a LocalStorageStore under a given namespace in the Web DOM
-- LocalStorage.
newLocalStorageStore
  :: Text
  -> LocalStorageStore k v
newLocalStorageStore n = LocalStorageStore n JS.localStorage

instance
  ( Serialize k
  , Serialize v
  , Ord v
  , Eq v
  ) => Store LocalStorageStore k v where
  store  = storeInLocalStorage
  lookup = lookupFromLocalStorage

-- | Store a key-value association in the Web DOM LocalStorage.
storeInLocalStorage
  :: ( Serialize k
     , Serialize v
     , Ord v
     , Eq v
     )
  => LocalStorageStore k v
  -> k
  -> v
  -> IO (Maybe (LocalStorageStore k v, StoreResult v))
storeInLocalStorage localStorage key value = do
  let qualifiedKey = qualifiedKeyName localStorage key

  -- Check whether a value is already stored at this key.
  -- If it contains the same content, we don't need to do anything
  -- If it differs, we replace and return the old valu
  mJSString <- JS.getItem qualifiedKey (_storage localStorage)
  case mJSString of
    Nothing
      -> do JS.setItem qualifiedKey (serializeJSString value) (_storage localStorage)
            pure $ Just (localStorage, Successfully)

    Just jsString
      -> case deserializeJSString jsString of
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
             | existingValue == value
              -> pure $ Just (localStorage, AlreadyStored)

             | otherwise
              -> do JS.setItem qualifiedKey (serializeJSString value) (_storage localStorage)
                    pure $ Just (localStorage, Overwritten $ Set.fromList [existingValue])

-- | Read a value by consulting the Web DOM LocalStorage.
lookupFromLocalStorage
  :: ( Serialize k
     , Serialize v
     )
  => LocalStorageStore k v
  -> k
  -> IO (Maybe (LocalStorageStore k v, v))
lookupFromLocalStorage localStorage key = do
  let qualifiedKey = qualifiedKeyName localStorage key
  mJSString <- JS.getItem qualifiedKey (_storage localStorage)
  case mJSString of
    Nothing
      -> pure Nothing

    Just jsString
      -> case deserializeJSString jsString of
           Nothing
             -> error "Could not deserialize value to expected type in localstorage"

           Just value
             -> pure $ Just (localStorage, value)

-- Compute a keys name in the LocalStorage by prefixing the namespace.
qualifiedKeyName
  :: Serialize k
  => LocalStorageStore k v
  -> k
  -> JS.JSString
qualifiedKeyName (LocalStorageStore namespace _storage) key = mconcat
  [ JS.pack . Text.unpack $ namespace
  , JS.pack "/"
  , serializeJSString key
  ]

-- Take a trip through every string type to Serialize to a JSString.
-- TODO: Don't.
serializeJSString :: Serialize s => s -> JS.JSString
serializeJSString = JS.pack . Text.unpack . decodeUtf8 . serialize

-- Take a trip through every string type to Deserialize from a JSString.
-- TODO: Don't.
deserializeJSString :: Serialize s => JS.JSString -> Maybe s
deserializeJSString = deserialize . encodeUtf8 . Text.pack . JS.unpack

