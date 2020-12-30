{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : PLReplJS.LocalStorage
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Implementation of PL.Store backed by Web DOM LocalStorage.
-}

module PLReplJS.LocalStorage
  ( LocalStorageStore (..)
  , newLocalStorageStore

  , storeInLocalStorage
  , lookupFromLocalStorage

  , largerKeysInLocalStorage
  , shortenKeyAgainstLocalStorage

  -- Util
  , serializeJSString
  , deserializeJSString
  )
  where

-- JS
import qualified JavaScript.Web.Storage as JS
import qualified Data.JSString as JS
import qualified GHCJS.Types as JS
import qualified GHCJS.Marshal as JS
import qualified GHCJS.Foreign as JS

-- Core PL
import PL.Serialize
import PL.Error

-- Other PL
import PLStore
import PLStore.Short
import PLStore.File.Path
import PLPrinter.Doc

import Data.Text (Text)
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text.Encoding
import qualified Data.Set as Set

-- | Store key-values in the Web DOM LocalStorage.
data LocalStorageStore k v = LocalStorageStore
  { _namespaces          :: [Text]
  , _keyPattern          :: PathPattern k
  , _serializeJSString   :: v -> JS.JSString
  , _deserializeJSString :: forall phase. JS.JSString -> Either Doc v
  , _valuesEqual         :: v -> v -> Bool

  , _storage   :: JS.Storage
  }

instance Show (LocalStorageStore k v) where
  show _ = "LocalStorageStore"

-- | Create a LocalStorageStore in the Web DOM LocalStorage.
newLocalStorageStore
  :: [Text]
  -> PathPattern k
  -> (v -> JS.JSString)
  -> (forall phase. JS.JSString -> Either Doc v)
  -> (v -> v -> Bool)
  -> LocalStorageStore k v
newLocalStorageStore namespaces keyPattern serializeJSString deserializeJSString valuesEqual = LocalStorageStore
  { _namespaces          = namespaces
  , _keyPattern          = keyPattern
  , _serializeJSString   = serializeJSString
  , _deserializeJSString = deserializeJSString
  , _valuesEqual         = valuesEqual
  , _storage = JS.localStorage
  }

instance
  ( Ord v
  , Show k
  )
  => Store LocalStorageStore k v where
  store  = storeInLocalStorage
  lookup = lookupFromLocalStorage

-- | Store a key-value association in the Web DOM LocalStorage.
storeInLocalStorage
  :: ( Ord v
     , Show k
     )
  => LocalStorageStore k v
  -> k
  -> v
  -> IO (Either Doc (LocalStorageStore k v, StoreResult v))
storeInLocalStorage localStorage key value = do
  -- Check whether a value is already stored at this key.
  -- If it contains the same content, we don't need to do anything
  -- If it differs, we replace and return the old valu
  case generateLocalStorageKey key localStorage of
    Left err
      -> pure . Left $ err

    Right path
      -> do mJSString <- JS.getItem path (_storage localStorage)
            case mJSString of
              Nothing
                -> do JS.setItem path (_serializeJSString localStorage value) (_storage localStorage)
                      pure $ Right (localStorage, Successfully)

              Just jsString
                -> case _deserializeJSString localStorage jsString of
                     Left err
                       -> pure . Left . mconcat $
                            [ text "When attempting to store a key-value in the LocalStorage store we encountered an existing value which did not deserialize as expected. This could indicate:"
                            , lineBreak
                            , text "- Serialization does not round trip correctly"
                            , lineBreak
                            , text "- The localstorage has been tampered with"
                            , lineBreak
                            , text "- We have a hash collision"
                            , lineBreak
                            , lineBreak
                            , text "The file in question is at: "
                            , text . Text.pack . JS.unpack $ path
                            , lineBreak
                            , indent1 $ err
                            ]

                     Right existingValue
                       | _valuesEqual localStorage existingValue value
                        -> pure . Right $ (localStorage, AlreadyStored)

                       | otherwise
                        -> do JS.setItem path (_serializeJSString localStorage value) (_storage localStorage)
                              pure . Right $ (localStorage, Overwritten $ Set.fromList [existingValue])

-- | Read a value by consulting the Web DOM LocalStorage.
lookupFromLocalStorage
  :: ( Show k
     )
  => LocalStorageStore k v
  -> k
  -> IO (Either Doc (LocalStorageStore k v, Maybe v))
lookupFromLocalStorage localStorage key = do
  case generateLocalStorageKey key localStorage of
    Left err
      -> pure . Left $ err

    Right path
      -> do mJSString <- JS.getItem path (_storage localStorage)
            case mJSString of
              Nothing
                -> pure . Right $ (localStorage, Nothing)

              Just jsString
                -> case _deserializeJSString localStorage jsString of
                     Left err
                       -> pure . Left . mconcat $ [ text "Could not deserialize value to expected type in localstorage"
                                                  , lineBreak
                                                  , indent1 err
                                                  ]

                     Right value
                       -> pure . Right $ (localStorage, Just value)

-- Generate the local storage key name associated with a key.
generateLocalStorageKey
  :: Show k
  => k
  -> LocalStorageStore k v
  -> Either Doc JS.JSString
generateLocalStorageKey key f = case generatePath key (_keyPattern f) of
  Left err
    -> Left err

  Right path
    -> Right $ baseKey f <> JS.pack path

baseKey
  :: LocalStorageStore k v
  -> JS.JSString
baseKey = JS.pack . Text.unpack . (<> "/") . mconcat . List.intersperse "/" . _namespaces

-- | Take a trip through every string type to Serialize to a JSString.
serializeJSString :: Serialize s => s -> JS.JSString
serializeJSString = JS.pack . Text.unpack . decodeUtf8 . serialize
-- TODO: Don't.

-- | Take a trip through every string type to Deserialize from a JSString.
deserializeJSString :: Serialize s => JS.JSString -> Either Doc s
deserializeJSString = deserialize . encodeUtf8 . Text.pack . JS.unpack
-- TODO: Don't.


instance
  ( Ord v
  , Ord shortK
  , Show k
  , Shortable k shortK
  ) => ShortStore LocalStorageStore k shortK v where
  largerKeys s shortK = Right . (s,) <$> largerKeysInLocalStorage s shortK
  shortenKey s k      = Right . (s,) <$> shortenKeyAgainstLocalStorage s k

-- | Lookup all known keys that are 'larger' than a short key.
largerKeysInLocalStorage
  :: ( Ord shortK
     , Shortable k shortK
     , Show k
     )
  => LocalStorageStore k v
  -> shortK
  -> IO [k]
largerKeysInLocalStorage f shortKey = filter (isShortened shortKey) <$> getAllKeys f
-- TODO: We should be able to search along promising paths rather than grabbing
-- every path and checking it.

shortenKeyAgainstLocalStorage
  :: (Shortable k shortK, Show k)
  => LocalStorageStore k v
  -> k
  -> IO shortK
shortenKeyAgainstLocalStorage f key = do
  allKeys <- getAllKeys f
  case fmap (shortenAgainst key . Just) allKeys of
    []
      -> pure . shortenAgainst key $ Nothing

    xs
      -> do let shortenings = List.sortOn shortLength $ xs
            pure . head $ shortenings
-- TODO: We should be able to compare against less keys.

-- Get all keys stores in LocalStorage
getAllKeys
  :: Show k
  => LocalStorageStore k v
  -> IO [k]
getAllKeys s = do
  jsVal <- localStorageKeys
  (mNames :: Maybe [String]) <- JS.fromJSVal jsVal


  case mNames of
    Nothing
      -> error "Failed to read keys from local storage"

    Just names
      -> do -- Drop the namespace
            -- TODO: Actually drop the namespace, not the same amount of
            -- characters.
            let base = baseKey s
                potentialKeys = fmap (\name
                                       -> readPathKey (Text.pack . List.drop (Prelude.length . JS.unpack $ base) $ name) (_keyPattern s)
                                     )
                                     names
              in pure $ foldr (\(leftovers,mKey) acc
                                 -> if leftovers /= ""
                                      then acc
                                      else case mKey of
                                             Nothing
                                               -> acc

                                             Just key
                                               -> key : acc
                              )
                              []
                              potentialKeys

foreign import javascript unsafe "$r = Object.keys(localStorage);"
  localStorageKeys :: IO JS.JSVal

