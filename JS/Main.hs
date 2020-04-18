-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String

-- PL dependencies
import qualified PL as PL

import qualified PL.Expr as PL
import PL.Expr hiding (Expr, App)

import qualified PL.Type as PL
import PL.Type hiding (Type)

import PLRepl.Repl
import PL.Var
import PL.TyVar
import PL.FixExpr
import PL.FixType

-- Other dependencies
import Data.Text (Text)
import qualified Data.Text as Text

type Expr = PL.Expr Var Type TyVar
type Type = PL.Type TyVar

idNat :: Expr
idNat = FixExpr $ Lam (FixType $ Named "Nat") (FixExpr $ Binding $ VZ)

-- | Type synonym for an application model
data Model = Model
  { output :: Text
  }
  deriving Eq

-- | Sum type for application events
data Action
  = NoOp
  | PrintExpr Expr
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where

    -- Executed on application load
    initialAction = PrintExpr idNat
    model  = Model "EMPTY"
    update = updateModel
    view   = viewModel

    -- Delegated events
    events = defaultEvents

    -- Subsriptions
    subs   = []

    -- Nothing defaults to 'body'
    mountPoint = Nothing

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel a m = case a of
  NoOp
    -> noEff m

  PrintExpr expr
    -> noEff (m{ output = Text.pack . show $ expr })

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick (PrintExpr idNat) ] [ text "Print expression" ]
 , text (ms . output $ x)
 ]

