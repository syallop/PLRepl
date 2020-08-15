{-# LANGUAGE
    FlexibleContexts
  , ImplicitParams
  , OverloadedStrings
  , TemplateHaskell
  , TypeSynonymInstances
  , FlexibleInstances
  #-}
{-|
Module      : Main
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

The main module provides a cli repl executable for the PL language.

Our cli widgets are found under PLRepl.Widgets/WIDGET underwhich by convention:
  - WIDGET.Event defines the events the widget emits/ understands.
  -
  - WIDGET.Name defines names used to reference components of the widget, such
    as its cursors or viewport.

  - WIDGET.State defines the state of the widget. Often a drawWIDGET function
    will be exported to draw this function as a renderable widget.

In particular:
  - Editor: An editable text editor, build upon PLEditor.
    A use case is entering source code.

  - Output: An output text area. This is currently an alias to Editor
    where we expect editing events not to be forwarded.
    A use case is responses to parsing/ type checking source code entered in an editor.

  - TypeCtx: An output area containing defined type-definitions.

  - Usage: An output area containing usage information

PLRepl.Repl abstracts the Read Eval Print Loop for some PL repl.
It accepts a repl configuration which it understands how to drive.

Repl configuration(s) are found under PLRepl.Repl.* This abstraction allows
defining multiple repls which can be switched between, for example we might
want a repl for different expression grammars or a repl exclusively for type
signatures.

-}
module Main where

import PLReplTUI

main :: IO ()
main = run

