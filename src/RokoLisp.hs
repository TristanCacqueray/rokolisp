-- |
-- Copyright: (c) 2021 Tristan de Cacqueray
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tristan de Cacqueray <tdecacqu@redhat.com>
--
-- See README for more info
module RokoLisp
  ( Term (..),
    Value (..),
    Literal (..),
    parse,
    format,
    eval,
    functions,
    doEval,
  )
where

import Relude
import RokoLisp.Eval
import RokoLisp.Runtime
import RokoLisp.Syntax

-- | Full eval function that resolves imports
doEval :: MonadIO m => Maybe FilePath -> Text -> m Value
doEval fp input = case parse input of
  Right term -> do
    func <- functions
    resolve (fromMaybe "" fp) term >>= eval func
  Left err -> error (show err)
