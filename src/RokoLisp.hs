-- |
-- Copyright: (c) 2021 Tristan de Cacqueray
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tristan de Cacqueray <tdecacqu@redhat.com>
--
-- See README for more info
module RokoLisp
  ( someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
