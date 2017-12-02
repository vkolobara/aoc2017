module Lib.IOLib (
  readLinesToStringList
) where

import Data.List
import Data.Char
import System.Environment
import System.IO

readLinesToStringList = do
  s <- getContents
  return (lines s)
