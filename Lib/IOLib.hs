module Lib.IOLib (
  readLinesToStringList,
  readFileToStringList
) where

import Data.List
import Data.Char
import System.Environment
import System.IO

readLinesToStringList = do
  s <- getContents
  return (lines s)

readFileToStringList f = do
  s <- readFile f
  return (lines s)