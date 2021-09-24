{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO

import Parser
import Syntax
import Eval

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  line <- getLine
  case parseString line of
    Left err -> print err >> main
    Right t -> do
      print t
      case eval t of
        Nothing -> putStrLn "Evaluation error" >> main
        Just r -> print r >> main
