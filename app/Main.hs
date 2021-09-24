{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import Syntax
import Eval

main :: IO ()
main = do
  putStr "> "
  line <- getLine
  case parseString line of
    Left err -> print err >> main
    Right t -> do
      print t
      case eval t of
        Nothing -> putStrLn "Evaluation error" >> main
        Just r -> print r >> main
