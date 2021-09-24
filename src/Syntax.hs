module Syntax where

import qualified Data.Text as T

type Ident = T.Text

data Term = Atom Ident           -- x
          | Abs  Ident Term      -- λ x . <body>
          | App  Term Term       -- (x y)

instance Show Term where
  show (Atom x) = T.unpack x
  show (Abs v b) = "(λ " <> T.unpack v <> " . " <> show b <> ")"
  show (App t1 t2) = "(" <> show t1 <> " " <> show t2 <> ")"

