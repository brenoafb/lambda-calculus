module Parser (parseText, parseString) where

import Prelude hiding (abs)

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Text as T

import Syntax

languageDef =
  emptyDef { Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "lambda"
                                     , "λ"
                                     ]
           , Token.reservedOpNames = [ "."
                                     , "("
                                     , ")"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
whiteSpace = Token.whiteSpace lexer

languageParser :: Parser Term
languageParser = whiteSpace >> term

term :: Parser Term
term = try app <|> try abs <|> atom

app :: Parser Term
app = parens contents
  where
    contents = do
      t1 <- term
      t2 <- term
      pure $ App t1 t2

abs :: Parser Term
abs = parens contents
  where
    contents = do
      reserved "lambda" <|> reserved "λ"
      v <- T.pack <$> identifier
      reserved "."
      b <- term
      pure $ Abs v b

atom :: Parser Term
atom = do
  v <- identifier
  pure $ Atom (T.pack v)

parseString :: String -> Either ParseError Term
parseString = parse (languageParser <* eof) ""

parseText :: T.Text -> Either ParseError Term
parseText = parseString . T.unpack
