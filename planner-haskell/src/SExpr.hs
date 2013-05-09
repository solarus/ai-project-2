module SExpr where

import Control.Applicative hiding ((<|>), many)
import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.String

data SExpr = Nil | Atom String | SString String | List [SExpr]
  deriving (Eq)

instance Show SExpr where
    show Nil         = "()"
    show (Atom s)    = s
    show (SString s) = "\"" ++  s ++ "\""
    show (List ss)   = "(" ++ intercalate " " (map show ss) ++ ")"

instance Read SExpr where
    readsPrec _ s = [readSExpr s]

car :: SExpr -> SExpr
car (List (s:_)) = s
car _            = error "SExpr.car: Not a cons cell."

cdr :: SExpr -> SExpr
cdr (List (_:s)) = List s
cdr _            = error "SExpr.cdr: Not a cons cell."

toList :: SExpr -> [SExpr]
toList (List ss) = ss
toList _         = error "SExpr.toList: not a cons cell."

readSExpr :: String -> (SExpr, String)
readSExpr s = case parse ((,) <$> (many space *> parseSExpr) <*> many anyChar) "" s of
    Left err -> error ("readSExpr:\ns = " ++ s ++ "\nError = " ++ show err)
    Right a  -> a

parseNil :: Parser SExpr
parseNil = Nil <$ string "()"

parseAtom :: Parser SExpr
parseAtom = Atom <$> some (satisfy charOK)
  where
    charOK c = not (isSpace c) &&  c `notElem` "()\""

parseSString :: Parser SExpr
parseSString = SString <$> parseString

parseList :: Parser SExpr
parseList = List <$> (char '(' *> some parseSExpr <* char ')')

parseSExpr :: Parser SExpr
parseSExpr =   pad parseAtom
           <|> try (pad parseSString)
           <|> try (pad parseList)
           <|> pad parseNil
  where
    pad p = many space *> p <* many space

-- FIXME: Stupid string parser
parseString :: Parser String
parseString = char '"' *> manyTill anyChar (char '"')