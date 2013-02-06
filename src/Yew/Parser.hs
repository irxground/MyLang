module Yew.Parser where

import Text.ParserCombinators.Parsec


parseYew :: String -> Either ParseError [Def]
parseYew = parse parseDoc "(unknown)"


----- ----- ----- ----- ----- PARSER FUNCTION ----- ----- ----- ----- -----

parseDoc :: Parser [Def]
parseDoc = do
    many $ do
        def <- parseDef
        spaces
        return def

parseDef :: Parser Def
parseDef = do
    char '#'
    spaces
    parseDefCls <|> parseDefFun

parseDefCls :: Parser Def
parseDefCls = do
    yewType <- parseType
    return $ DefCls yewType

parseDefFun :: Parser Def
parseDefFun = do
    name <- idToken <?> "Error C"
    spaces
    params <- many $ do
        pName <- idToken <?> "Error A"
        spaces
        pType <- parseType <?> "Error B"
        spaces
        return (pName, pType)
    return $ DefFun YewFunc { funcName = name, funcParams = params }


parseType :: Parser YewType
parseType = do
    char '['
    spaces
    names <- many1 $ do
        name <- idToken
        spaces
        return name
    char ']'
    return YewType {
        typeName = (last names),
        typeParams = (init names)
    }

----- ----- ----- ----- ----- HELPER FUNCTION ----- ----- ----- ----- -----

idToken :: Parser String
idToken = do
    c  <- letter
    cs <- many alphaNum
    return (c:cs)

----- ----- ----- ----- ----- Data types ----- ----- ----- ----- -----

data Def =
      DefCls YewType
    | DefFun YewFunc
    deriving Show

data YewType = YewType { typeName :: String, typeParams :: [String] } deriving Show
data YewFunc = YewFunc { funcName :: String, funcParams :: [(String, YewType)]} deriving Show
