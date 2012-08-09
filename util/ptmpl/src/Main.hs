
module Main where

import System.Environment
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
    putStrLn "Starting plain templater"
    args <- getArgs
    let outFile = head args
    input <- mapM readFile $ tail args
    -- TODO: parse files separately
    let outputE = parse ptemplParser "plain templater" $ unlines input
    case outputE of
        Left e -> print e
        Right output -> do
            putStrLn "Translation is completed successfully"
            writeFile outFile output

ptemplParser :: GenParser Char () String
ptemplParser = do
    res <- concat `fmap` many tmplFunc
    let header = "module Template where\n\n"
    return $ header ++ res

tmplFunc :: GenParser Char () String
tmplFunc = do
    try $ manyTill anyChar $
        try $ string "<!--h "
    spaces
    funcName <- many1 alphaNum
    spaces
    funcParams <- manyTill param $
        try $ string "h-->\n"
    lines' <- manyTill line $
        try $ string "<!--^^^-->" >> spaces
    return $ unwords (funcName : funcParams) ++ " = unlines [\"\",\n"
           ++ unlines lines'
           ++ " \"\"]\n"

param :: GenParser Char () String
param = do
    res <- many1 alphaNum
    spaces
    return res

line :: GenParser Char () String
line = do
    elements <- manyTill lineElement $ char '\n'
    return $ " \"\""
           ++ unwords elements
           ++ ","

lineElement :: GenParser Char () String
lineElement = inlineParam <|> plainString

inlineParam :: GenParser Char () String
inlineParam = do
    try $ string "!@#"
    res <- many1 alphaNum
    return $ "++" ++ res

plainString :: GenParser Char () String
plainString = do
    res <- manyTill anyChar
                    ( lookAhead (try $ string "!@#")
                      <|> lookAhead ((: []) `fmap` char '\n')
                    )
    return $ "++\"" ++ screenString res ++ "\""

screenString :: String -> String
screenString [] = []
screenString (x : xs) | x == '"' = '\\' : '"' : screenString xs
                      | otherwise = x : screenString xs
