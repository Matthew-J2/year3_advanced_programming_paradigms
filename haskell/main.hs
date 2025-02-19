module Main where
main :: IO ()
main = do
  source <- readFile "source.txt"
  putStrLn source
  writeFile "output.html" source

data Token = 
  HEADER1 String | HEADER2 String | HEADER3 String | PARAGRAPH String | BOLD String 
  | OLIST String | ULIST String | LISTITEM String | WHITESPACE String | EOF
  deriving(Show, Eq)

scan :: String -> [Token] -- define type signature
scan [] = [] -- handles empty case

scan source 
  --if first 2 characters match, let the head and rest of the file = the current line minus the first 2 characters, 
  --and the rest of the file be the rest
  --create a token with the head of the text then call scan with the rest of the file
  --TODO: get rid of leading whitespace and write cases for other tokens
  | take 2 source == "0 " = 
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in HEADER1 headTxt : scan(rest)
  | source == "1 " = [HEADER2 "<h2> something </h2>"]
  | source == "2 " = [HEADER3 "<h3> something </h3>"]

