module Main where
main :: IO ()
main = do
  source <- readFile "source.txt"
  putStrLn source
  writeFile "output.html" source

data Tokens = 
  HEADER1 | HEADER2 | HEADER3 | PARAGRAPH | BOLD | OLIST | ULIST | LISTITEM | WHITESPACE | EOF
  deriving(Show, Eq)

scan x 
  | x == "0" = HEADER1
  | x == "1" = HEADER2
  | x == "2" = HEADER2
