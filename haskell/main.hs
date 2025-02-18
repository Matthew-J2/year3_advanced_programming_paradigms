module Main where
main :: IO ()
main = return ()
data Tokens = 
  HEADER1 | HEADER2 | HEADER3 | PARAGRAPH | BOLD | OLIST | ULIST | LISTITEM | WHITESPACE | EOF
  deriving(Show, Eq)
 