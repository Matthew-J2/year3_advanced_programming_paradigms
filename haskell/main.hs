module Main where
import Data.List (isPrefixOf)
main :: IO ()
main = do
  source <- readFile "source.txt"
  --putStrLn source
  let scanned = scan(source)
  putStrLn (show scanned)
  writeFile "output.html" source

data Token = 
  HEADER1 String | HEADER2 String | HEADER3 String | PARAGRAPH String | BOLDL String | BOLDR String
  | OLIST String | ULIST String | LISTITEM String | WHITESPACE String | EOF
  deriving(Show, Eq)

rm_wspace :: String -> String
rm_wspace [] = []
rm_wspace (srch : srct)
  | srch == ' ' || srch == '\n' = rm_wspace(srct)
  | otherwise = srch : srct

scan :: String -> [Token] -- define type signature
scan [] = [EOF] -- handles empty case
scan source 
  --if first 2 characters match, let the head and rest of the file = the current line minus the first 2 characters, 
  --and the rest of the file be the rest
  --create a token with the head of the text then call scan with the rest of the file
  --TODO: write cases for numbered/un-numbered lists
  --TODO: change scanner to split on tokens instead of just \n
  | "0 " `isPrefixOf` source = 
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in HEADER1 headTxt : scan(rm_wspace (rest))
  | "1 " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in HEADER2 headTxt : scan(rm_wspace(rest))
  | "2 " `isPrefixOf` source = 
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in HEADER3 headTxt : scan(rm_wspace (rest))
  | "# " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in OLIST headTxt : scan(rm_wspace (rest))
  | "* " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in ULIST headTxt : scan(rm_wspace (rest))
  | "[" `isPrefixOf` source = 
      BOLDL "[" : scan(rm_wspace (drop 1 source))
  | "]" `isPrefixOf` source = 
      BOLDR "]" : scan(rm_wspace (drop 1 source))
  | otherwise = let (headTxt, rest) = break (\c -> c == '\n' || c == '[' || c == ']') source
      in PARAGRAPH headTxt : scan(rm_wspace (rest))

