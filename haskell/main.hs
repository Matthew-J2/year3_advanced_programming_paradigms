module Main where
import Data.List (isPrefixOf)
import Data.Bool
import Debug.Trace (trace)

main :: IO ()
main = do
  source <- readFile "source.txt"
  --putStrLn source
  let scanned = scan(source)
  putStrLn (show scanned)
  putStrLn (['\n'])
  let output = renderHTML scanned
  putStrLn (show output)
  writeFile "output.html" output

data Token = 
  HEADER1 String | HEADER2 String | HEADER3 String | PARAGRAPH String | BOLDL String | BOLDR String
  | OLIST String | ULIST String | LISTITEM String | WHITESPACE String | TEXT String | EOF
  deriving(Show, Eq)


rm_wspace :: String -> String
rm_wspace [] = []
rm_wspace ('\n':'\n':xs) = '\n':'\n':rm_wspace xs  -- Preserve paragraph breaks
rm_wspace (' ' : xs) = rm_wspace xs                -- Remove spaces
rm_wspace ('\n' : xs)
  | take 2 xs `elem` ["0 ", "1 ", "2 ", "# ", "* "] = '\n' : rm_wspace xs  -- Keep leading newlines for headers and lists
  | otherwise = rm_wspace xs  -- Remove other single newlines
rm_wspace xs = xs

trim :: String -> String
trim = unwords . words  -- Removes leading/trailing spaces but keeps content

scan :: String -> [Token]
scan [] = [EOF]
scan source
  | "0 " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in HEADER1 (trim headTxt) : scan (rm_wspace rest)
  | "1 " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in HEADER2 (trim headTxt) : scan (rm_wspace rest)  
  | "2 " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in HEADER3 (trim headTxt) : scan (rm_wspace rest)  
  | "# " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in OLIST (trim headTxt) : scan (rm_wspace rest)
  | "* " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in ULIST (trim headTxt) : scan (rm_wspace rest)
  | "[" `isPrefixOf` source = 
      BOLDL "[" : scan(rm_wspace (drop 1 source))
  | "]" `isPrefixOf` source = 
      BOLDR "]" : scan(rm_wspace (drop 1 source))
  | ("\n\n" `isPrefixOf` source) && not (take 2 (drop 2 source) `elem` ["0 ", "1 ", "2 ", "# ", "* "]) =
      let (headTxt, rest) = break (\c -> c == '\n' || c == '[' || c == ']') (drop 2 source)
      in PARAGRAPH (trim headTxt) : scan (rm_wspace rest)
  | otherwise =
      let (headTxt, rest) = break (\c -> c == '\n' || c == '[' || c == ']') source
          remaining = rm_wspace rest
      in if null headTxt
         then case remaining of
              [] -> [EOF]
              (_:xs) -> scan xs  -- Skip empty lines
         else TEXT (trim headTxt) : scan remaining

renderHTML :: [Token] -> String
renderHTML [] = ""
renderHTML (HEADER1 text : xs) = "<h1>" ++ text ++ "</h1>" ++ renderHTML xs
renderHTML (HEADER2 text : xs) = "<h2>" ++ text ++ "</h2>" ++ renderHTML xs
renderHTML (HEADER3 text : xs) = "<h3>" ++ text ++ "</h3>" ++ renderHTML xs

renderHTML (PARAGRAPH text : BOLDL _ : xs) = "<p>" ++ text ++ " " ++ renderHTML (BOLDL "[" : xs)  -- Workaround for inline bolding
renderHTML (PARAGRAPH text : xs) = "<p>" ++ text ++ "</p>" ++ renderHTML xs

renderHTML (BOLDL text : xs) = "<b>" ++ renderHTML xs
renderHTML (BOLDR text : xs) = " </b>" ++ renderHTML xs
renderHTML (OLIST text : xs) = "<ol><li>" ++ text ++ "</li></ol>" ++ renderHTML xs
renderHTML (ULIST text : xs) = "<ul><li>" ++ text ++ "</li></ul>" ++ renderHTML xs
renderHTML (TEXT text : xs) = text ++ renderHTML xs
renderHTML (EOF : _) = ""  -- End of file, stop rendering
renderHTML (_ : xs) = renderHTML xs  -- Ignore unknown tokens

-- TODO: leave marker for paragraph to close after right bold
-- marker has to either be a number that increments and then counts how many you have left
-- or a second inline bold in the paragraph probably breaks it
-- TODO: fix lists
-- TODO: change renderer to include newlines in output
