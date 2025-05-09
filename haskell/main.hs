-- This code was written with the assistance of generative AI
module Main where
import Data.List (isPrefixOf)
import Data.Bool
import Debug.Trace (trace)

main :: IO ()
-- Do block for I/O
-- Reads in source file for testing
-- Prints tokenized source file
-- Prints rendered html and writes it to output.html
-- source.txt contains the text included in the portfolio problem
-- output.html when rendered by a browser should result in the 
-- same output to that included in the portfolio problem.
main = do
  source <- readFile "source.txt"
  
  let scanned = scan(source)
  putStrLn (show scanned)
  putStrLn (['\n'])
  let output = renderHTML scanned
  putStrLn (show output)
  writeFile "output.html" output

data Token = 
  HEADER1 String | HEADER2 String | HEADER3 String | PARAGRAPH String | BOLDL String | BOLDR String
  | OLIST String | ULIST String | ENDLIST | WHITESPACE String | TEXT String | EOF
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

-- Takes string, returns list of tokens
scan :: String -> [Token]
-- Base case, returns end of file
scan [] = [EOF]
scan source
  -- isPrefixOf checks if a string starts with another string
  -- Used with infix notation for clarity
  -- break splits strings into a tuple of lists at a condition (e.g. at a newline character)
  -- headTxt = before newline, rest = after newline. 
  -- As we don't want e.g. "0 " we drop these characters from rest
  -- Returns HEADER1 token with headTxt as its string for the head of the list, then calls
  -- rm_wspace on the rest of the source code, and recursively calls scan on the rest of 
  -- the source code.
  | "0 " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in HEADER1 (trim headTxt) : scan (rm_wspace rest)
  | "1 " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in HEADER2 (trim headTxt) : scan (rm_wspace rest)  
  | "2 " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in HEADER3 (trim headTxt) : scan (rm_wspace rest)  
  -- For ordered list items, delegate to scanList:
  | "# " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in OLIST (trim headTxt) : scanList (rm_wspace rest)
  -- For unordered list items, delegate to scanList:
  | "* " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in ULIST (trim headTxt) : scanList (rm_wspace rest)
  -- Does not break the line as bold text is terminated by a closing token
  -- not the end of a line.
  | "[" `isPrefixOf` source = 
      BOLDL "[" : scan(rm_wspace (drop 1 source))
  | "]" `isPrefixOf` source = 
      BOLDR "]" : scan(rm_wspace (drop 1 source))
  -- A double newline can indicate the end of a paragraph token.
  -- The condition in the function guard checks for a double newline and for a character indicating 
  -- a new token, then the paragraph token splits on a new line or bold text.
  | ("\n\n" `isPrefixOf` source) && not (take 2 (drop 2 source) `elem` ["0 ", "1 ", "2 ", "# ", "* "]) =
      let (headTxt, rest) = break (\c -> c == '\n' || c == '[' || c == ']') (drop 2 source)
      in PARAGRAPH (trim headTxt) : scan (rm_wspace rest)
  -- Used for text without a token e.g. after a bold open or bold close tag in a paragraph
  -- (a HTML paragraph tag adds a line break so has different semantics to this).
  -- If line is empty skip it, if not then scan the remaining text and return a TEXT token
  -- With the relevant text attached.
  | otherwise =
      let (headTxt, rest) = break (\c -> c == '\n' || c == '[' || c == ']') source
          remaining = rm_wspace rest
      in case (null headTxt, remaining) of
          (True, [])       -> [EOF]
          (True, _ : xs)   -> scan xs  -- Skip empty lines
          (False, _)       -> TEXT (trim headTxt) : scan remaining
          
  -- Helper function to scan inside a list.
-- It continues consuming list items until it detects the end of the list.
scanList :: String -> [Token]
scanList [] = [EOF]
scanList source
  -- If a double newline is encountered, end the list.
  | "\n\n" `isPrefixOf` source = ENDLIST : scan (rm_wspace (drop 2 source))
  -- If a single newline (but not a double) is found, skip it and continue scanning.
  | "\n" `isPrefixOf` source && not ("\n\n" `isPrefixOf` source) =
      scanList (rm_wspace (drop 1 source))
  -- Continue with ordered list items.
  | "# " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in OLIST (trim headTxt) : scanList (rm_wspace rest)
  -- Continue with unordered list items.
  | "* " `isPrefixOf` source =
      let (headTxt, rest) = break (== '\n') (drop 2 source)
      in ULIST (trim headTxt) : scanList (rm_wspace rest)
  -- Otherwise, the list has ended.
  | otherwise = ENDLIST : scan source

renderHTML :: [Token] -> String
renderHTML [] = ""
renderHTML (HEADER1 text : xs) = "<h1>" ++ text ++ "</h1>" ++ renderHTML xs
renderHTML (HEADER2 text : xs) = "<h2>" ++ text ++ "</h2>" ++ renderHTML xs
renderHTML (HEADER3 text : xs) = "<h3>" ++ text ++ "</h3>" ++ renderHTML xs

renderHTML (PARAGRAPH text : BOLDL _ : xs) =
  "<p>" ++ text ++ " " ++ renderHTML (BOLDL "[" : xs)  -- Workaround for inline bolding
renderHTML (PARAGRAPH text : xs) = "<p>" ++ text  ++ renderHTML xs

renderHTML (BOLDL text : xs) = "<b>" ++ renderHTML xs
renderHTML (BOLDR text : xs) = " </b>" ++ renderHTML xs

-- When encountering an ordered list token, collect all consecutive list items
renderHTML (OLIST text : xs) =
  let (items, rest) = collectOrderedItems xs
      listHTML = "<ol><li>" ++ text ++ "</li>" ++ concatMap (\t -> "<li>" ++ t ++ "</li>") items ++ "</ol>"
  in listHTML ++ renderHTML rest

-- When encountering an unordered list token, collect all consecutive list items
renderHTML (ULIST text : xs) =
  let (items, rest) = collectUnorderedItems xs
      listHTML = "<ul><li>" ++ text ++ "</li>" ++ concatMap (\t -> "<li>" ++ t ++ "</li>") items ++ "</ul>"
  in listHTML ++ renderHTML rest

renderHTML (TEXT text : xs) = text ++ renderHTML xs
renderHTML (ENDLIST : xs) = renderHTML xs  -- ENDLIST should be consumed by the helpers
renderHTML (EOF : _) = ""  -- End of file, stop rendering
renderHTML (_ : xs) = renderHTML xs  -- Ignore unknown tokens

-- Collect consecutive ordered list items until an ENDLIST is reached.
-- If the head of the list is another OLIST token, extract the text and recursively call it 
-- on the rest of the tokens until an ENDLIST token is encountered.
-- If any other token is encountered, stop collecting.
collectOrderedItems :: [Token] -> ([String], [Token])
collectOrderedItems [] = ([], [])
collectOrderedItems (OLIST text : xs) =
  let (restItems, rest) = collectOrderedItems xs
  in (text : restItems, rest)
collectOrderedItems (ENDLIST : xs) = ([], xs)
collectOrderedItems tokens = ([], tokens)

-- Similar to collectedOrderedItems but for unordered items.
collectUnorderedItems :: [Token] -> ([String], [Token])
collectUnorderedItems [] = ([], [])
collectUnorderedItems (ULIST text : xs) =
  let (restItems, rest) = collectUnorderedItems xs
  in (text : restItems, rest)
collectUnorderedItems (ENDLIST : xs) = ([], xs)
collectUnorderedItems tokens = ([], tokens)

-- TODO: leave marker for paragraph to close after right bold
-- marker has to either be a number that increments and then counts how many you have left
-- or a second inline bold in the paragraph probably breaks it
-- TODO: fix bug caused by paragraphs (but would extend to all tokens) not matching closing tags
-- but looking for newline characters etc
-- TODO: change renderer to include newlines in output
