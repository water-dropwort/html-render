module TreeConstructor (
  ElementType,
  HNode,
  construct,
  toString)where

import           Data.Char   (toLower)
import           Data.Tree
import           Text.Parsec
import           Tokenizer

data ElementType = HTML|HEAD|BODY|HEADING|DIV|PARAGRAPH|UL|LI|NONE
                 deriving Show

data HNode = DOCUMENT
           | ELEMENT{tag::String, elemType::ElementType}
           | TEXT{text::String}
           deriving Show

execute :: String -> Tree HNode
execute html =
  case parse tokenizer "" html of
    Left _      -> error "execute error"
    Right token -> Node DOCUMENT (fst $ constructDomTree token)

construct :: [Token] -> Tree HNode
construct tokens = Node DOCUMENT (fst $ constructDomTree tokens)

toString :: Int -> Tree HNode -> [String]
toString inc t =
  if null (subForest t) then
    [s]
  else
    foldl (++) [s] $ map (toString (inc + 1)) (subForest t)
  where
    s = replicate (4 * inc) ' ' ++ show (rootLabel t)

constructDomTree :: [Token] -> ([Tree HNode],[Token])
constructDomTree []     = ([],[])
constructDomTree (x:xs) =
  case t_type x of
    DOCTYPE ->
      let (trees, tokens) = constructDomTree xs in
        (nodeElement x [] : trees , tokens)
    START_TAG
      | self_closing x ->
        let (trees, tokens) = constructDomTree xs in
          (nodeElement x [] : trees, tokens)
      | otherwise ->
        let
          (trees, tokens) = constructDomTree xs
          (trees', tokens') = constructDomTree tokens
        in
          (nodeElement x trees : trees', tokens')
    CHAR ->
      let (trees, tokens) = constructDomTree xs in
        (Node (TEXT $ char_data x) [] : trees, tokens)
    _ -> ([], xs)
    where
      nodeElement token subtree = Node (ELEMENT (tag_name token) (toElementType $ tag_name token)) subtree

toElementType :: String -> ElementType
toElementType s
  | isHtmlTag    s = HTML
  | isHeadTag    s = HEAD
  | isBodyTag    s = BODY
  | isHeadingTag s = HEADING
  | isDivTag     s = DIV
  | isParagraph  s = PARAGRAPH
  | isUl         s = UL
  | isLi         s = LI
  | otherwise      = NONE
  where
    isHtmlTag s = s == "html"
    isHeadTag s = s == "head"
    isBodyTag s = s == "body"
    isHeadingTag s = s == "h1" || s == "h2" || s == "h3" || s == "h4" || s == "h5" || s == "h6"
    isDivTag  s = s == "div"
    isParagraph s = s == "p"
    isUl s = s == "ul"
    isLi s = s == "li"
