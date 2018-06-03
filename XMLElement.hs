module XMLElement where

type Name = String
type Content = String

-- Express XML Element
data Element = Element Name Content [Element]

instance Show Element where
    show (Element name content []) = "<" ++ name ++ ">" ++ content ++ "</" ++ name ++ ">"
    show (Element name content xs) = "<" ++ name ++ ">" ++ content ++ "\n\t" ++ print xs ++ "\n</" ++ name ++ ">"
        where print xs = foldl (\acc x -> acc ++ show x) "" xs

-- Define manipulation for xml element
class Manipulatable elem where
    insert :: elem -> elem -> elem

-- Instantiate Element to Manipulatable
instance Manipulatable Element where
    insert (Element name content xs) elem = Element name content (xs ++ [elem])

-- -- Convert to String class
-- class Printable c where
--     convert :: c -> String

-- -- Instantiate XMLStructure Convertable
-- instance (Show c) => Printable (Element c) where
--     convert (Element c []) = show c
--     convert (Element c xs) = show c ++ foldl (\acc x -> acc ++ show x) "" xs

-- create new xml element
createXMLElement :: Name -> Content -> Element
createXMLElement name content = Element name content []

-- insertXMLElement :: Element -> Element -> Element
-- insertXMLElement (Element name content xs) elem =

-- printContent :: Element c -> String
-- printContent (Element c []) = "Element has " ++ c
