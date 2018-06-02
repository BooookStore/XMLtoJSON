module XMLElement (
    Printable,
    createXMLElement,
    putUpXMLElement,
    insertXMLElement,
) where

-- Express XML Element
data Element content = Element content [(Element content)] deriving (Show)

-- Type define string holding xmle element
type StringElement = Element String

instance Show (Element c) where
    show (Element c []) = 

-- Convert to String class
class Printable c where
    convert :: c -> String

-- Instantiate XMLStructure Convertable
instance (Show c) => Printable (Element c) where
    convert (Element c []) = show c
    convert (Element c xs) = show c ++ foldl (\acc x -> acc ++ show x) "" xs

createXMLElement :: String -> StringElement
createXMLElement content = Element content []

putUpXMLElement :: String -> StringElement -> StringElement
putUpXMLElement ca child = Element ca [child]

insertXMLElement :: String -> StringElement -> StringElement
insertXMLElement ca (Element cb xs) = Element cb (xs ++ [createXMLElement ca])
