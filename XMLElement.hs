module XMLElement (
    XMLElement(..)
) where

-- Express XML Element
data XMLElement content = 
      Element content (XMLElement content) | None
      deriving (Show)

-- Convert to String class
class Convertable c where
    convert :: c -> String

-- Instantiate XMLElement Convertable
instance (Show c) => Convertable (XMLElement c) where
    convert (Element c None) = show c
    convert (Element c elem) = show c ++ " - " ++convert elem
    convert None = ""