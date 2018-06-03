module XMLElement where

type Name = String
type Content = String

-- Express XML Element
data Element = Element Name Content [Element]

instance Show Element where
    show (Element name content []) = "<" ++ name ++ ">" ++ content ++ "</" ++ name ++ ">"
    show (Element name content xs) = "<" ++ name ++ ">" ++ content ++ "\n\t" ++ print xs ++ "\n</" ++ name ++ ">"
        where print xs = foldl (\acc x -> acc ++ show x) "" xs

-- -- Convert to String class
-- class Printable c where
--     convert :: c -> String

-- -- Instantiate XMLStructure Convertable
-- instance (Show c) => Printable (Element c) where
--     convert (Element c []) = show c
--     convert (Element c xs) = show c ++ foldl (\acc x -> acc ++ show x) "" xs

-- createXMLElement :: String -> Element c
-- createXMLElement content = Element content []

-- putUpXMLElement :: String -> Element c -> Element c
-- putUpXMLElement ca child = Element ca [child]

-- insertXMLElement :: String -> Element c -> Element c
-- insertXMLElement ca (Element cb xs) = Element cb (xs ++ [createXMLElement ca])

-- printContent :: Element c -> String
-- printContent (Element c []) = "Element has " ++ c
