main :: IO ()
main = do
    xmlContent <- getContents
    putStrLn $ converted xmlContent

converted :: String -> String
converted = lines