addSign :: String -> String
addSign = (++ "!")

main :: IO ()
main = putStrLn (addSign "Hello, World!")
