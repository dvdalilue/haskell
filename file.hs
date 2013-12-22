import System.Environment

main :: IO ()
main = getArgs >>= print . haq . head

haq s = "Hola!!! " ++ s
