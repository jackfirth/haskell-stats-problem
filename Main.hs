import           System.Process

main :: IO ()
main = generateRaw 3 >>= putStrLn


generateRaw :: Integer -> IO String
generateRaw n = readProcess "./generator" [show n] ""
