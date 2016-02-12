{-
meant to be compiled and run
$ ghi readIO.hs
$ ./readIO
i am a file
i have some data
goodbye
-}

addFoo :: [Char] -> [Char]
addFoo str = str ++ "FOO!"

messUpFile :: [Char] -> [Char]
messUpFile fileStr = unlines $ map addFoo $ lines fileStr

putThing :: [Char] -> IO ()
putThing  = putStrLn

readThing :: [Char] -> IO String
readThing filename = readFile filename >>= readFile . head . lines

readFile' = readFile . head . lines

main = do
    --let readAction1 = readFile "contains_filename.txt"
    --let readAction2 = readAction1 >>= readFile . head . lines
    --readAction2 >>= putStrLn


    --readFile "contains_filename.txt" >>= readFile . head . lines >>= putStrLn


    --file1 <- readFile "contains_filename.txt"
    --file2 <- (readFile . head . lines) file1
    --putStrLn file2


    -- readThing "contains_filename.txt" >>= putThing


    file1 <- readFile "contains_filename.txt"

    let file1M = return file1

    file1again <- file1M

    readFile' file1again  >>= putStrLn


