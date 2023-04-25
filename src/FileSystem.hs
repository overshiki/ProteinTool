module FileSystem (endInd, end, body,
                slice, sliceFromBegin, sliceToEnd, groupBy, reducel,
                writeToFile, write2file, append2file,
                readFromFile, listDir, fmkdir,
                matchString, isIn, sep2list,
                Content(..), cbind, comma, bindWith, chainWith,
                Writable(..),
                Chainable(..),
                Command(..), genCommand,
                build, run, readRun, copyto, rm, mkdir,
                Path(..), TarFile, DirPath,
                    ) where 
import System.Directory
import System.Process
import System.IO
import Data.List.Split

-- utils
slice :: Int -> Int -> [a] -> [a]
slice s l xs = map (xs !! ) [s..end] where end = min ((length xs)-1) (s+l-1)

sliceFromBegin :: Int -> [a] -> [a]
sliceFromBegin = slice 0

sliceToEnd :: Int -> [a] -> [a]
sliceToEnd s xs = slice s l xs where l = length xs

groupBy :: Int -> [a] -> [[a]]
groupBy num xs
            | length xs <= num = [xs]
            | otherwise        = (sliceFromBegin num xs):(groupBy num (sliceToEnd num xs))

endInd :: [a] -> Int 
endInd xs = (length xs) - 1

end :: [a] -> a 
end xs = xs !! (endInd xs)

body :: [a] -> [a]
body xs = map (xs !!) [0..(endInd xs)-1]

reducel :: (a -> a -> a) -> [a] -> a 
reducel func (x:xs) = foldl func x xs



type Target = String 
type Source = String 

_matchString :: Target -> Source -> Bool
_matchString (x:xs) (y:ys)
                | x==y          = _matchString xs ys
                | otherwise     = False
_matchString [] _ = True
_matchString _ [] = False

matchString :: Target -> Source -> Bool
matchString ax ay = any id vec 
        where 
            slide match_func x ay@(y:ys) = (match_func x ay):(slide match_func x ys)
            slide match_func x [] = []
            vec = slide _matchString ax ay

isIn :: String -> [String] -> Bool 
isIn target source = any id (map (matchString target) source)


sep2list :: Target -> Source -> [String]
sep2list sep xs = filter (\x -> (length x) > 0) alist
        where 
            alist = splitOn sep xs 



-- typeclass 
class (Show c) => Writable c where 
    write :: c -> Path -> IO ()
    write c (Path file) = do 
            sFile <- openFile file WriteMode
            hPutStrLn sFile (show c)
            hClose sFile


class (Semigroup c) => Chainable c where 
    chain :: [c] -> c 
    chain (x:xs) = foldl (<>) x xs 


-- Path 
data Path = Path String
instance Semigroup Path where 
    (<>) (Path x) (Path y) = Path (cjoin x y)
            where 
                cjoin x ay@(y:ys)
                        | (end x) == '/' && y == '/' = x ++ ys 
                        | (end x) == '/' || y == '/' = x ++ ay 
                        | otherwise                  = x ++ "/" ++ ay
instance Show Path where 
    show (Path x) = x


type TarFile = Path 
type DirPath = Path


-- Content 
data Content = Content String
instance Semigroup Content where 
    (<>) (Content ca) (Content cb) = Content (ca ++ "\n" ++ cb) 

cbind :: Content -> (String -> String) -> Content 
cbind (Content x) func = Content (func x)

instance Show Content where 
    show (Content c) = c

instance Chainable Content 
instance Writable Content

comma :: Content -> Content
comma (Content x) = Content (x ++ ":") 

bindWith :: String -> Content -> Content -> Content
bindWith sep (Content x) (Content y) = Content (x ++ sep ++ y)

chainWith :: String -> [Content] -> Content
chainWith sep (x:xs) = foldl (><) x xs where (><) x y = bindWith sep x y


-- Command 
data Command = Command String

instance Semigroup Command where 
    (<>) (Command xa) (Command xb) = Command (xa ++ "\n" ++ xb)
instance Show Command where 
    show (Command x) = x

instance Chainable Command

genCommand :: [String] -> Command 
genCommand (x:xs) = Command $ foldl ccat x xs where ccat x y = x ++ " " ++ y


copyto :: String -> String -> Command
copyto from to = Command ("cp " ++ from ++ " " ++ to)

-- some Command instance 
rm :: Path -> Command
rm (Path file) = genCommand ["rm", "-rf", file]

mkdir :: Path -> Command
mkdir (Path file) = genCommand ["mkdir", file]

-- run the Command 
run :: Command -> IO ()
run (Command c) = callCommand c

readRun :: Command -> IO String
readRun (Command c) = readProcess x xs "" where (x:xs) = splitOn " " c 

build :: Command -> Path -> IO ()
build (Command c) file = do 
        writeToFile (lines c) file


-- io 
writeToFile :: [String] -> Path -> IO ()
writeToFile contents file = do 
        -- sFile <- openFile file WriteMode
        let 
            c = map Content contents
            cc = chain c 
        write cc file
        -- hPutStrLn sFile (show cc)
        -- hClose sFile
 

write2file :: Path -> String -> IO ()
write2file (Path file) content = do 
    sFile <- openFile file WriteMode
    hPutStrLn sFile content
    hClose sFile

append2file :: Path -> String -> IO ()
append2file (Path file) content = do 
    sFile <- openFile file AppendMode
    hPutStrLn sFile content
    hClose sFile



readFromFile :: FilePath -> IO [String]
readFromFile file = do 
        contents <- readFile file 
        return (lines contents)

listDir :: FilePath -> IO [String]
listDir data_dir = do 
    files <- readProcess "ls" [data_dir] []
    return (lines files)

fmkdir :: Path -> IO ()
fmkdir (Path p) = createDirectoryIfMissing True p