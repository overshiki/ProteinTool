import FileSystem
import System.Environment
import System.IO

data FastaRead = FastaRead String String deriving (Eq)
data Fasta = Fasta [FastaRead] deriving (Eq)

instance Show FastaRead where 
    show (FastaRead head content) = head ++ ", " ++ content

instance Show Fasta where 
    show (Fasta xs) = cs
        where 
            Content cs = chain (map (Content . show) xs)
instance Writable Fasta

get_nth :: Int -> Fasta -> FastaRead
get_nth index (Fasta rs) = rs !! index

empty :: Fasta
empty = Fasta []

singleton :: FastaRead -> Fasta
singleton r = Fasta [r]

join :: Fasta -> String -> Fasta 
join (Fasta af@((FastaRead head content):fs)) ax@(x:xs)
    | x=='>'    = Fasta ((FastaRead ax ""):af)
    | otherwise = Fasta (nf:fs)
        where
            nax = if (end ax)=='\n' then body ax else ax
            nf = FastaRead head (content ++ ax)


read_fasta :: Path -> IO Fasta
read_fasta (Path path) = do 
    (x:xs) <- readFromFile path 
    let h = singleton (FastaRead x "")
        fasta = foldl join h xs
    return fasta


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

to_csv :: Path -> Fasta -> IO ()
-- to_csv csv_path fasta = write fasta (Path csv_path)
to_csv csv_path (Fasta (r:rs)) = do 
    write2file csv_path (show r)
    mapM_ (\r -> append2file csv_path (show r)) rs

-- "../example/BLAT_ECOLX_1_b0.5_lc_weights.fa"
-- real    7m40.034s, n=14692
-- real    0m32.880s, n=14692

main :: IO ()
main = do 
    [fasta_path, csv_path] <- getArgs
    fasta <- read_fasta (Path fasta_path)
    to_csv (Path csv_path) fasta