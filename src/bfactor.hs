-- retrive bfactor from pdb file 
import FileSystem
import Format
import System.Environment

data Position = Position Float Float Float deriving (Show, Eq)
type Bfactor = Float
data Atom = Atom String Position Bfactor deriving (Eq)
data Residule = Residule String [Atom] deriving (Eq)
data Protein = Protein [Residule] deriving (Show, Eq)

instance Show Atom where 
    show (Atom id (Position x y z) bfactor) = sline
            where 
                Format sline = foldl (<>) 
                                (Format "ATOM: {}, {}, {}, {}, {}") 
                                ((Format id):(map (Format . show) [x, y, z, bfactor]))

instance Show Residule where 
    show (Residule rtype xs) = r
            where 
                nxs = map (\x -> ((Format "RES: {}, {}") <> (Format rtype)) <> (Format $ show x)) xs 
                nnxs = map (\(Format x) -> Content x) nxs 
                Content r = reducel (<>) nnxs


get_atom :: Residule -> Int -> Atom
get_atom (Residule rtype xs) index = xs !! index 

process_line :: String -> (String, Atom)
process_line r = (res, (Atom id (Position x y z) bf))
        where 
            alist = sep2list " " r 
            id = alist !! 1 
            res = alist !! 3
            bf = read (alist !! 10) :: Float
            x = read (alist !! 6) :: Float
            y = read (alist !! 7) :: Float
            z = read (alist !! 8) :: Float


_reduce :: [Residule] -> (String, Atom) -> [Residule]
_reduce rs@((Residule _name atoms):xs) (name, atom)
            | _name==name = (Residule name (atom:atoms)):xs 
            | otherwise   = (Residule name [atom]):rs 
_reduce [] (name, atom)   = (Residule name [atom]):[]

fold2residules :: [(String, Atom)] -> [Residule]
fold2residules xs = foldr (flip _reduce) [] xs

parse :: String -> IO Protein
parse pdb_file = do 
        reads <- readFromFile pdb_file
        let freads = filter (matchString "ATOM") reads
            info = map process_line freads
            res = fold2residules info
        return (Protein res)

atom_bfactor :: Atom -> Float
atom_bfactor (Atom name pos bf) = bf

residule_bfactors :: Residule -> [Float]
residule_bfactors (Residule name xs) = map atom_bfactor xs 

residule_numAtoms :: Residule -> Int 
residule_numAtoms (Residule name xs) = length xs 

numAtoms :: [Residule] -> Int 
numAtoms xs = sum (map residule_numAtoms xs)

integerToFloat :: Int -> Float
integerToFloat x = read (show x) :: Float


plevel_bfactor :: [Residule] -> Float
plevel_bfactor res = sum (map sum bf)
        where 
            bf = map residule_bfactors res 

instance Writable Protein where 
    write (Protein res) save_path = do 
            let c = reducel (<>) (map (Content . show) res)
            write c save_path

main :: IO ()
main = do 
    args <- getArgs 
    let [pdb_file, save_path] = args
    -- res <- parse "../example/s8081_pred.pdb"
    Protein res <- parse pdb_file
    write (Protein res) (Path save_path)
    -- let bf = plevel_bfactor res
    --     num = integerToFloat (numAtoms res)  
    --     m_bf = bf / num 
    -- print m_bf

    -- print $ get_atom (res !! 0) 0
    -- print $ (res !! 0)