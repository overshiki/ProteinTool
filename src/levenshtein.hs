-- import Control.Monad (forM)
-- import System.Directory (doesDirectoryExist, getDirectoryContents)
-- import System.FilePath ((</>))


levenshtein :: [Char] -> [Char] -> Int
levenshtein x [] = length x 
levenshtein [] x = length x
levenshtein (x:xs) (y:ys)
    | x==y      = levenshtein xs ys 
    | otherwise = 1 + min (min (levenshtein xs (y:ys)) (levenshtein (x:xs) ys)) (levenshtein xs ys)


-- test = levenshtein "kitten" "sitting"

-- simpleFileSize :: FilePath -> IO Integer

-- simpleFileSize path = do
--   h <- openFile path ReadMode
--   size <- hFileSize h
--   hClose h
--   return size
lc = "HPETLVKVKDAEDQLGARVGYIELDLNSGKILESFRPEERFPMMSTFKVLLCGAVLSRVDAGQEQLGRRIHYSQNDLVEYSPVTEKHLTDGMTVRELCSAAITMSDNTAANLLLTTIGGPKELTAFLHNMGDHVTRLDRWEPELNEAIPNDERDTTMPAAMATTLRKLLTGELLTLASRQQLIDWMEADKVAGPLLRSALPAGWFIADKSGAGERGSRGIIAALGPDGKPSRIVVIYTTGSQATMDERNRQIAEIGASLIKHW"
rc = "NQLKNLSAMYPSIHPSIYVWDYDTGNYADVNADEVFPTASIIKLPVLVQLFRSIEKNQLTIYDEMPLTEYYRTEGSGSLQFKAANSKYTIDTLARMMITESDNSATNMIMARLGSMTDINQGLREWGLKHTYVQTWLPDLGGTNHSTARDMATILYNIDNPQFLSTSSREKIFDYMGHVHNNRLIAAGLPAGAEFLHKTGDIGKMLGDAGIVFAPNGKKYIVVIFANRPHNSPLGKEFIVKASETIY"

main :: IO ()
main = do
    print $ levenshtein lc rc