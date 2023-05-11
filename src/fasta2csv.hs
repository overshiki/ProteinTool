import Fasta
import FileSystem
-- getArgs 
import System.Environment
import System.IO

import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.FileSystem.File as File
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)

import qualified Data.ByteString.Char8 as DBC

main :: IO ()
main = do 
    args <- getArgs
    case args of 
        [fasta_file, csv_file, batch_size] -> do
            let
                b = (read batch_size) :: Int
                s = File.read fasta_file
                -- c = fmap (\x -> BS.w2c x) s
                d = Fold.foldlM' joinChar (emptyLineInit (Path csv_file) b)

            LineF (Line bs) p iocount current <- S.fold d s
            -- print bs
            append2file (Path csv_file) $ (DBC.unpack bs)

        otherwise -> print $ "usage: fasta_file csv_file batchsize"