module Fasta (
    Line(..),
    LineF(..),
    emptyLineInit, joinChar
) where
import FileSystem
-- getArgs 
import System.Environment
import System.IO

import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.FileSystem.File as File
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)

import Data.ByteString (ByteString, snoc, singleton)
import qualified Data.ByteString.Char8 as DBC
import Data.Word
import Data.Maybe
-- import Data.Text hiding (snoc, singleton)

data FastaRead = FastaRead String String | EmptyFR
-- data Fasta = Fasta [FastaRead] deriving (Eq)

instance Show FastaRead where 
    show (FastaRead head content) = head ++ ", " ++ content
    show EmptyFR = "empty FastaRead!!!"

data Line = Line ByteString | EmptyLine deriving (Eq)

instance Show Line where 
    show (Line s) = DBC.unpack s 
    show EmptyLine = "EmptyLine!!!"

data LineF = LineF Line Path Int Int

emptyLineInit :: Path -> Int -> IO LineF 
emptyLineInit path iocount = do 
    write2file path "id,seq"
    return $ LineF EmptyLine path iocount 0


rev_first_case :: Char -> Char -> ByteString -> Maybe Char
rev_first_case c1 c2 s = do 
    (_body, _last) <- DBC.unsnoc s 
    case ((_last==c1), (_last==c2)) of 
        (True, False) -> return c1 
        (False, True) -> return c2 
        otherwise -> rev_first_case c1 c2 _body


joinChar :: LineF -> Word8 -> IO LineF
joinChar (LineF l path iocount current) c = case (DBC.unpack $ singleton c) of 
    ">" -> case l of 
        EmptyLine -> do 
            -- write2file path ""
            return $ LineF (Line $ singleton c) path iocount current
        Line s -> case (iocount==current) of 
            True -> do 
                append2file path $ (DBC.unpack s)
                return $ LineF (Line (singleton c)) path iocount 0
            False -> do
                let 
                    ns = snoc (DBC.snoc s '\n') c
                return $ LineF (Line ns) path iocount (current + 1)

    "\n" -> do 
        let 
            (Line s) = l
        case (rev_first_case '>' ',' s) of
            (Just '>') -> return $ LineF (Line $ DBC.snoc s ',') path iocount current
            (Just ',') -> return $ LineF l path iocount current
            otherwise  -> return $ LineF l path iocount current
    " " -> do 
        return $ LineF l path iocount current

    otherwise -> do 
            let (Line s) = l
                ns = snoc s c
            return $ LineF (Line ns) path iocount current



-- fileStream :: S.Stream IO Char 
-- fileStream = let


        

-- to_csv :: Path -> Fasta -> IO ()
-- to_csv csv_path (Fasta (r:rs)) = do 
--     write2file csv_path (show r)
--     mapM_ (\r -> append2file csv_path (show r)) rs