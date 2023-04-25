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

data FastaRead = FastaRead String String | EmptyFR
-- data Fasta = Fasta [FastaRead] deriving (Eq)

instance Show FastaRead where 
    show (FastaRead head content) = head ++ ", " ++ content
    show EmptyFR = "empty FastaRead!!!"

-- data FastaReadF = FastaReadF FastaRead Path

-- -- note that this is a special b->a->IO b function. 
--     -- it will append to file when b is ready, return an empty b 
--     -- otherwise, do normal concatenate operation  
-- join :: FastaReadF -> String -> IO FastaReadF 
-- join (FastaReadF fr path) ax@(x:xs) = do
--     case x of 
--         '>' -> case fr of 
--             EmptyFR -> return (FastaReadF (FastaRead ax "") path)
--             FastaRead head content -> do 
--                 append2file path (show fr)
--                 return (FastaReadF (FastaRead ax "") path)

--         otherwise -> let 
--             FastaRead head content = fr
--             nax = if (end ax)=='\n' then body ax else ax
--             nf = FastaRead head (content ++ nax)
--             in return (FastaReadF nf path)


-- emptyInit :: Path -> IO FastaReadF
-- emptyInit path = do
--     write2file path ""
--     return FastaReadF EmptyFR path 


data Line = Line String | EmptyLine deriving (Eq)

instance Show Line where 
    show (Line s) = s 
    show EmptyLine = "EmptyLine!!!"

data LineF = LineF Line Path Int Int

emptyLineInit :: Path -> Int -> IO LineF 
emptyLineInit path iocount = do 
    write2file path "id,seq"
    return $ LineF EmptyLine path iocount 0

joinChar :: LineF -> Char -> IO LineF
joinChar (LineF l path iocount current) c = case c of 
    '>' -> case l of 
        EmptyLine -> do 
            -- write2file path ""
            return $ LineF (Line [c]) path iocount current
        Line s -> case (iocount==current) of 
            True -> do 
                append2file path s 
                return $ LineF (Line [c]) path iocount 0
            False -> do
                let (Line s) = l
                    ns = s ++ [c]
                return $ LineF (Line ns) path iocount (current + 1)

    -- '\n' -> do 
        -- let (Line s) = l
        -- return $ LineF l path iocount current
    ' ' -> do 
        return $ LineF l path iocount current

    otherwise -> do 
            let (Line s) = l
                ns = s ++ [c]
            return $ LineF (Line ns) path iocount current



-- fileStream :: S.Stream IO Char 
-- fileStream = let


        

-- to_csv :: Path -> Fasta -> IO ()
-- to_csv csv_path (Fasta (r:rs)) = do 
--     write2file csv_path (show r)
--     mapM_ (\r -> append2file csv_path (show r)) rs