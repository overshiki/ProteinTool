module Format (Format(..), replaceOnce) where 
import FileSystem


data Format = Format String deriving (Show, Eq)

instance Semigroup Format where 
    (<>) (Format (x:y:xs)) (Format t)
            | x=='{' && y=='}' = Format (t ++ xs)
            | otherwise        = (Format [x]) <+> ((Format (y:xs)) <> (Format t))
            where 
                (<+>) (Format x) (Format y) = Format (x++y)

    (<>) (Format [x]) (Format t) = Format [x]


type Target = String
replaceOnce :: String -> String -> Target -> String 
replaceOnce pattern term ax@(x:xs)
        | (sliceFromBegin (length pattern) ax) == pattern = term ++ (sliceToEnd (length pattern) ax)
        | otherwise = x:(replaceOnce pattern term xs)

main :: IO ()
main = do 
    let x = Format "/job/save/group{}_{}"
        y = Format "10"
    print $ (x <> y) <> y

    let x = "axs(hh)"
    print $ replaceOnce ")" "" $ replaceOnce "(" "" x 