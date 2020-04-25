
module Main where
import Data.Char
import Data.List as L

-- "JHMCM56557C404453" -- Random VIN numbers.
-- "4T1BF1FK9GU572575"

getCodes :: [(Char, Int)]
getCodes = a_h ++ j_r ++ s_z
        where a_h = zip ['A'..'H'] [1..8]
              j_r = zip [l | l <- ['J'..'R'], l /= 'Q', l /= 'O']
                                        [n | n <- [1..9], n /= 6]
              s_z = zip ['S'..'Z'] [2..9]

findChar :: [(Char, Int)] -> Char -> Char
findChar [] _ = ' '
findChar (x:xs)   n
    | (fst x)  == n = head . show . snd $ x
    | isDigit n =     n
    | otherwise =     findChar xs n

-- The algorithms above are used for the sake of convenience. 
-- They are rather slow, so i don't recomend to use them in
-- any real life scenario, especially when there are more than
-- couple of VIN codes. Use predifined values or array with
-- binary search instead. By the way, even without those
-- this piece of code seems to work pretty fast.

replaceLetters :: String -> String
replaceLetters [] = []
replaceLetters (x:xs) = findChar getCodes x
                        : replaceLetters xs

-- Valild sum should be equal 9th (from 1) element of the list.
isValid :: Char -> Int -> Bool
isValid n ctrl 
    | n == 'X' && ctrl == 10 = True
    | digitToInt  n == ctrl  = True
    | otherwise              = False

handleCode :: String -> (String, Bool)
handleCode str = let n1 = s `div` 11
                     n2 = n1 * 11
                     in   (str, isValid (str !! 8) (s - n2))
           where ints = map digitToInt $ replaceLetters str
                 to8  = zipWith  (*) (take 8 ints) [8,7,6,5,4,3,2,10]
                 to17 = zipWith  (*) (drop 9 ints) [9,8..2]
                 s    = sum $ to8 ++ to17

main :: IO ()
main = do print $ handleCode "4T1BF1FK9GU572575"
