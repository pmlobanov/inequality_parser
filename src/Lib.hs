module Lib
    ( messymain
    ) where
import Control.Applicative (Alternative(..))
import Data.Char (digitToInt,isDigit)
import Data.Maybe
newtype Parser tok a = Parser { runParser :: [tok] -> Maybe ([tok],a) }

instance Functor (Parser tok) where
   fmap g = Parser . (fmap . fmap . fmap) g . runParser

instance Applicative (Parser tok) where 
    pure x =Parser $ \s-> Just(s,x)
    Parser u <*> Parser v = Parser f where
        f xs = case u xs of 
            Nothing       -> Nothing
            Just (xs', g) -> case v xs' of 
                Nothing        -> Nothing
                Just (xs'', x) -> Just (xs'', g x)

instance Alternative (Parser tok) where
  empty = Parser $ const Nothing
  
  Parser u <|> Parser v = Parser f where 
    f xs = case u xs of
      Nothing -> v xs
      z       -> z

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy pr = Parser f where
    f (c:cs) | pr c = Just (cs,c)
    f _ = Nothing

comparison :: Parser Char Bool
comparison = ((==) <$> digits <* char '=' <*> digits) 
            <|> ((>) <$> digits <* char '>' <*> digits)
            <|> ((<) <$> digits <* char '<' <*> digits)

char :: Char -> Parser Char Char
char c = satisfy (== c)

charinSeq :: [Char] -> Parser Char Char
charinSeq cs = satisfy (`elem` cs)

digits :: Parser Char Int
digits =  convInt <$> many (digitToInt <$> satisfy isDigit)

convInt :: [Int] -> Int 
convInt [ ] = 0
convInt list = last list + convInt (init list) * 10

--checkline :: String -> [String]
checkline :: [Char] -> [String]
checkline "" = []
checkline str =  start :  checkline finish where
    start = snd (fromJust (runParser (some(charinSeq "><=1234567890")<* some(charinSeq "\n")) str))
    finish = fst (fromJust (runParser (some(charinSeq "><=1234567890")<* some(charinSeq "\n")) str))

--getCalc list = snd (fromJust(runParser comparison (head list)))

getCalc :: [[Char]] -> [Bool]
getCalc [] = []
getCalc list =  do 
    start : getCalc (tail list) where
    start = snd (fromJust(runParser comparison (head list)))

printResults:: [String]-> [Bool] -> IO()
printResults [l1] [l2] = do 
    putStrLn $ "Неравенство : "++  l1 ++" Результат: "++ show l2
printResults list listres  = do
    putStrLn $ "Неравенство : "++ head list ++" Результат: "++ show (head listres) 
    printResults  (tail list) (tail listres)

messymain :: String -> IO()   
messymain line = do
    let equotes = checkline  line
    if null equotes then putStrLn "Файл пуст"
    else do 
        let res = getCalc (checkline  line)
        --putStrLn "here 123"
        printResults equotes res

