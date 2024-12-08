module Lib
    ( messymain
    ) where
import Control.Applicative --(Alternative(..))
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
checkline str =  start : checkline finish where
    start = if snd (testfunc  str) == "" then "error" else snd (testfunc  str)
    finish = fst (testfunc  str)

testfunc :: [Char] -> (String, String)
testfunc str = 
    case runParser(some (optional (some (charinSeq " ")) *> some (charinSeq "><=1234567890") <* optional (charinSeq " ")) <* optional (charinSeq "\n")) str of
        Nothing -> (fst(fromJust (x str)), "") -- runParser (satysfy (/='\n')) )
        Just (d,s) -> (d, concat s)
    
x :: [Char] -> Maybe ([Char], [Char])
x = runParser  (some(satisfy (/='\n')) <* optional (charinSeq "\n"))

printandcalc :: [String] -> IO ()
printandcalc [] = putStrLn "Конец!!" -- Обработка пустого списка
printandcalc (x:xs) = 
    case runParser comparison x of
        Nothing ->  
             putStrLn "Ошибка чтения" >>
             printandcalc xs
        Just ("", s) -> 
            putStrLn ("Неравенство: " ++ x ++ " Результат: " ++ show s) >>
            printandcalc xs
        Just (_, _) ->
             putStrLn "Ошибка чтения" >>
             printandcalc xs
             
messymain :: String -> IO()   
messymain line = 
    let equotes = checkline  line
    in printandcalc equotes
    {-
    if null equotes then putStrLn "Файл пуст"
    else do 
        let res = getCalc (checkline line)
        --putStrLn "here 123"
        printResults equotes res-}

