--
-- EPITECH PROJECT, 2021
-- pushswap_checker
-- File description:
-- pushswap_checker
--

import Data.Char
import System.IO
import System.Environment
import System.Exit

-- Check params

isThisNumber :: [Char] -> Bool
isThisNumber [] = True
isThisNumber (x:xs)
    | (x == '-') || isDigit x = isThisNumber xs
    | otherwise = False

areArgsNumbers :: [String] -> Bool
areArgsNumbers [] = True
areArgsNumbers (x:xs)
    | isThisNumber x = areArgsNumbers xs
    | otherwise = False

isFunction :: [String] -> Bool
isFunction [] = True
isFunction (x:xs)
    | x == "sa" || x == "sb" || x == "sc" || x == "pa" || x == "pb" || x == "ra" || x == "rb" || x == "rr" || x == "rra" || x == "rrb"  || x == "rrr" = isFunction xs
    | otherwise = False

isAllOk :: [String] -> [String] -> Bool
isAllOk a b
    | isFunction a && areArgsNumbers b = True
    | otherwise = False

-- Functions

s :: [String] -> [String]
s [] = []
s [x] = [x]
s (xa : xb : xs) = xb : xa : xs

p :: [String] -> [String] -> [String]
p x [] = x
p x y = head y:x

r :: [String] -> [String]
r [] = []
r [x] = [x]
r (x : xs) = xs ++ [x]

rr :: [String] -> [String]
rr [] = []
rr [x] = [x]
rr x = last x : init x

-- Pushswap checker

isSort :: [String] -> Bool
isSort [] = True
isSort [x] = True
isSort (x:y:xs) = x <= y && isSort (y:xs)

pushswapChecker :: [String] -> [String] -> [String] -> Bool
pushswapChecker [] l_a l_b
    | isSort l_a = True
    | otherwise = False
pushswapChecker a l_a l_b
    | head a == "sa" = pushswapChecker (drop 1 a) (s l_a) l_b
    | head a == "sb" = pushswapChecker (drop 1 a) l_a (s l_b)
    | head a == "sc" = pushswapChecker (drop 1 a) (s l_a) (s l_b)
    | head a == "pa" = pushswapChecker (drop 1 a) (p l_a l_b) (tail l_b)
    | head a == "pb" = pushswapChecker (drop 1 a) (tail l_a) (p l_b l_a)
    | head a == "ra" = pushswapChecker (drop 1 a) (r l_a) l_b
    | head a == "rb" = pushswapChecker (drop 1 a) l_a (r l_b)
    | head a == "rr" = pushswapChecker (drop 1 a) (r l_a) (r l_b)
    | head a == "rra" = pushswapChecker (drop 1 a) (rr l_a) l_b
    | head a == "rrb" = pushswapChecker (drop 1 a) l_a (rr l_b)
    | head a == "rrr" = pushswapChecker (drop 1 a) (rr l_a) (rr l_b)
    | otherwise = False

main = do
a <- getLine
let b = words a
c <- getArgs
if isAllOk b c && pushswapChecker b c [] then
        putStrLn "OK" >> exitSuccess
else
    putStrLn "KO" >> exitWith (ExitFailure 84)