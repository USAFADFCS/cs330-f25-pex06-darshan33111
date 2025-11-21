-- pex6.hs 
-- unKnot Haskell

-- name: C2C Darshan Kiran Koushik

{- DOCUMENTATION:
Used the following to understand Haskell guards a bit better: https://www.futurelearn.com/info/courses/functional-programming-haskell/0/steps/27226 
Used the following to understand Haskell boolean a bit better: https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Bool.html
Used the following to understand last and init better: 
http://www.zvon.org/other/haskell/Outputprelude/last_f.html
http://www.zvon.org/other/haskell/Outputprelude/init_f.html
-}

unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "not a knot"
   | isTypeIKnot tripCode = unKnot (removeTypeI tripCode)
   | isTypeIKnotWrap tripCode = unKnot (removeTypeIWrap tripCode)
   | isTypeIIKnot tripCode = unKnot (removeTypeII tripCode)
   | isTypeIIKnotWrap tripCode = unKnot (removeTypeIIWrap tripCode)
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)


isTypeIKnot :: [(Char, Char)] -> Bool
isTypeIKnot tripCode
   | null tripCode = False 
   | length tripCode < 2 = False 
   | fst (head tripCode) == fst (head (tail tripCode)) = True 
   | otherwise = isTypeIKnot (drop 1 tripCode) 

removeTypeI :: [(Char, Char)] -> [(Char, Char)]
removeTypeI tripCode
   | null tripCode = []
   | length tripCode < 2 = tripCode
   | fst (head tripCode) == fst (head (tail tripCode)) = (drop 2 tripCode) 
   | otherwise = (take 1 tripCode) ++ removeTypeI (drop 1 tripCode)

isTypeIKnotWrap :: [(Char, Char)] -> Bool
isTypeIKnotWrap tripCode
   | null tripCode = False 
   | length tripCode < 2 = False 
   | fst (head tripCode) == fst (last tripCode) = True 
   | otherwise = False 

removeTypeIWrap :: [(Char, Char)] -> [(Char, Char)]
removeTypeIWrap tripCode 
   | null tripCode = []
   | length tripCode < 2 = tripCode 
   | otherwise = tail (init tripCode)


isTypeIIKnot :: [(Char, Char)] -> Bool
isTypeIIKnot tripCode
   | null tripCode = False 
   | length tripCode < 4 = False 
   | snd (head tripCode) == snd (head (tail tripCode)) && (isTypeIIKnotHelp (fst (head tripCode)) (fst (head (tail tripCode))) (drop 2 tripCode)) = True 
   | otherwise = isTypeIIKnot (drop 1 tripCode)

isTypeIIKnotHelp :: Char -> Char -> [(Char, Char)] -> Bool
isTypeIIKnotHelp one two tripCode
   | null tripCode = False 
   | length tripCode < 2 = False 
   | (fst (head tripCode) == one && (fst (head (tail tripCode))) == two) || (fst (head tripCode) == two && (fst (head (tail tripCode))) == one) = True  
   | otherwise = isTypeIIKnotHelp one two (drop 1 tripCode) 

removeTypeII :: [(Char, Char)] -> [(Char, Char)]
removeTypeII tripCode
   | null tripCode = [] 
   | length tripCode < 4 = tripCode
   | snd (head tripCode) == snd (head (tail tripCode)) && (isTypeIIKnotHelp (fst (head tripCode)) (fst (head (tail tripCode))) (drop 2 tripCode)) = (removeTypeIIHelp (fst (head tripCode)) (fst (head (tail tripCode))) (drop 2 tripCode))
   | otherwise = (take 1 tripCode) ++ removeTypeII (drop 1 tripCode)

removeTypeIIHelp :: Char -> Char -> [(Char, Char)] -> [(Char, Char)]
removeTypeIIHelp one two tripCode
   | null tripCode = []
   | length tripCode < 2 = tripCode 
   | (fst (head tripCode) == one && (fst (head (tail tripCode))) == two) || (fst (head tripCode) == two && (fst (head (tail tripCode))) == one) = (drop 2 tripCode)   
   | otherwise = (take 1 tripCode) ++ removeTypeIIHelp one two (drop 1 tripCode)

isTypeIIKnotWrap :: [(Char, Char)] -> Bool
isTypeIIKnotWrap tripCode
   | null tripCode = False 
   | length tripCode < 4 = False 
   | snd (head tripCode) == snd (last tripCode) = (isTypeIIKnotHelp (fst (head tripCode)) (fst (last tripCode)) (tail (init tripCode)))
   | otherwise = False 

removeTypeIIWrap :: [(Char, Char)] -> [(Char, Char)]
removeTypeIIWrap tripCode
   | null tripCode = []
   | length tripCode < 4 = tripCode
   | otherwise = (removeTypeIIHelp(fst (head tripCode)) (fst (last tripCode)) (tail (init tripCode)))






main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

