
test :: Int -> Int -> (Int,String)
test n' n = (n+1,show n')

foldWithLabel :: Int -> (a -> (Int,String)) -> [a] -> (Int,String)
foldWithLabel lNum func [] = (lNum,"")
foldWithLabel lNum func [x] = (lNum',str)
    where
        (lNum',str) = func x
foldWithLabel lNum func (x:xs) = (lNum2,str)
    where
        (lNum1,str1) = func x 
        (lNum2,str2) = foldWithLabel lNum1 func xs 
        str = str1 ++ str2
