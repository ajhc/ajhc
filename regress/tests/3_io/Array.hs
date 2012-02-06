import Data.Array


abcs = ['a' .. 'z']

abcs' = listArray (0,length abcs - 1) abcs
abcs'' =  listArray (1,length abcs) abcs

main = do
    putStrLn abcs
    putStrLn (elems abcs')
    print (indices abcs')
    print (elems abcs'')
    print (indices abcs'')
    putStrLn abcs

