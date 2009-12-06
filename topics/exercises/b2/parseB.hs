import B
import Parsing

b = true +++ false +++ cond

true = 
 do
    token (string "1")
    return TrueB

false = 
 do
    token (string "0")
    return FalseB

cond = 
 do
    token (string "if")
    x <- b
    token (string "then")
    y <- b
    token (string "else")
    z <- b
    token (string "fi")
    return (IfB x y z)

main = do
		print (parse b "0")
		print (parse b "if 1 then 0 else if 0 then 1 else 0 fi fi")
