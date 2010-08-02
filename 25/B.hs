main :: IO ()
main = interact func

func :: String -> String
func s = func'.head.tail$lines s
            
func' :: String -> String
func' [] = ""
func' (_:[]) = error ""
func' xs@(_:_:[]) = xs
func' xs@(_:_:_:[]) = xs
func' (a:b:c:d:[]) = a:b:'-':c:d:[]
func' (a:b:c:xs) = a:b:c:'-':(func' xs)