main :: IO ()
main = interact func

func :: String -> String
func s = show.func' 1.map (`mod` 2).map read.words.head.tail$lines s
            
func' :: Int -> [Int] -> Int
func' _ [] = error ""
func' n (0:0:1:_) = n+2
func' n (0:1:0:_) = n+1
func' n (1:0:0:_) = n
func' n (1:1:0:_) = n+2
func' n (1:0:1:_) = n+1
func' n (0:1:1:_) = n
func' n (_:xs) = func' (n+1) xs

