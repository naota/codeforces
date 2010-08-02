main :: IO ()
main = interact func

func :: String -> String
func s = show.func' 1.map (`mod` 2).map read.words.head.tail$lines s
            
func' :: Int -> [Int] -> Int
func' n (a:b:c:xs)
  | a == b && b /= c = n + 2
  | a == c && b /= c = n+1
  | a /= b && b == c = n
  | otherwise = func' (n+1) (b:c:xs)
func' _ _ = error ""
