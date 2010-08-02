import Data.List

main :: IO ()
main = interact func

func :: String -> String
func s = show.func'.lines $ s
     where func' (a:b:c:_) = let m1 = f a b c 200000
                                 m2 = f a c b m1
                                 m3 = f b c a m2
                             in m3
           func' _ = error "" 
           
f :: String -> String -> String -> Int -> Int
f a b c cmin = let candicates = concat $ map (mostin cmin c) $ mostin cmin a b
                   lst = map length candicates
               in if null lst then cmin else foldl min (head lst) (tail lst)

mostin :: Int -> String -> String -> [String]
mostin cmin x y
  | length x > length y = mostin cmin y x
  | x `isInfixOf` y = [y]
  | otherwise = filter (\cs -> length cs <= cmin) [headmatch x "", lastmatch x ""]
                where headmatch "" cs = cs++y
                      headmatch x'@(h:t) cs = if x' `isPrefixOf` y 
                                              then cs ++ y
                                              else headmatch t (cs++[h])
                      lastmatch "" cs = y++cs
                      lastmatch x' cs = if x' `isSuffixOf` y 
                                        then y ++ cs
                                        else lastmatch (init x') (last x':cs)
