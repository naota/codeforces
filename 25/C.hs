import Maybe

main :: IO ()
main = interact func

func :: String -> String
func s = let ss = lines s
             ncity = read.head $ ss
             mstr = take ncity.tail $ ss
             roads = drop (ncity+1) ss
             tobuild = tail roads
         in func' (toMatrix mstr) (map toBuildRoad tobuild) ncity
            
func' dic [] _ = []
func' dic ((s,d,l):rs) ncity = minroad:(func' newdic rs)
  where upd = ((s,d),l):dic
        newdic = recalc upd ncity
        minroad = calcmin newdic
        
recalc dic n


toMatrix :: [String] -> [((Int,Int),Int)]
toMatrix foo = help 0 (map (\x -> map read $ words x) foo) []
  where help _ [] dic = dic
        help n (xs:xss) dic = help (n+1) xss (help' 0 n xs dic)
        help' _ _ [] dic = dic
        help' m n (y:ys) dic
          | n < m = ((n,m),y):(help' (m+1) n ys dic)
          | otherwise = help' (m+1) n ys dic

toBuildRoad :: String -> (Int, Int, Int)
toBuildRoad s = let lst = map read $ words s
                in (head lst, lst!!1, lst!!2)

distant x y dic
  | x > y = distant y x dic
  | x == y = 0
  | otherwise = fromJust $ lookup (x,y) dic
