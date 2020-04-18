module Cauchy where

    data CauchyList = CauchyList Int [Int]

    summer::[Int]->[Int]->Int->[Int]
    summer x [] p = x
    summer [] x p = x
    summer (xh:[]) (yh:[]) p = [( mod (xh+yh) p)]
    summer (xh:xt) (yh:[]) p = ( mod (xh+yh) p) :(xt)
    summer (yh:[]) (xh:xt) p = ( mod (xh+yh) p) :(xt)
    summer (xh:xt) (yh:yt) p = ( mod (xh+yh) p) : (summer (xt) (yt) p)

    subtractor::[Int]->[Int]->Int->[Int]
    subtractor x [] p = x
    subtractor [] x p = x
    subtractor (xh:[]) (yh:[]) p = [( mod (xh-yh) p)]
    subtractor (xh:xt) (yh:[]) p = ( mod (xh-yh) p) :(xt)
    subtractor (yh:[]) (xh:xt) p = ( mod (xh-yh) p) :(map (0-) xt)
    subtractor (xh:xt) (yh:yt) p = ( mod (xh-yh) p) : (subtractor (xt) (yt) p)

    addZero:: [Int]->Int->[Int]
    addZero x n = x ++ take n (repeat 0)

    takei:: [Int]->[Int]->[Int]
    takei x y =
      if length x < length y
        then addZero x ((length y) - (length x))
        else x

    mmult::[Int]->[Int]->Int->Int->Int
    mmult x y 0 n=
      if (length y <= n)
        then 0
      else (x!!0)*(y!!n)
    mmult x y i n =
      if ((length x) <= i)
        then  mmult x y (i-1) (n+1)
        else if (length y) <= n
          then mmult x y (i-1) (n+1)
          else (x!!i)*(y!!n) + mmult x y (i-1) (n+1)

    multiplyer::Int->[Int]->[Int]->Int->[Int]
    multiplyer _ _ _ (-1) = []
    multiplyer m x y n = [mod (mmult x y n 0) m] ++ multiplyer m x y (n-1)

    instance Eq CauchyList where
        (CauchyList x1 y1) == (CauchyList x2 y2) = (x1 == x2 && y1 == y2)

    instance Num CauchyList where
        (CauchyList x1 y1) + (CauchyList x2 y2) = CauchyList (x1) (summer y1 y2 x1)
        (CauchyList x1 y1) - (CauchyList x2 y2) = CauchyList (x1) (subtractor y1 y2 x1)
        (CauchyList x1 y1) * (CauchyList x2 y2) = CauchyList (x1) (reverse (multiplyer x1 (takei y1 y2) (takei y2 y1) ((length y1) + (length y2) - 2)))
        signum (CauchyList x1 y1) = CauchyList x1 (map signum y1)
        abs (CauchyList x1 y1) = CauchyList x1 (map abs y1)
        fromInteger n = let a = (fromInteger n) in CauchyList (a+1) ([a])

    instance Show CauchyList where
        show (CauchyList x y) = "p:" ++ (show x) ++ "\n" ++ "length:" ++ (show (length y))  ++ "\n" ++ "content:" ++ (show y)
