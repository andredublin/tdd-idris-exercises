data Shape =
  Triangle Double Double
  | Rectangle Double Double
  | Circle Double

Eq Shape where
    (==) (Triangle b l) (Triangle b' l') = b == b' && l == l'
    (==) (Rectangle l h) (Rectangle l' h') = l == l' && h == h'
    (==) (Circle r) (Circle r') = r == r'
    (==) _ _ = False

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

Ord Shape where
    compare x y = compare (area x) (area y)
