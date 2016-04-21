||| Represents shapes
data Shape = ||| A triangle, with its base and length
            Triangle Double Double
            | ||| A rectangle, with is length and height
            Rectangle Double Double
            | ||| A circle, with its radius
            Circle Double

data Picture = Primitive Shape
              | Combine Picture Picture
              | Rotate Double Picture
              | Translate Double Double Picture

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

picture_area : Picture -> Double
picture_area (Primitive shape) = area shape
picture_area (Combine pic pic1) = picture_area pic + picture_area pic1
picture_area (Rotate x pic) = picture_area pic
picture_area (Translate x y pic) = picture_area pic

data Tree elem = Empty
                | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => orig
                                      GT => Node left val (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = let elem = listToTree xs in
  insert x elem

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = treeToList left ++ x :: treeToList right

data Expr = Val Int
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mul x y) = evaluate x * evaluate y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe (Just x) Nothing = Just x
maxMaybe Nothing (Just y) = Just y
maxMaybe (Just x) (Just y) = if x > y then Just x
                             else Just y

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive triangle@(Triangle x y)) = Just (area triangle)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine x y) = maxMaybe (biggestTriangle x) (biggestTriangle y)
biggestTriangle (Rotate x picture) = biggestTriangle picture
biggestTriangle (Translate x y picture) = biggestTriangle picture
