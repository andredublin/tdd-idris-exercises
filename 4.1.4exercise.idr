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

test_picture : Picture
test_picture = Combine (Translate 5 5 rectangle) (Combine (Translate 35 5 circle) (Translate 15 25 triangle))

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

picture_area : Picture -> Double
picture_area (Primitive shape) = area shape
picture_area (Combine pic pic1) = picture_area pic + picture_area pic1
picture_area (Rotate x pic) = picture_area pic
picture_area (Translate x y pic) = picture_area pic

data Biggest = NoTriangle | Size Double

biggestTriangle : Picture -> Biggest
biggestTriangle pic = if (picture_area pic) == 0 then NoTriangle
                                                 else Size (picture_area pic)
