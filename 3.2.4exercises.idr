import Data.Vect

insert : Ord elem => (x : elem) -> (xs_sorted : Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                     False => y :: insert x xs
                     True => x :: y :: xs

ins_sort : Ord elem => Vect n elem -> Vect n elem
ins_sort [] = []
ins_sort (x :: xs) = let xs_sorted = ins_sort xs in
  insert x xs_sorted

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

reverse_sort : (x : a) -> (xs_reverse : List a) -> List a
reverse_sort x [] = [x]
reverse_sort x (y :: xs) = y :: reverse_sort x xs

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = let xs_reverse = my_reverse xs in
  reverse_sort x xs_reverse

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_map_vect : (a -> b) -> Vect n a -> Vect n b
my_map_vect f [] = []
my_map_vect f (x :: xs) = f x :: my_map_vect f xs
