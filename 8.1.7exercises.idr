--- states that if you add the same value onto the front of equal lists, the resulting lists are also equal
same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons Refl = Refl

--- states that if two values x and y are equal, and two lists xs and ys are equal,
--- then the two lists x :: xs and y :: ys must also be equal
same_lists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl

data ThreeEq : a -> b -> c -> Type where
  AllEqual : ThreeEq x x x

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z AllEqual = AllEqual
