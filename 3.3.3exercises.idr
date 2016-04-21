import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeHelper : (x : Vect n elem) -> (xs_trans : Vect n (Vect k elem)) -> Vect n (Vect (S k) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMatrix : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMatrix [] = createEmpties
transposeMatrix (x :: xs) = let xs_trans = transposeMatrix xs in
  transposeHelper x xs_trans

transposeMatrixZip : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMatrixZip [] = createEmpties
transposeMatrixZip (x :: xs) = let xs_trans = transposeMatrixZip xs in
  zipWith (::) x xs_trans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

multiplyRowHelper : Num a => (x : Vect n a) -> (y : Vect n a) -> a
multiplyRowHelper x y = sum (zipWith (*) x y)

rowHelper : Num a => (x : Vect n a) -> (ys_trans : Vect p (Vect n a)) -> Vect p a
rowHelper x [] = []
rowHelper x (y :: xs) = multiplyRowHelper x y :: rowHelper y xs

multiplyMatrixHelper : Num a => (xs : Vect m (Vect n a)) -> (ys_trans : Vect p (Vect n a)) -> Vect m (Vect p a)
multiplyMatrixHelper [] ys_trans = []
multiplyMatrixHelper (x :: xs) ys_trans = rowHelper x ys_trans :: multiplyMatrixHelper xs ys_trans

multiplyMatrix : Num a => Vect m (Vect n a) -> Vect n (Vect p a) -> Vect m (Vect p a)
multiplyMatrix xs ys = let ys_trans = transposeMatrix ys in
  multiplyMatrixHelper xs ys_trans
