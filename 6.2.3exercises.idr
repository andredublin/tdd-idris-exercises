import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix k j = Vect k (Vect j Double)

data Format =
  -- represents %d followed by the rest of the format specifier
  Number Format
  -- represents %s followed by the rest of the format specifier
  | Str Format
  | Ch Format
  | Dbl Format
  -- A literal string followed by the rest of the format specifier
  | Lit String Format
  -- an empty format specifier
  | End

PrintfType : Format -> Type
-- The Number directive means that our printf function will need another Int argument.
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
-- The Str directive means that our printf function will need another String argument.
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
-- No additional argument is needed here, since we have a literal string, so calculate the type from the rest of the
-- Format.
PrintfType (Lit str fmt) = PrintfType fmt
-- This gives the return type of printf.
PrintfType End = String
PrintfType (Ch fmt) = (c : Char) -> PrintfType fmt
PrintfType (Dbl fmt) = (d : Double) -> PrintfType fmt

-- The String is an accumulator, in which we build the String to be returned
printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
-- PrintfType calculates a function type from Number fmt, so we need to build a function here.
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ show str)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
-- At the end, there are no further arguments to read and no further literal inputs, so return the accumulator.
printfFmt End acc = acc
printfFmt (Dbl fmt) acc = \c => printfFmt fmt (acc ++ show c)
printfFmt (Ch fmt) acc = \d => printfFmt fmt (acc ++ show d)

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Ch (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dbl (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) =
  case toFormat chars of
    -- strCons builds a String from an initial character and the rest of the string
    Lit lit chars' => Lit (strCons c lit) chars'
    fmt => Lit (strCons c "") fmt

-- We use an underscore _ for the format, because Idris can infer from the type that it
-- must be toFormat (unpack fmt).
printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

TupleVect : (n : Nat) -> Type -> Type
TupleVect Z x = ()
TupleVect (S k) x = (x, TupleVect k x)

test : TupleVect 4 Nat
test = ?test_rhs
