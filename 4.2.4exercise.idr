import Data.Vect

-- enum describing the power source
data PowerSource = Petrol | Pedal | Electric

-- vehicle type annotated with powersource
data Vehicle : PowerSource -> Type where
  -- vehicles powered by pedal
  Unicycle : Vehicle Pedal
  Bicycle : Vehicle Pedal
  -- vehicles powered by petrol
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  -- vehicles powered by electricity
  Train : Vehicle Electric
  ElectricCar : Vehicle Electric

wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Motorcycle fuel) = 2
wheels ElectricCar = 4
wheels Train = 100

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 50

myTake : (n : Nat) -> Vect (n + m) a -> Vect n a
myTake Z xs = []
myTake (S k) (x :: xs) = x :: take k xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                Just idx => Just (index idx xs + index idx ys)
