module Nat where
    import Prelude

data Nat = O | S Nat
    deriving ( Eq , Show )

(+) :: Nat -> Nat -> Nat
(+) m O = m
(+) m (S n) = S (m + n)

(*) :: Nat -> Nat -> Nat
(*) m O = O
(*) m (S n) = (n * m) + n

(^) :: Nat -> Nat -> Nat
(^) m ^ O = S O
(^) m ^ (S n) = m * (m ^ n)

(-) :: Nat -> Nat -> Nat
(-) 0 - m = 0
(-) m - 0 = m
(-) (S m) - (S n) = m - n

fib :: Nat -> Nat
fib O = O
fib S O = S O
fib S (S m) = fib S m + fib m

min :: Nat -> Nat -> Nat
min m O = O
min O m = O
min (S m) (S n) = S (min m n)

max :: Nat -> Nat -> Nat
max m O = m
max O m = m
max (S m) (S n) = S (max m n)

double :: Nat -> Nat
double O = O
double S m = S (S (double m))

pred :: Nat -> Nat
pred 0 = 0
pred (S m) = m

fact :: Nat -> Nat
fact 0 = S0
fact (S m) = (S m) * fact m