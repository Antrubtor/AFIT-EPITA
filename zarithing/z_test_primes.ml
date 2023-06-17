open Z
open Z_power

let is_prime n =
   let rec is_prime_rec nbr di = match nbr with
      |_ when ((pred nbr) <= one) -> if (pred n) = one then
                               true
                            else
                               false
      |_ when ((n mod di) = zero) -> false
      |_ when ((mul di di) > nbr) -> true
      |_ -> is_prime_rec nbr (succ di)
   in is_prime_rec n (succ one);;