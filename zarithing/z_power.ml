open Z

let mod_power x n m = Z.powm x n m;;

let prime_mod_power x n p =
   if (x mod p = zero) || (n < p) then
      mod_power x n p
   else
      mod_power x (n mod (pred p)) p;;