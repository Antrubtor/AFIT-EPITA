open Builtin
open Basic_arithmetics

let pow x n = if n < 0 then
                  invalid_arg("n must be positive")
              else
                 let rec pow_rec x n = 
                 if n = 0 then
                     1
                 else
                     x * (pow_rec x (n-1))
              in pow_rec x n;;

let power x n = if n <= 0 then
                    if n = 0 then
                        1
                    else
                        invalid_arg("n must be positive")
                else
                    let rec pow_rec x n = match n with
                    |1 -> x
                    |_ when (modulo n 2) = 0 -> pow_rec (x*x) (quot n 2)
                    |_ when (modulo n 2) = 1 -> x * (pow_rec (x*x) (quot (n-1) 2))
                    |_ -> 0
              in pow_rec x n;;

let mod_power x n m =
   if n <= 0 then
      if n = 0 then
         1
      else
         invalid_arg("n must be positive")
   else
      let rec mod_power_rec xx nn mm = match nn with
         |0 -> 0
         |1 -> (modulo xx mm)
         |_ when (modulo nn 2) = 0 -> mod_power_rec (modulo ((modulo (xx) mm) * (modulo (xx) mm)) mm) (quot nn 2) mm
         |_ ->modulo ((modulo xx mm) * (mod_power_rec (modulo xx mm) (nn - 1) mm)) mm
      in mod_power_rec x n m;;

let prime_mod_power x n p =
   if x = 0 || x = 1 then
      x
   else
      let u = modulo n (p - 1)
         in mod_power x u p;;