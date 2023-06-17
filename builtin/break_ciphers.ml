open Builtin
open Basic_arithmetics

let rec break_rec key nbr =
   if (modulo key nbr) = 0 then
      (nbr, quot key nbr)
   else
      break_rec key (nbr-1);;

let break key =
   let (a, _) = key in
   let a = float_of_int(a) in
   let r_key = sqrt a in
   if modulo (int_of_float(floor(r_key))) 2 = 1 then
      break_rec  (int_of_float(floor(a))) ((int_of_float(floor(r_key)))-1)
   else
      break_rec (int_of_float(floor(a))) (int_of_float(floor(r_key)));;