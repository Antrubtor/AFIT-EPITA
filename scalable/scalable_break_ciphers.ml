open Scalable
open Scalable_basic_arithmetics

let rec break_rec key nbr =
   if (mod_b key nbr) = [] then
      (nbr, quot_b key nbr)
   else
      break_rec key (diff_b nbr [0;1]);;

let break key =
   let (a, _) = key in
   let aa = from_int(to_int(a)) in
   let a = float_of_int(to_int(a)) in
   let r_key = from_int(int_of_float(floor(sqrt a))) in
   if mod_b r_key [0;0;1] = [0;1] then
      break_rec aa (diff_b r_key [0;1])
   else
      break_rec aa r_key;;