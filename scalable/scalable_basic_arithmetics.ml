open Scalable

let rec gcd_b bA bB =
   if bB = [] then
      bA
   else
      gcd_b bB (mod_b bA bB);;


let bezout_b bA bB = let rec aux u v r u1 v1 r1 = match r1 with
   |[]->(u,v,r)
   |_-> aux u1 v1 r1  (diff_b u (mult_b (quot_b r r1) u1)) (diff_b v (mult_b (quot_b r r1) v1)) (diff_b r (mult_b (quot_b r r1) r1))
   in aux [0;1] [] bA [] [0;1] bB;;