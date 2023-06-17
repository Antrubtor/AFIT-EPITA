open Builtin

let rec gcd a b = if b = 0 then
                       a
                  else
                       gcd b (modulo a b);;


let bezout a b = let rec aux u v r u1 v1 r1 = match r1 with
   |0->(u,v,r)
   |_-> aux u1 v1 r1 (u-(quot r r1)*u1) (v-(quot r r1)*v1) (r-(quot r r1)*r1)
   in aux 1 0 a 0 1 b;;