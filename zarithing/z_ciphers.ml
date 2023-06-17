open Z
open Z_power

let generate_keys_rsa p q =
   let n = p * q in
   let phi = (pred p) * (pred q) in
   let e = pred (phi / (succ one)) in(* check pour les divisions*)
   let d = invert e phi in
   ((n, e), (n, d));;

let encrypt_rsa m (n, e) = mod_power m e n;;

let decrypt_rsa m (n , d) = mod_power m d n;;

let rec public_data_g p =
   let key = of_int (Random.int (to_int p)) in
   if mod_power key p p <> one then
      (key, p)
   else
      public_data_g p;;

let generate_keys_g (g, p) =
   let p_key = of_int (Random.int (to_int p)) in
   (prime_mod_power g p_key p, p_key);;

let encrypt_g msg (g, p) kA =
   let key = of_int (Random.int ((to_int (pred (pred p))))) in
   (prime_mod_power g key p, msg * (prime_mod_power kA key p));;

let decrypt_g (msgA, msgB) a (g, p) = div msgB (prime_mod_power msgA a p);;