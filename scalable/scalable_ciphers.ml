open Scalable
open Scalable_basic_arithmetics
open Scalable_power

let rec euclide_etendu a b =
   if a = [] then
      (b, [], [0;1])
   else
      let (g, y, x) = euclide_etendu (mod_b b a) a in
      (g, (diff_b x (mult_b (quot_b b a) y)), y);;

let inv_modulo e phi =
   let g, c, d = euclide_etendu e phi in
   if g <> [0;1] then
      invalid_arg "inv_modulo: linverse modulo n existe pas"
   else
      mod_b c phi;;

let generate_keys_rsa p q =
   let n = mult_b p q in
   let phi = mult_b (diff_b p [0;1]) (diff_b q [0;1]) in
   let e = [0;1;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1] in
   let d = inv_modulo e phi in
   ((n, e), (n, d));;

let encrypt_rsa m (n, e) = mod_power m e n;;

let decrypt_rsa m (n , d) = mod_power m d n;;

let rec public_data_g p =
   let key = from_int(Random.int(to_int(p))) in
   if mod_power key p p <> [0;1] then
      (key, p)
   else
      public_data_g p;;

let generate_keys_g (g, p) =
   let p_key = from_int(Random.int(to_int(p))) in
   (prime_mod_power g p_key p, p_key);;

let encrypt_g msg (g, p) kA =
   let key = from_int(Random.int(to_int(p) - 2) + 2) in
   (prime_mod_power g key p, (mult_b msg  (prime_mod_power kA key p)));;

let decrypt_g (msgA, msgB) a (g, p) = quot_b msgB (prime_mod_power msgA a p);;