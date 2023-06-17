open Builtin
open Basic_arithmetics
open Power

let encrypt_cesar k m b =
   let rec encrypt_cesar_rec lis = match lis with
      |[] -> []
      |e::l -> modulo (e + k) (b + 1)::encrypt_cesar_rec l
   in encrypt_cesar_rec m;;

let decrypt_cesar k m b =
   let rec decrypt_cesar_rec lis = match lis with
      |[] -> []
      |e::l -> modulo (e - k) (b + 1)::decrypt_cesar_rec l
   in decrypt_cesar_rec m;;

let rec euclide_etendu a b =
   if a = 0 then
      (b, 0, 1)
   else
      let (g, y, x) = euclide_etendu (modulo b a) a in
      (g, x - (quot b a) * y, y);;

let inv_modulo e phi =
   let g, c, d = euclide_etendu e phi in
   if g != 1 then
      invalid_arg "inv_modulo: linverse modulo n existe pas"
   else
      modulo c phi;;

let generate_keys_rsa p q =
   let n = p * q in
   let phi = (p - 1) * (q - 1) in
   let e = 65537 in
   let d = inv_modulo e phi in
   ((n, e), (n, d));;

let encrypt_rsa m (n, e) = mod_power m e n;;

let decrypt_rsa m (n , d) = mod_power m d n;;

let rec public_data_g p =
   let key = Random.int p in
   if mod_power key p p <> 1 then
      (key, p)
   else
      public_data_g p;;

let generate_keys_g (g, p) =
   let p_key = Random.int(p) in
   (prime_mod_power g p_key p, p_key);;

let encrypt_g msg (g, p) kA =
   let key = Random.int(p - 2) + 2 in
   (prime_mod_power g key p, msg * (prime_mod_power kA key p));;

let decrypt_g (msgA, msgB) a (g, p) = quot msgB (prime_mod_power msgA a p);;
