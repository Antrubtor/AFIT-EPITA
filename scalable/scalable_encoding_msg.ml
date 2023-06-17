open Scalable
open Scalable_basic_arithmetics
open Scalable_power

let encode str bits =
   let len =  from_int(String.length(str)) and bitss = (from_int(bits)) in
   let rec encode_rec i j sum = match i with
      |_ when i = [1;1] || j >>= (add_b len [0;1]) -> sum
      |_ -> encode_rec (diff_b i [0;1]) (add_b j [0;1]) (add_b sum (mult_b (from_int(int_of_char str.[to_int(i)])) (power (power [0;0;1] bitss) j)))
   in encode_rec (diff_b len [0;1]) [] [];;

let decode msg bits = 
   let rec decode_rec msg bits = match msg with
      |[] -> ""
      |_ -> let pw = power [0;0;1] bits in
              decode_rec (quot_b msg pw) bits ^ Char.escaped (char_of_int (to_int(mod_b msg pw)))
   in decode_rec msg (from_int(bits));;