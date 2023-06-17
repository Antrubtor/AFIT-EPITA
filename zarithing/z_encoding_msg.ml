open Z
open Z_power

let encode str bits =
   let len = of_int (String.length(str)) in
   let rec encode_rec i j sum = match i with
      |_ when (succ i = zero) || j >= (succ len) -> sum
      |_ -> encode_rec (pred i) (succ j) (sum + (of_int (int_of_char str.[to_int i]) * (pow (pow (succ one) bits) (to_int(j)))))
   in encode_rec (pred len) zero zero;;

let decode msg bits =
   let rec decode_rec msg bits = match msg with
      |_ when msg = zero -> ""
      |_ -> let pw = pow (succ one) bits in
            decode_rec (msg / pw) bits ^ Char.escaped (char_of_int (to_int(msg mod pw)))
   in decode_rec msg bits;;