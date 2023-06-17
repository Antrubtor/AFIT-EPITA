open Builtin
open Basic_arithmetics
open Power

let encode str bits =
   let len =  String.length(str) in
   let rec encode_rec i j sum = match i with
      |_ when i = -1 || j >= (len + 1) -> sum
      |_ -> encode_rec (i-1) (j+1) (sum + (int_of_char str.[i]*power(power 2 bits) j))
   in encode_rec (len-1) 0 0;;

let decode msg bits =
   let rec decode_rec msg bits = match msg with
      |0 -> ""
      |msg -> let pw = power 2 bits in
              decode_rec (quot msg pw) bits ^ Char.escaped (char_of_int (modulo msg pw))
   in decode_rec msg bits;;