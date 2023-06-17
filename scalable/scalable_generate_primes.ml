open Scalable
open Scalable_basic_arithmetics

let init_eratosthenes n =
   let rec init_eratosthenes_rec k ele = match k with
      |_ when ele >> n -> []
      |_ -> ele::init_eratosthenes_rec (add_b k [0;1]) (add_b ele [0;0;1])
   in [0;0;1]::init_eratosthenes_rec [] [0;1;1];;

let eratosthenes n =
   let rec eratosthenes_rec list = match list with
      |[] -> []
      |e::l -> let rec erato_rec list_1 list_era = match list_1 with
                     |[] -> []
                     |e1::l1 when (mod_b e1 e) = [] -> erato_rec l1 (list_era)
                     |e1::l1 -> e1::erato_rec l1 (e1::list_era)
                  in e::eratosthenes_rec (erato_rec l [])
   in eratosthenes_rec (init_eratosthenes n);;

let rec int_list_to_string_list list = match list with
   |[] -> []
   |e::l -> string_of_int(e)::int_list_to_string_list l;;
let write_list li file =()(*
   let oc = open_out file and lis = int_list_to_string_list li in
   let rec aux = function
      [] -> close_out oc
      |e::l -> Printf.fprintf oc "%s\n" e; aux l
   in aux lis;;*)


let write_list_primes n file = ()(*write_list (eratosthenes n) file;;*)


let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

let create_list in_c = ()

let rec string_list_to_int_list list = match list with
   |[] -> []
   |e::l -> int_of_string(e)::string_list_to_int_list l;;
let read_list_primes file =[](*
   let ic = open_in file in
   let try_read () =
   try Some (input_line ic) with End_of_file -> None in
      let rec loop () = match try_read () with
         |Some s -> s::(loop ())
         |None -> close_in ic; []
   in string_list_to_int_list (loop ());;*)

let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

let double_primes limit isprime =
  let rec double_primes_rec tab = match tab with
    |[] -> []
    |e::l -> if isprime (add_b (mult_b [0;0;1] e) [0;1]) then
               (e,(add_b (mult_b [0;0;1] e) [0;1])) :: double_primes_rec l
             else
               double_primes_rec l
  in double_primes_rec (eratosthenes limit);;

let twin_primes limit isprime =
  let rec twin_primes_rec tab = match tab with
    |[] -> []
    |e::l -> if isprime (add_b e [0;0;1]) then
               (e,(add_b e [0;0;1]))::twin_primes_rec l
             else
               twin_primes_rec l
  in twin_primes_rec (eratosthenes limit);;