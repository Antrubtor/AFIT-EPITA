open Builtin
open Basic_arithmetics

let init_eratosthenes n =
   let rec init_eratosthenes_rec k ele = match k with
      |_ when ele > n -> []
      |_ -> ele::init_eratosthenes_rec (k+1) (ele+2)
   in 2::init_eratosthenes_rec 0 3;;

let eratosthenes n =
   let rec eratosthenes_rec list = match list with
      |[] -> []
      |e::l -> let rec erato_rec list_1 list_era = match list_1 with
                     |[] -> []
                     |e1::l1 when (modulo e1 e) = 0 -> erato_rec l1 (list_era)
                     |e1::l1 -> e1::erato_rec l1 (e1::list_era)
                  in e::eratosthenes_rec (erato_rec l [])
   in eratosthenes_rec (init_eratosthenes n);;

let rec int_list_to_string_list list = match list with
   |[] -> []
   |e::l -> string_of_int(e)::int_list_to_string_list l;;

let write_list li file =
   let oc = open_out file and lis = int_list_to_string_list li in
   let rec aux = function
      |[] -> close_out oc
      |e::l -> Printf.fprintf oc "%s\n" e; aux l
   in aux lis;;

let write_list_primes n file =  write_list (eratosthenes n) file;;

let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

let rec string_list_to_int_list list = match list with
   |[] -> []
   |e::l -> int_of_string(e)::string_list_to_int_list l;;

let read_list_primes file =
   let ic = open_in file in
   let try_read () =
   try Some (input_line ic) with End_of_file -> None in
      let rec loop () = match try_read () with
         |Some s -> s::(loop ())
         |None -> close_in ic; []
   in string_list_to_int_list (loop ());;

let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

let double_primes limit isprime = 
   let rec double_primes_rec tab = match tab with
      |[] -> []
      |e::l -> if isprime (2*e+1) then
                  (e,2*e+1) :: double_primes_rec l
               else
                  double_primes_rec l
  in double_primes_rec (eratosthenes limit);;

let twin_primes limit isprime =
  let rec twin_primes_rec tab = match tab with
    |[] -> []
    |e::l -> if isprime (e+2) then
                (e,e+2) :: twin_primes_rec l
             else
                twin_primes_rec l
  in twin_primes_rec (eratosthenes limit);;