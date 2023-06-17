open Z

let init_eratosthenes n =
   let rec init_eratosthenes_rec k ele = match k with
      |_ when ele > n -> []
      |_ -> ele::init_eratosthenes_rec (succ k) (succ (succ ele))
   in (succ one)::init_eratosthenes_rec zero (succ (succ one));;

let eratosthenes n =
   let rec eratosthenes_rec list = match list with
      |[] -> []
      |e::l -> let rec erato_rec list_1 list_era = match list_1 with
                     |[] -> []
                     |e1::l1 when (e1 mod e) = zero -> erato_rec l1 (list_era)
                     |e1::l1 -> e1::erato_rec l1 (e1::list_era)
                  in e::eratosthenes_rec (erato_rec l [])
   in eratosthenes_rec (init_eratosthenes n);;

let write_list li file = ()

let write_list_primes n file = ()

let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

let create_list in_c = []

let read_list_primes file = []

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
    |e::l -> if isprime (succ (mul e (succ one))) then
                (e,succ (mul e (succ one)))::double_primes_rec l
              else
                double_primes_rec l
  in double_primes_rec (eratosthenes limit);;