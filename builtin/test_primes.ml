open Builtin
open Basic_arithmetics
open Power

let is_prime n =
   let rec is_prime_rec nbr di = match nbr with
      |_ when (nbr <= 2) -> if n = 2 then
                               true
                            else
                               false
      |_ when ((modulo nbr di) = 0) -> false
      |_ when ((di * di) > nbr) -> true
      |_ -> is_prime_rec nbr (di + 1)
   in is_prime_rec n 2;;

let is_pseudo_prime p test_seq =
   let rec is_pseudo_prime_rec p test_seq = match test_seq with
      |[] -> true
      |n::l when ((mod_power n p p) != (modulo n p)) -> false
      |n::l -> is_pseudo_prime_rec p l
   in is_pseudo_prime_rec p test_seq;;