let sign x = if x >= 0 then 1 else -1;;

let quot a b = let q = a/b and r = a mod b in
   if r >= 0 then
      q
   else
      q - (sign b);;

let modulo a b = let r = a mod b in
   if r >= 0 then
      r
   else
      r + (sign b) * b;;

let from_int x =
   let rec from_int_rec nbr = match nbr with
      |0 -> []
      |_ -> (modulo nbr 2)::from_int_rec (quot nbr 2)
   in
   if (sign x) = -1 then
      1::from_int_rec (-x)
   else
      if x = 0 then
         []
      else
         0::from_int_rec x;;

let power x n = if n <= 0 then
                    if n = 0 then
                        1
                    else
                        invalid_arg("n must be positive")
                else
                    let rec pow_rec x n = match n with
                    |1 -> x
                    |_ when (modulo n 2) = 0 -> pow_rec (x*x) (quot n 2)
                    |_ when (modulo n 2) = 1 -> x * (pow_rec (x*x) (quot (n-1) 2))
                    |_ -> 0
              in pow_rec x n;;

let rec to_int_rec nbr pw = match nbr with
      |[] -> 0
      |e::l -> e * (power 2 pw) + to_int_rec l (pw+1);;
let to_int bA = match bA with
   |[] -> 0
   |e::l -> if e = 0 then
               to_int_rec l 0
            else
               -(to_int_rec l 0);;

let print_b bA =
  let rec print_b_rec bA = match bA with
    |[] -> ""
    |e::l -> (print_b_rec l)^(string_of_int e)
  in match bA with
    |[] -> print_string("0")
    |1::l -> print_string("-"^(print_b_rec l))
    |e::l -> print_string((print_b_rec l));;


let reverse list1 =
   let rec reverse2 list1 list2 = match list1 with
      |[] -> list2
      |e::l -> reverse2 l (e::list2)
   in reverse2 list1 [];;

let rec shift bA d = match d with
   |0 -> bA
   |_ -> shift (0::bA) (d - 1);;

let shift_l l d = reverse ((shift (reverse l) d));;

let tail l = match l with
   |[] -> []
   |e::l -> l;;

let compare_n nA nB =
   let lenA = List.length nA and lenB = List.length nB in
   if lenA < lenB then
      (-1)
   else
      if lenA > lenB then
         1
      else
         let a = (reverse nA) and b = (reverse nB) in
         let rec compare_n_rec nA nB = match (nA,nB) with
            |([],[]) -> 0
            |([],_) -> (-1)
            |(_,[]) -> 1
            |(eA::lA, eB::lB) when eA > eB -> 1
            |(eA::lA, eB::lB) when eA < eB -> (-1)
            |(eA::lA, eB::lB) -> compare_n_rec lA lB
         in compare_n_rec a b;;


let (>>!) nA nB = if (compare_n nA nB) = 1 then
                     true
                  else
                     false;;


let (<<!) nA nB = if (compare_n nA nB) = (-1) then
                     true
                  else
                     false;;

let (>=!) nA nB =
   let rep = (compare_n nA nB) in
   if rep = 0 || rep = 1 then
      true
   else
      false;;

let (<=!) nA nB =
   let rep = (compare_n nA nB) in
   if rep = 0 || rep = (-1) then
      true
   else
      false;;

let compare_b bA bB = match (bA,bB) with
   |([],[]) -> 0
   |([],e::l) when e = 1 -> 1
   |([],e::l) when e = 0 -> -1
   |(e::l,[]) when e = 0 -> 1
   |(e::l,[]) when e = 1 -> -1
   |(ea::la,eb::lb) when ea = 1 && eb = 1-> -(compare_n la lb)
   |(ea::la,eb::lb) when ea < eb -> 1
   |(ea::la,eb::lb) when ea > eb -> (-1)
   |(ea::la,eb::lb) -> compare_n la lb
   |_ -> 0;;

let (>>) bA bB = if (compare_b bA bB) = 1 then
                    true
                 else
                    false;;


let (<<) bA bB = if (compare_b bA bB) = (-1) then
                     true
                  else
                     false;;

let (>>=) bA bB =
   let rep = (compare_b bA bB) in
   if rep = 0 || rep = 1 then
      true
   else
      false;;

let (<<=) bA bB =
   let rep = (compare_b bA bB) in
   if rep = 0 || rep = (-1) then
      true
   else
      false;;

let sign_b bA = match bA with
   |[] -> 1
   |e::l -> if e = 1 then
               (-1)
            else
               1;;

let abs_b bA = match bA with
   |[] -> bA
   |e::l -> if e = 1 then
               bA
            else
               0::bA;;

let _quot_t a = if a < 2 then 0 else 1


let _mod_t a = if a = 1 || a = 3 then 1 else 0

let _div_t a = (_quot_t a, _mod_t a)

let add_n nA nB =
   let rec add_n_rec na nb r = match (na,nb) with
      |([],[])-> if r = 0 then
                    []
                 else
                    [r]
      |([],e::l) -> if r = 0 then
                       e::add_n_rec [] l 0
                    else
                       if e = 1 then
                          0::add_n_rec [] l 1
                       else
                          1::add_n_rec [] l 0
      |(e::l,[]) -> if r = 0 then
                       e::add_n_rec l [] 0
                    else
                       if e = 1 then
                          0::add_n_rec l [] 1
                       else
                          1::add_n_rec l [] 0
      |(eA::lA,eB::lB) -> let aux sum = match sum with
                             |0 -> 0::add_n_rec lA lB 0
                             |1 -> 1::add_n_rec lA lB 0
                             |2 -> 0::add_n_rec lA lB 1
                             |3 -> 1::add_n_rec lA lB 1
                             |_ -> [0]
                          in aux (eA + eB + r)
   in add_n_rec nA nB 0;;

let remove_zero list =
   let list_r = reverse list in
   match list_r with
   |[] -> []
   |(e::l) -> if e = 1 then
                list
             else
   let rec remove_zero_rec lisr = match lisr with
      |[] -> []
      |(e::l) when e = 0 -> remove_zero_rec l
      |(e::l) -> reverse lisr
   in remove_zero_rec (reverse list);;

let diff_n nA nB =
   let rec diff_n_rec na nb r = match (na,nb) with
      |([],[]) -> []
      |(e::l,[]) -> if r = 0 then
                       e::diff_n_rec l [] 0
                    else
                       if e = 1 then
                          0::diff_n_rec l [] 0
                       else
                          1::diff_n_rec l [] 1
      |(eA::lA,eB::lB) -> let aux (a,b,r) = match (a,b,r) with
                             |(0,0,0) -> 0::diff_n_rec lA lB 0
                             |(1,0,0) -> 1::diff_n_rec lA lB 0
                             |(1,1,0) -> 0::diff_n_rec lA lB 0
                             |(0,1,0) -> 1::diff_n_rec lA lB 1
                             |(1,0,1) -> 0::diff_n_rec lA lB 0
                             |(1,1,1) -> 1::diff_n_rec lA lB 1
                             |(0,1,1) -> 0::diff_n_rec lA lB 1
                             |(0,0,1) -> 1::diff_n_rec lA lB 1
                             |_ -> failwith "diff_n: erreur 0"
                          in aux (eA,eB,r)
      |_ -> failwith "diff_n: erreur 1"
   in remove_zero(diff_n_rec nA nB 0);;

let add_b bA bB = match (bA,bB) with
   |([],e::l) -> bB
   |(e::l,[]) -> bA
   |(eA::lA,eB::lB) when eA = 0 && eB = 0 -> 0::add_n lA lB
   |(eA::lA,eB::lB) when eA = 1 && eB = 1 -> 1::add_n lA lB
   |(eA::lA,eB::lB) when eA <> eB && compare_n lA lB = 0 -> []
   |(eA::lA,eB::lB) ->
      let add_b_bis bA bB cm = match (bA,bB,cm) with
         |(eA::lA,eB::lB,1) when eA = 0 && eB = 1 -> 0::diff_n lA lB
         |(eA::lA,eB::lB,1) when eA = 1 && eB = 0  -> 1::diff_n lA lB
         |(eA::lA,eB::lB,1) when eA = 1 && eB = 1  -> 1::add_n lA lB

         |(eA::lA,eB::lB,(-1)) when eA = 0 && eB = 1 -> 1::diff_n lB lA
         |(eA::lA,eB::lB,(-1)) when eA = 1 && eB = 0 -> 0::diff_n lB lA
         |(eA::lA,eB::lB,(-1)) when eA = 1 && eB = 1 -> 1::add_n lA lB
         |_ -> failwith "add_b: erreur 0"
      in add_b_bis bA bB (compare_n bA bB)
   |_ -> failwith "add_b: erreur 1";;

let diff_b bA bB = match (bA,bB) with
   |([],[]) -> []
   |([],e::l) when e = 1 -> 0::l
   |([],e::l) when e = 0 -> 1::l
   |(e::l,[]) when e = 0 -> bA
   |(e::l,[]) when e = 1 -> 1::l
   |(e1::l1,e2::l2) when l1 = l2 && e1 = 0 && e2 = 1 -> []
   |(e1::l1,e2::l2) when l1 = l2 && e1 = 1 && e2 = 0 -> []
   |(eA::lA,eB::lB) ->
      let diff_b_bis bA bB cm = match (bA,bB,cm) with
         |(eA::lA,eB::lB,0) when eA = 1 && eB = 1 -> []
         |(eA::lA,eB::lB,0) when eA = 0 && eB = 0 -> []

         |(eA::lA,eB::lB,1) when eA = 0 && eB = 0 -> 0::diff_n lA lB
         |(eA::lA,eB::lB,1) when eA = 0 && eB = 1 -> 0::add_n lA lB
         |(eA::lA,eB::lB,1) when eA = 1 && eB = 0 -> 1::add_n lA lB
         |(eA::lA,eB::lB,1) when eA = 1 && eB = 1 -> 1::diff_n lA lB

         |(eA::lA,eB::lB,(-1)) when eA = 0 && eB = 0 -> 1::diff_n lB lA
         |(eA::lA,eB::lB,(-1)) when eA = 1 && eB = 0 -> 1::add_n lA lB
         |(eA::lA,eB::lB,(-1)) when eA = 1 && eB = 1 -> 0::diff_n lB lA
         |(eA::lA,eB::lB,(-1)) when eA = 0 && eB = 1 -> 0::add_n lA lB
         |_ -> failwith "diff_b: erreur 0"
      in remove_zero(diff_b_bis bA bB (compare_n lA lB))
   |_ -> failwith "diff_b: erreur 1";;

(*
let rec shift bA d = match d with
   |0 -> bA
   |_ -> shift (0::bA) (d - 1);;
*)
let mult_n nA nB =
   let rec mult_n_rec m a sum = match m with
      |[] -> sum
      |e::l when e = 0 -> mult_n_rec l (shift a 1) sum
      |e::l -> mult_n_rec l (shift a 1) (add_n a sum)
   in mult_n_rec nA nB [];;

let mult_b bA bB = match (bA,bB) with
   |([],[]) -> []
   |(e::l,[]) -> []
   |([],e::l) -> []
   |(eA::lA,eB::lB) ->
      let mult_b_bis bA bB cm = match (bA,bB,cm) with
         |(eA::lA,eB::lB,0) when eA = eB -> 0::mult_n lA lB
         |(eA::lA,eB::lB,1) when eA = eB -> 0::mult_n lA lB
         |(eA::lA,eB::lB,(-1)) when eA = eB -> 0::mult_n lB lA

         |(eA::lA,eB::lB,0) -> 1::mult_n lA lB
         |(eA::lA,eB::lB,1) -> 1::mult_n lA lB
         |(eA::lA,eB::lB,(-1)) -> 1::mult_n lB lA
         |_ -> failwith "mult_b: erreur 0"
      in mult_b_bis bA bB (compare_n lA lB);;
(*
let quot_n bA bB =
   let rec quot_n_rec bAa q = match q with
      |_ when bAa << bB -> (q, false)
      |_ when bAa = bB -> (q, true)
      |_ when bAa >>= bB -> quot_n_rec (diff_b bAa bB) (add_b q [0;1])
      |_ -> (q, false)
   in quot_n_rec bA [];;

let quot_b bA bB = match (bA,bB) with
   |([],[]) -> failwith "quot_b: division by 0"
   |([],e::l) -> []
   |(e::l,[]) -> failwith "quot_b: division by 0"
   |(_,_) when bA = bB -> [0;1]
   |(eA::lA,eB::lB) when lA = lB && eA <> eB -> [1;1]
   |(eA::lA,eB::lB) ->
      let quot_b_bis bA bB cm = match (bA,bB,cm) with
         |(eA::lA,eB::lB,1) when eA = 0 && eB = 0 -> let (result, equal) = quot_n bA bB in
                                                     if equal = true then
                                                        add_b result [0;1]
                                                     else
                                                        result
         |(eA::lA,eB::lB,1) when eA = 0 && eB = 1 -> let (result, equal) = quot_n bA (0::lB) in
                                                     mult_b result [1;1]
         |(eA::lA,eB::lB,1) when eA = 1 && eB = 0 -> let (result, equal) = quot_n (add_b (0::lA) (0::lB)) (0::lB) in
                                                     mult_b result [1;1]
         |(eA::lA,eB::lB,1) when eA = 1 && eB = 1 -> let (result, equal) = quot_n (0::lA) (0::lB) in
                                                     add_b result [0;1]

         |(eA::lA,eB::lB,(-1)) when eA = 0 && eB = 0 -> let (result, equal) = quot_n bA bB in
                                                        result
         |(eA::lA,eB::lB,(-1)) when eA = 0 && eB = 1 -> let (result, equal) = quot_n bA (0::lB) in
                                                        result
         |(eA::lA,eB::lB,(-1)) when eA = 1 && eB = 0 -> [1;1]
         |(eA::lA,eB::lB,(-1)) when eA = 1 && eB = 1 -> [0;1]
         |_ -> failwith "quot_b: erreur 0"
      in quot_b_bis bA bB (compare_n lA lB);;
*)
(*
let quot_n nA nB =
   let s = (List.length(nA)-List.length(nB)) in
   let rec quot_n_rec a b res sh = match res with
      |e::l when (compare_n a b) = 0 && sh = 0 -> (0::remove_zero(l),true)
      |_ when sh = (-1) -> (0::remove_zero(res),false)
      |_ when (a >=! b) -> quot_n_rec (diff_n a b) (tail b) (1::res) (sh-1)
      |_ -> quot_n_rec a (tail b) (0::res) (sh-1)
   in quot_n_rec nA (shift nB s) [] s;;

let quot_b bA bB = match (bA,bB) with
   |([],[]) -> failwith "quot_b: division by 0"
   |([],e::l) -> []
   |(e::l,[]) -> failwith "quot_b: division by 0"
   |(_,_) when bA = bB -> [0;1]
   |(eA::lA,eB::lB) when lA = lB && eA <> eB -> [1;1]
   |_ when bA << bB -> let quot_aux bA bB = match (bA,bB) with
                          |(eA::lA,eB::lB) when eA = 0 && eB = 0 -> []
                          |(eA::lA,eB::lB) when eA = 0 && eB = 1 -> [0]
                          |(eA::lA,eB::lB) when eA = 1 && eB = 0 -> [1;1]
                          |(eA::lA,eB::lB) when eA = 1 && eB = 1 -> [0;1]
                          |_ -> failwith "quot_b: erreur aux"
                       in quot_aux bA bB
   |(eA::lA,eB::lB) ->
      let quot_b_bis bA bB cm = match (bA,bB,cm) with
         |(eA::lA,eB::lB,1) when eA = 0 && eB = 0 -> let (result, equal) = quot_n bA bB in
                                                     if equal = true then
                                                        add_b result [0;1]
                                                     else
                                                        result
         |(eA::lA,eB::lB,1) when eA = 0 && eB = 1 -> let (result, equal) = quot_n bA (0::lB) in
                                                     mult_b result [1;1]
         |(eA::lA,eB::lB,1) when eA = 1 && eB = 0 -> let (result, equal) = quot_n (add_b (0::lA) (0::lB)) (0::lB) in
                                                     mult_b result [1;1]
         |(eA::lA,eB::lB,1) when eA = 1 && eB = 1 -> let (result, equal) = quot_n (0::lA) (0::lB) in
                                                     add_b result [0;1]

         |(eA::lA,eB::lB,(-1)) when eA = 0 && eB = 0 -> let (result, equal) = quot_n bA bB in
                                                        result
         |(eA::lA,eB::lB,(-1)) when eA = 0 && eB = 1 -> let (result, equal) = quot_n bA (0::lB) in
                                                        result
         |(eA::lA,eB::lB,(-1)) when eA = 1 && eB = 0 -> [1;1]
         |(eA::lA,eB::lB,(-1)) when eA = 1 && eB = 1 -> [0;1]
         |_ -> failwith "quot_b: erreur 0"
      in quot_b_bis bA bB (compare_n lA lB);;
*)

let quot_n nA nB =
   let s = (List.length(nA)-List.length(nB)) in
   let rec quot_n_rec a b res sh = match res with
      |_ when a = [] && sh = (-1) -> (remove_zero(res),true)
      |_ when sh = (-1) -> (remove_zero(res),false)
      |_ when (a >>= b) -> quot_n_rec (diff_b a b) (tail b) (1::res) (sh-1)
      |_ -> quot_n_rec a (tail b) (0::res) (sh-1)
   in quot_n_rec (0::nA) (0::shift nB s) [] s;;

let quot_b bA bB = match (bA,bB) with
   |([],[]) -> failwith "quot_b: division by 0"
   |([],e::l) -> []
   |(e::l,[]) -> failwith "quot_b: division by 0"
   |(_,_) when bA = bB -> [0;1]
   |(eA::lA,eB::lB) when lA = lB && eA <> eB -> [1;1]
   |_ when bA <<! bB -> let quot_aux bA bB = match (bA,bB) with
                          |(eA::lA,eB::lB) when eA = 0 && eB = 0 -> []
                          |(eA::lA,eB::lB) when eA = 0 && eB = 1 -> [0]
                          |(eA::lA,eB::lB) when eA = 1 && eB = 0 -> [1;1]
                          |(eA::lA,eB::lB) when eA = 1 && eB = 1 -> [0;1]
                          |_ -> failwith "quot_b: erreur aux"
                       in quot_aux bA bB
   |(eA::lA,eB::lB) ->
      let quot_b_bis bA bB cm = match (bA,bB,cm) with
         |(eA::lA,eB::lB,1) when eA = 0 && eB = 0 -> let (result, equal) = quot_n lA lB in
                                                     0::result
         |(eA::lA,eB::lB,1) when eA = 0 && eB = 1 -> let (result, equal) = quot_n lA lB in
                                                     if equal = true then
                                                        1::result
                                                     else
                                                        1::result
         |(eA::lA,eB::lB,1) when eA = 1 && eB = 0 -> let (result, equal) = quot_n lA lB in
                                                     if equal = true then
                                                        1::result
                                                     else
                                                        add_b (1::result) [1;1]
         |(eA::lA,eB::lB,1) when eA = 1 && eB = 1 -> let (result, equal) = quot_n lA lB in
                                                     if equal = true then
                                                        0::result
                                                     else
                                                        add_b (0::result) [0;1]
         |(eA::lA,eB::lB,(-1)) when eA = 0 && eB = 0 -> let (result, equal) = quot_n lA lB in
                                                        result
         |(eA::lA,eB::lB,(-1)) when eA = 0 && eB = 1 -> let (result, equal) = quot_n lA lB in
                                                        result
         |(eA::lA,eB::lB,(-1)) when eA = 1 && eB = 0 -> [1;1]
         |(eA::lA,eB::lB,(-1)) when eA = 1 && eB = 1 -> [0;1]
         |_ -> failwith "quot_b: erreur 0"
      in quot_b_bis bA bB (compare_n lA lB);;


let mod_b bA bB = match (bA,bB) with
   |_ when bA = bB -> []
   |_ when (mult_b (quot_b bA bB) bB) = bA -> []
   |_ -> diff_b bA (mult_b (quot_b bA bB) bB);;

let div_b bA bB = ((quot_b bA bB),(mod_b bA bB));;