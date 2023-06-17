open Z

let rec break_rec key nbr =
   if (key mod nbr) = zero then
      (nbr, key / nbr)
   else
      break_rec key (pred nbr);;

let break key =
   let (a, _) = key in
   let r_key = sqrt a in
   if  r_key mod (succ one) = one then
      break_rec a (pred r_key)
   else
      break_rec a r_key;;