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

let div a b = let q = a/b and r = a mod b in
   if r >= 0 then
      (q, r)
   else
      (q - (sign b), r + (sign b) * b);;