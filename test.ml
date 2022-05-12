
open Random;;
open Num;;
open Big_int;;

Random.self_init;;

let rec mod_pow x y n = 
  let base = x mod n in
  match y with 
  | 0 -> 1
  | 1 -> x
  | y -> 
      if y mod 2 = 0 then mod_pow (base * ( base mod n)) (y / 2) n
      else (base * (mod_pow base (y - 1) n)) mod n
;;

let rec pow a b = 
  if b = 0 then 1
  else if b = 1 then a
  else 
    let tmp = pow a (b / 2) in
    if b mod 2 == 0 then tmp * tmp
    else a * tmp * tmp
  ;;
;;

let miller_rabin n a =
  (*n-1 = 2^s * d, retourne le couple (d, s)*)
  let rec miller_rabin_sd nm1 s =
    let nm1_shift = nm1 lsr s in
    let last_bit = nm1_shift land 1 in
    if last_bit = 0 then miller_rabin_sd nm1 (s + 1)
    else (nm1_shift, s)
  in 

  let (d, s) = miller_rabin_sd (n - 1) 0 in

  let x = mod_pow a d n in

  if x = 1 || x = (n-1) then false
  else
    let rec miller_rabin_aux i =
      if i = (s - 1) then true
      else 
        let x = (x * x) mod n in
        if x = n - 1 then false
        else miller_rabin_aux (i + 1)
    in miller_rabin_aux 0;;
;;
  
      
let is_prime n k = 
  let rec is_prime_aux i = 
    if i = k then true
    else
      let a = 2 + Random.full_int (n - 2) in
      if miller_rabin n a = true then false
      else is_prime_aux (i + 1) 
  in is_prime_aux 0;;
;;

let rec generate_prime () = 
  let tmp = Random.int 230 in
  Printf.printf "random: %d \n" tmp;
  if tmp mod 2 != 0 then
    if is_prime tmp 128 then tmp
    else generate_prime ()
  else generate_prime ()
  ;;
;;

let rec bin_pgcd a b = 
  Printf.printf "a: %d b: %d\n" a b;
  let find_sup a b = if a > b then a else b in let sup = find_sup a b in
  let find_sub a b = if a > b then b else a in let sub = find_sub a b in
  if a = 0 then b
  else if sup  mod 2 = 0 && sub mod 2 = 0 then 2 * (bin_pgcd (sup / 2) (sub / 2))
  else if sup mod 2 != 0 && sub mod 2 = 0 then bin_pgcd sup (sub / 2)
  else if sup mod 2 = 0 && sub mod 2 != 0 then bin_pgcd (sup / 2) sub
  else bin_pgcd ((sup - b) / 2) sub
;;
let test = bin_pgcd 30 24;;
Printf.printf "test: %d\n" test;;

let rec find_coprime  i phi= 
  Printf.printf "i: %d, phi: %d\n" i phi;
  if i >= phi then -1
  else if bin_pgcd phi i = 1 then i
  else find_coprime (i + 1) phi
;;

let t = find_coprime 2 35;;
Printf.printf "coprime: %d\n" t;;

let rec inv_mod a b =
  if b = 0 then (a, 1, 0)
  else 
    let (d, u, v) = inv_mod b (a mod b) in
    (d, v, u - ((a / b) * v))
;;
let rsa_keys = 
  let p = generate_prime () in
  Printf.printf "got p: %d \n" p;
  let q = generate_prime () in
  Printf.printf "got q: %d \n" q;
  let n = p * q in
  Printf.printf "got n: %d \n" n;
  let phi = (p - 1) * (q - 1) in
  Printf.printf "got phi: %d\n" phi;
  let e = find_coprime 2 phi in
  Printf.printf "got e: %d\n" e; 
  let (d, u, v) = inv_mod e phi in
  Printf.printf "got d: %d, u: %d, v: %d\n" d u v;
  (d, e, n)
;;

let (d, e, n) = rsa_keys;;

Printf.printf "d: %d e: %d n: %d \n" d e n;;

let crypt m e n = 
  if m >= n then
    mod_pow m e n 
  else 
    pow m e
  ;;
;;

let decrypt c d n = 
  if c >= n then
    mod_pow c d n
  else 
    pow c d
  ;;
;;

let crypted = crypt 123456789 e n;;
Printf.printf "crypted: %d\n" crypted;;
let decrypted = decrypt crypted d n;;
Printf.printf "decrypted %d\n" decrypted;;