
open Random;;
open Num;;
open Big_int;;

Random.self_init ();;

let i2n n = num_of_int n;;
let n2i n = int_of_num n;;
let i2b n = big_int_of_int n;;
let b2n b = num_of_big_int b;;
let n2b n = big_int_of_num n;;
let n2s n = string_of_num n;;
let bsr b s = shift_right_big_int b s;;
let band a b = and_big_int a b;;
let one = num_of_int 1;;
let zero = num_of_int 0;;
let two = num_of_int 2;;


let rec mod_pow x y n = 
  let base = mod_num x n in
  if y =/ zero then i2n 1
  else if y =/ one then x
  else 
    if (mod_num y two) =/ zero then mod_pow (base */ ( mod_num base n)) (y // two) n
    else mod_num (base */ (mod_pow base (y -/ one) n)) n
  ;;  
;;

let miller_rabin n a =
  (*n-1 = 2^s * d, retourne le couple (d, s)*)
  let rec miller_rabin_sd nm1 s =
    let nm1_shift = bsr nm1 s in
    let last_bit = band nm1_shift (i2b 1) in
    if (eq_big_int last_bit (i2b 0)) = true then miller_rabin_sd nm1 (s + 1)
    else ((b2n nm1_shift), s)
  in 

  let (d, s) = miller_rabin_sd  (n2b (n -/ one)) 0 in

  let x = mod_pow a d n in

  if x =/ one || x =/ (n -/ one) then false
  else
    let rec miller_rabin_aux i =
      if i = (s - 1) then true
      else 
        let y = mod_num (x */ x) n in
        if y =/ (n -/ one) then false
        else miller_rabin_aux (i + 1)
    in miller_rabin_aux 0;;
;;
  
      
let is_prime n k = 
  let rec is_prime_aux i = 
    if i =/ k then true
    else
      let a = 2 + Random.full_int (n2i (n -/ two)) in
      (*Printf.printf "a: %d\n" a; *)
      let a_num = i2n a in
      if miller_rabin n a_num = true then false
      else is_prime_aux (i +/ one) 
  in is_prime_aux zero;;
;;

let rec generate_prime () = 
  let tmp = i2n (Random.full_int 1000) in
  (*Printf.printf "random: %s \n" (n2s tmp);*)
  if (mod_num tmp two = zero) = false then
    if is_prime tmp (i2n 8192) then tmp
    else generate_prime ()
  else generate_prime ()
  ;;
;;

let rec bin_pgcd a b = 
  (*Printf.printf "a: %s b: %s\n" (n2s a) (n2s b);*)
  let find_sup a b = if a >/ b then a else b in let sup = find_sup a b in
  let find_sub a b = if a >/ b then b else a in let sub = find_sub a b in
  if a =/ zero then b
  else if (mod_num sup two =/ zero) = true && (mod_num sub two =/ zero ) = true then two */ (bin_pgcd (sup // two) (sub // two))
  else if (mod_num sup two =/ zero) = false && (mod_num sub two =/ zero) = true then bin_pgcd sup (sub // two)
  else if (mod_num sup two =/ zero) = true && (mod_num sub two =/ zero) = false then bin_pgcd (sup // two) sub
  else bin_pgcd ((sup -/ b) // two) sub
;;

let rec find_coprime i phi= 
  (*Printf.printf "i: %s, phi: %s\n" (n2s i) (n2s phi);*)
  if i >=/ phi then i2n (-1)
  else if bin_pgcd phi i =/ one then i
  else find_coprime (i +/ one) phi
;;


let extended_euclid a n = 
  let rec extended_euclid_aux t r newr newt = 
    if (newr =/ zero) = false then
      let quo = quo_num r newr in
      let _t = newt in
      let _newt = t -/ (quo */ newt) in
      let _r = newr in 
      let _newr = r -/ (quo */ newr) in
      extended_euclid_aux _t _r _newr _newt
    else
      (r, t)
    in 

  let (r, t) = extended_euclid_aux zero n a one in
  (*Printf.printf "r: %s, t: %s \n" (n2s r) (n2s t);*)
  if r >/ one then (i2n (-1))
  else if t </ zero then (t +/ n)
  else t
;;


let rsa_keys = 
  let p = generate_prime () in
  Printf.printf "got p: %s \n" (n2s p);
  let q = generate_prime () in
  Printf.printf "got q: %s \n" (n2s q);
  let n = p */ q in
  Printf.printf "got n: %s \n" (n2s n);
  let phi = (p -/ one) */ (q -/ one) in
  Printf.printf "got phi: %s\n" (n2s phi);
  let e = find_coprime two phi in
  Printf.printf "got e: %s\n" (n2s e); 
  let d = extended_euclid e phi in
  (d, e, n)
;;

let (d, e, n) = rsa_keys;;

Printf.printf "d: %s e: %s n: %s \n" (n2s d) (n2s e) (n2s n);;

let crypted = mod_num (power_num (i2n 400) e) n;;

Printf.printf "crypted: %s\n" (n2s crypted);;

let decrypted = mod_num (power_num crypted d) n;;
Printf.printf "decrypted %s\n" (string_of_num decrypted);;