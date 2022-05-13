
open Random;;
open Num;;
open Big_int;;
open Char;;
open Camlimages;;
open Color;;
let load_rgb_matrix name =
  let img = Images.load name [] in
  let gimg = Graphic_image.array_of_image img in
  let rgb color =
    let quot n = n mod 256, n / 256 in
    let b, rg = quot color in
    let g, r = quot rg in
    r, g, b
  in
  Array.map (Array.map rgb) gimg;;

  let image = load_rgb_matrix "mario.bmp";;

let (r,g,b) = image.(0).(0);;
Printf.printf "test : %d" r;;




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
  let tmp = (i2n (Random.full_int Int.max_int))in
  (*Printf.printf "random: %s \n" (n2s tmp);*)
  if (mod_num tmp two = zero) = false then
    if is_prime tmp (i2n 128) then tmp
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


let string_to_bits s = 
      String.fold_right (fun car out -> let ascii = Char.code car in 
      let rec ascii_to_bits ascii (out,acc) = 
        if acc < 8 then
          if ascii/2 = 0 then ascii_to_bits (ascii/2) (((ascii mod 2)::(out)),acc+1)
          else ascii_to_bits (ascii/2) (((ascii mod 2)::(out)),acc+1)
        else
          out
      in
      ascii_to_bits ascii (out,0)) s []
    ;;



    let recupbits img size = 
      let rec recupbits_aux img size acc x y = 
        let (r,g,b) = img.(y).(x)in
        if size = 0 then 
          acc
        else
          if y < Array.length img then
            recupbits_aux img (size-1) (acc@[r land 1]) x (y+1)
          else
            recupbits_aux img (size-1) (acc@[r land 1]) (x+1) 0
      in
      recupbits_aux img size [] 0 0;;

let len_binary a =
  let rec len_binary_aux a r =
    if a // (i2n 2) =/ zero then r+1
    else len_binary_aux (quo_num a (i2n 2)) r+1 in
    len_binary_aux a 0
;;

let num_to_bits num = 
  let rec num_to_bits_aux num acc =
    (*Printf.printf "number : %s" (n2s num);*)
    if num =/ zero then (n2i (mod_num num two))::acc
    else num_to_bits_aux (quo_num num two) ((n2i (mod_num num two))::acc) in
    let list = num_to_bits_aux num [] in
  Array.of_list list;;

let bits_to_num bits =
  let (number,index) = List.fold_right (fun bit (acc,n) -> if bit = 0 then (acc,n+1) 
  else (acc +/ ( i2n (bit) */ (two **/ (i2n n))),n+1)) bits (zero,0) in
  number;;

  let bits_to_string bits = 
    let (res,acc) = Array.fold_left (fun (out,acc) bit -> 
      (*List.iter (Printf.printf "/ %d ") acc;*)
      (*Printf.printf "\n";*)
      if List.length acc = 7 then 
        let cha = (chr (n2i (bits_to_num (acc@[bit])))) in ((out^String.make 1 cha),[])
    else (out, (acc@[bit]))) ("",[]) bits
  in 
  res;;

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

(* --------------------------------------------CRYPT AND DECRYPT ------------------------------------------------ *)

let (d, e, n) = rsa_keys;;


(* TAILLE DE N A NE PAS DEPASSER *)
Printf.printf "d: %s e: %s n: %s \n" (n2s d) (n2s e) (n2s n);;
let size = len_binary n;;
Printf.printf "binarylength n : %d\n" (size);;

(* message à encrypt transformé en bit*)
let bits = string_to_bits "hello world!";; 
List.iter (Printf.printf "%d / ") bits;;
Printf.printf "\n";;

(* ---------- bits convertie en grand nombre ----------------- *)
  let number = bits_to_num bits;;
  Printf.printf "number to encrypt : %s\n" (n2s number);;

(*Cryptage du grand nombre*)
  let crypted = mod_num (power_num (number) e) n;;
  Printf.printf "crypted: %s\n" (n2s crypted);;

  (* passage du grand nombre crypté en bits*)
  let bitscrypted = num_to_bits crypted;;
  Printf.printf "length %d\n" (Array.length bitscrypted);;
  Array.iter (Printf.printf "%d / ") bitscrypted;;
  Printf.printf "\n";;
 

  (* affectation du message crypté dans l'images *)
  let h = Array.length image;;
  let w = Array.length image.(0);;
  Printf.printf "w : %d / h : %d \n" w h;; 

  let black = {r = 0; g=0; b=0; } in
  let acc = ref 0 in
  let img = Rgb24.make w h black in
    for i = 0 to w-1 do
      for j = 0 to h-1 do
          let (_r,_g,_b) = image.(j).(i) in
          let colorencrypt = _r land 254 in 
          if !acc < Array.length bitscrypted then    
            let rfinal = (colorencrypt) lor (bitscrypted.(!acc)) in
            let couleur = {r = rfinal; g = _g ; b = _b} in
            acc := !acc+1;
            Rgb24.set img i j couleur;
            (*Printf.printf "rfinal : %d / g : %d / b : %d \n" rfinal _g _b;*)
          else
            let rfinal = colorencrypt lor (Random.int 1) in
            let couleur = {r = rfinal; g = _g ; b = _b}in
            Rgb24.set img i j couleur;
            (*Printf.printf "rfinal : %d / g : %d / b : %d \n" rfinal _g _b;*)
      done;
    done ;

    (* sauvegarde de l'image *)
    Bmp.save "mario_res.bmp" [] (Images.Rgb24 img);;


    (*recupération du message crypté*)
    let img2decrypt = load_rgb_matrix "mario_res.bmp";;
    
    
    (* recupération des bits dans l'image*)
    let bitsdecrypt = recupbits img2decrypt ((Array.length bitscrypted));;
    Printf.printf "length %d\n" (List.length bitsdecrypt);;
    List.iter (Printf.printf "%d / ") bitsdecrypt;;
    Printf.printf "\n";;
    let numdecrypt = bits_to_num bitsdecrypt;;
    Printf.printf "nums to decrypt : %s \n" (n2s numdecrypt);;


    let decrypted = mod_pow numdecrypt d n;;
Printf.printf "decrypted %s\n" (n2s decrypted);;

let bitsdecrypted = num_to_bits decrypted;;
Printf.printf "length : %d \n" (Array.length bitsdecrypted);;
Array.iter (Printf.printf "%d / ") bitsdecrypted;;
Printf.printf "\n";;
let resstring = bits_to_string bitsdecrypted;;
Printf.printf "message decrypter : %s" resstring;;
