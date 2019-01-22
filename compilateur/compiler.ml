open Ast

let rec (^*) s n = if n < 0 then failwith "" else if n == 0 then "" else s^(s ^* (n-1))

let bstr_of_bin b =
  let s = string_of_int b in
  ("0" ^* (6 - String.length s)) ^ s

let bob = bstr_of_bin

let bstr_of_int n =
  if n < 0 then failwith "nombre invalide" else
  if n = 0 then "000000000000" else
  let rec aux acc n =
    if n = 0 then acc else
    aux (string_of_int (n land 1) :: acc) (n lsr 1)
  in
  let s = String.concat "" (aux [] n) in
  ("0" ^* (12 - String.length s)) ^ s

let boi = bstr_of_int

let traite_jumps fichier =
  let i = ref 0 in
  let rec aux = function | [] -> []
                         | t::q -> let instr, pos = t in
    (match instr with
      | Je (p, r1, r2)  -> let p2 = ("virtual_jump_no_" ^ (string_of_int !i)) in
                           i := !i + 1;
                           (Te (r1, r2)) :: (J p2) :: (J p) :: (Setpos p2) :: []
      | Jne (p, r1, r2) -> (Te (r1, r2)) :: (J p) :: []
      | x -> [x] ) @ (aux q)
  in match fichier with 
      | None   -> []
      | Some p -> (aux p)

let production jumps prog_ =
  let rec aux = function | [] -> ""
                         | t::q -> (match t with
      | Lw (r, v)     -> "0011"^(bob r)^(bob v)^"\n"
      | Ci (r, v)     -> "0101"^(bob r)^(bob v)^"\n"
      | Di (r1, r2)   -> "0111"^(bob r1)^(bob r2)^"\n"
      | Incr (r1, r2) -> "1100"^(bob r1)^(bob r2)^"\n"
      | Te (r1, r2)   -> "0001"^(bob r1)^(bob r2)^"\n"
      | J p           -> "0000"^(boi (Hashtbl.find jumps p))^"\n"
      | Setpos p      -> ""
      | _             -> failwith "pas normal ici" ) ^ aux q
  in aux prog_
