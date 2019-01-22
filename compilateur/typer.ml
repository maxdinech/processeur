open Ast
open Lexing

let jumps = Hashtbl.create 17

exception TypingError of string

let int_of_bin n = int_of_string ("0b"^(string_of_int n))

let check_val b =
  let i = int_of_bin b in
  if not (0 <= i && i <= 65535)
  then raise (TypingError ("Format de nombre invalide : "^(string_of_int b)))

let check_reg b =
  let i = int_of_bin b in
  if not (0 <= i && i <= 31)
  then raise (TypingError ("Numéro de registre invalide : "^(string_of_int b)))

let check_pos s =
  if not (Hashtbl.mem jumps s)
  then raise (TypingError ("Position inconnue : '" ^ s ^ "'"))

let add_pos s i =
  if Hashtbl.mem jumps s
  then raise (TypingError ("Position en double : '" ^ s ^ "'"))
  else Hashtbl.add jumps s i

let check_prog prog_ =
  let d = ref 0 in
  let rec add_marks i = function
    | [] -> ()
    | t::q -> begin match t with
        | Setpos s -> d := 0 ; add_pos s i
        | _        -> d := 1 end ; add_marks (i + !d) q in
  let rec check_rest = function
    | [] -> ()
    | t::q -> begin match t with
        | Lw (v, r) -> check_val v ; check_reg r
        | Ci (v, r) -> check_val v ; check_reg r
        | Di (r1, r2)   -> check_reg r1 ; check_reg r2
        | Te (r1, r2)   -> check_reg r1 ; check_reg r2
        | Incr (r1, r2) -> check_reg r1 ; check_reg r2
        | J p -> check_pos p
        | Je (p, r1, r2)  -> check_pos p ; check_reg r1 ; check_reg r2
        | Jne (p, r1, r2) -> check_pos p ; check_reg r1 ; check_reg r2
        | Setpos s -> () end
  in add_marks 0 prog_ ; check_rest prog_ ; jumps

