open Netlist_ast

let nb_cycles = 1024

(*pour demander inputs print puis read_line*)

let read_arg a env = (* retourne la valeur de arg*)
match a with
|Avar(x) -> Env.find x env
|Aconst(v) -> v

let applique_not v =
match v with
|VBit(b) -> VBit(not b)
|VBitArray(ba) -> let n = Array.length ba in
                  let res = Array.make n true in
                  for i=0 to n-1 do res.(i) <- not ba.(i) done;
                  VBitArray(res)

let applique_binop f v1 v2 =
match v1,v2 with
|VBit(b1),VBit(b2) -> VBit(f b1 b2)
|VBit(_),VBitArray(_) |VBitArray(_),VBit(_) -> failwith "données incompatibles bool et bool array"
|VBitArray(ba1),VBitArray(ba2) -> let n1 = Array.length ba1 and n2 = Array.length ba2 in
  if n1!=n2 then failwith "Array de tailles differentes";
  let res = Array.make n1 true in
  for i=0 to n1-1 do res.(i)<- f ba1.(i) ba2.(i) done;VBitArray(res)

let applique_mux v1 v2 s =
match v1,v2,s with
|VBit(_),VBit(_),VBit(bs) -> if bs then v2 else v1
|VBitArray(ba1),VBitArray(ba2),VBitArray(bas) -> let n1 = Array.length ba1 and n2 = Array.length ba2 and ns = Array.length bas in
  if n1!=n2 || n1!=ns || n2!=ns then failwith "Array de tailles differentes";
  let res = Array.make n1 true in
  for i=0 to n1-1 do
      if bas.(i) then res.(i) <- ba2.(i) else res.(i) <- ba1.(i)
  done;VBitArray(res)
|_,_,_ -> failwith "donnees incompatibles bool et bool array"

let applique_concat v1 v2 =
match v1,v2 with
|VBit(b1),VBit(b2) -> VBitArray([|b1;b2|])
|VBit(b1),VBitArray(ba2) -> let n = Array.length ba2 in
  let res = Array.make (n+1) true in
  res.(0) <- b1;
  for i=1 to n do res.(i) <- ba2.(i-1) done;
  VBitArray(res)
|VBitArray(ba1),VBit(b2) -> let n = Array.length ba1 in
  let res = Array.make (n+1) b2 in
  for i=0 to n-1 do res.(i) <- ba1.(i) done;
  VBitArray(res)
|VBitArray(ba1),VBitArray(ba2) -> let n1 = Array.length ba1 and n2 = Array.length ba2 in
  let res = Array.make (n1+n2) true in
  for i=0 to n1-1 do res.(i) <- ba1.(i) done;
  for i=0 to n2-1 do res.(n1+i) <- ba2.(i) done;
  VBitArray(res)

let applique_slice i1 i2 v =
match v with
|VBit(b) when i1=0 && i2=0 -> v
|VBit(_) -> failwith "i2 trop grand pour slice"
|VBitArray(ba) when i1=i2 -> VBit(ba.(i1))
|VBitArray(ba) -> let n = Array.length ba in
  if n<=i2 then failwith "i2 trop grand pour slice";
  let res = Array.make (i2-i1+1) true in
  for i=0 to i2-i1 do res.(i) <- ba.(i+i1) done;
  VBitArray(res)

let applique_select i v =
match v with
|VBit(b) -> if i=0 then v else failwith "i trop grand pour select"
|VBitArray(ba) -> let n = Array.length ba in
  if i>=n then failwith "i trop grand pour select";
  VBit(ba.(i))

let ident_of_val v =
match v with
|VBit(b) -> if b then "1" else "0"
|VBitArray(ba) -> let n = Array.length ba and res = ref "" in
  for i=0 to n-1 do
      if ba.(i) then res := !res ^ "1" else res := !res ^ "0"
  done
  ;!res

let calcule_val_e e env env_prec env_memo =
match e with
|Earg(a) -> read_arg a env
|Ereg(y) -> Env.find y env_prec
|Enot(a) -> applique_not (read_arg a env)
|Ebinop(op,a1,a2) -> begin
  match op with
  |Or -> applique_binop (fun x y -> x||y) (read_arg a1 env) (read_arg a2 env)
  |Xor -> applique_binop (fun x y -> (x && not(y)) || (not(x) && y)) (read_arg a1 env) (read_arg a2 env)
  |And -> applique_binop (fun x y -> x && y) (read_arg a1 env) (read_arg a2 env)
  |Nand -> applique_binop (fun x y -> not (x && y)) (read_arg a1 env) (read_arg a2 env)
  end
|Emux(a1,a2,s) -> applique_mux (read_arg a1 env) (read_arg a2 env) (read_arg s env)
|Erom(addr_size,word_size,a) |Eram(addr_size,word_size,a,_,_,_) -> begin let addr = ident_of_val (read_arg a env) in
    if (String.length addr) != addr_size then failwith "taille addresse mauvaise";
    try applique_slice 0 (word_size-1) (Env.find addr env_memo)
    with |Not_found -> (*cas où addr non init*)
        if word_size = 1 then VBit(false) else VBitArray(Array.make word_size false)
    end
|Econcat(a1,a2) -> applique_concat (read_arg a1 env) (read_arg a2 env)
|Eslice(i1,i2,a) -> applique_slice i1 i2 (read_arg a env)
|Eselect(i,a) -> applique_select i (read_arg a env)

let convertit_entree s =
let n = String.length s in
if n = 1 then begin
  if s="0" then VBit(false) else VBit(true)
  end
else begin
  let res = Array.make n true in
  for i=0 to n-1 do
      if s.[i] = '0' then res.(i) <- false
      done;
  VBitArray(res)
  end

let rec demande_entrees inputs env =
match inputs with
|[] -> env
|x::q -> print_string x; print_string " = ";
  let s = read_line () in
  let e = convertit_entree s in
  demande_entrees q (Env.add x e env)

let rec determiner_reg l_eq =
match l_eq with
|[] -> []
|(_,Ereg(x))::q -> x::determiner_reg q
|_::q -> determiner_reg q

let rec init_reg l_reg env =
match l_reg with
|[] -> env
|reg::q -> init_reg q (Env.add reg (VBit(false)) env)

let print_valeur v =
match v with
|VBit(b) -> if b then print_string "1" else print_string "0"
|VBitArray(ba) -> let n = Array.length ba in
  for i=0 to n-1 do
      if ba.(i) then print_string "1" else print_string "0"
  done

let rec retourne_sortie out env =
match out with
|[] -> ()
|x::q -> print_string x; print_string " = "; print_valeur (Env.find x env); print_newline(); retourne_sortie q env

let rec print_list l =
match l with
|[] -> print_newline()
|t::q -> print_string t;print_string " ; ";print_list q

let simulator p =
let env_prec = init_reg (determiner_reg p.p_eqs) Env.empty
 in
let rec applique_equations l_eq env env_prec env_memo env_memo2 =
match l_eq with
|[] -> (env,env_memo2)
|(x,Eram(_,_,_,write_enable,write_addr,data))::q  when (read_arg write_enable env) = VBit(true) ->
    let e = snd(List.hd l_eq) in
    let valeur = calcule_val_e e env env_prec env_memo in
    applique_equations q (Env.add x valeur env) env_prec env_memo (Env.add (ident_of_val (read_arg write_addr env )) (read_arg data env ) env_memo2)
|(x,e)::q -> let valeur = calcule_val_e e env env_prec env_memo in
    applique_equations q (Env.add x valeur env) env_prec env_memo env_memo2
in
let rec boucle numero env env_prec env_memo=
if numero = nb_cycles then ()
else begin
    print_string "Cycle "; print_int numero; print_newline ();
    let env_inputs = demande_entrees p.p_inputs env in
    let (env2,env_memo2) = applique_equations p.p_eqs env_inputs env_prec env_memo env_memo in
    print_string "résultat :"; print_newline ();
    retourne_sortie p.p_outputs env2;
    boucle (numero+1) env2 env2 env_memo2
end
in boucle 0 Env.empty env_prec Env.empty

let print_only = ref false
let number_steps = ref (-1)

let compile filename =
  try
    let p = Netlist.read_file filename in
    let out_name = (Filename.chop_suffix filename ".net") ^ "_sch.net" in
    let out = open_out out_name in
    let close_all () =
      close_out out
    in
    begin try
        let p = Scheduler.schedule p in
        Netlist_printer.print_program out p;
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
            close_all (); exit 2
    end;
    close_all ();
    simulator (Scheduler.schedule p)
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-print", Arg.Set print_only, "Only print the result of scheduling";
     "-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()
