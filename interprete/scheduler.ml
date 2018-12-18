open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp eq =
let (x,e) = eq in
let aux1 a =
match a with
|Avar(y) -> [y]
|_-> []
in
match e with
|Earg(a) -> aux1 a
|Ereg(y) -> []
|Enot(a) -> aux1 a
|Ebinop(_,a1,a2) -> (aux1 a1)@(aux1 a2)
|Emux(a1,a2,a3) -> (aux1 a1)@(aux1 a2)@(aux1 a3)
|Erom(_,_,a) -> aux1 a
|Eram(_,_,a1,a2,a3,a4) -> (aux1 a1)@(aux1 a2)
|Econcat(a1,a2) -> (aux1 a1)@(aux1 a2)
|Eslice(_,_,a) -> aux1 a
|Eselect(_,a) -> aux1 a

let rec print_list l =
match l with
|[] -> print_newline()
|t::q -> print_string t;print_string " ; ";print_list q

let schedule p =
let rec fusion l1 l2 =
match l1 with
|[] -> l2
|t::q when List.mem t l2 -> fusion q l2
|t::q -> fusion q (t::l2)
in
let rec enumere_var l_eq =
match l_eq with
|[] -> []
|(x,e)::q -> fusion (x::(read_exp (x,e))) (enumere_var q)
in
let g = Graph.mk_graph () in
let rec creation_sommets l =
match l with
|[] -> ()
|t::q -> Graph.add_node g t;
        creation_sommets q
in
let l_var = enumere_var p.p_eqs in
creation_sommets (l_var);
let rec creation_aretes_eq x l =
match l with
|[] -> ()
|t::q -> Graph.add_edge g t x;
        creation_aretes_eq x q
in
let rec creation_aretes l_eq =
match l_eq with
|[] -> ()
|(x,e)::q -> creation_aretes_eq x (read_exp (x,e)); creation_aretes q
in
creation_aretes p.p_eqs;
(*affichage*)
(*print_list (List.map (fun n -> n.n_label ) (node_for_label g "r").n_link_to);*)
if Graph.has_cycle g then raise Combinational_cycle;
let l_var = Graph.topological g in
print_list l_var;
let rec trouve_eq l_eq x =
match l_eq with
|(y,e)::q when x=y -> ((y,e),q)
|t::q -> let (eq,l)=trouve_eq q x in
        (eq,t::l)
in
let rec ordonne_eq l_eq l_var =
match l_var with
|[] -> []
|x::q when List.mem x p.p_inputs -> ordonne_eq l_eq q
|x::q -> let (eq,l) = trouve_eq l_eq x in
        eq::ordonne_eq l q
in
print_list l_var;
{p_eqs = ordonne_eq p.p_eqs l_var ; p_inputs = p.p_inputs ; p_outputs = p.p_outputs ; p_vars = p.p_vars}
