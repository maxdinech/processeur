exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

let node_for_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let rec union a b =
match a with
|[] -> b
|t::q when List.mem t b -> union q b
|t::q -> t::union q b

let has_cycle g =
let roots = find_roots g in
if roots = [] then true
else begin
let rec test_sommet s avoir vu=
match avoir with
|[] -> false
|n::q -> (n.n_label = s.n_label) || (test_sommet s (union q (List.filter (fun n -> not (List.mem n vu)) n.n_link_to)) (n::vu))
in
let rec test_graph l =
match l with
|[] -> false
|s::q when List.mem s roots -> test_graph q
|s::q -> (test_sommet s s.n_link_to []) || (test_graph q)
in test_graph g.g_nodes
end

let topological g =
let rec superieur avoir vu n2 =
match avoir with
|[] -> false
|n::_ when List.mem n2 n.n_link_to -> true
|n::q -> superieur (q @ (List.filter (fun n -> not (List.mem n vu)) n.n_link_to )) (n::vu) n2
in
let rec insere x l =
match l with
|[] -> [x.n_label]
|t::q when not( superieur [x] [] (node_for_label g t) )-> t::insere x q
|_ -> x.n_label::l
in
let rec trie l =
match l with
|[] -> []
|t::q -> insere t (trie q)
in
trie g.g_nodes
