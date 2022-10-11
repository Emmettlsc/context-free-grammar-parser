type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* -----1----- *)

let rec hw1_to_hw2 gram1 fill arg = match gram1 with
   | [] -> fill
   | hd :: tl -> if (fst hd) = arg then hw1_to_hw2 tl (fill@[(snd hd)]) arg else hw1_to_hw2 tl fill arg;;

let convert_grammar gram1 = (fst gram1, hw1_to_hw2 (snd gram1) []);;

(* snd output of convert_grammar will give a func *)


(* -----2----- *)

type ('nonterminal, 'terminal) parse_tree =
   | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
   | Leaf of 'terminal;;

   
let rec dfs tree = match tree with
   | Leaf x -> [x]
   | Node (x,y)  -> dfs_list y
and
dfs_list y = match y with 
        | [] -> []
        | hd :: tl -> (dfs hd) @ (dfs_list tl);;

let parse_tree_leaves tree = dfs tree;;


(* -----3----- *)
  

(* handles lists of rhs *) (* start here *)
let rec list_of_rhs_expand rhs_list func acceptor frag = match rhs_list with
   | [] -> None (* nothing to match with *)
   | hd :: tl -> match (single_rhs_expand hd func acceptor frag) with
                 | None -> list_of_rhs_expand tl func acceptor frag
                 | Some x -> Some x
and
single_rhs_expand rhs func acceptor frag = match rhs with (* handles a single rhs *)
   | [] -> acceptor frag
   | hd :: tl -> match hd with (* use matcher2 as acceptor for matcher1, appends the 2 (1st is hd, 2nd matches the rest of the rule *)
                        | N x -> list_of_rhs_expand (func x) func (fun x -> single_rhs_expand tl func acceptor x) frag 
			| T y -> if frag = [] then None
			else if y = (List.hd frag) then (match_helper tl func acceptor (List.tl frag))
			         else None (* If terminal is not a match *)                
and
match_helper rhs_tl func acceptor frag_tl = if( List.length frag_tl >= List.length rhs_tl) then
   match (List.length rhs_tl) with
      | 0 -> acceptor (frag_tl)
      | _ -> single_rhs_expand (rhs_tl) func acceptor (frag_tl)
      else None (* rule is too long for it to work *)
   
let make_matcher gram acceptor frag = list_of_rhs_expand ((snd gram) (fst gram)) (snd gram) acceptor frag;;  
		 
		
(* -----4----- *)

(* allows the tree to recursivly bubble up *) 
let parse_acceptor suffix tree = match suffix with (* acceptor for parser, must be exact match *)
   | [] -> Some tree
   | _ -> None 

   
(* handles lists of rhs *) (* start here *) (* need to keep track of tree (tree) and last nonterm (parent) *)
let rec new_list_of_rhs_expand rhs_list func parent acceptor frag tree = match rhs_list with
   | [] -> None (* nothing to match with *)
   | hd :: tl -> match (new_single_rhs_expand hd func parent acceptor frag tree) with
                 | None -> new_list_of_rhs_expand tl func parent acceptor frag tree 
                 | Some x -> Some x 
and
new_single_rhs_expand rhs func parent acceptor frag tree = match rhs with (* handles a single rhs *)
   | [] -> acceptor frag (Node (parent, tree))
   | hd :: tl -> match hd with (* use matcher2 as acceptor for matcher1, appends the 2 (1st is hd, 2nd matches the rest of the rule *)
			| N x ->  new_list_of_rhs_expand (func x) func x (fun f t -> (new_single_rhs_expand tl func parent acceptor f (tree @ [t]))) frag [] 
                        | T y -> if frag = [] then None
                        else if y = (List.hd frag) then (new_match_helper tl func parent acceptor (List.tl frag) (tree @ [Leaf y]))
                                 else None (* If terminal is not a match *)
and
new_match_helper rhs_tl func parent acceptor frag_tl tree = if( List.length frag_tl >= List.length rhs_tl) then
   match (List.length rhs_tl) with
      | 0 -> acceptor (frag_tl) (Node (parent, tree))
      | _ -> new_single_rhs_expand (rhs_tl) func parent acceptor (frag_tl) tree 
      else None (* rule is too long for it to work *)

let new_make_matcher gram acceptor frag = new_list_of_rhs_expand ((snd gram) (fst gram)) (snd gram) (fst gram) acceptor frag []

let make_parser gram frag = new_make_matcher gram parse_acceptor frag
