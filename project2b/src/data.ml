open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree ;;

let empty_int_tree = IntLeaf;;

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, None, IntLeaf, IntLeaf, IntLeaf)
  | IntNode (value1, None, left, middle, right) -> if value1 = x then t else if value1 > x then IntNode (x, Some value1, left, middle, right) else IntNode (value1, Some x, left, middle, right)
  | IntNode (value1, Some value2, left, middle, right) -> if value1 = x then t else if x < value1 then IntNode (value1, Some value2, int_insert x left, middle, right) else if Some x > Some value2 then IntNode (value1, Some value2, left, middle, int_insert x right) else IntNode (value1, Some value2, left, int_insert x middle, right);;

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (value1, None, left, middle, right) -> if value1 = x then true else false
  | IntNode (value1, Some value2, left, middle, right) -> if value1 = x then true else if Some value2 = Some x then true else if x < value1 then int_mem x left else if Some x > Some value2 then int_mem x right else int_mem x middle;;


let rec int_size t =
  match t with
  | IntLeaf -> 0
  | IntNode (_, None, left, middle, right) -> int_size left + 1
  | IntNode (_, _, left, middle, right) -> int_size left + int_size middle + int_size right + 2;;

let rec int_max t =
  match t with
  | IntLeaf -> raise (Invalid_argument("int_max"))
  | IntNode (value1, None, left, middle, right) -> value1
  | IntNode (value1, Some value2, left, middle, right) -> if right = empty_int_tree then value2 else int_max right;;

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map;;

let empty_tree_map = MapLeaf;;

let rec map_put k v t = 
  match t with
  | MapLeaf -> MapNode((k,v), None, MapLeaf, MapLeaf, MapLeaf)
  | MapNode ((key1, value1), None, left, middle, right) -> if key1 = k then raise (Invalid_argument("map_put")) else if key1 > k then MapNode ((k,v), Some (key1, value1), left, middle, right) else MapNode ((key1, value1), Some (k, v), left, middle, right)
  | MapNode ((key1, value1), Some (key2, value2), left, middle, right) -> if key1 = k then raise (Invalid_argument("map_put")) else if k < key1 then MapNode ((key1, value1), Some (key2, value2), map_put k v left, middle, right) else if Some k > Some key2 then MapNode ((key1, value1), Some (key2, value2), left, middle, map_put k v right) else MapNode ((key1, value1), Some (key2, value2), left, map_put k v middle, right);;


let rec map_contains k t = 
  match t with
  | MapLeaf -> false
  | MapNode ((key1, value1), None, left, middle, right) -> if key1 = k then true else false
  | MapNode ((key1, value1), Some (key2, value2), left, middle, right) -> if key1 = k then true else if k < key1 then map_contains k left else if Some k > Some key2 then map_contains k right else map_contains k middle;;


let rec map_get k t =
  match t with
  | MapLeaf -> raise (Invalid_argument("map_get"))
  | MapNode ((key1, value1), None, left, middle, right) -> if key1 = k then value1 else raise (Invalid_argument("map_get"))
  | MapNode ((key1, value1), Some (key2, value2), left, middle, right) -> if key1 = k then value1 else if key2 = k then value2 else if k < key1 then map_get k left else if Some k > Some key2 then map_get k right else map_get k middle;;


(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = 
  | TableLeaf
  | TableNode of (string * int) list * lookup_table;;

let empty_table : lookup_table = TableLeaf;;

let push_scope (table : lookup_table) : lookup_table = 
  TableNode ([], table);;

let pop_scope (table : lookup_table) : lookup_table =
  match table with
  |TableLeaf -> failwith "No scopes remain!"
  |TableNode(lst, t) -> t;;

let rec contains_table lst x=
  match lst with
  |[] -> false
  |h::t -> match h with
          |(n,v) -> if n = x then true else contains_table t x;;

let add_var name value (table : lookup_table) : lookup_table =
  match table with
  |TableLeaf -> failwith "There are no scopes to add a variable to!"
  |TableNode(lst, t) -> if contains_table lst name then failwith "Duplicate variable binding in scope!" else TableNode ((name, value)::lst, t);;

let rec get_table lst x=
  match lst with
  |[] -> -1
  |h::t -> match h with
          |(n,v) -> if n = x then v else get_table t x;;

let rec lookup name (table : lookup_table) =
  match table with
  |TableLeaf -> failwith "Variable not found!"
  |TableNode ([],_) -> failwith "Variable not found!"
  |TableNode(lst, t) -> if contains_table lst name then get_table lst name else lookup name t;;

