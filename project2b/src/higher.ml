open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = 
    fold (fun counter head -> if head = e then true else counter) false lst;;

let is_present lst x = 
    map (fun head -> if head = x then 1 else 0) lst;;

let count_occ lst target = 
     fold (fun counter head -> if target = head then counter + 1 else counter) 0 lst;;

let uniq lst =
    let func_helper lst e = if contains_elem lst e then lst else e::lst in
    rev (fold func_helper [] lst);;

let assoc_list lst = 
     map (fun a -> (a, count_occ lst a)) (uniq lst);;

let concat lst = 
    let append lst e = fold_right (fun tail counter -> tail::counter) lst e in 
    fold_right (fun tail counter -> append tail counter) lst [];;

let ap fns args = 
    concat (map (fun fn -> map fn args) fns);;




