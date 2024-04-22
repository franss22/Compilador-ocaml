(* Generates a list from start to n-1 *)
let rec range (start:int) (n:int) : int list =
  if start < n
    then start::(range (start+1) n)
    else []

  (* Checks if there are duplicate elements in a list *)
let rec dup_exist = function
  | [] -> false
  | hd::tl -> List.exists ((=) hd) tl || dup_exist tl

(* Takes the first n elements of a list *)
let rec take (ls : 'a list)  (n : int) : 'a list = 
  match ls with
  | [] -> []
  | h::t -> if n > 0
    then h::(take t (n-1))
    else []

let member (a:'a) (ls:'a list):bool =
  List.exists ((=) a) ls

let rec remove_duplicates (ls: 'a list):'a list =
  match ls with
  | [] -> []
  | elem::tail -> 
    if (member elem tail)
      then remove_duplicates tail
      else elem::(remove_duplicates tail)