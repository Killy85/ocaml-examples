open naturel;;

suc suc 0 ;;

let rec sumlist l = match l with
  | [] -> 0
  | h::t -> h + (sumlist t);;

  let somme liste =
    let rec aux acc l = match l with
     | []    -> acc
     | x::xs -> aux (acc + x) xs
    in aux 0 liste
  ;;

  let rev_list l =
    let rec rev_acc acc = function
      | [] -> acc
      | hd::tl -> rev_acc (hd::acc) tl
    in
    rev_acc [] l

let rec mullist l = match l with
    | [] -> 0
    | h::t -> h * (mullist t);;

let functest a  b  c = a + (b *c );;

let compo a b c = a(b(c));;

let carre x = x*x;;

let cube x = carre(x) * x;;

 let rec plus_term x y =
      if est_nul(y)
      then x
      else plus_term (x+1) (y-1) ;;


      let est_nul x = if x = 0 then true else false;;

    let rec est_pair_term n =
      if est_nul(n)
        then
          true
        else
          if est_nul(pred(n))
          then
            false
          else
            est_pair_term(pred(pred(n)));;
(*)define integer

zero : unit -> naturel

succ : naturel -> naturel


*)
